/* Copyright 2016-2017 TensorHub, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

var Guild = Guild || {};

Guild.CompareTable = new function() {

    var TIME_FORMAT = d3.time.format("%b %d %H:%M:%S");

    var fieldsDataSource = function(fields) {
        var sources = new Set();
        fields.forEach(function(field) {
            field.sources.split(",").forEach(function(source) {
                sources.add(source);
            });
        });
        if (sources.size > 0) {
            return "compare?sources=" + Array.from(sources).join(",");
        } else {
            return "compare";
        }
    };

    var init = function(table, fields) {
        return jQuery(table).DataTable({
            data: [],
            columns: columns(fields),
            order: [[1, 'desc']],
            scrollY: "360px",
            scrollCollapse: true,
            paging: false,
            language: {
                info: "_TOTAL_ runs",
                infoFiltered: " (filtered from _MAX_)",
                infoEmpty: "_TOTAL_ runs",
                search: "",
                searchPlaceholder: "Filter",
                zeroRecords: "Waiting for data..."
            },
            dom: "<'row'<'col-sm-12'f>>" +
                "<'row'<'col-sm-12'tr>>" +
                "<'row'<'col-sm-12'i>>"
        });
    };

    var columns = function(fields) {
        return baseCols().concat(fieldCols(fields));
    };

    var baseCols = function() {
        return [
            //selectedCol(),
            statusCol(),
            timeCol(),
            modelCol()
        ];
    };

    var selectedCol = function() {
        return {
            title: "Selected",
            orderable: false
        };
    };

    var statusCol = function() {
        return {
            title: "",
            data: "status",
            width: "20px",
            render: {
                display: function(status) {
                    return statusIcon(status);
                },
                sort: "sort",
                filter: "label"
            }
        };
    };

    var statusIcon = function(status) {
        return "<fa-awesome class='" + status.iconClass + "'"
            + maybeSpinAttr(status.spin)
            + "' icon='" + status.icon
            + "' size='22'></fa-awesome>"
            + "<paper-tooltip position='right'"
            + " animation-delay='250'>"
            + status.label + "</paper-tooltip>";
    };

    var maybeSpinAttr = function(spin) {
        return spin ? " spin" : "";
    };

    var timeCol = function() {
        return {
            title: "Time",
            data: "time",
            orderSequence: ["desc", "asc"],
            width: "8em",
            render: {
                display: function(time, _type, row) {
                    return runLink(time.value, row.run);
                },
                sort: "sort"
            }
        };
    };

    var runLink = function(val, run) {
        var link = "/?run=" + run.id;
        return "<a href=\"javascript:window.location='"
            + link + "'\" class='date'>" + val + "</a>";
    };

    var modelCol = function() {
        return {
            title: "Model",
            data: "run",
            render: {
                display: "model",
                sort: "model",
                filter: "model"
            }
        };
    };

    var fieldCols = function(fields) {
        return fields.map(function(field, index) {
            return {
                title: field.label,
                data: "f" + index,
                orderSequence: ["desc", "asc"],
                type: fieldType(field),
                render: function(field, type) {
                    if (type == "sort") {
                        return field.sort;
                    } else {
                        return field.value;
                    }
                },
                render_: {
                    display: "value",
                    sort: "sort",
                    filter: "value"
                }
            };
        });
    };

    var fieldType = function(field) {
        // Infer numeric type by reduce function
        return field.reduce ? "num" : "string";
    };

    var refresh = function(dt, data, fields) {
        var items = formatItems(data, fields);
        items.forEach(function(item) {
            var row = dt.row.add(item);
            row.draw();
        });
        //deleteMissingRows(dt, rows);
        //addOrUpdateRows(dt, rows);
    };

    var formatItems = function(data, fields) {
        return data.map(function(item, index) {
            return Object.assign(
                itemBase(item, index),
                itemFields(item, fields));
        });
    };

    var itemBase = function(item, index) {
        return {
            run: item.run,
            time: formatTime(item.run.started),
            status: runStatus(item.run),
            index: index
        };
    };

    var runStatus = function(run) {
        var status = Guild.Run.runStatus(run);
        status.sort = statusSort(status);
        return status;
    };

    var statusSort = function(status) {
        var label = status.label;
        if (label == "Running") {
            return 0;
        } else if (label == "Completed") {
            return 1;
        } else if (label == "Terminated") {
            return 2;
        } else if (label == "Error") {
            return 3;
        } else {
            return 4;
        }
    };

    var formatTime = function(epoch) {
        return {
            value: TIME_FORMAT(new Date(epoch)),
            sort: epoch
        };
    };

    var itemFields = function(item, fieldDefs) {
        var fields = {};
        fieldDefs.forEach(function(field, index) {
            // field.sources here is spelled correctly - we're assuming
            // for the time being that field.sources will only every be
            // a single source and can be used to lookup corresponding
            // data in item.
            var data = item[field.sources];
            var name = "f" + index;
            fields[name] = fieldValue(data, field);
        });
        return fields;
    };

    var fieldValue = function(data, field) {
        var raw = fieldRawValue(data, field);
        var sort = raw == undefined ? null: raw;
        var formatted = fieldFormattedValue(raw, field) || "";
        return {sort: sort, value: formatted};
    };

    var fieldRawValue = function(data, field) {
        if (field.attribute) {
            return data[field.attribute];
        } else if (field.reduce) {
            var reduce = Guild.Reduce[field.reduce];
            if (reduce) {
                var reduced = reduce(data);
                return reduced[Object.keys(reduced)[0]];
            }
        }
        return undefined;
    };

    var fieldFormattedValue = function(raw, field) {
        return field.format
            ? Guild.Util.tryFormat(raw, field.format)
            : raw;
    };

    var deleteMissingRows = function(dt, rows) {
        var ids = {};
        rows.map(function(row) { ids[row[0].id] = null; });
        var missing = [];
        dt.rows().every(function(index) {
            var id = dt.row(index).data()[0].id;
            if (!(id in ids)) {
                missing.push(index);
            }
        });
        if (missing.length > 0) {
            dt.rows(missing).remove().draw();
        }
    };

    var addOrUpdateRows = function(dt, rows) {
        for (var i in rows) {
            var newRowData = rows[i];
            var curRowIndex = findTableRow(dt, newRowData[0].id);
            if (curRowIndex != null) {
                updateRowData(dt, curRowIndex, newRowData);
            } else {
                var newRow = dt.row.add(newRowData);
                newRow.draw();
            }
        }
        dt.columns.adjust();
    };

    var initRows = function(data, coldefs) {
        return data.map(function(item) { return initRow(item, coldefs); });
    };

    var initRow = function(item, coldefs) {
        var row = [];
        row.push(item.run);
        for (var i in coldefs) {
            row.push(coldefValue(coldefs[i], item));
        }
        return row;
    };

    var updateRowData = function(table, rowIndex, newData) {
        for (var i = 0; i < newData.length; i++) {
            var curVal = table.cell(rowIndex, i).data();
            var newVal = newData[i];
            if (rowCellChanged(i, curVal, newVal)) {
                table.cell(rowIndex, i).data(newVal);
            }
        }
    };

    var rowCellChanged = function(index, curVal, newVal) {
        if (index == 0) {
            return curVal.status != newVal.status;
        } else {
            return curVal != newVal;
        }
    };

    this.fieldsDataSource = fieldsDataSource;
    this.init = init;
    this.refresh = refresh;
};
