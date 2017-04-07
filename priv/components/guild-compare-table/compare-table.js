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

    var init = function(table, fields, options) {
        return jQuery(table).DataTable({
            data: [],
            rowId: "run.id",
            columns: columns(fields),
            order: [[2, 'desc']],
            scrollY: (options && options.height) || "360px",
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
            dom: "<'row'<'col-12'f>>"
                + "<'row'<'col-12'tr>>"
                + "<'row'<'col-12'i>>"
        });
    };

    var columns = function(fields) {
        return baseCols().concat(fieldCols(fields));
    };

    var baseCols = function() {
        return [
            selectedCol(),
            statusCol(),
            timeCol(),
            modelCol()
        ];
    };

    var selectedCol = function() {
        return {
            title: "<guild-compare-table-select header></guild-compare-table-select>",
            data: null,
            orderable: false,
            width: "18px",
            render: {
                display: function(item) {
                    return tableSelect();
                }
            }
        };
    };

    var tableSelect = function() {
        return "<guild-compare-table-select></guild-compare-table-select>";
    };

    var statusCol = function() {
        return {
            title: statusTitle(),
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

    var statusTitle = function() {
        return "<span class='header-title status-title'></span>";
    };

    var statusIcon = function(status) {
        return "<fa-awesome class='" + status.iconClass + "'"
            + maybeSpinAttr(status.spin)
            + " icon='" + status.icon
            + "' size='22'></fa-awesome>"
            + "<paper-tooltip position='right'"
            + " animation-delay='250' offset='0'>"
            + status.label + "</paper-tooltip>";
    };

    var maybeSpinAttr = function(spin) {
        return spin ? " spin" : "";
    };

    var timeCol = function() {
        return {
            title: headerTitle("Time"),
            data: "time",
            orderSequence: ["desc", "asc"],
            width: "8em",
            render: {
                display: function(time, _type, row) {
                    return runLink(time.value, row.run);
                },
                sort: "sort",
                filter: "value"
            }
        };
    };

    var headerTitle = function(title) {
        return "<span class='header-title'>" + title + "</span>";
    };

    var runLink = function(val, run) {
        var link = "/?run=" + run.id;
        return "<a href=\"javascript:window.location='"
            + link + "'\" class='date'>" + val + "</a>";
    };

    var modelCol = function() {
        return {
            title: headerTitle("Model"),
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
                title: headerTitle(field.label),
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
        deleteMissingRows(dt, items);
        addOrUpdateRows(dt, items);
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
            index: index,
            selected: false
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

    var deleteMissingRows = function(dt, items) {
        var itemIds = itemIdLookup(items);
        var missing = [];
        dt.rows().every(function(index) {
            if (!itemIds.has(dt.row(index).data().run.id)) {
                missing.push(index);
            }
        });
        if (missing.length > 0) {
            dt.rows(missing).remove().draw();
        }
    };

    var itemIdLookup = function(items) {
        var ids = items.map(function(item) { return item.run.id; });
        return new Set(ids);
    };

    var addOrUpdateRows = function(dt, items) {
        var added = false;
        items.forEach(function(item, index) {
            var curRow = findRow(dt, item);
            if (curRow != null) {
                updateRow(dt, curRow, item);
            } else {
                addRow(dt, item);
                added = true;
            }
        });
        if (added) {
            dt.columns.adjust();
        }
    };

    var findRow = function(dt, target) {
        var row = dt.row("#" + target.run.id);
        return row.data() != undefined ? row : null;
    };

    var updateRow = function(dt, row, newItem) {
        var curItem = row.data();
        dt.columns().every(function(colIndex) {
            var property = dt.column(colIndex).dataSrc();
            var curVal = curItem[property];
            var newVal = newItem[property];
            if (itemValChanged(curVal, newVal)) {
                dt.cell(row, colIndex).data(newVal);
            }
        });
    };

    var itemValChanged = function(a, b) {
        return JSON.stringify(a) != JSON.stringify(b);
    };

    var rowCellChanged = function(index, curVal, newVal) {
        if (index == 0) {
            return curVal.status != newVal.status;
        } else {
            return curVal != newVal;
        }
    };

    var addRow = function(dt, item) {
        var row = dt.row.add(item);
        row.draw();
    };

    var highlightSelectedRows = function() {
        console.log("TODO: highlight selected yo");
    };

    this.fieldsDataSource = fieldsDataSource;
    this.init = init;
    this.refresh = refresh;
    this.highlightSelected = highlightSelectedRows;
};
