/********************************************************************
 * Run select
 ********************************************************************/

RUN_STATUS = "run_status";

function initRunSelect(widget, state) {
    widget.on("changed.bs.select", function (e) {
        window.location = "?run=" + $(this).val();
    });
    scheduleRunSelectUpdate(widget, state);
}

function scheduleRunSelectUpdate(widget, state) {
    scheduleWidgetData("/data/runs", widget, updateRunSelect, state);
}

function updateRunSelect(widget, runs, state) {
    state.run = selectedRun(runs, state);
    notify(RUN_STATUS, state.run, state);
    refreshRunSelect(widget, runs, state.run);
    scheduleRunSelectUpdate(widget, state);
}

function selectedRun(runs, state) {
    var runId = state.run == null ? state.selectedRun : state.run.id;
    return findRun(runId, runs);
}

function findRun(id, runs) {
    if (id) {
        for (var i in runs) {
            var run = runs[i];
            if (run.id == id) {
                return run;
            }
        }
    } else {
        if (runs.length > 0) {
            return runs[0];
        }
    }
    return null;
}

function refreshRunSelect(widget, runs, selected) {
    var changed = false;
    for (var i = runs.length - 1; i >= 0; i--) {
        var run = runs[i];
        var curOpt = currentRunSelectOption(run.id);
        var isSelected = selected && run.id == selected.id;
        if (curOpt == null) {
            widget.prepend(selectOptionForRun(run, isSelected));
            changed = true;
        } else {
            var newOpt = selectOptionForRun(run, isSelected);
            if (runSelectOptionChanged(curOpt, newOpt)) {
                newOpt.insertBefore(curOpt);
                curOpt.remove();
                changed = true;
            }
        }
    }
    if (changed) {
        widget.selectpicker("refresh");
    }
}

function currentRunSelectOption(runId) {
    var option = document.getElementById("run-option-" + runId);
    return option != null ? $(option) : null;
}

function selectOptionForRun(run, isSelected) {
    var option = $("<option>");
    var label = runLabel(run);
    option.append(label);
    option.attr("id", "run-option-" + run.id);
    option.attr("value", run.id);
    option.attr("data-content", runOptionContent(label, run));
    if (isSelected) {
        option.attr("selected", "");
    }
    return option;
}

function runLabel(run) {
    var label = "";
    if (run.started) {
        var started = new Date(run.started);
        label += started.toDateString() + ", " + started.toLocaleTimeString();
    }
    if (run.model) {
        if (label) {
            label += " - ";
        }
        label += run.model;
    }
    return label;
}

function runOptionContent(label, run) {
    var uiAttrs = runStatusUIAttrs(run.status, run.exit_status);
    var iconCls = "fa fa-" + uiAttrs.icon;
    if (uiAttrs.spin) {
        iconCls += " fa-spin";
    }
    return "<span><i class='" + iconCls + "' style='margin-right:10px'></i>"
        + label + "</span>";
}

function runSelectOptionChanged(a, b) {
    return a.attr("data-content") != b.attr("data-content");
}

/********************************************************************
 * Run status UI rules
 ********************************************************************/

var runStatusUIRules = [
    [["running"],    {label: "Running",    color: "blue-600",  icon: "circle-o-notch", spin: true}],
    [["stopped", 0], {label: "Completed",  color: "green-700", icon: "check-circle-o"}],
    [["stopped"],    {label: "Error",      color: "red-800",   icon: "exclamation-triangle"}],
    [["crashed"],    {label: "Terminated", color: "red-800",   icon: "times-circle"}],
    [[],             {label: "--",         color: "grey-600",  icon: "question-circle-o"}]
];

function runStatusUIAttrs(status, exitCode) {
    for (var i in runStatusUIRules) {
        var rule = runStatusUIRules[i][0];
        var parts = rule.length;
        if ((parts == 0)
            || (parts == 1
                && rule[0] == status)
            || (parts == 2
                && rule[0] == status
                && rule[1] == exitCode))
        {
            return runStatusUIRules[i][1];
        }
    }
    throw "unreachable";
}

/********************************************************************
 * Value panel
 ********************************************************************/

function initValuePanel(widget, state) {
    scheduleValuePanelUpdate(widget, state);
}

function scheduleValuePanelUpdate(widget, state) {
    scheduleWidgetData(
        widgetRunSource(widget, state),
        widget, updateValuePanel, state);

}

function updateValuePanel(widget, data, state) {
    var value = widgetValue(widget, data);
    setPanelLabel(widget, value);
    scheduleValuePanelUpdate(widget, state);
}

function setPanelLabel(widget, label) {
    if (label != undefined) {
        widget.text(label);
    } else {
        widget.text("--");
    }
}

/********************************************************************
 * Flags
 ********************************************************************/

function initFlags(widget, state) {
    scheduleWidgetData(
        runSource("/data/flags", state),
        widget, updateFlags, state);
}

function updateFlags(widget, flags) {
    if (!flags) return;
    var split = splitFlags(flags);
    widget.empty();
    if (split.caption) {
        var caption = $("<caption>" + split.caption + "</caption>");
        widget.append(caption);
    }
    for (var name in split.flags) {
        var val = flags[name];
        var row = $("<tr><th scope=\"row\">"+ name + "</th>" +
                    "<td>" + val + "</td></tr>");
        widget.append(row);
    }
}

function splitFlags(flags) {
    var split = {};
    split.caption = null;
    split.flags = [];
    for (var name in flags) {
        if (name == "description") {
            split.caption = flags[name];
        } else {
            split.flags[name] = flags[name];
        }
    }
    return split;
}

/********************************************************************
 * Attrs
 ********************************************************************/

function initAttrs(widget, state) {
    scheduleWidgetData(
        runSource("/data/attrs", state),
        widget, updateAttrs, state);
}

function updateAttrs(widget, attrs) {
    if (!attrs) return;
    widget.empty();
    for (var name in attrs) {
        var val = attrs[name];
        var row = $("<tr><th scope=\"row\">"+ name + "</th>" +
                    "<td>" + val + "</td></tr>");
        widget.append(row);
    }
}

/********************************************************************
 * Status
 ********************************************************************/

function initStatus(widget, state) {
    registerWidgetCallback(RUN_STATUS, widget, updateStatus, state);
}

function updateStatus(widget, run) {
    if (!run) return;
    var attrs = runStatusUIAttrs(run.status, run.exit_status);
    if (widget.data("label") != attrs.label ) {
        var icon = statusIcon(attrs.icon, attrs.spin, attrs.color);
        widget.find("i.status-icon").replaceWith(icon);
        widget.find("span.status-label").text(attrs.label);
        widget.data("label", attrs.label);
    }
}

function statusIcon(icon, spin, color) {
    var iconClass = " fa-" + icon;
    var spinClass = spin ? " fa-spin" : "";
    var colorClass = " mdc-text-" + color;
    return "<i class=\"fa"
        + iconClass
        + spinClass
        + colorClass
        + " status-icon\"></i>";
}

/********************************************************************
 * Timeseries
 ********************************************************************/

function initTimeseries(widget, state) {
    var label = widget.attr("data-widget-label");
    var format = timeseriesFormatFun(widget);
    var chart = initTimeseriesChart(widget, label, format);
    initTimeseriesRedrawHandler(chart, widget, state);
    widget.data("chart", chart);
    scheduleTimeseriesUpdate(widget, state);
}

function timeseriesFormatFun(widget) {
    var format = widget.attr("data-widget-format");
    return format ? function(x) { return numeral(x).format(format); } : null;
}

function initTimeseriesChart(widget, label, formatFun) {
    var formatTime = d3.time.format("%H:%M:%S");
    var formatTimeTooltip = d3.time.format("%b %d %H:%M:%S.%L");
    return c3.generate({
        bindto: widget[0], // bind to underlying DOM element
        data: {
            xs: {},
            columns: []
        },
        size: { height: 320 },
        axis: {
            x: {
                type: 'timeseries',
                tick: {
                    count: 20,
                    format: function(time) {
                        return formatTime(new Date(time));
                    }
                },
                label: { text: "Time" }
            },
            y: {
                label: { text: label },
                tick: { format: formatFun }
            }
        },
        point: { show: true, r: 1.8 },
        legend: { show: true },
        tooltip: {
            format: {
                title: function(time) {
                    return formatTimeTooltip(new Date(time));
                }
            }
        },
        transition: { duration: 0 },
        subchart: {
            show: true,
            size: {
                height: 30
            }
        }
    });
}

function initTimeseriesRedrawHandler(chart, widget, state) {
    var handler = function(panel) {
        var parent = widget.closest(".panel");
        if (parent[0] == panel[0]) {
            if (panel.hasClass("panel-fullscreen")) {
                chart.resize({height: 640});
            } else {
                chart.resize({height: 320});
            }
        }
    };
    registerCallback(FULL_SCREEN_TOGGLE, handler, state);
}

function scheduleTimeseriesUpdate(widget, state) {
    scheduleWidgetData(
        widgetRunSource(widget, state),
        widget, updateTimeseries, state);
}

function updateTimeseries(widget, data, state) {
    var chart = widget.data("chart");
    xs = timeseriesXs(data);
    columns = timeseriesCols(data);
    chart.load({xs: xs, columns: columns});
    scheduleTimeseriesUpdate(widget, state);
}

function timeseriesXs(data) {
    var xs = {};
    for (var name in data) {
        xs[name] = "t:" + name;
    }
    return xs;
}

function timeseriesCols(data) {
    var cols = [];
    var sortedNames = data != null ? Object.keys(data).sort() : [];
    for (var i in sortedNames) {
        var name = sortedNames[i];
        var series = data[name];
        var times = ["t:" + name];
        var vals = [name];
        cols.push(times);
        cols.push(vals);
        for (var j in series) {
            times.push(series[j][0]);
            vals.push(series[j][2]);
        }
    }
    return cols;
}

/********************************************************************
 * Output
 ********************************************************************/

function initOutput(widget, state) {
    initOutputDataTable(widget);
    scheduleOutputUpdate(widget, state);
}

function initOutputDataTable(widget) {
    // Setting data on a data table directly fails - something appears
    // to reset the value - so we wrap the table in a div and set data
    // there.
    var formatTime = d3.time.format("%b %d %H:%M:%S.%L");
    var table = $("<table class=\"output table table-sm table-hover\" width=\"100%\"></table>");
    widget.append(table);
    var dataTable = table.DataTable({
        data: [],
        columns: [
            { title: "Time",
              data: function(row) {
                  return new Date(row[0]);
              },
              render: function(val) {
                  return formatTime(val);
              },
              width: "8em"
            },
            { visible: false },
            { title: "Message", orderable: false }
        ],
        scrollY: "360px",
        scrollCollapse: true,
        paging: false,
        deferRender: true,
        language: {
            info: "_TOTAL_ events",
            infoFiltered: " (filtered from _MAX_)",
            infoEmpty: "_TOTAL_ events",
            search: "",
            searchPlaceholder: "Filter",
            zeroRecords: "No matching events"
        },
        dom: "<'row'<'col-sm-12'f>>" +
             "<'row'<'col-sm-12'tr>>" +
             "<'row'<'col-sm-12'i>>",
        rowCallback: function(row, data) {
            if (data[1] == 1) {
                $(row).addClass("stderr");
            }
        }
    });
    widget.data("dataTable", dataTable);
    widget.data("lastTime", 0);
}

function scheduleOutputUpdate(widget, state) {
    scheduleWidgetData(
        runSource("/data/output", state),
        widget, updateOutput, state);
}

function updateOutput(widget, output, state) {
    appendNewOutput(widget, output);
    scheduleOutputUpdate(widget, state);
}

function appendNewOutput(widget, output) {
    if (!output) {
        return;
    }
    var lastTime = widget.data("lastTime");
    var dataTable = widget.data("dataTable");
    var nextRow = findNextOutputRow(output, lastTime);
    dataTable.rows.add(output.slice(nextRow));
    dataTable.draw("full-hold");
    widget.data("lastTime", lastOutputTime(output));
}

function findNextOutputRow(output, time) {
    var len = output.length;
    for (var i = 0; i < len; i++) {
        if (output[i][0] > time) {
            return i;
        }
    }
    return len;
}

function lastOutputTime(output) {
    return (output != null && output.length > 0)
        ? output[output.length - 1][0]
        : 0;
}

/********************************************************************
 * Compare table
 ********************************************************************/

function initCompareTable(widget, state) {
    initCompareDataTable(widget);
    scheduleCompareTableUpdate(widget, state);
}

function initCompareDataTable(widget) {
    var coldefs = compareTableColdefs(widget);
    var columns = compareTableColumns(coldefs);
    var dataTable = widget.DataTable({
        data: [],
        columns: columns,
        order: [[0, 'desc']],
        scrollY: "360px",
        scrollCollapse: true,
        paging: false,
        deferRender: true,
        language: {
            info: "_TOTAL_ runs",
            infoFiltered: " (filtered from _MAX_)",
            infoEmpty: "_TOTAL_ runs",
            search: "",
            searchPlaceholder: "Filter",
            zeroRecords: "No runs"
        },
        dom: "<'row'<'col-sm-12'f>>" +
            "<'row'<'col-sm-12'tr>>" +
            "<'row'<'col-sm-12'i>>"
    });
    widget.data("dataTable", dataTable);
    widget.data("coldefs", coldefs);
}

function compareTableColdefs(widget) {
    var coldefsId = widget.attr("data-coldefs");
    var coldefsEl = document.getElementById(coldefsId);
    if (coldefsEl == null) {
        return [];
    }
    var coldefs = [];
    $(".coldef", coldefsEl).each(function() {
        var coldefEl = $(this);
        coldefs.push({
            title: coldefEl.attr("data-title"),
            source: coldefEl.attr("data-source"),
            attribute: coldefEl.attr("data-attribute"),
            reduce: coldefEl.attr("data-reduce"),
            format: coldefEl.attr("data-format")
        });
    });
    return coldefs;
}

function compareTableColumns(coldefs) {
    var columns = [
        compareTableRunCol(),
        compareTableStatusCol(),
    ];
    for (var i = 0; i < coldefs.length; i++) {
        columns.push(compareTableCol(coldefs[i]));
    }
    return columns;
}

function compareTableRunCol() {
    return {
        title: "Run",
        orderSequence: ["desc", "asc"],
        render: {
            display: function(run) {
                return "<a href=\"/?run=" + run.id + "\">" + runLabel(run) + "</a>";
            },
            sort: function(run) {
                return run.started;
            }
        }
    };
}

function compareTableStatusCol() {
    return {
        orderSequence: ["asc", "desc"],
        title: "Status"
    };
}

function compareTableCol(coldef) {
    return {
        title: coldef.title,
        orderSequence: ["desc", "asc"],
        render: {
            display: compareTableColRenderer(coldef)
        }
    };
}

function compareTableColRenderer(coldef) {
    var format = coldef.format;
    return function(val) {
        if (val == null || val == undefined) {
            return "--";
        } else if (format) {
            return tryFormat(val, format);
        } else {
            return val;
        }
    };
}

function scheduleCompareTableUpdate(widget, state) {
    scheduleWidgetData(
        compareDataSource(widget),
        widget, updateCompareTable, state);
}

function compareDataSource(widget) {
    var coldefs = widget.data("coldefs");
    var sources = coldefs.map(function(coldef) {
        return coldef.source;
    });
    return "/data/compare?sources=" + sources.join();
}

function updateCompareTable(widget, data, state) {
    var table = widget.data("dataTable");
    var coldefs = widget.data("coldefs");
    var rows = compareDataRows(data, coldefs);
    table.rows.add(rows);
    table.columns.adjust();
    table.draw();
}

function compareDataRows(data, coldefs) {
    var rows = [];
    for (var i = 0; i < data.length; i++) {
        var run = data[i].run;
        var row = [run, runStatus(run)];
        rows.push(row);
        for (var j in coldefs) {
            row.push(coldefValue(coldefs[j], data[i]));
        }
    }
    return rows;
}

function runStatus(run) {
    var attrs = runStatusUIAttrs(run.status, run.exit_status);
    return attrs.label;
}

function coldefValue(coldef, data) {
    var rawVal = data[coldef.source];
    var widgetProxy = coldefWidgetProxy(coldef);
    var val = widgetReduce(widgetProxy, widgetAttr(widgetProxy, rawVal));
    return ensureLegalDataTableVal(ensureFormattable(val));
}

function coldefWidgetProxy(coldef) {
    // Lets us treat a coldef like a widget for calculating values
    return {
        attr: function(name) {
            if (name == "data-widget-attribute") {
                return coldef.attribute;
            } else if (name == "data-widget-reduce") {
                return coldef.reduce;
            } else {
                return undefined;
            }
        }
    };
}

function ensureLegalDataTableVal(val) {
    return val != undefined ? val : null;
}
