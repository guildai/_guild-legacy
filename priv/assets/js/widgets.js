/********************************************************************
 * Value panel
 ********************************************************************/

function initValuePanel(widget, data) {
    updateValuePanel(widget, data);
}

function updateValuePanel(widget, data) {
    var value = valueForPanel(data);
    if (value != undefined) {
        widget.text(formatWidgetValue(value, widget));
    } else {
        widget.text("--");
    }
}

function valueForPanel(data) {
    if (typeof data === "object" && data != null) {
        // A reduced value from stats is provided as a dict of
        // name+values - use the first value since we can only show
        // one
        return data[Object.keys(data)[0]];
    } else {
        return data;
    }
}

function formatWidgetValue(value, widget) {
    var format = widget.attr("data-widget-format");
    return format ? formatValue(value, format) : value;
}


/********************************************************************
 * Flags
 ********************************************************************/

function initFlags(widget, flags) {
    if (!flags) return;
    widget.empty();
    for (var name in flags) {
        var val = flags[name];
        var row = $("<tr><th scope=\"row\">"+ name + "</th>" +
                    "<td>" + val + "</td></tr>");
        widget.append(row);
    }
}

/********************************************************************
 * Status
 ********************************************************************/

function initStatus(widget, _null, state) {
    updateStatus(widget, null, state);
}

function updateStatus(widget, _null, state) {
    var run = state.run;
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

function initTimeseries(widget, data) {
    var label = widget.attr("data-widget-label");
    var format = timeseriesFormatFun(widget);
    var chart = initTimeseriesChart(widget, label, format);
    initTimeseriesRedrawHandler(chart, widget);
    widget.data("chart", chart);
    updateTimeseries(widget, data);
}

function timeseriesFormatFun(widget) {
    var format = widget.attr("data-widget-format");
    if (format) {
        return function(x) {
            return numeral(x).format(format);
        };
    } else {
        return null;
    }
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

function initTimeseriesRedrawHandler(chart, widget) {
    SIGNALS.fullscreenToggle.add(function(panel) {
        var parent = widget.closest(".panel");
        if (parent[0] == panel[0]) {
            if (panel.hasClass("panel-fullscreen")) {
                chart.resize({height: 640});
            } else {
                chart.resize({height: 320});
            }
        }
    });
}

function updateTimeseries(widget, data) {
    var chart = widget.data("chart");
    xs = timeseriesXs(data);
    columns = timeseriesCols(data);
    chart.load({xs: xs, columns: columns});
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
    var sortedNames = Object.keys(data).sort();
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

function initOutput(widget, output) {
    // Setting data on a data table directly fails - something appears
    // to reset the value - so we wrap the table in a div and set data
    // there.
    var formatTime = d3.time.format("%b %d %H:%M:%S.%L");
    var table = $("<table class=\"output table table-sm table-hover\" width=\"100%\"></table>");
    widget.append(table);
    var dataTable = table.DataTable({
        data: output,
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
    widget.data("lastTime", lastOutputTime(output));
}

function lastOutputTime(output) {
    return (output != null && output.length > 0)
        ? output[output.length - 1][0]
        : 0;
}

function updateOutput(widget, output) {
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

/********************************************************************
 * Compare table
 ********************************************************************/

function initCompareTable(widget) {
    var coldefs = compareTableColdefs(widget);
    initCompareTableData(coldefs, widget);
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

function initCompareTableData(coldefs, widget) {
    var dataUrl = compareDataUrl(coldefs);
    var timeout = window.setTimeout(function() { blockUI("Loading data"); }, 1500);
    fetchGuildData(dataUrl, function(runs) {
        window.clearTimeout(timeout);
        unblockUI();
        var rows = compareDataRows(runs, coldefs);
        initCompareTableWidget(widget, coldefs, rows);
      });
}

function compareDataUrl(coldefs) {
    var sources = coldefs.map(function(coldef) {
        return coldef.source;
    });
    return "/data/compare?sources=" + sources.join();
}

function compareDataRows(runs, coldefs) {
    var rows = [];
    for (var i in runs) {
        var run = runs[i].run;
        var row = [[run.id, run.started, runLabel(run)], runStatus(run)];
        rows.push(row);
        for (var j in coldefs) {
            row.push(coldefValue(coldefs[j], runs[i]));
        }
    }
    return rows;
}

function runStatus(run) {
    var attrs = runStatusUIAttrs(run.status, run.exit_status);
    return attrs.label;
}

function coldefValue(coldef, data) {
    var source = data[coldef.source];
    return (
        ensureValidTableVal(
            valueForPanel(
                maybeReduce(
                    maybeAttr(source, coldef.attribute),
                    coldef.reduce))));
}

function ensureValidTableVal(val) {
    return val != undefined ? val : null;
}

function initCompareTableWidget(widget, coldefs, data) {
    var columns = compareTableColumns(coldefs);
    widget.DataTable({
        data: data,
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
}

function compareTableColumns(coldefs) {
    var columns = [
        { title: "Run",
          render: {
              display: function(val) {
                  var id = val[0];
                  var label = val[2];
                  return "<a href=\"/?run=" + id + "\">" + label + "</a>";
              },
              sort: function(val) {
                  var start = val[1];
                  return start;
              }
          }
        },
        { title: "Status" }
    ];
    for (var i in coldefs) {
        columns.push(compareTableColumn(coldefs[i]));
    }
    return columns;
}

function compareTableColumn(coldef) {
    return {
        title: coldef.title,
        render: {
            display: compareTableColRenderer(coldef)
        }
    };
}

function compareTableColRenderer(coldef) {
    var format = coldef.format;
    return function(val) {
        if (val == null) {
            return "--";
        } else if (format) {
            return formatValue(val, format);
        } else {
            return val;
        }
    };
}

/********************************************************************
 * Core panel support
 ********************************************************************/

function toggleCollapseIconForElement(el, curState) {
    var icon = expandCollapseIconForElement(el);
    if (curState == "expanded") {
        icon.removeClass("fa-angle-down");
        icon.addClass("fa-angle-up");
    } else {
        icon.removeClass("fa-angle-up");
        icon.addClass("fa-angle-down");
    }
}

function expandCollapseIconForElement(el) {
    return $("[data-toggle='collapse'][href='#" + el.attr("id") + "'] i");
}

function initPanelShowHide() {
    $(".collapse").on("show.bs.collapse", function() {
        toggleCollapseIconForElement($(this), "expanded");
    });
    $(".collapse").on("hide.bs.collapse", function() {
        toggleCollapseIconForElement($(this), "collapsed");
    });
}

function initPanelExpand() {
    SIGNALS.fullscreenToggle = new signals.Signal();
    $("[data-toggle='expand']").click(function (e) {
        e.preventDefault();
        var panel = $(this).closest('.panel');
        toggleExpandIconForPanel(panel);
        panel.toggleClass('panel-fullscreen');
        SIGNALS.fullscreenToggle.dispatch(panel);
    });
}

function toggleExpandIconForPanel(panel) {
    var icon = $("i", panel);
    var expanded = !panel.data("expanded");
    if (expanded) {
        icon.removeClass("fa-expand");
        icon.addClass("fa-compress");
    } else {
        icon.removeClass("fa-compress");
        icon.addClass("fa-expand");
    }
    panel.data("expanded", expanded);
}

$(function() {
    initPanelShowHide();
    initPanelExpand();
});
