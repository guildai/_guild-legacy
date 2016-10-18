/********************************************************************
 * View support
 ********************************************************************/

function initView(selectedRun, dataRefreshInterval) {
    initRunsSelect();
    initViewChangeHandlers();
    var state = viewState(selectedRun, dataRefreshInterval);
    refreshViewState(state);
}

function initRunsSelect() {
    $("#runs-select").on("changed.bs.select", function (e) {
        window.location = "?run=" + $(this).val();
    });
}

function viewState(selectedRun, refreshIntervalSeconds) {
    return {
        selectedRun: selectedRun,
        refreshInterval: refreshIntervalSeconds * 1000,
        runs: null,
        run: null,
        widgetsInit: false,
        runStopped: false
    };
}

function initViewChangeHandlers() {
    SIGNALS.viewStateChanged = new signals.Signal();
    SIGNALS.viewStateChanged.add(updateRunsSelect);
    SIGNALS.viewStateChanged.add(maybeUpdateWidgets);
    SIGNALS.viewStateChanged.add(updateRunStatus);
    SIGNALS.viewStateChanged.add(scheduleNextViewStateRefresh);
}

function refreshViewState(state) {
    fetchGuildData("/data/runs", function(runs) {
        state.runs = runs;
        var runId = state.run == null ? state.selectedRun : state.run.id;
        state.run = findRun(runId, runs);
        SIGNALS.viewStateChanged.dispatch(state);
    });
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

function updateRunsSelect(state) {
    var select = $("#runs-select");
    var changed = false;
    for (var i = state.runs.length - 1; i >= 0; i--) {
        var run = state.runs[i];
        var curOpt = currentRunSelectOption(run.id);
        var selected = isRunSelected(run, state);
        if (curOpt == null) {
            select.prepend(selectOptionForRun(run, selected));
            changed = true;
        } else {
            var newOpt = selectOptionForRun(run, selected);
            if (runSelectOptionChanged(curOpt, newOpt)) {
                newOpt.insertBefore(curOpt);
                curOpt.remove();
                changed = true;
            }
        }
    }
    if (changed) {
        select.selectpicker("refresh");
    }
}

function currentRunSelectOption(runId) {
    var option = document.getElementById("run-option-" + runId);
    return option != null ? $(option) : null;
}

function isRunSelected(run, state) {
    return state.run.id == run.id;
}

function selectOptionForRun(run, selected) {
    var option = $("<option>");
    var label = runLabel(run);
    option.append(label);
    option.attr("id", "run-option-" + run.id);
    option.attr("value", run.id);
    option.attr("data-content", runOptionContent(label, run));
    if (selected) {
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

function maybeUpdateWidgets(state) {
    if (!state.widgetsInit || !state.runStopped) {
        updateWidgets(state);
    }
}

function updateRunStatus(state) {
    if (!state.runStopped && state.run && state.run.status != "running") {
        state.runStopped = true;
    }
}

function scheduleNextViewStateRefresh(state) {
    if (state.run) {
        state.lastStatus = state.run.status;
    }
    window.setTimeout(refreshViewState, state.refreshInterval, state);
}

/********************************************************************
 * Data support
 ********************************************************************/

function fetchGuildData(url, callback) {
    $.ajax({
        url: encodeDataUrl(url),
        success: callback,
        dataType: "json"
    });
}

function encodeDataUrl(url) {
    return url.replace("/./", "/.{1}/").replace("/../", "/.{2}/");
}

/********************************************************************
 * Widgets support
 ********************************************************************/

function updateWidgets(state) {
    var opAttr = widgetsOpAttr(state);
    var widgets = widgetsByDataSource(opAttr);
    for (var dataSource in widgets) {
        var sourceWidgets = widgets[dataSource];
        if (dataSource) {
            applyWidgetsDataSource(dataSource, sourceWidgets, opAttr, state);
        } else {
            applyWidgetsNullData(sourceWidgets, opAttr, state);
        }
    }
    state.widgetsInit = true;
}

function widgetsOpAttr(state) {
    return state.widgetsInit ? "data-widget-update" : "data-widget-init";
}

function widgetsByDataSource(opAttr) {
    var widgets = {};
    $("[" + opAttr + "]").each(function() {
        var widget = $(this);
        var source = widget.attr("data-widget-source");
        if (!source) source = "";
        var working = widgets[source];
        if (working == undefined) {
            working = [];
            widgets[source] = working;
        }
        working.push(widget);
    });
    return widgets;
}

function applyWidgetsDataSource(source, widgets, opAttr, state) {
    var sourceUrl = widgetSourceUrl(source, state.run);
    var handler = widgetsDataHandler(widgets, opAttr, state);
    fetchGuildData(sourceUrl, handler);
}

function widgetSourceUrl(source, run) {
    var url = "/data/" + source;
    if (run) {
        url += "?run=" + run.id;
    }
    return url;
}

function widgetsDataHandler(widgets, opAttr, state) {
    return function(data) {
        for (var i in widgets) {
            applyWidgetData(widgets[i], opAttr, data, state);
        }
    };
}

function applyWidgetData(widget, opAttr, data, state) {
    var op = findFunction(widget.attr(opAttr));
    if (op != undefined) {
        var value = (
            maybeReduceForWidget(
                maybeAttrForWidget(data, widget),
                widget));
        op(widget, value, state);
    }
}

function findFunction(name) {
    return window[name];
}

function maybeAttrForWidget(data, widget) {
    return maybeAttr(data, widget.attr("data-widget-attribute"));
}

function maybeReduceForWidget(data, widget) {
    return maybeReduce(data, widget.attr("data-widget-reduce"));
}

function applyWidgetsNullData(widgets, opAttr, state) {
    for (var i in widgets) {
        applyWidgetData(widgets[i], opAttr, null, state);
    }
}

/********************************************************************
 * Value manipulation and formatting
 ********************************************************************/

function formatValue(value, format) {
    if (value == undefined
        || value == null
        || value != value) return value;
    try {
        return numeral(value).format(format);
    } catch (err) {
        console.error(err);
        return value;
    }
}

function maybeReduce(data, reduceName) {
    if (!reduceName) return data;
    if (!data) return null;
    var reduce = findFunction("reduce_" + reduceName);
    if (!reduce) {
        console.error("Unknown reduce function " + reduceName);
        return null;
    }
    return reduce(data);
}

function maybeAttr(data, name) {
    if (!name) return data;
    if (!data) return null;
    return data[name];
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
 * Reduce functions
 ********************************************************************/

function reduce_last_5_average(data) {
    return mapSeries(function(series) {
        return seriesAverage(series.slice(-5));
    }, data);
}

function reduce_average(data) {
    return mapSeries(function(series) {
        return seriesAverage(series);
    }, data);
}

function reduce_last(data) {
    return mapSeries(function(series) {
        return seriesLast(series);
    }, data);
}

function reduce_steps(data) {
    return mapSeries(function(series) {
        return seriesSteps(series);
    }, data);
}

function reduce_steps0(data) {
    return mapSeries(function(series) {
        return seriesSteps0(series);
    }, data);
}

function reduce_duration(data) {
    return mapSeries(function(series) {
        return seriesDuration(series);
    }, data);
}

function mapSeries(f, data) {
    var result = {};
    for (var name in data) {
        var series = data[name];
        result[name] = series ? f(series) : undefined;
    }
    return result;
}

function seriesAverage(series) {
    if (!series) {
        return undefined;
    }
    var total = 0;
    for(var i in series) {
        total += series[i][2];
    }
    return total / series.length;
}

function seriesLast(series) {
    if (!series) {
        return undefined;
    }
    return series[series.length - 1][2];
}

function seriesSteps(series) {
    if (!series) {
        return 0;
    }
    return series[series.length - 1][1];
}

function seriesSteps0(series) {
    if (!series || series.length < 2) {
        return 0;
    }
    var interval = series[1][1] - series[0][1];
    return series[series.length - 1][1] + interval;
}

function seriesDuration(series) {
    if (!series || series.length < 2) {
        return null;
    }
    return (series[series.length - 1][0] - series[0][0]) / 1000;
}

/********************************************************************
 * Block UI
 ********************************************************************/

function blockUI(msg) {
    if (!msg) {
        msg = "Please wait";
    }
    var html =
        "<div><i class=\"fa fa-circle-o-notch fa-spin fa-fw\"></i> "
        + msg + "</div>";
    // Hack to get fades to work
    $.blockUI.defaults.fadeIn = 0;
    $.blockUI.defaults.fadeOut = 500;
    $.blockUI({
        message: html,
        baseZ: 1000,
        css: {
            border: '0',
            padding: '0',
            backgroundColor: 'none',
            fontSize: '18px'
        },
        overlayCSS: {
            backgroundColor: '#666',
            opacity: 0.3
        }
    });
}

function unblockUI() {
    $.unblockUI();
}

/********************************************************************
 * Globals
 ********************************************************************/

var SIGNALS = {};
