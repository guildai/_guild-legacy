/********************************************************************
 * View support
 ********************************************************************/

function initView(selectedRun, dataRefreshInterval) {
    var state = initViewState(selectedRun, dataRefreshInterval);
    initPanelSupport(state);
    initWidgets(state);
    //startDataDispatcher(state);
}

function initViewState(selectedRun, refreshIntervalSeconds) {
    return {
        selectedRun: selectedRun,
        refreshInterval: refreshIntervalSeconds * 1000,
        signals: {},
        sources: {}
    };
}

/********************************************************************
 * Panel support
 ********************************************************************/

FULL_SCREEN_TOGGLE = "full_screen_toggle";

function initPanelSupport(state) {
    initPanelShowHide();
    initPanelExpand(state);
}

function initPanelShowHide() {
    $(".collapse").on("show.bs.collapse", function() {
        toggleCollapseIconForElement($(this), "expanded");
    });
    $(".collapse").on("hide.bs.collapse", function() {
        toggleCollapseIconForElement($(this), "collapsed");
    });
}

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

function initPanelExpand(state) {
    $("[data-toggle='expand']").click(function (e) {
        e.preventDefault();
        var panel = $(this).closest('.panel');
        toggleExpandIconForPanel(panel);
        panel.toggleClass('panel-fullscreen');
        notify(FULL_SCREEN_TOGGLE, panel, state);
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

/********************************************************************
 * Widget support
 ********************************************************************/

function initWidgets(state) {
    $("[data-widget-init]").each(function() {
        var widget = $(this);
        var init = window[widget.attr("data-widget-init")];
        if (init) {
            init(widget, state);
        }
    });
}

function registerCallback(event, callback, state) {
    var signal = state.signals[event];
    if (signal == undefined) {
        state.signals[event] = signal = new signals.Signal();
    }
    signal.add(callback);
}

function registerWidgetCallback(event, widget, callback, state) {
    var dispatch = function(arg) {
        callback(widget, arg, state);
    };
    registerCallback(event, dispatch, state);
}

function notify(event, arg, state) {
    var signal = state.signals[event];
    if (signal != undefined) {
        signal.dispatch(arg, state);
    }
}

function removeCallbacks(event, state) {
    var signal = state.signals[event];
    if (signal) {
        signal.removeAll();
    }
}

/*
function scheduleWidgetData(source, widget, callback, state) {
    var callbacks = state.sources[source];
    if (callbacks == undefined) {
        state.sources[source] = callbacks = [];
    }
    callbacks.push({widget: widget, fun: callback});
}
*/

function runSource(source, run) {
    if (run) {
        return source + "?run=" + run.id;
    } else {
        return source;
    }
}

function widgetRunSource(widget, state) {
    return runSource("/data/" + widget.attr("data-widget-source"), state);
}

function widgetValue(widget, data) {
    return widgetFormat(widget, widgetReduce(widget, widgetAttr(widget, data)));
}

function widgetAttr(widget, data) {
    var attr = widget.attr("data-widget-attribute");
    if (attr && data) {
        return data[attr];
    } else {
        return data;
    }
}

function widgetReduce(widget, data) {
    var name = widget.attr("data-widget-reduce");
    if (name && data) {
        var reduce = window["reduce_" + name];
        if (reduce) {
            return reduce(data);
        } else {
            console.error("Unknown reduce function " + reduceName);
            return null;
        }
    } else {
        return data;
    }
}

function widgetFormat(widget, data) {
    var format = widget.attr("data-widget-format");
    if (format) {
        return tryFormat(ensureFormattable(data), format);
    } else {
        return data;
    }
}

function ensureFormattable(data) {
    if (typeof data === "object" && data != null) {
        return data[Object.keys(data)[0]];
    } else {
        return data;
    }
}

function tryFormat(val, format) {
    if (val == undefined || val == null || val != val) {
        return val;
    } else {
        try {
            return numeral(val).format(format);
        } catch (err) {
            console.error(err);
            return val;
        }
    }
}

/********************************************************************
 * Data dispatcher
 ********************************************************************/

/*
function startDataDispatcher(state) {
    window.setTimeout(dispatchDataLoop, 0, state);
}

function dispatchDataLoop(state) {
    console.log("-------------------------");
    var sources = state.sources;
    state.sources = [];
    for (var source in sources) {
        var callbacks = sources[source];
        fetchData(source, dataDispatcher(callbacks, state));
    }
    window.setTimeout(dispatchDataLoop, state.refreshInterval, state);
}

function dataDispatcher(callbacks, state) {
    return function(data) {
        for (var i = 0; i < callbacks.length; i++) {
            var cb = callbacks[i];
            cb.fun(cb.widget, data, state);
        }
    };
}
*/

function fetch(url, widget, callback, state) {
    var dispatch = function(data) {
        callback(widget, data, state);
    };
    fetchData(url, dispatch);
}

function scheduleNextFetch(url, widget, callback, state) {
    var scheduledFetch = function() {
        fetch(url, widget, callback, state);
    };
    window.setTimeout(scheduledFetch, state.refreshInterval);
}

function fetchData(url, callback) {
    console.log("Fetching " + url);
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
