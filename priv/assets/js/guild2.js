/********************************************************************
 * View support
 ********************************************************************/

function initView(selectedRun, dataRefreshInterval) {
    var state = initViewState(selectedRun, dataRefreshInterval);
    initWidgets(state);
    startDataDispatcher(state);
}

function initViewState(selectedRun, refreshIntervalSeconds) {
    return {
        selectedRun: selectedRun,
        refreshInterval: refreshIntervalSeconds * 1000,
        signals: {},
        sources: {}
    };
}

function initWidgets(state) {
    $("[data-widget-init]").each(function() {
        var widget = $(this);
        var init = window[widget.attr("data-widget-init")];
        if (init) {
            init(widget, state);
        }
    });
}

function startDataDispatcher(state) {
    window.setTimeout(dispatchDataLoop, 0, state);
}

function dispatchDataLoop(state) {
    for (var source in state.sources) {
        var callbacks = state.sources[source];
        fetchAndDispatchData(source, dataDispatcher(callbacks, state));
    }
    state.sources = [];
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

/********************************************************************
 * Data support
 ********************************************************************/

function fetchAndDispatchData(url, callback) {
    console.debug("Fetching " + url);
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
 * Widget support
 ********************************************************************/

function register(event, callback, state) {
    var signal = state.signals[event];
    if (signal == undefined) {
        state.signals[event] = signal = new signals.Signal();
    }
    signal.add(callback);
}

function registerWidgetCallback(event, widget, callback, state) {
    register(event, function(args) {callback(widget, args); }, state);
}

function notify(event, args, state) {
    var signal = state.signals[event];
    if (signal != undefined) {
        signal.dispatch(args);
    }
}

function scheduleImmediate(source, widget, callback, state) {
    var callbacks = state.sources[source];
    if (callbacks == undefined) {
        state.sources[source] = callbacks = [];
    }
    callbacks.push({widget: widget, fun: callback});
}

scheduleNextInterval = scheduleImmediate;

function runSource(source, state) {
    if (state.selectedRun) {
        return source + "?run=" + state.selectedRun;
    } else {
        return source;
    }
}
