/* Copyright 2106 TensorHub, Inc.
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

var guild = guild || {};

guild.view = new function() {

    var FULL_SCREEN_TOGGLE = "full_screen_toggle";

    var init = function(selectedRun, dataRefreshInterval) {
        var state = initState(selectedRun, dataRefreshInterval);
        initPanelSupport(state);
        guild.widget.initAll(state);
    };

    var initState = function(selectedRun, refreshIntervalSeconds) {
        return {
            selectedRun: selectedRun,
            refreshInterval: refreshIntervalSeconds * 1000,
            signals: {},
            sources: {}
        };
    };

    var initPanelSupport = function(state) {
        initPanelShowHide();
        initPanelExpand(state);
    };

    var initPanelShowHide = function() {
        $(".collapse").on("show.bs.collapse", function() {
            toggleCollapseIconForElement($(this), "expanded");
        });
        $(".collapse").on("hide.bs.collapse", function() {
            toggleCollapseIconForElement($(this), "collapsed");
        });
    };

    var toggleCollapseIconForElement = function(el, curState) {
        var icon = expandCollapseIconForElement(el);
        if (curState == "expanded") {
            icon.removeClass("fa-angle-down");
            icon.addClass("fa-angle-up");
        } else {
            icon.removeClass("fa-angle-up");
            icon.addClass("fa-angle-down");
        }
    };

    var expandCollapseIconForElement = function(el) {
        return $("[data-toggle='collapse'][href='#" + el.attr("id") + "'] i");
    };

    var initPanelExpand = function(state) {
        $("[data-toggle='expand']").click(function (e) {
            e.preventDefault();
            var panel = $(this).closest('.panel');
            toggleExpandIconForPanel(panel);
            panel.toggleClass('panel-fullscreen');
            guild.event.notify(FULL_SCREEN_TOGGLE, panel, state);
        });
    };

    var toggleExpandIconForPanel = function(panel) {
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
    };

    this.FULL_SCREEN_TOGGLE = FULL_SCREEN_TOGGLE;
    this.init = init;
};

guild.widget = new function() {

    var registered = {};

    var register = function(name, funs) {
        registered[name] = funs;
    };

    var initAll = function(state) {
        $("[data-widget]").each(function() {
            var widget = $(this);
            var init = registered[widget.attr("data-widget")];
            if (init) {
                init(widget, state);
            }
        });
    };

    var registerCallback = function (event, widget, callback, state) {
        var dispatch = function(arg) {
            callback(widget, arg, state);
        };
        guild.event.register(event, dispatch, state);
    };

    var value = function(widget, data) {
        return ensureValue(tryReduce(widget, tryAttr(widget, data)));
    };

    var ensureValue = function(value) {
        if (typeof value === "object" && value != null) {
            return value[Object.keys(value)[0]];
        } else {
            return value;
        }
    };

    var formattedValue = function(widget, data) {
        return tryFormat(widget, value(widget, data));
    };

    var tryAttr = function(widget, value) {
        var attr = widget.attr("data-widget-attribute");
        if (attr && value) {
            return value[attr];
        } else {
            return value;
        }
    };

    var tryReduce = function(widget, value) {
        var name = widget.attr("data-widget-reduce");
        if (name && value) {
            var reduce = guild.reduce[name];
            if (reduce) {
                return reduce(value);
            } else {
                console.error("Unknown reduce function " + name);
                return null;
            }
        } else {
            return value;
        }
    };

    var tryFormat = function(widget, value) {
        var format = widget.attr("data-widget-format");
        if (format) {
            return guild.util.tryFormat(value, format);
        } else {
            return value;
        }
    };

    var runSource = function(widget, run) {
        var base = "/data/" + widget.attr("data-widget-source");
        return guild.util.runSource(base, run);
    };

    this.register = register;
    this.initAll = initAll;
    this.registerCallback = registerCallback;
    this.formattedValue = formattedValue;
    this.value = value;
    this.runSource = runSource;
};

guild.event = new function() {

    var register = function(event, callback, state) {
        var signal = state.signals[event];
        if (signal == undefined) {
            state.signals[event] = signal = new signals.Signal();
        }
        signal.add(callback);
    };

    var notify = function(event, arg, state) {
        var signal = state.signals[event];
        if (signal != undefined) {
            signal.dispatch(arg, state);
        }
    };

    var unregister = function(event, callback, state) {
        var signal = state.signals[event];
        if (signal != undefined) {
            signal.remove(callback);
        }
    };

    var unregisterAll = function(event, state) {
        var signal = state.signals[event];
        if (signal) {
            signal.removeAll();
        }
    };

    this.register = register;
    this.notify = notify;
    this.unregister = unregister;
    this.unregisterAll = unregisterAll;
};

guild.util = new function() {

    var tryFormat = function(value, format) {
        if (value != null && value != undefined && value == value) {
            try {
                return numeral(value).format(format);
            } catch (err) {
                console.error(err);
                return value;
            }
        } else {
            return value;
        }
    };

    var runSource = function(source, run) {
        if (run) {
            return source + "?run=" + run.id;
        } else {
            return source;
        }
    };

    this.tryFormat = tryFormat;
    this.runSource = runSource;
};

guild.data = new function() {

    var fetch = function(url, callback) {
        console.log("Fetching " + url);
        $.ajax({
            url: encodeDataUrl(url),
            success: callback,
            dataType: "json"
        });
    };

    var encodeDataUrl = function(url) {
        return url.replace("/./", "/.{1}/").replace("/../", "/.{2}/");
    };

    var scheduleFetch = function(url, callback, when) {
        var scheduledFetch = function() {
            guild.data.fetch(url, callback);
        };
        window.setTimeout(scheduledFetch, when);
    };

    this.fetch = fetch;
    this.scheduleFetch = scheduleFetch;
};

guild.reduce = new function() {

    var last_5_average = function(data) {
        return mapSeries(function(series) {
            return seriesAverage(series.slice(-5));
        }, data);
    };

    var average = function(data) {
        return mapSeries(function(series) {
            return seriesAverage(series);
        }, data);
    };

    var last = function(data) {
        return mapSeries(function(series) {
            return seriesLast(series);
        }, data);
    };

    var steps = function(data) {
        return mapSeries(function(series) {
            return seriesSteps(series);
        }, data);
    };

    var steps0 = function(data) {
        return mapSeries(function(series) {
            return seriesSteps0(series);
        }, data);
    };

    var duration = function(data) {
        return mapSeries(function(series) {
            return seriesDuration(series);
        }, data);
    };

    var mapSeries = function(f, data) {
        var result = {};
        for (var name in data) {
            var series = data[name];
            result[name] = series ? f(series) : undefined;
        }
        return result;
    };

    var seriesAverage = function(series) {
        if (!series) {
            return undefined;
        }
        var total = 0;
        for(var i in series) {
            total += series[i][2];
        }
        return total / series.length;
    };

    var seriesLast = function(series) {
        if (!series) {
            return undefined;
        }
        return series[series.length - 1][2];
    };

    var seriesSteps = function(series) {
        if (!series) {
            return 0;
        }
        return series[series.length - 1][1];
    };

    var seriesSteps0 = function(series) {
        if (!series || series.length < 2) {
            return 0;
        }
        var interval = series[1][1] - series[0][1];
        return series[series.length - 1][1] + interval;
    };

    var seriesDuration = function(series) {
        if (!series || series.length < 2) {
            return null;
        }
        return (series[series.length - 1][0] - series[0][0]) / 1000;
    };

    this.last_5_average = last_5_average;
    this.average = average;
    this.last = last;
    this.steps = steps;
    this.steps0 = steps0;
    this.duration = duration;
};
