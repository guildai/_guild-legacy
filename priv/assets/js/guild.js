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

var guild = guild || {};

/********************************************************************
 * View support
 ********************************************************************/

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

/********************************************************************
 * Widget support
 ********************************************************************/

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

    var runSource = function(widget, run, attrs) {
        var sourceAttr = widget.attr("data-widget-source");
        if (!sourceAttr) {
            return undefined;
        } else if (sourceAttr.startsWith("/")) {
            return guild.util.runSource(sourceAttr.substring(1), run, attrs);
        } else {
            return guild.util.runSource("/data/" + sourceAttr, run, attrs);
        }
    };

    this.register = register;
    this.initAll = initAll;
    this.registerCallback = registerCallback;
    this.formattedValue = formattedValue;
    this.tryReduce = tryReduce;
    this.tryFormat = tryFormat;
    this.value = value;
    this.runSource = runSource;
};

/********************************************************************
 * Event support
 ********************************************************************/

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
                return formatValue(value, format);
            } catch (err) {
                console.error(err);
                return value;
            }
        } else {
            return value;
        }
    };

    var formatValue = function(value, format) {
        var split = splitFormatAndSuffix(format);
        var formatted;
        if (split.format.endsWith("e")) {
            formatted = formatExponential(value, split.format.slice(0, -1));
        } else {
            formatted = numeral(value).format(split.format);
        }
        return formatted + split.suffix;

    };

    var formatExponential = function(value, format) {
        var match = /0\.(0+)/.exec(format);
        if (match) {
            return value.toExponential(match[1].length);
        } else {
            return value.toExponential();
        }
    };

    var splitFormatAndSuffix = function(format) {
        // Guild specific additions to numeral formatting support
        var suffixes = [" ms"];
        for (var i in suffixes) {
            var suffix = suffixes[i];
            if (format.endsWith(suffix)) {
                return {
                    format: format.substring(0, format.length - suffix.length),
                    suffix: suffix
                };
            }
        }
        return {
            format: format,
            suffix: ""
        };
    };

    var runSource = function(source, run, attrs) {
        var base = baseRunSource(source, run);
        return (attrs && attrs.length > 0) ? base + "&" + attrs : base;
    };

    var baseRunSource = function(source, run) {
        if (run) {
            return source + "?run=" + run.id;
        } else {
            return source;
        }
    };

    this.tryFormat = tryFormat;
    this.runSource = runSource;
};

/********************************************************************
 * Data support
 ********************************************************************/

guild.data = new function() {

    var waiting = {};

    var fetch = function(url, callback) {
        if (url in waiting) {
            waiting[url].push(callback);
        } else {
            waiting[url] = [callback];
            $.ajax({
                url: encodeDataUrl(url),
                success: fetchHandler(url),
                dataType: "json"
            });
        }
    };

    var fetchHandler = function(url) {
        return function(data) {
            waiting[url].map(function(callback) {
                callback(data);
            });
            delete waiting[url];
        };
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

/********************************************************************
 * Reduce functions
 ********************************************************************/

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
        return series[series.length - 1][1] + 1;
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
    this.duration = duration;
};

/********************************************************************
 * Auth
 ********************************************************************/

guild.auth = new function() {
    var login = function() {
        $.blockUI({
            "message": "Logging in with GitHub...",
            "css": {
                "top": "33%",
                "padding": 15,
                "width": "20%",
                "left": "40%",
                "border": "none",
                "color": "#ddd",
                "background": "#000",
                "border-radius": "4px"
            },
            "overlayCSS": {
                "opacity": 0.4
            }
        });
        window.location = "/login?next=" + window.location;
    };

    var logout = function() {
        window.location = "/logout?next=" + window.location;
    };

    this.login = login;
    this.logout = logout;
};
