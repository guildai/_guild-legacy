/********************************************************************
 * Events
 ********************************************************************/

RUN_STATUS = "run status";

/********************************************************************
 * Run select
 ********************************************************************/

function initRunSelect(widget, state) {
    widget.on("changed.bs.select", function (e) {
        window.location = "?run=" + $(this).val();
    });
    scheduleImmediate("/data/runs", widget, updateRunSelect, state);
}

function updateRunSelect(widget, runs, state) {
    var runId = state.run == null ? state.selectedRun : state.run.id;
    state.run = findRun(runId, runs);
    notify(RUN_STATUS, state.run, state);
    refreshRunSelect(widget, runs, state.run);
    scheduleNextInterval("/data/runs", widget, updateRunSelect, state);
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

function initValuePanel(widget) {
    console.log("TODO: init value panel");
}

function initValuePanel_stub(widget, state) {
    scheduleImmediate(
        widgetRunSource(widget, state),
        widget, updateValuePanel, state);
}

function updateValuePanel(widget, data) {
    setPanelLabel(widget, panelLabel(data));
    scheduleNextInterval(
        widgetRunSource(widget, state),
        widget, updateValuePanel, state);
}

function panelLabel(data) {
    if (typeof data === "object" && data != null) {
        // A reduced value from stats is provided as a dict of
        // name+values - use the first value since we can only show
        // one
        return data[Object.keys(data)[0]];
    } else {
        return data;
    }
}

function setPanelLabel(widget, label) {
    if (label != undefined) {
        widget.text(formatWidgetValue(widget, label));
    } else {
        widget.text("--");
    }
}

function formatWidgetValue(value, widget) {
    var format = widget.attr("data-widget-format");
    return format ? formatValue(value, format) : value;
}

/********************************************************************
 * Flags
 ********************************************************************/

function initFlags(widget, state) {
    scheduleImmediate(
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
    scheduleImmediate(
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
 * Run status
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

function initTimeseries(widget) {
    console.log("TODO: init timeseries");
}

/********************************************************************
 * Output
 ********************************************************************/

function initOutput(widget) {
    console.log("TODO: init output");
}


/********************************************************************
 * Compare table
 ********************************************************************/

function initCompareTable(widget) {
    console.log("TODO: init compare table");
}
