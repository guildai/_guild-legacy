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

Guild.Generate = new function() {

    var generate = function(parent, item, context) {
        var type = item.type;
        if (type == "container") {
            generateContainer(parent, item, context);
        } else if (type == "row") {
            generateRow(parent, item, context);
        } else if (type == "col") {
            generateCol(parent, item, context);
        } else if (type == "component") {
            generateComponent(parent, item, context);
        } else {
            throw ["Unknown page item", item];
        }
    };

    var generateChildren = function(parent, children, context) {
        children.forEach(function(item) {
            generate(parent, item, context);
        });
    };

    var generateContainer = function(parent, containerDef, context) {
        var container = document.createElement("div");
        container.setAttribute("class", "container-fluid");
        Polymer.dom(parent).appendChild(container);
        generateChildren(container, containerDef.items, context);
    };

    var generateRow = function(parent, rowDef, context) {
        var row = document.createElement("div");
        row.setAttribute("class", "row");
        Polymer.dom(parent).appendChild(row);
        generateChildren(row, rowDef.items, context);
    };

    var generateCol = function(parent, colDef, context) {
        var col = document.createElement("div");
        col.setAttribute("class", colDef.classes);
        Polymer.dom(parent).appendChild(col);
        generateChildren(col, colDef.items, context);
    };

    var generateComponent = function(parent, componentDef, context) {
        var source = componentSource(componentDef, context);
        var link = context.importHref(source);
        var component = document.createElement(componentDef.name);
        var attrs = componentDef.attrs || {};
        Object.keys(attrs).forEach(function(key) {
            component.setAttribute(key, attrs[key]);
        });
        Polymer.dom(parent).appendChild(component);
    };

    var componentSource = function(componentDef, context) {
        if (componentDef.source) {
            return context.resolveUrl(componentDef.source);
        } else {
            return defaultComponentSource(componentDef.name, context);
        }
    };

    var defaultComponentSource = function(name, context) {
        return context.resolveUrl("../" + name + "/" + name + ".html");
    };

    this.generate = generate;
};
