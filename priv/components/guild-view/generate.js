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

    var generate = function(parent, item, state) {
        var type = item.type;
        if (type == "container") {
            generateContainer(parent, item, state);
        } else if (type == "row") {
            generateRow(parent, item, state);
        } else if (type == "col") {
            generateCol(parent, item, state);
        } else if (type == "component") {
            generateComponent(parent, item, state);
        } else {
            throw ["Unknown page item", item];
        }
    };

    var generateChildren = function(parent, children, state) {
        children.forEach(function(item) {
            generate(parent, item, state);
        });
    };

    var generateContainer = function(parent, containerDef, state) {
        var container = document.createElement("div");
        container.setAttribute("class", "container-fluid");
        Polymer.dom(parent).appendChild(container);
        generateChildren(container, containerDef.items, state);
    };

    var generateRow = function(parent, rowDef, state) {
        var row = document.createElement("div");
        row.setAttribute("class", "row guild-view-row");
        Polymer.dom(parent).appendChild(row);
        generateChildren(row, rowDef.items, state);
    };

    var generateCol = function(parent, colDef, state) {
        var col = document.createElement("div");
        col.setAttribute("class", colDef.classes);
        Polymer.dom(parent).appendChild(col);
        generateChildren(col, colDef.items, state);
    };

    var generateComponent = function(parent, componentDef, state) {
        var source = componentSource(componentDef, state);
        var link = state.page.importHref(source);
        var component = document.createElement(componentDef.name);
        var attrs = componentDef.attrs || {};
        Object.keys(attrs).forEach(function(key) {
            component.setAttribute(key, attrs[key]);
        });
        component.env = state.env;
        component.config = componentDef.config;
        wrapper = wrapComponent(component);
        Polymer.dom(parent).appendChild(wrapper);
    };

    var componentSource = function(componentDef, state) {
        if (componentDef.source) {
            return state.page.resolveUrl(componentDef.source);
        } else {
            return defaultComponentSource(componentDef.name, state);
        }
    };

    var defaultComponentSource = function(name, state) {
        return state.page.resolveUrl("../" + name + "/" + name + ".html");
    };

    var wrapComponent = function(component) {
        var wrapper = document.createElement("div");
        wrapper.setAttribute("class", "component-wrapper");
        wrapper.appendChild(component);
        return wrapper;
    };

    this.generate = generate;
};
