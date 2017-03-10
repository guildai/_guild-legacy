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

Guild.Event = new function() {

    var signals = {};

    var register = function(event, callback) {
        var signal = signals[event];
        if (signal == undefined) {
            signals[event] = signal = new signals.Signal();
        }
        signal.add(callback);
    };

    var notify = function(event, arg) {
        var signal = signals[event];
        if (signal != undefined) {
            signal.dispatch(arg, state);
        }
    };

    var unregister = function(event, callback) {
        var signal = signals[event];
        if (signal != undefined) {
            signal.remove(callback);
        }
    };

    var unregisterAll = function(event) {
        var signal = signals[event];
        if (signal) {
            signal.removeAll();
        }
    };

    this.register = register;
    this.notify = notify;
    this.unregister = unregister;
    this.unregisterAll = unregisterAll;
};
