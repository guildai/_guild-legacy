function ArrayDataSource(arr) {
    function _filter(items, filter) {
        if (filter.length === 0) {
            return items;
        }

        return Array.prototype.filter.call(items, function(item, index) {
            for (var i = 0; i < filter.length; i++) {
                var value = Polymer.Base.get(filter[i].path, item);
                if ([undefined, null, ''].indexOf(filter[i].filter) > -1) {
                    continue;
                } else if ([undefined, null].indexOf(value) > -1
                           || !_filterMatches(filter[i].filter, value)) {
                    return false;
                }
            }
            return true;
        });
    }

    function _filterMatches(filter, value) {
        try {
            var regex = RegExp(filter, 'i');
            return regex.test(value.toString());
        } catch(_err) {
            var valLower = value.toString().toLowerCase();
            var filterLower = filter.toString().toLowerCase();
            return valLower.indexOf(filterLower) > -1;
        }
    }

    function _compare(a, b) {
        if (a < b) {
            return -1;
        }
        if (a > b) {
            return 1;
        }
        return 0;
    }

    function _sort(items, sortOrder) {
        if (!sortOrder || sortOrder.length === 0) {
            return items;
        }

        var multiSort = function() {
            return function(a, b) {
                return sortOrder.map(function(sort) {
                    if (sort.direction === 'asc') {
                        return _compare(Polymer.Base.get(sort.path, a), Polymer.Base.get(sort.path, b));
                    } else if (sort.direction === 'desc') {
                        return _compare(Polymer.Base.get(sort.path, b), Polymer.Base.get(sort.path, a));
                    }
                    return 0;
                }).reduce(function firstNonZeroValue(p, n) {
                    return p ? p : n;
                }, 0);
            };
        };

        // make sure a copy is used so that original array is unaffected.
        return Array.prototype.sort.call(items.slice(0), multiSort(sortOrder));
    }

    return function(opts, cb, err) {
        var filteredItems = _filter(arr, opts.filter);

        var sortedItems = _sort(filteredItems, opts.sortOrder);

        var start = opts.page * opts.pageSize;
        var end = start + opts.pageSize;
        var slice = sortedItems.slice(start, end);

        cb(slice, filteredItems.length);
    };
}
