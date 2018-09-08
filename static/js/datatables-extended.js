/**
 * Based on absolute-order Datatables plug-in by Allan Jardine
 */

(function( factory ){
	if ( typeof define === 'function' && define.amd ) {
		// AMD
		define( ['jquery', 'datatables.net'], function ( $ ) {
			return factory( $, window, document );
		} );
	}
	else if ( typeof exports === 'object' ) {
		// CommonJS
		module.exports = function (root, $) {
			if ( ! root ) {
				root = window;
			}

			if ( ! $ || ! $.fn.dataTable ) {
				$ = require('datatables.net')(root, $).$;
			}

			return factory( $, root, root.document );
		};
	}
	else {
		// Browser
		factory( jQuery, window, document );
	}
}(function( $, window, document, undefined ) {
'use strict';

function extractValue(a) {
    if ( typeof a === 'string' ) {
	var m = a.match(/<span title="([^"]+)"/)
        if (m != null) {
            return m[1]
        }
    } else {
        return a
    }
}

function extractNumber(a) {
    // Cast as a number if required
    if ( typeof a === 'string' ) {
	return (a.replace(/[^\d\-\.]/g, '') * 1);
    } else {
        return a;
    }
}

function toSign(a, b) {
   return ((a < b) ? -1 : ((a > b) ? 1 : 0));
}

// Unique value allowing multiple absolute ordering use cases on a single page.
var _unique = 0;

// Function to encapsulate code that is common to both the string and number
// ordering plug-ins.
var _setup = function ( isHigherTheBetter ) {
	var o = {
		name: 'absoluteOrder'+(_unique++),
		alwaysHighest: {},
		alwaysLowest: {}
	};

	// In order to provide performance, the symbols that are to be looked for
	// are stored as parameter keys in an object, allowing O(1) lookup, rather
	// than O(n) if it were in an array.

        o.alwaysHighest["Infinity"] = 1
        if (isHigherTheBetter) {
            o.alwaysLowest["N/A"] = -1
        } else {
            o.alwaysHighest["N/A"] = 2
        }

	// Ascending ordering method
	o.asc = function ( a, b ) {
                a = extractValue(a)
                b = extractValue(b)

                if ( o.alwaysLowest[ a ] && o.alwaysLowest[ b ] ) {
		    return toSign(o.alwaysLowest[a], o.alwaysLowest[b]);
		}
		else if ( o.alwaysHighest[ a ] && o.alwaysHighest[ b ] ) {
                    return toSign(o.alwaysHighest[a], o.alwaysHighest[b]);
		}
		else if ( o.alwaysLowest[ a ] || o.alwaysHighest[ b ] ) {
			return -1;
		}
		else if ( o.alwaysHighest[ a ] || o.alwaysLowest[ b ] ) {
			return 1;
		}

                a = extractNumber(a);
                b = extractNumber(b);

     	        return toSign(a, b);
	};

	// Descending ordering method
	o.desc = function ( a, b ) {
                a = extractValue(a)
                b = extractValue(b)

	        if ( o.alwaysLowest[ a ] && o.alwaysLowest[ b ] ) {
		    return toSign(o.alwaysLowest[b], o.alwaysLowest[a]);
		}
		else if ( o.alwaysHighest[ a ] && o.alwaysHighest[ b ] ) {
                    return toSign(o.alwaysHighest[b], o.alwaysHighest[a]);
		}
		else if ( o.alwaysLowest[ a ] || o.alwaysHighest[ b ] ) {
			return 1;
		}
		else if ( o.alwaysHighest[ a ] || o.alwaysLowest[ b ] ) {
			return -1;
		}

                a = extractNumber(a);
                b = extractNumber(b);

                return toSign(b, a);
	};

	return o;
};

// Number based ordering - strips out everything but the number information
$.fn.dataTable.extendedOrderNumber = function ( isHigherTheBetter ) {
	var conf = _setup( isHigherTheBetter );

	$.fn.dataTable.ext.type.order[ conf.name+'-asc' ] = function ( a, b ) {
	    return conf.asc( a, b, true );
	};
	$.fn.dataTable.ext.type.order[ conf.name+'-desc' ] = function ( a, b ) {
	    return conf.desc( a, b, true );
	};

	return conf.name;
};

$.fn.dataTable.getColumnDef = function (ix, isHigherTheBetter) {
    return {
        type: $.fn.dataTable.extendedOrderNumber(isHigherTheBetter),
        targets: ix
    };
}

$.fn.dataTable.getColumnDefs = function (delta, isHigherTheBetterArray) {
    var ix = delta;

    return isHigherTheBetterArray.map(isHigherTheBetter => $.fn.dataTable.getColumnDef(ix++, isHigherTheBetter));
};

}));
