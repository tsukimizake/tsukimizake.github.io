(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}




var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log = F2(function(tag, value)
{
	return value;
});

var _Debug_log_UNUSED = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString(value)
{
	return '<internals>';
}

function _Debug_toString_UNUSED(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash_UNUSED(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.aZ.ar === region.bb.ar)
	{
		return 'on line ' + region.aZ.ar;
	}
	return 'on lines ' + region.aZ.ar + ' through ' + region.bb.ar;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**_UNUSED/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**_UNUSED/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**/
	if (typeof x.$ === 'undefined')
	//*/
	/**_UNUSED/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0 = 0;
var _Utils_Tuple0_UNUSED = { $: '#0' };

function _Utils_Tuple2(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2_UNUSED(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3_UNUSED(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr(c) { return c; }
function _Utils_chr_UNUSED(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil = { $: 0 };
var _List_Nil_UNUSED = { $: '[]' };

function _List_Cons(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons_UNUSED(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**_UNUSED/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap_UNUSED(value) { return { $: 0, a: value }; }
function _Json_unwrap_UNUSED(value) { return value.a; }

function _Json_wrap(value) { return value; }
function _Json_unwrap(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ct,
		impl.c$,
		impl.cW,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**_UNUSED/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**/
	var node = args['node'];
	//*/
	/**_UNUSED/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		R: func(record.R),
		a_: record.a_,
		aX: record.aX
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.R;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.a_;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.aX) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ct,
		impl.c$,
		impl.cW,
		function(sendToApp, initialModel) {
			var view = impl.c2;
			/**/
			var domNode = args['node'];
			//*/
			/**_UNUSED/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.ct,
		impl.c$,
		impl.cW,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.aY && impl.aY(sendToApp)
			var view = impl.c2;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.b8);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.bW) && (_VirtualDom_doc.title = title = doc.bW);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.cJ;
	var onUrlRequest = impl.cK;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		aY: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.bD === next.bD
							&& curr.bk === next.bk
							&& curr.bz.a === next.bz.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		ct: function(flags)
		{
			return A3(impl.ct, flags, _Browser_getUrl(), key);
		},
		c2: impl.c2,
		c$: impl.c$,
		cW: impl.cW
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { cq: 'hidden', cb: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { cq: 'mozHidden', cb: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { cq: 'msHidden', cb: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { cq: 'webkitHidden', cb: 'webkitvisibilitychange' }
		: { cq: 'hidden', cb: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		bN: _Browser_getScene(),
		b_: {
			b0: _Browser_window.pageXOffset,
			b1: _Browser_window.pageYOffset,
			b$: _Browser_doc.documentElement.clientWidth,
			bi: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		b$: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		bi: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			bN: {
				b$: node.scrollWidth,
				bi: node.scrollHeight
			},
			b_: {
				b0: node.scrollLeft,
				b1: node.scrollTop,
				b$: node.clientWidth,
				bi: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			bN: _Browser_getScene(),
			b_: {
				b0: x,
				b1: y,
				b$: _Browser_doc.documentElement.clientWidth,
				bi: _Browser_doc.documentElement.clientHeight
			},
			ch: {
				b0: x + rect.left,
				b1: y + rect.top,
				b$: rect.width,
				bi: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.cj.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.cj.b, xhr)); });
		$elm$core$Maybe$isJust(request.bX) && _Http_track(router, xhr, request.bX.a);

		try {
			xhr.open(request.cw, request.c1, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.c1));
		}

		_Http_configureRequest(xhr, request);

		request.b8.a && xhr.setRequestHeader('Content-Type', request.b8.a);
		xhr.send(request.b8.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.cp; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.c_.a || 0;
	xhr.responseType = request.cj.d;
	xhr.withCredentials = request.b5;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		c1: xhr.responseURL,
		cT: xhr.status,
		cU: xhr.statusText,
		cp: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			cS: event.loaded,
			bP: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			cN: event.loaded,
			bP: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}



// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});


// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.cy) { flags += 'm'; }
	if (options.ca) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$List$cons = _List_cons;
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (!node.$) {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === -2) {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Basics$EQ = 1;
var $elm$core$Basics$GT = 2;
var $elm$core$Basics$LT = 0;
var $author$project$Main$GotArticles = function (a) {
	return {$: 0, a: a};
};
var $author$project$Models$Article = F4(
	function (title, tags, articleText, updatedTime) {
		return {b7: articleText, cZ: tags, bW: title, c0: updatedTime};
	});
var $elm$core$Result$Err = function (a) {
	return {$: 1, a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 0, a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 2, a: a};
};
var $elm$core$Basics$False = 1;
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Maybe$Nothing = {$: 1};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 0:
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 1) {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 1:
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 2:
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 1, a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 0, a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.k) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.m),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.m);
		} else {
			var treeLen = builder.k * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.r) : builder.r;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.k);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.m) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.m);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{r: nodeList, k: (len / $elm$core$Array$branchFactor) | 0, m: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = 0;
var $elm$core$Result$isOk = function (result) {
	if (!result.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map4 = _Json_map4;
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$time$Time$Posix = $elm$core$Basics$identity;
var $elm$time$Time$millisToPosix = $elm$core$Basics$identity;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $author$project$Models$OtherTag = $elm$core$Basics$identity;
var $author$project$ArticlesDecoder$tagDecoder = A2($elm$json$Json$Decode$map, $elm$core$Basics$identity, $elm$json$Json$Decode$string);
var $author$project$ArticlesDecoder$articleDecoder = A5(
	$elm$json$Json$Decode$map4,
	$author$project$Models$Article,
	A2($elm$json$Json$Decode$field, 'title', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'tags',
		$elm$json$Json$Decode$list($author$project$ArticlesDecoder$tagDecoder)),
	A2($elm$json$Json$Decode$field, 'articleText', $elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$field,
		'updatedTime',
		A2($elm$json$Json$Decode$map, $elm$time$Time$millisToPosix, $elm$json$Json$Decode$int)));
var $author$project$ArticlesDecoder$articlesDecoder = $elm$json$Json$Decode$list($author$project$ArticlesDecoder$articleDecoder);
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 0:
			return 0;
		case 1:
			return 1;
		case 2:
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 1, a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 0, a: a};
};
var $elm$browser$Browser$Dom$NotFound = $elm$core$Basics$identity;
var $elm$url$Url$Http = 0;
var $elm$url$Url$Https = 1;
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {bf: fragment, bk: host, bx: path, bz: port_, bD: protocol, bE: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 1) {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		0,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		1,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = $elm$core$Basics$identity;
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return 0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0;
		return A2($elm$core$Task$map, tagger, task);
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			A2($elm$core$Task$map, toMessage, task));
	});
var $elm$browser$Browser$document = _Browser_document;
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 3, a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 4, a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 2};
var $elm$http$Http$Receiving = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$Timeout_ = {$: 1};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: -2};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Maybe$isJust = function (maybe) {
	if (!maybe.$) {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === -2) {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1) {
					case 0:
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 1:
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = 1;
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: -1, a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = 0;
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === -1) && (!right.a)) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === -1) && (!left.a)) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === -1) && (!left.a)) && (left.d.$ === -1)) && (!left.d.a)) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					0,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 1, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === -2) {
			return A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1) {
				case 0:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 1:
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === -1) && (dict.d.$ === -1)) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.e.d.$ === -1) && (!dict.e.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, 1, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === -1) && (dict.d.$ === -1)) && (dict.e.$ === -1)) {
		if ((dict.d.d.$ === -1) && (!dict.d.d.a)) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				0,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, 1, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr === 1) {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					1,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, 0, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, 0, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === -1) && (!left.a)) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, 0, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === -1) && (right.a === 1)) {
					if (right.d.$ === -1) {
						if (right.d.a === 1) {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === -1) && (dict.d.$ === -1)) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor === 1) {
			if ((lLeft.$ === -1) && (!lLeft.a)) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === -1) {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === -2) {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === -1) && (left.a === 1)) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === -1) && (!lLeft.a)) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === -1) {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === -1) {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === -1) {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === -1) && (!_v0.a)) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, 1, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (!_v0.$) {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (!result.$) {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 4, a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 3, a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$NetworkError = {$: 2};
var $elm$http$Http$Timeout = {$: 1};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 0:
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 1:
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 2:
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 3:
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.cT));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 1, a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {bG: reqs, bU: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (!cmd.$) {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 1) {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.bX;
							if (_v4.$ === 1) {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.bG));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (!_v0.$) {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.bU)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 0, a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (!cmd.$) {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					b5: r.b5,
					b8: r.b8,
					cj: A2(_Http_mapExpect, func, r.cj),
					cp: r.cp,
					cw: r.cw,
					c_: r.c_,
					bX: r.bX,
					c1: r.c1
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{b5: false, b8: r.b8, cj: r.cj, cp: r.cp, cw: r.cw, c_: r.c_, bX: r.bX, c1: r.c1}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{b8: $elm$http$Http$emptyBody, cj: r.cj, cp: _List_Nil, cw: 'GET', c_: $elm$core$Maybe$Nothing, bX: $elm$core$Maybe$Nothing, c1: r.c1});
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$none;
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$update = F2(
	function (msg, model) {
		if (!msg.$) {
			var result = msg.a;
			if (!result.$) {
				var articles = result.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{aG: articles}),
					$elm$core$Platform$Cmd$none);
			} else {
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			}
		} else {
			return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Option$ExtendedMath = 2;
var $author$project$Main$NoOp = {$: 1};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$li = _VirtualDom_node('li');
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $elm$html$Html$map = $elm$virtual_dom$VirtualDom$map;
var $author$project$Models$showTag = function (_v0) {
	var s = _v0;
	return s;
};
var $elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return $elm$core$Basics$floor(numerator / denominator);
	});
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0;
	return millis;
};
var $elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.aZ, posixMinutes) < 0) {
					return posixMinutes + era.b;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var $elm$time$Time$toAdjustedMinutes = F2(
	function (_v0, time) {
		var defaultOffset = _v0.a;
		var eras = _v0.b;
		return A3(
			$elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2($elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		a8: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		bu: month,
		b2: year + ((month <= 2) ? 1 : 0)
	};
};
var $elm$time$Time$toDay = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).a8;
	});
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm$time$Time$toHour = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			24,
			A2(
				$elm$time$Time$flooredDiv,
				A2($elm$time$Time$toAdjustedMinutes, zone, time),
				60));
	});
var $author$project$Main$toIntMonth = function (month) {
	switch (month) {
		case 0:
			return 1;
		case 1:
			return 2;
		case 2:
			return 3;
		case 3:
			return 4;
		case 4:
			return 5;
		case 5:
			return 6;
		case 6:
			return 7;
		case 7:
			return 8;
		case 8:
			return 9;
		case 9:
			return 10;
		case 10:
			return 11;
		default:
			return 12;
	}
};
var $elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2($elm$time$Time$toAdjustedMinutes, zone, time));
	});
var $elm$time$Time$Apr = 3;
var $elm$time$Time$Aug = 7;
var $elm$time$Time$Dec = 11;
var $elm$time$Time$Feb = 1;
var $elm$time$Time$Jan = 0;
var $elm$time$Time$Jul = 6;
var $elm$time$Time$Jun = 5;
var $elm$time$Time$Mar = 2;
var $elm$time$Time$May = 4;
var $elm$time$Time$Nov = 10;
var $elm$time$Time$Oct = 9;
var $elm$time$Time$Sep = 8;
var $elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _v0 = $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).bu;
		switch (_v0) {
			case 1:
				return 0;
			case 2:
				return 1;
			case 3:
				return 2;
			case 4:
				return 3;
			case 5:
				return 4;
			case 6:
				return 5;
			case 7:
				return 6;
			case 8:
				return 7;
			case 9:
				return 8;
			case 10:
				return 9;
			case 11:
				return 10;
			default:
				return 11;
		}
	});
var $elm$time$Time$toSecond = F2(
	function (_v0, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				1000));
	});
var $elm$time$Time$toYear = F2(
	function (zone, time) {
		return $elm$time$Time$toCivil(
			A2($elm$time$Time$toAdjustedMinutes, zone, time)).b2;
	});
var $author$project$Main$showTime = F2(
	function (zone, time) {
		return $elm$core$String$fromInt(
			A2($elm$time$Time$toYear, zone, time)) + ('年 ' + ($elm$core$String$fromInt(
			$author$project$Main$toIntMonth(
				A2($elm$time$Time$toMonth, zone, time))) + ('月' + ($elm$core$String$fromInt(
			A2($elm$time$Time$toDay, zone, time)) + ('日 ' + ($elm$core$String$fromInt(
			A2($elm$time$Time$toHour, zone, time)) + (':' + ($elm$core$String$fromInt(
			A2($elm$time$Time$toMinute, zone, time)) + (':' + $elm$core$String$fromInt(
			A2($elm$time$Time$toSecond, zone, time)))))))))));
	});
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $zwilias$elm_rosetree$Tree$children = function (_v0) {
	var c = _v0.b;
	return c;
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $jxxcarlson$elm_markdown$Markdown$Render$masterId = $elm$html$Html$Attributes$id('__RENDERED_TEXT__');
var $jxxcarlson$elm_markdown$Markdown$Render$IDClicked = $elm$core$Basics$identity;
var $jxxcarlson$elm_markdown$Markdown$Parse$MDBlock = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$property = $elm$virtual_dom$VirtualDom$property;
var $jxxcarlson$elm_markdown$Markdown$Render$mathText = function (content) {
	return A3(
		$elm$html$Html$node,
		'math-text',
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('mm-math'),
				A2(
				$elm$html$Html$Attributes$property,
				'content',
				$elm$json$Json$Encode$string(content))
			]),
		_List_Nil);
};
var $elm$core$String$trim = _String_trim;
var $jxxcarlson$elm_markdown$Markdown$Render$displayMathText = function (str) {
	var str2 = $elm$core$String$trim(str);
	return $jxxcarlson$elm_markdown$Markdown$Render$mathText('$$\n' + (str2 + '\n$$'));
};
var $jxxcarlson$elm_markdown$Markdown$Parse$stringFromId = function (_v0) {
	var id = _v0.a;
	var version = _v0.b;
	return 'i' + ($elm$core$String$fromInt(id) + ('v' + $elm$core$String$fromInt(version)));
};
var $jxxcarlson$elm_markdown$Markdown$Render$idAttr = function (id) {
	return $elm$html$Html$Attributes$id(
		$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id));
};
var $zwilias$elm_rosetree$Tree$label = function (_v0) {
	var v = _v0.a;
	return v;
};
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 0, a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $jxxcarlson$elm_markdown$Markdown$Parse$project = function (_v0) {
	var bt = _v0.b;
	var lev = _v0.c;
	var content = _v0.d;
	return A3($jxxcarlson$elm_markdown$Markdown$Parse$MDBlock, bt, lev, content);
};
var $jxxcarlson$elm_markdown$Markdown$Parse$projectedStringOfBlockContent = function (blockContent) {
	if (!blockContent.$) {
		var mmInline = blockContent.a;
		return '';
	} else {
		var str = blockContent.a;
		return str;
	}
};
var $jxxcarlson$elm_markdown$Markdown$Render$blockLevelClass = function (k) {
	return $elm$html$Html$Attributes$class(
		'mm-block-' + $elm$core$String$fromInt(k));
};
var $elm$html$Html$code = _VirtualDom_node('code');
var $jxxcarlson$elm_markdown$BlockType$stringOfLanguage = function (lang_) {
	switch (lang_) {
		case 0:
			return 'elm';
		case 1:
			return 'css';
		case 2:
			return 'javascript';
		case 3:
			return 'json';
		case 4:
			return 'python';
		case 5:
			return 'sql';
		default:
			return 'xml';
	}
};
var $jxxcarlson$elm_markdown$BlockType$deleteLangPrefix = F2(
	function (lang, str) {
		return A2(
			$elm$core$String$dropLeft,
			$elm$core$String$length(
				$jxxcarlson$elm_markdown$BlockType$stringOfLanguage(lang)) + 1,
			str);
	});
var $elm$html$Html$hr = _VirtualDom_node('hr');
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (!ra.$) {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Render$marginOfLevel = function (level) {
	return A2(
		$elm$html$Html$Attributes$style,
		'margin-left',
		$elm$core$String$fromInt(0 * level) + 'px');
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme = $elm$core$Basics$identity;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule = function (a) {
	return {$: 1, a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$ClassExtends = 8;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$Css = function (a) {
	return {$: 3, a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$DeclarationKeyword = 3;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$Elm = function (a) {
	return {$: 0, a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex = function (a) {
	return {$: 1, a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Identifier = 0;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$Javascript = function (a) {
	return {$: 2, a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Property = {$: 3};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$TypeSignature = 6;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$bold = function (style) {
	return _Utils_update(
		style,
		{ad: true});
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$italic = function (style) {
	return _Utils_update(
		style,
		{ae: true});
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$DefaultColor = {$: 0};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$backgroundColor = function (background) {
	return {al: background, ad: false, ae: false, ap: false, a0: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$DefaultColor};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$noEmphasis = F2(
	function (text, background) {
		return {al: background, ad: false, ae: false, ap: false, a0: text};
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor = function (text) {
	return {al: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$DefaultColor, ad: false, ae: false, ap: false, a0: text};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Monokai$requiredStyles = {
	aj: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$backgroundColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#003800')),
	am: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#75715e')),
	an: A2(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$noEmphasis,
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#f8f8f2'),
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#23241f')),
	ao: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$backgroundColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#380000')),
	ab: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$backgroundColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#343434')),
	au: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#ae81ff')),
	av: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#e6db74')),
	aw: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#f92672')),
	ax: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#66d9ef')),
	ay: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#a6e22e')),
	az: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#ae81ff')),
	aA: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#fd971f'))
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Monokai$theme = {
	ce: _List_fromArray(
		[
			_Utils_Tuple2(
			_List_fromArray(
				[
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$Elm(6),
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$Javascript(3),
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$Css($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Property)
				]),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$italic(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#66d9ef')))),
			_Utils_Tuple2(
			_List_fromArray(
				[
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$Javascript(8)
				]),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$italic(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#a6e22e')))),
			_Utils_Tuple2(
			_List_fromArray(
				[
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$Css(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(0))
				]),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$bold(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$textColor(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Hex('#f92672'))))
		]),
	cQ: $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Monokai$requiredStyles
};
var $elm$core$Tuple$mapFirst = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			func(x),
			y);
	});
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$List$intersperse = F2(
	function (sep, xs) {
		if (!xs.b) {
			return _List_Nil;
		} else {
			var hd = xs.a;
			var tl = xs.b;
			var step = F2(
				function (x, rest) {
					return A2(
						$elm$core$List$cons,
						sep,
						A2($elm$core$List$cons, x, rest));
				});
			var spersed = A3($elm$core$List$foldr, step, _List_Nil, tl);
			return A2($elm$core$List$cons, hd, spersed);
		}
	});
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style1 = 2;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style2 = 3;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style3 = 4;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style4 = 5;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style5 = 6;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleToFragment = function (a) {
	switch (a) {
		case 0:
			return _Utils_Tuple2(4, 'css-ar-i');
		case 1:
			return _Utils_Tuple2(6, 'css-ar-p');
		case 2:
			return _Utils_Tuple2(4, 'css-ar-k');
		default:
			return _Utils_Tuple2(5, 'css-ar-v');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Default = 0;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style7 = 8;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeSelectorToFragment = function (att) {
	switch (att) {
		case 0:
			return _Utils_Tuple2(6, 'css-s-a-an');
		case 1:
			return _Utils_Tuple2(3, 'css-s-a-av');
		default:
			return _Utils_Tuple2(4, 'css-s-a-o');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$selectorToFragment = function (s) {
	switch (s.$) {
		case 0:
			return _Utils_Tuple2(4, 'css-s-e');
		case 1:
			return _Utils_Tuple2(6, 'css-s-i');
		case 2:
			return _Utils_Tuple2(6, 'css-s-cl');
		case 3:
			return _Utils_Tuple2(8, 'css-s-c');
		case 4:
			return _Utils_Tuple2(4, 'css-s-u');
		case 5:
			var att = s.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeSelectorToFragment(att);
		case 6:
			return _Utils_Tuple2(0, 'css-s-pe');
		default:
			return _Utils_Tuple2(0, 'css-s-pc');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$syntaxToStyle = function (syntax) {
	switch (syntax.$) {
		case 0:
			return _Utils_Tuple2(3, 'css-s');
		case 1:
			var a = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleToFragment(a);
		case 2:
			var s = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$selectorToFragment(s);
		case 3:
			return _Utils_Tuple2(5, 'css-p');
		case 4:
			return _Utils_Tuple2(5, 'css-pv');
		case 5:
			return _Utils_Tuple2(2, 'css-n');
		default:
			return _Utils_Tuple2(4, 'css-u');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Style6 = 7;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$syntaxToStyle = function (syntax) {
	switch (syntax) {
		case 0:
			return _Utils_Tuple2(3, 'elm-s');
		case 1:
			return _Utils_Tuple2(4, 'elm-bs');
		case 2:
			return _Utils_Tuple2(5, 'elm-gs');
		case 3:
			return _Utils_Tuple2(7, 'elm-c');
		case 4:
			return _Utils_Tuple2(4, 'elm-k');
		case 5:
			return _Utils_Tuple2(6, 'elm-f');
		case 6:
			return _Utils_Tuple2(5, 'elm-ts');
		default:
			return _Utils_Tuple2(2, 'elm-n');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$syntaxToStyle = function (syntax) {
	switch (syntax) {
		case 0:
			return _Utils_Tuple2(2, 'js-n');
		case 1:
			return _Utils_Tuple2(3, 'js-s');
		case 2:
			return _Utils_Tuple2(4, 'js-k');
		case 3:
			return _Utils_Tuple2(5, 'js-dk');
		case 4:
			return _Utils_Tuple2(5, 'js-fe');
		case 5:
			return _Utils_Tuple2(6, 'js-f');
		case 6:
			return _Utils_Tuple2(7, 'js-lk');
		case 7:
			return _Utils_Tuple2(8, 'js-p');
		default:
			return _Utils_Tuple2(6, 'js-ce');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$syntaxToStyle = function (syntax) {
	switch (syntax) {
		case 0:
			return _Utils_Tuple2(2, 'py-n');
		case 1:
			return _Utils_Tuple2(3, 'py-s');
		case 2:
			return _Utils_Tuple2(4, 'py-k');
		case 3:
			return _Utils_Tuple2(5, 'py-dk');
		case 5:
			return _Utils_Tuple2(6, 'py-f');
		case 6:
			return _Utils_Tuple2(7, 'py-lk');
		case 7:
			return _Utils_Tuple2(8, 'py-p');
		default:
			return _Utils_Tuple2(0, 'py-fe');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$syntaxToStyle = function (syntax) {
	switch (syntax) {
		case 0:
			return _Utils_Tuple2(2, 'sql-n');
		case 1:
			return _Utils_Tuple2(3, 'sql-s');
		case 2:
			return _Utils_Tuple2(4, 'sql-k');
		case 3:
			return _Utils_Tuple2(5, 'sql-o');
		case 4:
			return _Utils_Tuple2(6, 'sql-f');
		case 5:
			return _Utils_Tuple2(7, 'sql-p');
		default:
			return _Utils_Tuple2(8, 'sql-l');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$syntaxToStyle = function (syntax) {
	switch (syntax) {
		case 0:
			return _Utils_Tuple2(4, 'xml-t');
		case 1:
			return _Utils_Tuple2(6, 'xml-a');
		default:
			return _Utils_Tuple2(3, 'xlm-av');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$syntaxToSelector = function (syntax) {
	switch (syntax.$) {
		case 0:
			var elmSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$syntaxToStyle(elmSyntax).b;
		case 1:
			var xmlSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$syntaxToStyle(xmlSyntax).b;
		case 2:
			var jsSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$syntaxToStyle(jsSyntax).b;
		case 3:
			var cssSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$syntaxToStyle(cssSyntax).b;
		case 4:
			var pythonSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$syntaxToStyle(pythonSyntax).b;
		default:
			var sqlSyntax = syntax.a;
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$syntaxToStyle(sqlSyntax).b;
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$syntaxesToSelectors = function (syntaxes) {
	return $elm$core$String$concat(
		A2(
			$elm$core$List$intersperse,
			', ',
			A2(
				$elm$core$List$map,
				$elm$core$Basics$append('.elmsh-'),
				A2($elm$core$List$map, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$syntaxToSelector, syntaxes))));
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$colorToCss = F2(
	function (property, color) {
		switch (color.$) {
			case 0:
				return '';
			case 1:
				var hex = color.a;
				return property + (hex + ';');
			case 2:
				var r = color.a;
				var g = color.b;
				var b = color.c;
				return $elm$core$String$concat(
					_List_fromArray(
						[
							property,
							'rgb(',
							$elm$core$String$fromInt(r),
							', ',
							$elm$core$String$fromInt(g),
							',',
							$elm$core$String$fromInt(b),
							');'
						]));
			default:
				var r = color.a;
				var g = color.b;
				var b = color.c;
				var a = color.d;
				return $elm$core$String$concat(
					_List_fromArray(
						[
							property,
							'rgba(',
							$elm$core$String$fromInt(r),
							', ',
							$elm$core$String$fromInt(g),
							',',
							$elm$core$String$fromInt(b),
							', ',
							$elm$core$String$fromFloat(a),
							');'
						]));
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$emptyIfFalse = F2(
	function (bool, str) {
		return bool ? str : '';
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$styleToCss = function (_v0) {
	var isBold = _v0.ad;
	var isItalic = _v0.ae;
	var isUnderline = _v0.ap;
	var text = _v0.a0;
	var background = _v0.al;
	return $elm$core$String$concat(
		_List_fromArray(
			[
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$emptyIfFalse, isBold, 'font-weight: bold;'),
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$emptyIfFalse, isItalic, 'font-style: italic;'),
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$emptyIfFalse, isUnderline, 'text-decoration: underline;'),
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$colorToCss, 'color: ', text),
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$colorToCss, 'background: ', background)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$toCssClass = function (_v0) {
	var selectors = _v0.a;
	var style = _v0.b;
	return $elm$core$String$isEmpty(selectors) ? '' : (selectors + (' {' + ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$styleToCss(style) + '}')));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$toCss = function (classes) {
	return $elm$core$String$concat(
		A2($elm$core$List$map, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$toCssClass, classes));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$toCss = function (_v0) {
	var requiredStyles = _v0.cQ;
	var customStyles = _v0.ce;
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$toCss(
		_Utils_ap(
			_List_fromArray(
				[
					_Utils_Tuple2('.elmsh', requiredStyles.an),
					_Utils_Tuple2('.elmsh-hl', requiredStyles.ab),
					_Utils_Tuple2('.elmsh-add', requiredStyles.aj),
					_Utils_Tuple2('.elmsh-del', requiredStyles.ao),
					_Utils_Tuple2('.elmsh-comm', requiredStyles.am),
					_Utils_Tuple2('.elmsh1', requiredStyles.au),
					_Utils_Tuple2('.elmsh2', requiredStyles.av),
					_Utils_Tuple2('.elmsh3', requiredStyles.aw),
					_Utils_Tuple2('.elmsh4', requiredStyles.ax),
					_Utils_Tuple2('.elmsh5', requiredStyles.ay),
					_Utils_Tuple2('.elmsh6', requiredStyles.az),
					_Utils_Tuple2('.elmsh7', requiredStyles.aA)
				]),
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$mapFirst($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$syntaxesToSelectors),
				customStyles)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Monokai$css = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Type$toCss($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Monokai$theme);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$monokai = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$Monokai$css;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$monokai = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Theme$monokai;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$HCode = $elm$core$Basics$identity;
var $elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {a7: col, bA: problem, bK: row};
	});
var $elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3($elm$parser$Parser$DeadEnd, p.bK, p.a7, p.bA);
};
var $elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 0:
					return list;
				case 1:
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2($elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var $elm$parser$Parser$Advanced$run = F2(
	function (_v0, src) {
		var parse = _v0;
		var _v1 = parse(
			{a7: 1, d: _List_Nil, f: 1, b: 0, bK: 1, a: src});
		if (!_v1.$) {
			var value = _v1.b;
			return $elm$core$Result$Ok(value);
		} else {
			var bag = _v1.b;
			return $elm$core$Result$Err(
				A2($elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var $elm$parser$Parser$run = F2(
	function (parser, source) {
		var _v0 = A2($elm$parser$Parser$Advanced$run, parser, source);
		if (!_v0.$) {
			var a = _v0.a;
			return $elm$core$Result$Ok(a);
		} else {
			var problems = _v0.a;
			return $elm$core$Result$Err(
				A2($elm$core$List$map, $elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$newLine = function (fragments) {
	return {co: fragments, ab: $elm$core$Maybe$Nothing};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak = {$: 2};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Style$Comment = 1;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toFragment = F2(
	function (toStyle, _v0) {
		var syntax = _v0.a;
		var text = _v0.b;
		switch (syntax.$) {
			case 0:
				return {b4: '', cP: 0, a0: text};
			case 1:
				return {b4: '', cP: 1, a0: text};
			case 2:
				return {b4: '', cP: 0, a0: text};
			default:
				var c = syntax.a;
				var _v2 = toStyle(c);
				var requiredStyle = _v2.a;
				var additionalClass = _v2.b;
				return {b4: additionalClass, cP: requiredStyle, a0: text};
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLinesHelp = F3(
	function (toStyle, _v0, _v1) {
		var syntax = _v0.a;
		var text = _v0.b;
		var lines = _v1.a;
		var fragments = _v1.b;
		var maybeLastSyntax = _v1.c;
		if (_Utils_eq(syntax, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak)) {
			return _Utils_Tuple3(
				A2(
					$elm$core$List$cons,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$newLine(fragments),
					lines),
				_List_fromArray(
					[
						A2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toFragment,
						toStyle,
						_Utils_Tuple2(syntax, text))
					]),
				$elm$core$Maybe$Nothing);
		} else {
			if (_Utils_eq(
				$elm$core$Maybe$Just(syntax),
				maybeLastSyntax)) {
				if (fragments.b) {
					var headFrag = fragments.a;
					var tailFrags = fragments.b;
					return _Utils_Tuple3(
						lines,
						A2(
							$elm$core$List$cons,
							_Utils_update(
								headFrag,
								{
									a0: _Utils_ap(text, headFrag.a0)
								}),
							tailFrags),
						maybeLastSyntax);
				} else {
					return _Utils_Tuple3(
						lines,
						A2(
							$elm$core$List$cons,
							A2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toFragment,
								toStyle,
								_Utils_Tuple2(syntax, text)),
							fragments),
						maybeLastSyntax);
				}
			} else {
				return _Utils_Tuple3(
					lines,
					A2(
						$elm$core$List$cons,
						A2(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toFragment,
							toStyle,
							_Utils_Tuple2(syntax, text)),
						fragments),
					$elm$core$Maybe$Just(syntax));
			}
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLines = F2(
	function (toStyle, revTokens) {
		return function (_v0) {
			var lines = _v0.a;
			var frags = _v0.b;
			return A2(
				$elm$core$List$cons,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$newLine(frags),
				lines);
		}(
			A3(
				$elm$core$List$foldl,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLinesHelp(toStyle),
				_Utils_Tuple3(_List_Nil, _List_Nil, $elm$core$Maybe$Nothing),
				revTokens));
	});
var $elm$parser$Parser$Advanced$Parser = $elm$core$Basics$identity;
var $elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $elm$parser$Parser$Advanced$loopHelp = F4(
	function (p, state, callback, s0) {
		loopHelp:
		while (true) {
			var _v0 = callback(state);
			var parse = _v0;
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p1 = _v1.a;
				var step = _v1.b;
				var s1 = _v1.c;
				if (!step.$) {
					var newState = step.a;
					var $temp$p = p || p1,
						$temp$state = newState,
						$temp$callback = callback,
						$temp$s0 = s1;
					p = $temp$p;
					state = $temp$state;
					callback = $temp$callback;
					s0 = $temp$s0;
					continue loopHelp;
				} else {
					var result = step.a;
					return A3($elm$parser$Parser$Advanced$Good, p || p1, result, s1);
				}
			} else {
				var p1 = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p || p1, x);
			}
		}
	});
var $elm$parser$Parser$Advanced$loop = F2(
	function (state, callback) {
		return function (s) {
			return A4($elm$parser$Parser$Advanced$loopHelp, false, state, callback, s);
		};
	});
var $elm$parser$Parser$Advanced$map = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (!_v1.$) {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					func(a),
					s1);
			} else {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			}
		};
	});
var $elm$parser$Parser$map = $elm$parser$Parser$Advanced$map;
var $elm$parser$Parser$Advanced$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Advanced$Loop = function (a) {
	return {$: 0, a: a};
};
var $elm$parser$Parser$toAdvancedStep = function (step) {
	if (!step.$) {
		var s = step.a;
		return $elm$parser$Parser$Advanced$Loop(s);
	} else {
		var a = step.a;
		return $elm$parser$Parser$Advanced$Done(a);
	}
};
var $elm$parser$Parser$loop = F2(
	function (state, callback) {
		return A2(
			$elm$parser$Parser$Advanced$loop,
			state,
			function (s) {
				return A2(
					$elm$parser$Parser$map,
					$elm$parser$Parser$toAdvancedStep,
					callback(s));
			});
	});
var $elm$parser$Parser$Done = function (a) {
	return {$: 1, a: a};
};
var $elm$parser$Parser$Loop = function (a) {
	return {$: 0, a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal = {$: 0};
var $elm$core$Basics$always = F2(
	function (a, _v0) {
		return a;
	});
var $elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _v0) {
		var parseA = _v0;
		return function (s0) {
			var _v1 = parseA(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				var _v2 = callback(a);
				var parseB = _v2;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
					var p2 = _v3.a;
					var x = _v3.b;
					return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _v3.a;
					var b = _v3.b;
					var s2 = _v3.c;
					return A3($elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
				}
			}
		};
	});
var $elm$parser$Parser$andThen = $elm$parser$Parser$Advanced$andThen;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C = function (a) {
	return {$: 3, a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Prefix = 1;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$String = {$: 0};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRuleValue = 3;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Keyword = 2;
var $elm$parser$Parser$UnexpectedChar = {$: 11};
var $elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 1, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {a7: col, cd: contextStack, bA: problem, bK: row};
	});
var $elm$parser$Parser$Advanced$Empty = {$: 0};
var $elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, s.bK, s.a7, x, s.d));
	});
var $elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var $elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return function (s) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, s.b, s.a);
			return _Utils_eq(newOffset, -1) ? A2(
				$elm$parser$Parser$Advanced$Bad,
				false,
				A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				0,
				{a7: 1, d: s.d, f: s.f, b: s.b + 1, bK: s.bK + 1, a: s.a}) : A3(
				$elm$parser$Parser$Advanced$Good,
				true,
				0,
				{a7: s.a7 + 1, d: s.d, f: s.f, b: newOffset, bK: s.bK, a: s.a}));
		};
	});
var $elm$parser$Parser$chompIf = function (isGood) {
	return A2($elm$parser$Parser$Advanced$chompIf, isGood, $elm$parser$Parser$UnexpectedChar);
};
var $elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3($elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.a);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					$elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.b, offset) < 0,
					0,
					{a7: col, d: s0.d, f: s0.f, b: offset, bK: row, a: s0.a});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return function (s) {
		return A5($elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.b, s.bK, s.a7, s);
	};
};
var $elm$parser$Parser$chompWhile = $elm$parser$Parser$Advanced$chompWhile;
var $elm$parser$Parser$Advanced$map2 = F3(
	function (func, _v0, _v1) {
		var parseA = _v0;
		var parseB = _v1;
		return function (s0) {
			var _v2 = parseA(s0);
			if (_v2.$ === 1) {
				var p = _v2.a;
				var x = _v2.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p1 = _v2.a;
				var a = _v2.b;
				var s1 = _v2.c;
				var _v3 = parseB(s1);
				if (_v3.$ === 1) {
					var p2 = _v3.a;
					var x = _v3.b;
					return A2($elm$parser$Parser$Advanced$Bad, p1 || p2, x);
				} else {
					var p2 = _v3.a;
					var b = _v3.b;
					var s2 = _v3.c;
					return A3(
						$elm$parser$Parser$Advanced$Good,
						p1 || p2,
						A2(func, a, b),
						s2);
				}
			}
		};
	});
var $elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$always, keepParser, ignoreParser);
	});
var $elm$parser$Parser$ignorer = $elm$parser$Parser$Advanced$ignorer;
var $elm$parser$Parser$Advanced$succeed = function (a) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$Good, false, a, s);
	};
};
var $elm$parser$Parser$succeed = $elm$parser$Parser$Advanced$succeed;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile = function (isNotRelevant) {
	return A2(
		$elm$parser$Parser$ignorer,
		A2(
			$elm$parser$Parser$ignorer,
			$elm$parser$Parser$succeed(0),
			$elm$parser$Parser$chompIf(isNotRelevant)),
		$elm$parser$Parser$chompWhile(isNotRelevant));
};
var $elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _v0) {
		var parse = _v0;
		return function (s0) {
			var _v1 = parse(s0);
			if (_v1.$ === 1) {
				var p = _v1.a;
				var x = _v1.b;
				return A2($elm$parser$Parser$Advanced$Bad, p, x);
			} else {
				var p = _v1.a;
				var a = _v1.b;
				var s1 = _v1.c;
				return A3(
					$elm$parser$Parser$Advanced$Good,
					p,
					A2(
						func,
						A3($elm$core$String$slice, s0.b, s1.b, s0.a),
						a),
					s1);
			}
		};
	});
var $elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2($elm$parser$Parser$Advanced$mapChompedString, $elm$core$Basics$always, parser);
};
var $elm$parser$Parser$getChompedString = $elm$parser$Parser$Advanced$getChompedString;
var $elm$core$Set$Set_elm_builtin = $elm$core$Basics$identity;
var $elm$core$Set$empty = $elm$core$Dict$empty;
var $elm$core$Set$insert = F2(
	function (key, _v0) {
		var dict = _v0;
		return A3($elm$core$Dict$insert, key, 0, dict);
	});
var $elm$core$Set$fromList = function (list) {
	return A3($elm$core$List$foldl, $elm$core$Set$insert, $elm$core$Set$empty, list);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleKeywordSet = $elm$core$Set$fromList(
	_List_fromArray(
		['and', 'or', 'not', 'only']));
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (!_v0.$) {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Set$member = F2(
	function (key, _v0) {
		var dict = _v0;
		return A2($elm$core$Dict$member, key, dict);
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isAtRuleKeyword = function (n) {
	return A2($elm$core$Set$member, n, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleKeywordSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isCommentChar = function (c) {
	return c === '/';
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak = function (c) {
	return c === '\n';
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace = function (c) {
	return (c === ' ') || (c === '\t');
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace = function (c) {
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c);
};
var $elm$core$Basics$not = _Basics_not;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$selectorNameInvalidCharSet = $elm$core$Set$fromList(
	_List_fromArray(
		[':', '{', '}', ',', '.', '#', '>', '+', '~', '*', '[', ']', '|', ';', '(', ')']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isCommentChar(c) || A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$selectorNameInvalidCharSet)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleKeywordOrValue = function (revTokens) {
	return A2(
		$elm$parser$Parser$map,
		function (n) {
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isAtRuleKeyword(n) ? A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(2)),
					n),
				revTokens) : A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(3)),
					n),
				revTokens);
		},
		$elm$parser$Parser$getChompedString(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleSet = $elm$core$Set$fromList(
	_List_fromArray(
		['@page', '@font-face', '@swash', '@annotation', '@ornaments', '@stylistic', '@styleset', '@character-variant']));
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 2, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2($elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a;
				var remainingParsers = parsers.b;
				var _v1 = parse(s0);
				if (!_v1.$) {
					var step = _v1;
					return step;
				} else {
					var step = _v1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2($elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var $elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return function (s) {
		return A3($elm$parser$Parser$Advanced$oneOfHelp, s, $elm$parser$Parser$Advanced$Empty, parsers);
	};
};
var $elm$parser$Parser$oneOf = $elm$parser$Parser$Advanced$oneOf;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment = {$: 1};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$addThen = F3(
	function (f, list, plist) {
		return A2(
			$elm$parser$Parser$andThen,
			function (n) {
				return f(
					_Utils_ap(n, list));
			},
			plist);
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen = F3(
	function (f, list, pn) {
		return A2(
			$elm$parser$Parser$andThen,
			function (n) {
				return f(
					A2($elm$core$List$cons, n, list));
			},
			pn);
	});
var $elm$parser$Parser$ExpectingEnd = {$: 10};
var $elm$parser$Parser$Advanced$end = function (x) {
	return function (s) {
		return _Utils_eq(
			$elm$core$String$length(s.a),
			s.b) ? A3($elm$parser$Parser$Advanced$Good, false, 0, s) : A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $elm$parser$Parser$end = $elm$parser$Parser$Advanced$end($elm$parser$Parser$ExpectingEnd);
var $elm$parser$Parser$ExpectingSymbol = function (a) {
	return {$: 8, a: a};
};
var $elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var $elm$parser$Parser$Advanced$token = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(str);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, str, s.b, s.bK, s.a7, s.a);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			$elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{a7: newCol, d: s.d, f: s.f, b: newOffset, bK: newRow, a: s.a});
	};
};
var $elm$parser$Parser$Advanced$symbol = $elm$parser$Parser$Advanced$token;
var $elm$parser$Parser$symbol = function (str) {
	return $elm$parser$Parser$Advanced$symbol(
		A2(
			$elm$parser$Parser$Advanced$Token,
			str,
			$elm$parser$Parser$ExpectingSymbol(str)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile = F2(
	function (isNotRelevant, previousParser) {
		return A2(
			$elm$parser$Parser$ignorer,
			previousParser,
			$elm$parser$Parser$chompWhile(isNotRelevant));
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedUnnestable = F2(
	function (options, revAList) {
		var defaultMap = options.ba;
		var isNotRelevant = options.bp;
		var end = options.bb;
		var innerParsers = options.bm;
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(
						A2(
							$elm$core$List$cons,
							defaultMap(end),
							revAList)),
					$elm$parser$Parser$symbol(end)),
					A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(revAList),
					$elm$parser$Parser$end),
					A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$addThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedUnnestable(options),
					revAList,
					$elm$parser$Parser$oneOf(innerParsers)),
					A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedUnnestable(options),
					revAList,
					A2(
						$elm$parser$Parser$map,
						defaultMap,
						$elm$parser$Parser$getChompedString(
							A2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
								isNotRelevant,
								$elm$parser$Parser$chompIf(
									$elm$core$Basics$always(true))))))
				]));
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedNestable = F3(
	function (nestLevel, options, revAList) {
		var defaultMap = options.ba;
		var isNotRelevant = options.bp;
		var start = options.aZ;
		var end = options.bb;
		var innerParsers = options.bm;
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$andThen,
					function (n) {
						return (nestLevel === 1) ? $elm$parser$Parser$succeed(n) : A3($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedNestable, nestLevel - 1, options, n);
					},
					A2(
						$elm$parser$Parser$map,
						$elm$core$Basics$always(
							A2(
								$elm$core$List$cons,
								defaultMap(end),
								revAList)),
						$elm$parser$Parser$symbol(end))),
					A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen,
					A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedNestable, nestLevel + 1, options),
					revAList,
					A2(
						$elm$parser$Parser$map,
						defaultMap,
						$elm$parser$Parser$getChompedString(
							A2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
								isNotRelevant,
								$elm$parser$Parser$symbol(start))))),
					A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$addThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedUnnestable(options),
					revAList,
					$elm$parser$Parser$oneOf(innerParsers)),
					A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(revAList),
					$elm$parser$Parser$end),
					A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen,
					A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedNestable, nestLevel, options),
					revAList,
					A2(
						$elm$parser$Parser$map,
						defaultMap,
						$elm$parser$Parser$getChompedString(
							A2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
								isNotRelevant,
								$elm$parser$Parser$chompIf(
									$elm$core$Basics$always(true))))))
				]));
	});
var $elm$parser$Parser$Problem = function (a) {
	return {$: 12, a: a};
};
var $elm$parser$Parser$Advanced$problem = function (x) {
	return function (s) {
		return A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, x));
	};
};
var $elm$parser$Parser$problem = function (msg) {
	return $elm$parser$Parser$Advanced$problem(
		$elm$parser$Parser$Problem(msg));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedHelp = F2(
	function (options, revAList) {
		var start = options.aZ;
		var end = options.bb;
		var isNotRelevant = options.bp;
		var _v0 = _Utils_Tuple2(
			$elm$core$String$uncons(options.aZ),
			$elm$core$String$uncons(options.bb));
		if (_v0.a.$ === 1) {
			var _v1 = _v0.a;
			return $elm$parser$Parser$problem('Trying to parse a delimited helper, but the start token cannot be an empty string!');
		} else {
			if (_v0.b.$ === 1) {
				var _v2 = _v0.b;
				return $elm$parser$Parser$problem('Trying to parse a delimited helper, but the end token cannot be an empty string!');
			} else {
				var _v3 = _v0.a.a;
				var startChar = _v3.a;
				var _v4 = _v0.b.a;
				var endChar = _v4.a;
				return options.bo ? A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedNestable,
					1,
					_Utils_update(
						options,
						{
							bp: function (c) {
								return isNotRelevant(c) && ((!_Utils_eq(c, startChar)) && (!_Utils_eq(c, endChar)));
							}
						}),
					revAList) : A2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedUnnestable,
					_Utils_update(
						options,
						{
							bp: function (c) {
								return isNotRelevant(c) && (!_Utils_eq(c, endChar));
							}
						}),
					revAList);
			}
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited = function (options) {
	var start = options.aZ;
	var isNotRelevant = options.bp;
	var defaultMap = options.ba;
	return A2(
		$elm$parser$Parser$andThen,
		function (n) {
			return A2(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimitedHelp,
				options,
				_List_fromArray(
					[n]));
		},
		A2(
			$elm$parser$Parser$map,
			$elm$core$Basics$always(
				defaultMap(start)),
			$elm$parser$Parser$symbol(start)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$lineBreak = A2(
	$elm$parser$Parser$map,
	function (_v0) {
		return _List_fromArray(
			[
				_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak, '\n')
			]);
	},
	$elm$parser$Parser$symbol('\n'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$comment = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	{
		ba: function (b) {
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b);
		},
		bb: '*/',
		bm: _List_fromArray(
			[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$lineBreak]),
		bo: false,
		bp: function (c) {
			return !$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c);
		},
		aZ: '/*'
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace))),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(ns, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$lineBreak),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(ns, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$comment)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$keyframesOrCounterStyle = function (a) {
	return A2(
		$elm$parser$Parser$loop,
		_List_fromArray(
			[
				_Utils_Tuple2(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(0)),
				a)
			]),
		function (ns) {
			return $elm$parser$Parser$oneOf(
				_List_fromArray(
					[
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep(ns),
						A2(
						$elm$parser$Parser$map,
						function (b) {
							return $elm$parser$Parser$Loop(
								A2(
									$elm$core$List$cons,
									_Utils_Tuple2(
										$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
											$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(1)),
										b),
									ns));
						},
						$elm$parser$Parser$getChompedString(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar))),
						A2(
						$elm$parser$Parser$map,
						function (b) {
							return $elm$parser$Parser$Loop(
								A2(
									$elm$core$List$cons,
									_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
									ns));
						},
						$elm$parser$Parser$getChompedString(
							$elm$parser$Parser$chompIf(
								function (c) {
									return c !== '{';
								}))),
						$elm$parser$Parser$succeed(
						$elm$parser$Parser$Done(ns))
					]));
		});
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$nestableAtRuleOpener = function (ns) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				$elm$core$Basics$always(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '{'),
						ns)),
				$elm$parser$Parser$symbol('{')),
				$elm$parser$Parser$succeed(ns)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Number = {$: 5};
var $elm$parser$Parser$Advanced$backtrackable = function (_v0) {
	var parse = _v0;
	return function (s0) {
		var _v1 = parse(s0);
		if (_v1.$ === 1) {
			var x = _v1.b;
			return A2($elm$parser$Parser$Advanced$Bad, false, x);
		} else {
			var a = _v1.b;
			var s1 = _v1.c;
			return A3($elm$parser$Parser$Advanced$Good, false, a, s1);
		}
	};
};
var $elm$parser$Parser$backtrackable = $elm$parser$Parser$Advanced$backtrackable;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$escapableSet = $elm$core$Set$fromList(
	_List_fromArray(
		['\'', '\"', '\\', 'n', 'r', 't', 'b', 'f', 'v']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapableChar = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$escapableSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$escapable = A2(
	$elm$parser$Parser$ignorer,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed(0),
		$elm$parser$Parser$backtrackable(
			$elm$parser$Parser$symbol('\\'))),
	$elm$parser$Parser$chompIf($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapableChar));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$cssEscapable = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _List_fromArray(
			[
				_Utils_Tuple2(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Number),
				b)
			]);
	},
	$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$escapable));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapable = function (c) {
	return c === '\\';
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$quoteDelimiter = {
	ba: function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$String),
			b);
	},
	bb: '\'',
	bm: _List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$lineBreak, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$cssEscapable]),
	bo: false,
	bp: function (c) {
		return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapable(c));
	},
	aZ: '\''
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$doubleQuote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	_Utils_update(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$quoteDelimiter,
		{bb: '\"', aZ: '\"'}));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$quote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$quoteDelimiter);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringLiteral = function (revTokens) {
	return A2(
		$elm$parser$Parser$map,
		function (n) {
			return _Utils_ap(n, revTokens);
		},
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$quote, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$doubleQuote])));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$mediaOrSupports = function (a) {
	return A2(
		$elm$parser$Parser$andThen,
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$nestableAtRuleOpener,
		A2(
			$elm$parser$Parser$loop,
			_List_fromArray(
				[
					_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(0)),
					a)
				]),
			function (ns) {
				return $elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep(ns),
							A2(
							$elm$parser$Parser$map,
							$elm$parser$Parser$Loop,
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringLiteral(ns)),
							A2(
							$elm$parser$Parser$map,
							$elm$parser$Parser$Loop,
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleKeywordOrValue(ns)),
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return $elm$parser$Parser$Loop(
									A2(
										$elm$core$List$cons,
										_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
										ns));
							},
							$elm$parser$Parser$getChompedString(
								$elm$parser$Parser$chompIf(
									function (c) {
										return c !== '{';
									}))),
							$elm$parser$Parser$succeed(
							$elm$parser$Parser$Done(ns))
						]));
			}));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$PropertyValue = {$: 4};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringArg = F2(
	function (fnStr, revTokens) {
		return A2(
			$elm$parser$Parser$andThen,
			function (revT_) {
				return $elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringLiteral(revT_),
							A2(
							$elm$parser$Parser$map,
							function (n) {
								return A2(
									$elm$core$List$cons,
									_Utils_Tuple2(
										$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$String),
										n),
									revT_);
							},
							$elm$parser$Parser$getChompedString(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
									function (c) {
										return c !== ')';
									}))),
							$elm$parser$Parser$succeed(revT_)
						]));
			},
			A2(
				$elm$parser$Parser$map,
				$elm$core$Basics$always(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '('),
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$PropertyValue),
								fnStr),
							revTokens))),
				$elm$parser$Parser$symbol(fnStr + '(')));
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleHelper = function (a) {
	switch (a) {
		case '@import':
			return A2(
				$elm$parser$Parser$loop,
				_List_fromArray(
					[
						_Utils_Tuple2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(0)),
						a)
					]),
				function (ns) {
					return $elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep(ns),
								A2(
								$elm$parser$Parser$map,
								$elm$parser$Parser$Loop,
								A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringArg, 'url', ns)),
								A2(
								$elm$parser$Parser$map,
								$elm$parser$Parser$Loop,
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringLiteral(ns)),
								A2(
								$elm$parser$Parser$map,
								$elm$parser$Parser$Loop,
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleKeywordOrValue(ns)),
								A2(
								$elm$parser$Parser$map,
								function (b) {
									return $elm$parser$Parser$Loop(
										A2(
											$elm$core$List$cons,
											_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
											ns));
								},
								$elm$parser$Parser$getChompedString(
									$elm$parser$Parser$chompIf(
										function (c) {
											return c !== ';';
										}))),
								$elm$parser$Parser$succeed(
								$elm$parser$Parser$Done(ns))
							]));
				});
		case '@namespace':
			return A2(
				$elm$parser$Parser$loop,
				_List_fromArray(
					[
						_Utils_Tuple2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(0)),
						a)
					]),
				function (ns) {
					return $elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep(ns),
								A2(
								$elm$parser$Parser$map,
								$elm$parser$Parser$Loop,
								A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringArg, 'url', ns)),
								A2(
								$elm$parser$Parser$map,
								$elm$parser$Parser$Loop,
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringLiteral(ns)),
								A2(
								$elm$parser$Parser$map,
								function (b) {
									return $elm$parser$Parser$Loop(
										A2(
											$elm$core$List$cons,
											_Utils_Tuple2(
												$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
													$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(1)),
												b),
											ns));
								},
								$elm$parser$Parser$getChompedString(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar))),
								A2(
								$elm$parser$Parser$map,
								function (b) {
									return $elm$parser$Parser$Loop(
										A2(
											$elm$core$List$cons,
											_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
											ns));
								},
								$elm$parser$Parser$getChompedString(
									$elm$parser$Parser$chompIf(
										function (c) {
											return c !== ';';
										}))),
								$elm$parser$Parser$succeed(
								$elm$parser$Parser$Done(ns))
							]));
				});
		case '@charset':
			return A2(
				$elm$parser$Parser$loop,
				_List_fromArray(
					[
						_Utils_Tuple2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(0)),
						a)
					]),
				function (ns) {
					return $elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep(ns),
								A2(
								$elm$parser$Parser$map,
								$elm$parser$Parser$Loop,
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringLiteral(ns)),
								A2(
								$elm$parser$Parser$map,
								function (b) {
									return $elm$parser$Parser$Loop(
										A2(
											$elm$core$List$cons,
											_Utils_Tuple2(
												$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$String),
												b),
											ns));
								},
								$elm$parser$Parser$getChompedString(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar))),
								A2(
								$elm$parser$Parser$map,
								function (b) {
									return $elm$parser$Parser$Loop(
										A2(
											$elm$core$List$cons,
											_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
											ns));
								},
								$elm$parser$Parser$getChompedString(
									$elm$parser$Parser$chompIf(
										function (c) {
											return c !== ';';
										}))),
								$elm$parser$Parser$succeed(
								$elm$parser$Parser$Done(ns))
							]));
				});
		case '@media':
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$mediaOrSupports(a);
		case '@supports':
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$mediaOrSupports(a);
		case '@keyframes':
			return A2(
				$elm$parser$Parser$andThen,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$nestableAtRuleOpener,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$keyframesOrCounterStyle(a));
		case '@counter-style':
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$keyframesOrCounterStyle(a);
		case '@font-feature-values':
			return A2(
				$elm$parser$Parser$andThen,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$nestableAtRuleOpener,
				A2(
					$elm$parser$Parser$loop,
					_List_fromArray(
						[
							_Utils_Tuple2(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(0)),
							a)
						]),
					function (ns) {
						return $elm$parser$Parser$oneOf(
							_List_fromArray(
								[
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep(ns),
									A2(
									$elm$parser$Parser$map,
									function (b) {
										return $elm$parser$Parser$Loop(
											A2(
												$elm$core$List$cons,
												_Utils_Tuple2(
													$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
														$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(1)),
													b),
												ns));
									},
									$elm$parser$Parser$getChompedString(
										$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar))),
									A2(
									$elm$parser$Parser$map,
									function (b) {
										return $elm$parser$Parser$Loop(
											A2(
												$elm$core$List$cons,
												_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
												ns));
									},
									$elm$parser$Parser$getChompedString(
										$elm$parser$Parser$chompIf(
											function (c) {
												return c !== '{';
											}))),
									$elm$parser$Parser$succeed(
									$elm$parser$Parser$Done(ns))
								]));
					}));
		default:
			return A2($elm$core$Set$member, a, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleSet) ? $elm$parser$Parser$succeed(
				_List_fromArray(
					[
						_Utils_Tuple2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AtRule(0)),
						a)
					])) : $elm$parser$Parser$succeed(
				_List_fromArray(
					[
						_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, a)
					]));
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRule = A2(
	$elm$parser$Parser$andThen,
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRuleHelper,
	$elm$parser$Parser$getChompedString(
		A2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar,
			$elm$parser$Parser$symbol('@'))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isPropertyChar = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isCommentChar(c) || ((c === ':') || ((c === ';') || (c === '}')))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Unit = {$: 6};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$operatorCharSet = $elm$core$Set$fromList(
	_List_fromArray(
		['+', '-', '%', '*', '/']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isOperatorChar = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$operatorCharSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isPropertyValueChar = function (c) {
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isPropertyChar(c) && (!((c === '(') || ((c === ')') || ((c === ',') || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isOperatorChar(c)))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$hexColor = function (revTokens) {
	return A2(
		$elm$parser$Parser$map,
		function (n) {
			return A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Number),
					n),
				revTokens);
		},
		$elm$parser$Parser$getChompedString(
			A2(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isPropertyValueChar,
				$elm$parser$Parser$symbol('#'))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isNotPropertyValueChar = function (c) {
	return (c === '(') || ((c === ')') || ((c === ':') || ((c === ',') || (c === '/'))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$unitSet = $elm$core$Set$fromList(
	_List_fromArray(
		['em', 'ex', 'ch', 'rem', 'vw', 'vh', 'vmin', 'vmax', 'cm', 'mm', 'q', 'in', 'pt', 'pc', 'px', 'deg', 'grad', 'rad', 'turn', 's', 'ms', 'Hz', 'kHz', 'dpi', 'dpcm', 'dppx']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isUnit = function (n) {
	return A2($elm$core$Set$member, n, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$unitSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isNumber = function (c) {
	return $elm$core$Char$isDigit(c) || (c === '.');
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$positiveNumber = A2(
	$elm$parser$Parser$ignorer,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed(0),
		$elm$parser$Parser$chompIf($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isNumber)),
	$elm$parser$Parser$chompWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isNumber));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$negativeNumber = A2(
	$elm$parser$Parser$ignorer,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed(0),
		$elm$parser$Parser$backtrackable(
			$elm$parser$Parser$symbol('-'))),
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$positiveNumber);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$number = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$positiveNumber, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$negativeNumber]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$number = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Number),
			b);
	},
	$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$number));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$valueLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringLiteral(revTokens)),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$number),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$hexColor(revTokens)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringArg, 'url', revTokens)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringArg, 'format', revTokens)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringArg, 'local', revTokens)),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isUnit(n) ? $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Unit),
								n),
							revTokens)) : $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$PropertyValue),
								n),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isPropertyValueChar))),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isNotPropertyValueChar))),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Unit),
								b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isOperatorChar))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$valueHelper = function (opener) {
	return A2(
		$elm$parser$Parser$loop,
		_List_fromArray(
			[opener]),
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$valueLoop);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$value = A2(
	$elm$parser$Parser$andThen,
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$valueHelper,
	A2(
		$elm$parser$Parser$map,
		function (b) {
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
		},
		$elm$parser$Parser$getChompedString(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
				$elm$core$Basics$eq(':')))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$declarationLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Property),
								b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isPropertyChar))),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
						function (c) {
							return (c === ';') || (c === '/');
						}))),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$value),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$declarationBlockHelper = function (opener) {
	return A2(
		$elm$parser$Parser$loop,
		_List_fromArray(
			[opener]),
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$declarationLoop);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$declarationBlock = A2(
	$elm$parser$Parser$andThen,
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$declarationBlockHelper,
	A2(
		$elm$parser$Parser$map,
		function (b) {
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
		},
		$elm$parser$Parser$getChompedString(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
				function (c) {
					return c === '{';
				}))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Selector = function (a) {
	return {$: 2, a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AttributeName = 0;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AttributeSelector = function (a) {
	return {$: 5, a: a};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attSelOperatorCharSet = $elm$core$Set$fromList(
	_List_fromArray(
		['=', '~', '|', '^', '$', '*']));
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === -2) {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$core$Set$union = F2(
	function (_v0, _v1) {
		var dict1 = _v0;
		var dict2 = _v1;
		return A2($elm$core$Dict$union, dict1, dict2);
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$whitespaceCharSet = $elm$core$Set$fromList(
	_List_fromArray(
		[' ', '\t', '\n']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attSelNameInvalidCharSet = A2(
	$elm$core$Set$insert,
	']',
	A2($elm$core$Set$union, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attSelOperatorCharSet, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$whitespaceCharSet));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeName = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Selector(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AttributeSelector(0))),
			b);
	},
	$elm$parser$Parser$getChompedString(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
			function (c) {
				return !A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attSelNameInvalidCharSet);
			})));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AttributeOperator = 2;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeOperator = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Selector(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AttributeSelector(2))),
			b);
	},
	$elm$parser$Parser$getChompedString(
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					$elm$parser$Parser$symbol('~='),
					$elm$parser$Parser$symbol('|='),
					$elm$parser$Parser$symbol('^='),
					$elm$parser$Parser$symbol('$='),
					$elm$parser$Parser$symbol('*='),
					$elm$parser$Parser$symbol('=')
				]))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AttributeValue = 1;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeValue = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$stringLiteral(revTokens)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				A2(
					$elm$parser$Parser$map,
					function (b) {
						return A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Selector(
										$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$AttributeSelector(1))),
								b),
							revTokens);
					},
					$elm$parser$Parser$getChompedString(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
							function (c) {
								return (c !== ']') && (!$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c));
							})))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeSelectorLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeName),
				A2(
				$elm$parser$Parser$andThen,
				function (operator) {
					return A2(
						$elm$parser$Parser$map,
						function (n) {
							return $elm$parser$Parser$Loop(
								_Utils_ap(n, revTokens));
						},
						A2(
							$elm$parser$Parser$loop,
							_List_fromArray(
								[operator]),
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeValue));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeOperator),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeSelector = A2(
	$elm$parser$Parser$andThen,
	function (opener) {
		return A2(
			$elm$parser$Parser$loop,
			_List_fromArray(
				[opener]),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeSelectorLoop);
	},
	A2(
		$elm$parser$Parser$map,
		$elm$core$Basics$always(
			_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '[')),
		$elm$parser$Parser$symbol('[')));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Class = {$: 2};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$class = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Class, b);
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar,
			$elm$parser$Parser$symbol('.'))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Combinator = {$: 3};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$combinator = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Combinator, b);
	},
	$elm$parser$Parser$getChompedString(
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					$elm$parser$Parser$symbol('+'),
					$elm$parser$Parser$symbol('~'),
					$elm$parser$Parser$symbol('>')
				]))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Element = {$: 0};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$element = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Element, b);
	},
	$elm$parser$Parser$getChompedString(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Id = {$: 1};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$id = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Id, b);
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar,
			$elm$parser$Parser$symbol('#'))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$PseudoClass = {$: 7};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$pseudoClass = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$PseudoClass, b);
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar,
			$elm$parser$Parser$symbol(':'))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$PseudoElement = {$: 6};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$pseudoElement = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$PseudoElement, b);
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$isSelectorNameChar,
			$elm$parser$Parser$symbol('::'))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Universal = {$: 4};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$universal = A2(
	$elm$parser$Parser$map,
	$elm$core$Basics$always(
		_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Universal, '*')),
	$elm$parser$Parser$symbol('*'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$selector = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$map,
			function (_v0) {
				var n = _v0.a;
				var s = _v0.b;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$Selector(n)),
						s)
					]);
			},
			$elm$parser$Parser$oneOf(
				_List_fromArray(
					[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$id, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$class, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$element, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$universal, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$combinator, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$pseudoElement, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$pseudoClass]))),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$attributeSelector
		]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$mainLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$atRule),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$selector),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$declarationBlock),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$elm$parser$Parser$chompIf(
						$elm$core$Basics$always(true)))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$toRevTokens = A2($elm$parser$Parser$loop, _List_Nil, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$mainLoop);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$toLines = A2(
	$elm$core$Basics$composeR,
	$elm$parser$Parser$run($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$toRevTokens),
	$elm$core$Result$map(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLines($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$syntaxToStyle)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$css = A2(
	$elm$core$Basics$composeR,
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Css$toLines,
	$elm$core$Result$map($elm$core$Basics$identity));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$inlineComment = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _List_fromArray(
			[
				_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b)
			]);
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak),
			$elm$parser$Parser$symbol('--'))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreakList = A2(
	$elm$parser$Parser$map,
	function (_v0) {
		return _List_fromArray(
			[
				_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak, '\n')
			]);
	},
	$elm$parser$Parser$symbol('\n'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$multilineComment = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	{
		ba: function (b) {
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b);
		},
		bb: '-}',
		bm: _List_fromArray(
			[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreakList]),
		bo: true,
		bp: function (c) {
			return !$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c);
		},
		aZ: '{-'
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$comment = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$inlineComment, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$multilineComment]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$BasicSymbol = 1;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Capitalized = 3;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$GroupSymbol = 2;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Keyword = 4;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Number = 7;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$basicSymbols = $elm$core$Set$fromList(
	_List_fromArray(
		['|', '.', '=', '\\', '/', '(', ')', '-', '>', '<', ':', '+', '!', '$', '%', '&', '*']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isBasicSymbol = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$basicSymbols);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$basicSymbol = $elm$parser$Parser$getChompedString(
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isBasicSymbol));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$groupSymbols = $elm$core$Set$fromList(
	_List_fromArray(
		[',', '[', ']', '{', '}']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isGroupSymbol = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$groupSymbols);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isStringLiteralChar = function (c) {
	return (c === '\"') || (c === '\'');
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isVariableChar = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isBasicSymbol(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isGroupSymbol(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isStringLiteralChar(c))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$capitalized = $elm$parser$Parser$getChompedString(
	A2(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isVariableChar,
		$elm$parser$Parser$chompIf($elm$core$Char$isUpper)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$groupSymbol = $elm$parser$Parser$getChompedString(
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isGroupSymbol));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$Function = 5;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$infixSet = $elm$core$Set$fromList(
	_List_fromArray(
		['+', '-', '/', '*', '=', '.', '$', '<', '>', ':', '&', '|', '^', '?', '%', '#', '@', '~', '!', ',']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isInfixChar = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$infixSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$infixParser = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(5),
			b);
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$elm$parser$Parser$ignorer,
			A2(
				$elm$parser$Parser$ignorer,
				A2(
					$elm$parser$Parser$ignorer,
					$elm$parser$Parser$succeed(0),
					$elm$parser$Parser$backtrackable(
						$elm$parser$Parser$symbol('('))),
				$elm$parser$Parser$backtrackable(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isInfixChar))),
			$elm$parser$Parser$backtrackable(
				$elm$parser$Parser$symbol(')')))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$keywordSet = $elm$core$Set$fromList(
	_List_fromArray(
		['as', 'where', 'let', 'in', 'if', 'else', 'then', 'case', 'of', 'type', 'alias']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isKeyword = function (str) {
	return A2($elm$core$Set$member, str, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$keywordSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$variable = $elm$parser$Parser$getChompedString(
	A2(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isVariableChar,
		$elm$parser$Parser$chompIf($elm$core$Char$isLower)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$weirdText = $elm$parser$Parser$getChompedString(
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isVariableChar));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBodyContent = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(7),
					b);
			},
			$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$number)),
			A2(
			$elm$parser$Parser$map,
			$elm$core$Basics$always(
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(3),
					'()')),
			$elm$parser$Parser$symbol('()')),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$infixParser,
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(1),
					b);
			},
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$basicSymbol),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(2),
					b);
			},
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$groupSymbol),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(3),
					b);
			},
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$capitalized),
			A2(
			$elm$parser$Parser$map,
			function (n) {
				return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isKeyword(n) ? _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
					n) : _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, n);
			},
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$variable),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
			},
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$weirdText)
		]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$String = 0;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$elmEscapable = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _List_fromArray(
			[
				_Utils_Tuple2(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(3),
				b)
			]);
	},
	$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$escapable));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringDelimiter = {
	ba: function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(0),
			b);
	},
	bb: '\"',
	bm: _List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreakList, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$elmEscapable]),
	bo: false,
	bp: function (c) {
		return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapable(c));
	},
	aZ: '\"'
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$doubleQuote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringDelimiter);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$quote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	_Utils_update(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringDelimiter,
		{bb: '\'', aZ: '\''}));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$tripleDoubleQuote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	_Utils_update(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringDelimiter,
		{bb: '\"\"\"', aZ: '\"\"\"'}));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringLiteral = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$tripleDoubleQuote, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$doubleQuote, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$quote]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreak = A2(
	$elm$parser$Parser$map,
	function (_v0) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak, '\n');
	},
	$elm$parser$Parser$symbol('\n'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$space = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
	},
	$elm$parser$Parser$getChompedString(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$checkContext = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$space),
				A2(
				$elm$parser$Parser$andThen,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$checkContext,
				A2(
					$elm$parser$Parser$map,
					function (n) {
						return A2($elm$core$List$cons, n, revTokens);
					},
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreak)),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$comment)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(ns, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringLiteral),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBodyContent),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigIsNotRelevant = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || ((c === '(') || ((c === ')') || ((c === '-') || (c === ',')))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigContentHelp = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$map,
			$elm$core$Basics$always(
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(6),
					'()')),
			$elm$parser$Parser$symbol('()')),
			A2(
			$elm$parser$Parser$map,
			$elm$core$Basics$always(
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(1),
					'->')),
			$elm$parser$Parser$symbol('->')),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
			},
			$elm$parser$Parser$getChompedString(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
					function (c) {
						return (c === '(') || ((c === ')') || ((c === '-') || (c === ',')));
					}))),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(6),
					b);
			},
			$elm$parser$Parser$getChompedString(
				A2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigIsNotRelevant,
					$elm$parser$Parser$chompIf($elm$core$Char$isUpper)))),
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
			},
			$elm$parser$Parser$getChompedString(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigIsNotRelevant)))
		]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigContent = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigContentHelp),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionSignature = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				A2(
					$elm$parser$Parser$andThen,
					function (ns) {
						return A2($elm$parser$Parser$loop, ns, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$fnSigContent);
					},
					A2(
						$elm$parser$Parser$map,
						$elm$core$Basics$always(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(1),
									':'),
								revTokens)),
						$elm$parser$Parser$symbol(':')))),
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				A2($elm$parser$Parser$loop, revTokens, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody)),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isCommentChar = function (c) {
	return (c === '-') || (c === '{');
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$commentChar = $elm$parser$Parser$getChompedString(
	$elm$parser$Parser$chompIf($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isCommentChar));
var $elm$parser$Parser$ExpectingKeyword = function (a) {
	return {$: 9, a: a};
};
var $elm$parser$Parser$Advanced$keyword = function (_v0) {
	var kwd = _v0.a;
	var expecting = _v0.b;
	var progress = !$elm$core$String$isEmpty(kwd);
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$isSubString, kwd, s.b, s.bK, s.a7, s.a);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return (_Utils_eq(newOffset, -1) || (0 <= A3(
			$elm$parser$Parser$Advanced$isSubChar,
			function (c) {
				return $elm$core$Char$isAlphaNum(c) || (c === '_');
			},
			newOffset,
			s.a))) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A2($elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
			$elm$parser$Parser$Advanced$Good,
			progress,
			0,
			{a7: newCol, d: s.d, f: s.f, b: newOffset, bK: newRow, a: s.a});
	};
};
var $elm$parser$Parser$keyword = function (kwd) {
	return $elm$parser$Parser$Advanced$keyword(
		A2(
			$elm$parser$Parser$Advanced$Token,
			kwd,
			$elm$parser$Parser$ExpectingKeyword(kwd)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecIsNotRelevant = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isCommentChar(c) || (c === '(')));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mdpIsNotRelevant = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isCommentChar(c) || ((c === '(') || ((c === ')') || ((c === ',') || (c === '.'))))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mdpnIsSpecialChar = function (c) {
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isCommentChar(c) || ((c === '(') || (c === ')')));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$checkContextNested = function (_v1) {
	var nestLevel = _v1.a;
	var revTokens = _v1.b;
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStepNested(
				_Utils_Tuple2(nestLevel, revTokens)),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStepNested = function (_v0) {
	var nestLevel = _v0.a;
	var revTokens = _v0.b;
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_Tuple2(
							nestLevel,
							A2($elm$core$List$cons, n, revTokens)));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$space),
				A2(
				$elm$parser$Parser$andThen,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$checkContextNested,
				A2(
					$elm$parser$Parser$map,
					function (n) {
						return _Utils_Tuple2(
							nestLevel,
							A2($elm$core$List$cons, n, revTokens));
					},
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreak)),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_Tuple2(
							nestLevel,
							_Utils_ap(n, revTokens)));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$comment)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecParNest = function (_v0) {
	var nestLevel = _v0.a;
	var revTokens = _v0.b;
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStepNested(
				_Utils_Tuple2(nestLevel, revTokens)),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return $elm$parser$Parser$Loop(
						_Utils_Tuple2(nestLevel + 1, ns));
				},
				A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '('),
							revTokens)),
					$elm$parser$Parser$symbol('('))),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return (!nestLevel) ? $elm$parser$Parser$Done(ns) : $elm$parser$Parser$Loop(
						_Utils_Tuple2(nestLevel - 1, ns));
				},
				A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, ')'),
							revTokens)),
					$elm$parser$Parser$symbol(')'))),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_Tuple2(
							nestLevel,
							A2($elm$core$List$cons, n, revTokens)));
				},
				$elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
							},
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$commentChar),
							A2(
							$elm$parser$Parser$map,
							function (s) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, s);
							},
							$elm$parser$Parser$getChompedString(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
									A2($elm$core$Basics$composeL, $elm$core$Basics$not, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mdpnIsSpecialChar))))
						]))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecParentheses = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				A2(
					$elm$parser$Parser$map,
					$elm$core$Basics$always(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, ')'),
							revTokens)),
					$elm$parser$Parser$symbol(')'))),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$infixParser,
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
							},
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$commentChar),
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
							},
							$elm$parser$Parser$getChompedString(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
									function (c) {
										return (c === ',') || (c === '.');
									}))),
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(6),
									b);
							},
							$elm$parser$Parser$getChompedString(
								A2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mdpIsNotRelevant,
									$elm$parser$Parser$chompIf($elm$core$Char$isUpper)))),
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(5),
									b);
							},
							$elm$parser$Parser$getChompedString(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mdpIsNotRelevant)))
						]))),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (n) {
						return A2(
							$elm$parser$Parser$loop,
							_Utils_Tuple2(0, n),
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecParNest);
					},
					A2(
						$elm$parser$Parser$map,
						$elm$core$Basics$always(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '('),
								revTokens)),
						$elm$parser$Parser$symbol('(')))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$moduleDeclaration = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (n) {
						return A2($elm$parser$Parser$loop, n, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecParentheses);
					},
					A2(
						$elm$parser$Parser$map,
						$elm$core$Basics$always(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '('),
								revTokens)),
						$elm$parser$Parser$symbol('(')))),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$elm$parser$Parser$oneOf(
					_List_fromArray(
						[
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
							},
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$commentChar),
							A2(
							$elm$parser$Parser$map,
							$elm$core$Basics$always(
								_Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
									'exposing')),
							$elm$parser$Parser$keyword('exposing')),
							A2(
							$elm$parser$Parser$map,
							$elm$core$Basics$always(
								_Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
									'as')),
							$elm$parser$Parser$keyword('as')),
							A2(
							$elm$parser$Parser$map,
							function (b) {
								return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
							},
							$elm$parser$Parser$getChompedString(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$modDecIsNotRelevant)))
						]))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$portDeclarationHelp = F2(
	function (revTokens, str) {
		return (str === 'module') ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
					str),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$moduleDeclaration) : A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(5),
					str),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionSignature);
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$portDeclaration = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				A2(
					$elm$parser$Parser$andThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$portDeclarationHelp(revTokens),
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$variable)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Done,
				A2($elm$parser$Parser$loop, revTokens, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody)),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineStartVariable = F2(
	function (revTokens, n) {
		return ((n === 'module') || (n === 'import')) ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$moduleDeclaration) : ((n === 'port') ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$portDeclaration) : ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$isKeyword(n) ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody) : A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(5),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionSignature)));
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mainLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$space),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineBreak),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$comment),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$lineStartVariable(revTokens),
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$variable)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (s) {
						return A2(
							$elm$parser$Parser$loop,
							_Utils_ap(s, revTokens),
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody);
					},
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$stringLiteral)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (s) {
						return A2(
							$elm$parser$Parser$loop,
							A2($elm$core$List$cons, s, revTokens),
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBody);
					},
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$functionBodyContent)),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$toRevTokens = A2($elm$parser$Parser$loop, _List_Nil, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$mainLoop);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$toLines = A2(
	$elm$core$Basics$composeR,
	$elm$parser$Parser$run($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$toRevTokens),
	$elm$core$Result$map(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLines($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$syntaxToStyle)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$elm = A2(
	$elm$core$Basics$composeR,
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Elm$toLines,
	$elm$core$Result$map($elm$core$Basics$identity));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$groupSet = $elm$core$Set$fromList(
	_List_fromArray(
		['{', '}', '(', ')', '[', ']', ',', ';']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isGroupChar = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$groupSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$groupChar = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
	},
	$elm$parser$Parser$getChompedString(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isGroupChar)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isCommentChar = function (c) {
	return c === '/';
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$operatorSet = $elm$core$Set$fromList(
	_List_fromArray(
		['+', '-', '*', '/', '=', '!', '<', '>', '&', '|', '?', '^', ':', '~', '%', '.']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$punctuactorSet = A2($elm$core$Set$union, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$operatorSet, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$groupSet);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isPunctuaction = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$punctuactorSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isStringLiteralChar = function (c) {
	return (c === '\"') || ((c === '\'') || (c === '`'));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isIdentifierNameChar = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isPunctuaction(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isStringLiteralChar(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isCommentChar(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$Function = 5;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$Keyword = 2;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$LiteralKeyword = 6;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$Param = 7;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$inlineComment = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _List_fromArray(
			[
				_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b)
			]);
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak),
			$elm$parser$Parser$symbol('//'))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$lineBreakList = A2(
	$elm$parser$Parser$map,
	function (_v0) {
		return _List_fromArray(
			[
				_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak, '\n')
			]);
	},
	$elm$parser$Parser$symbol('\n'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$multilineComment = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	{
		ba: function (b) {
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b);
		},
		bb: '*/',
		bm: _List_fromArray(
			[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$lineBreakList]),
		bo: false,
		bp: function (c) {
			return !$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c);
		},
		aZ: '/*'
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$comment = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$inlineComment, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$multilineComment]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$whitespaceOrCommentStep = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace))),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(ns, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$lineBreakList),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(ns, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$comment)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$classExtendsLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(8),
								b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isIdentifierNameChar))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$classDeclarationLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$andThen,
				function (n) {
					return (n === 'extends') ? A2(
						$elm$parser$Parser$map,
						$elm$parser$Parser$Loop,
						A2(
							$elm$parser$Parser$loop,
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(2),
									n),
								revTokens),
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$classExtendsLoop)) : $elm$parser$Parser$succeed(
						$elm$parser$Parser$Loop(
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2(
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(5),
									n),
								revTokens)));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isIdentifierNameChar))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$argLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(7),
								b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
						function (c) {
							return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isCommentChar(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || ((c === ',') || (c === ')'))));
						}))),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
						function (c) {
							return (c === '/') || (c === ',');
						}))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$functionDeclarationLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(5),
								b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isIdentifierNameChar))),
				A2(
				$elm$parser$Parser$map,
				function (_v0) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(2),
								'*'),
							revTokens));
				},
				$elm$parser$Parser$symbol('*')),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (_v1) {
						return A2(
							$elm$parser$Parser$loop,
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '('),
								revTokens),
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$argLoop);
					},
					$elm$parser$Parser$symbol('('))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$FunctionEval = 4;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$functionEvalLoop = F3(
	function (identifier, revTokens, thisRevToken) {
		return $elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$whitespaceOrCommentStep(thisRevToken),
					A2(
					$elm$parser$Parser$map,
					function (_v0) {
						return $elm$parser$Parser$Done(
							_Utils_ap(
								A2(
									$elm$core$List$cons,
									_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '('),
									thisRevToken),
								A2(
									$elm$core$List$cons,
									_Utils_Tuple2(
										$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
										identifier),
									revTokens)));
					},
					$elm$parser$Parser$symbol('(')),
					$elm$parser$Parser$succeed(
					$elm$parser$Parser$Done(
						_Utils_ap(
							thisRevToken,
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, identifier),
								revTokens))))
				]));
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$declarationKeywordSet = $elm$core$Set$fromList(
	_List_fromArray(
		['var', 'const', 'let']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isDeclarationKeyword = function (str) {
	return A2($elm$core$Set$member, str, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$declarationKeywordSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$keywordSet = $elm$core$Set$fromList(
	_List_fromArray(
		['break', 'do', 'instanceof', 'typeof', 'case', 'else', 'new', 'catch', 'finally', 'return', 'void', 'continue', 'for', 'switch', 'while', 'debugger', 'this', 'with', 'default', 'if', 'throw', 'delete', 'in', 'try', 'enum', 'extends', 'export', 'import', 'implements', 'private', 'public', 'yield', 'interface', 'package', 'protected']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isKeyword = function (str) {
	return A2($elm$core$Set$member, str, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$keywordSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$literalKeywordSet = $elm$core$Set$fromList(
	_List_fromArray(
		['true', 'false', 'null', 'undefined', 'NaN', 'Infinity']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isLiteralKeyword = function (str) {
	return A2($elm$core$Set$member, str, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$literalKeywordSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$keywordParser = F2(
	function (revTokens, n) {
		return ((n === 'function') || (n === 'static')) ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(3),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$functionDeclarationLoop) : ((n === 'class') ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(3),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$classDeclarationLoop) : (((n === 'this') || (n === 'super')) ? $elm$parser$Parser$succeed(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(7),
					n),
				revTokens)) : ((n === 'constructor') ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(5),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$functionDeclarationLoop) : ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isKeyword(n) ? $elm$parser$Parser$succeed(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(2),
					n),
				revTokens)) : ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isDeclarationKeyword(n) ? $elm$parser$Parser$succeed(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(3),
					n),
				revTokens)) : ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isLiteralKeyword(n) ? $elm$parser$Parser$succeed(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(6),
					n),
				revTokens)) : A2(
			$elm$parser$Parser$loop,
			_List_Nil,
			A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$functionEvalLoop, n, revTokens))))))));
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$Number = 0;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$number = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(0),
			b);
	},
	$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$number));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isOperatorChar = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$operatorSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$operatorChar = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(2),
			b);
	},
	$elm$parser$Parser$getChompedString(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isOperatorChar)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$String = 1;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$jsEscapable = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _List_fromArray(
			[
				_Utils_Tuple2(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(6),
				b)
			]);
	},
	$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$escapable));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$quoteDelimiter = {
	ba: function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(1),
			b);
	},
	bb: '\'',
	bm: _List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$lineBreakList, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$jsEscapable]),
	bo: false,
	bp: function (c) {
		return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapable(c));
	},
	aZ: '\''
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$doubleQuote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	_Utils_update(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$quoteDelimiter,
		{bb: '\"', aZ: '\"'}));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$quote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$quoteDelimiter);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$templateString = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	_Utils_update(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$quoteDelimiter,
		{
			bb: '`',
			bm: _List_fromArray(
				[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$lineBreakList, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$jsEscapable]),
			bp: function (c) {
				return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapable(c));
			},
			aZ: '`'
		}));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$stringLiteral = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$quote, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$doubleQuote, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$templateString]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$mainLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (s) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(s, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$stringLiteral),
				A2(
				$elm$parser$Parser$map,
				function (s) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, s, revTokens));
				},
				$elm$parser$Parser$oneOf(
					_List_fromArray(
						[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$operatorChar, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$groupChar, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$number]))),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$keywordParser(revTokens),
					$elm$parser$Parser$getChompedString(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$isIdentifierNameChar)))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$toRevTokens = A2($elm$parser$Parser$loop, _List_Nil, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$mainLoop);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$toLines = A2(
	$elm$core$Basics$composeR,
	$elm$parser$Parser$run($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$toRevTokens),
	$elm$core$Result$map(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLines($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$syntaxToStyle)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$javascript = A2(
	$elm$core$Basics$composeR,
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Javascript$toLines,
	$elm$core$Result$map($elm$core$Basics$identity));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$syntaxToStyle = function (syntax) {
	switch (syntax) {
		case 0:
			return _Utils_Tuple2(3, 'json-s');
		case 1:
			return _Utils_Tuple2(2, 'json-e');
		case 2:
			return _Utils_Tuple2(2, 'json-n');
		case 3:
			return _Utils_Tuple2(4, 'json-b');
		case 4:
			return _Utils_Tuple2(4, 'json-null');
		case 5:
			return _Utils_Tuple2(5, 'json-k');
		case 6:
			return _Utils_Tuple2(0, 'json-o');
		default:
			return _Utils_Tuple2(0, 'json-a');
	}
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$Array = 7;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$Boolean = 3;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$Null = 4;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$Object = 6;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$ObjectKey = 5;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$String = 0;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$Number = 2;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$number = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(2),
			b);
	},
	$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$number));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$lineBreak = A2(
	$elm$parser$Parser$map,
	function (_v0) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak, '\n');
	},
	$elm$parser$Parser$symbol('\n'));
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$Escapable = 1;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$escapableSet = $elm$core$Set$fromList(
	_List_fromArray(
		['\"', '\\', '/', 'b', 'f', 'n', 'r', 't', 'u']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$isEscapableChar = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$escapableSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$escapable = A2(
	$elm$parser$Parser$ignorer,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed(0),
		$elm$parser$Parser$backtrackable(
			$elm$parser$Parser$symbol('\\'))),
	$elm$parser$Parser$chompIf($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$isEscapableChar));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$stringEscapable = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _List_fromArray(
			[
				_Utils_Tuple2(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(1),
				b)
			]);
	},
	$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$escapable));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$doubleQuoteDelimiter = function (syntax_) {
	return {
		ba: function (b) {
			return _Utils_Tuple2(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(syntax_),
				b);
		},
		bb: '\"',
		bm: _List_fromArray(
			[
				A2($elm$parser$Parser$map, $elm$core$List$singleton, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$lineBreak),
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$stringEscapable
			]),
		bo: false,
		bp: function (c) {
			return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapable(c));
		},
		aZ: '\"'
	};
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$stringLiteral = F2(
	function (syntax_, revTokens) {
		return A2(
			$elm$parser$Parser$map,
			function (n) {
				return _Utils_ap(n, revTokens);
			},
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$doubleQuoteDelimiter(syntax_)));
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$space = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
	},
	$elm$parser$Parser$getChompedString(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$whitespace = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$space, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$lineBreak]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$arrayLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$whitespace),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				A2(
					$elm$parser$Parser$map,
					function (_v4) {
						return _Utils_Tuple2(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(7),
							',');
					},
					$elm$parser$Parser$symbol(','))),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Done(
						A2($elm$core$List$cons, n, revTokens));
				},
				A2(
					$elm$parser$Parser$map,
					function (_v5) {
						return _Utils_Tuple2(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(7),
							']');
					},
					$elm$parser$Parser$symbol(']'))),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(ns, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$value()),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$objectLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$whitespace),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$stringLiteral, 5, revTokens)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (_v0) {
						var revTokens_ = A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(6),
								':'),
							revTokens);
						return A2(
							$elm$parser$Parser$map,
							function (ns) {
								return _Utils_ap(ns, revTokens_);
							},
							$elm$parser$Parser$oneOf(
								_List_fromArray(
									[
										A2(
										$elm$parser$Parser$andThen,
										function (ws) {
											return $elm$parser$Parser$oneOf(
												_List_fromArray(
													[
														A2(
														$elm$parser$Parser$map,
														function (v) {
															return _Utils_ap(
																v,
																_List_fromArray(
																	[ws]));
														},
														$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$value()),
														$elm$parser$Parser$succeed(
														_List_fromArray(
															[ws]))
													]));
										},
										$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$whitespace),
										$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$value(),
										$elm$parser$Parser$succeed(_List_Nil)
									])));
					},
					$elm$parser$Parser$symbol(':'))),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				A2(
					$elm$parser$Parser$map,
					function (_v1) {
						return _Utils_Tuple2(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(6),
							',');
					},
					$elm$parser$Parser$symbol(','))),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Done(
						A2($elm$core$List$cons, n, revTokens));
				},
				A2(
					$elm$parser$Parser$map,
					function (_v2) {
						return _Utils_Tuple2(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(6),
							'}');
					},
					$elm$parser$Parser$symbol('}'))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
function $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$value() {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$stringLiteral, 0, _List_Nil),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return _List_fromArray(
						[n]);
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$number),
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$object(),
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$array(),
				A2(
				$elm$parser$Parser$map,
				function (s) {
					return _List_fromArray(
						[
							_Utils_Tuple2(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
							s)
						]);
				},
				$elm$parser$Parser$getChompedString(
					$elm$parser$Parser$keyword('null'))),
				A2(
				$elm$parser$Parser$map,
				function (s) {
					return _List_fromArray(
						[
							_Utils_Tuple2(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(3),
							s)
						]);
				},
				$elm$parser$Parser$getChompedString(
					$elm$parser$Parser$oneOf(
						_List_fromArray(
							[
								$elm$parser$Parser$keyword('true'),
								$elm$parser$Parser$keyword('false')
							]))))
			]));
}
function $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$array() {
	return A2(
		$elm$parser$Parser$andThen,
		function (_v6) {
			return A2(
				$elm$parser$Parser$loop,
				_List_fromArray(
					[
						_Utils_Tuple2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(7),
						'[')
					]),
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$arrayLoop);
		},
		$elm$parser$Parser$symbol('['));
}
function $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$object() {
	return A2(
		$elm$parser$Parser$andThen,
		function (_v3) {
			return A2(
				$elm$parser$Parser$loop,
				_List_fromArray(
					[
						_Utils_Tuple2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(6),
						'{')
					]),
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$objectLoop);
		},
		$elm$parser$Parser$symbol('{'));
}
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$value = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$value();
$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$value = function () {
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$value;
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$array = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$array();
$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$array = function () {
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$array;
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$object = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$object();
$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$cyclic$object = function () {
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$object;
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$mainLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$whitespace),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$object),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$elm$parser$Parser$chompIf(
						$elm$core$Basics$always(true)))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$toRevTokens = A2($elm$parser$Parser$loop, _List_Nil, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$mainLoop);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$toLines = A2(
	$elm$core$Basics$composeR,
	$elm$parser$Parser$run($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$toRevTokens),
	$elm$core$Result$map(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLines($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$syntaxToStyle)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$json = A2(
	$elm$core$Basics$composeR,
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Json$toLines,
	$elm$core$Result$map($elm$core$Basics$identity));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$groupSet = $elm$core$Set$fromList(
	_List_fromArray(
		['{', '}', '(', ')', '[', ']', ',', ';']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isGroupChar = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$groupSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$groupChar = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
	},
	$elm$parser$Parser$getChompedString(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isGroupChar)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isCommentChar = function (c) {
	return c === '#';
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$operatorSet = $elm$core$Set$fromList(
	_List_fromArray(
		['+', '-', '*', '/', '=', '!', '<', '>', '&', '|', '?', '^', ':', '~', '%', '.']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$punctuationSet = A2($elm$core$Set$union, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$operatorSet, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$groupSet);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isPunctuation = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$punctuationSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isStringLiteralChar = function (c) {
	return (c === '\"') || (c === '\'');
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isIdentifierNameChar = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isPunctuation(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isStringLiteralChar(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isCommentChar(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$DeclarationKeyword = 3;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$FunctionEval = 4;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$Keyword = 2;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$LiteralKeyword = 6;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$Function = 5;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$inlineComment = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _List_fromArray(
			[
				_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b)
			]);
	},
	$elm$parser$Parser$getChompedString(
		A2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak),
			$elm$parser$Parser$symbol('#'))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$lineBreak = A2(
	$elm$parser$Parser$map,
	function (_v0) {
		return _List_fromArray(
			[
				_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak, '\n')
			]);
	},
	$elm$parser$Parser$symbol('\n'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$multilineComment = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	{
		ba: function (b) {
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b);
		},
		bb: '\'\'\'',
		bm: _List_fromArray(
			[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$lineBreak]),
		bo: false,
		bp: function (c) {
			return !$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c);
		},
		aZ: '\'\'\''
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$comment = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$inlineComment, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$multilineComment]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$whitespaceOrCommentStep = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (s) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, s),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace))),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(ns, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$lineBreak),
				A2(
				$elm$parser$Parser$map,
				function (ns) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(ns, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$comment)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$classDeclarationLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(5),
								b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isIdentifierNameChar))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$Param = 7;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$argLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(7),
								b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
						function (c) {
							return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isCommentChar(c) || ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || ((c === ',') || (c === ')'))));
						}))),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
						function (c) {
							return (c === '/') || (c === ',');
						}))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$functionDeclarationLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(5),
								b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isIdentifierNameChar))),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (_v0) {
						return A2(
							$elm$parser$Parser$loop,
							A2(
								$elm$core$List$cons,
								_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '('),
								revTokens),
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$argLoop);
					},
					$elm$parser$Parser$symbol('('))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$functionEvalLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (_v0) {
					return $elm$parser$Parser$Done(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '('),
							revTokens));
				},
				$elm$parser$Parser$symbol('(')),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$keywordSet = $elm$core$Set$fromList(
	_List_fromArray(
		['finally', 'is', 'return', 'continue', 'for', 'lambda', 'try', 'from', 'nonlocal', 'while', 'and', 'del', 'global', 'not', 'with', 'as', 'elif', 'if', 'or', 'yield', 'assert', 'else', 'import', 'pass', 'break', 'except', 'in', 'raise']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isKeyword = function (str) {
	return A2($elm$core$Set$member, str, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$keywordSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$literalKeywordSet = $elm$core$Set$fromList(
	_List_fromArray(
		['True', 'False', 'None']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isLiteralKeyword = function (str) {
	return A2($elm$core$Set$member, str, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$literalKeywordSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$keywordParser = F2(
	function (revTokens, n) {
		return (n === 'def') ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(3),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$functionDeclarationLoop) : ((n === 'class') ? A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(3),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$classDeclarationLoop) : ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isKeyword(n) ? $elm$parser$Parser$succeed(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(2),
					n),
				revTokens)) : ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isLiteralKeyword(n) ? $elm$parser$Parser$succeed(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(6),
					n),
				revTokens)) : A2(
			$elm$parser$Parser$loop,
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
					n),
				revTokens),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$functionEvalLoop))));
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$Number = 0;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$number = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(0),
			b);
	},
	$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$number));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isOperatorChar = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$operatorSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$operatorChar = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(2),
			b);
	},
	$elm$parser$Parser$getChompedString(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isOperatorChar)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$String = 1;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$quoteDelimiter = {
	ba: function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(1),
			b);
	},
	bb: '\'',
	bm: _List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$lineBreak]),
	bo: false,
	bp: function (c) {
		return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapable(c));
	},
	aZ: '\''
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$doubleQuote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	_Utils_update(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$quoteDelimiter,
		{bb: '\"', aZ: '\"'}));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$quote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$quoteDelimiter);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$stringLiteral = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$quote, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$doubleQuote]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$mainLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (s) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(s, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$stringLiteral),
				A2(
				$elm$parser$Parser$map,
				function (s) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, s, revTokens));
				},
				$elm$parser$Parser$oneOf(
					_List_fromArray(
						[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$operatorChar, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$groupChar, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$number]))),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$keywordParser(revTokens),
					$elm$parser$Parser$getChompedString(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$isIdentifierNameChar)))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$toRevTokens = A2($elm$parser$Parser$loop, _List_Nil, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$mainLoop);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$toLines = A2(
	$elm$core$Basics$composeR,
	$elm$parser$Parser$run($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$toRevTokens),
	$elm$core$Result$map(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLines($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$syntaxToStyle)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$python = A2(
	$elm$core$Basics$composeR,
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Python$toLines,
	$elm$core$Result$map($elm$core$Basics$identity));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$inlineComment = $elm$parser$Parser$oneOf(
	A2(
		$elm$core$List$map,
		A2(
			$elm$core$Basics$composeR,
			$elm$parser$Parser$symbol,
			A2(
				$elm$core$Basics$composeR,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile(
					A2($elm$core$Basics$composeL, $elm$core$Basics$not, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak)),
				A2(
					$elm$core$Basics$composeR,
					$elm$parser$Parser$getChompedString,
					$elm$parser$Parser$map(
						function (b) {
							return _List_fromArray(
								[
									_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b)
								]);
						})))),
		_List_fromArray(
			['--', '$', '#'])));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$lineBreakList = A2(
	$elm$parser$Parser$map,
	function (_v0) {
		return _List_fromArray(
			[
				_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak, '\n')
			]);
	},
	$elm$parser$Parser$symbol('\n'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$multilineComment = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	{
		ba: function (b) {
			return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b);
		},
		bb: '*/',
		bm: _List_fromArray(
			[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$lineBreakList]),
		bo: false,
		bp: A2($elm$core$Basics$composeL, $elm$core$Basics$not, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak),
		aZ: '/*'
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$comment = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$inlineComment, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$multilineComment]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$punctuatorSet = $elm$core$Set$fromList(
	_List_fromArray(
		[';', '[', ']', '(', ')', '`', ',', '.']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isPunctuationChar = function (c) {
	return A2($elm$core$Set$member, c, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$punctuatorSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isIdentifierChar = function (c) {
	return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isPunctuationChar(c));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$Function = 4;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$Keyword = 2;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$Literal = 6;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$Operator = 3;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$functionSet = $elm$core$Set$fromList(
	_List_fromArray(
		['AVG', 'COUNT', 'FIRST', 'FORMAT', 'LAST', 'LCASE', 'LEN', 'MAX', 'MID', 'MIN', 'MOD', 'NOW', 'ROUND', 'SUM', 'UCASE']));
var $elm$core$String$toUpper = _String_toUpper;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isFunction = function (str) {
	return A2(
		$elm$core$Set$member,
		$elm$core$String$toUpper(str),
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$functionSet);
};
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {cs: index, cv: match, cI: number, cV: submatches};
	});
var $elm$regex$Regex$contains = _Regex_contains;
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$never = _Regex_never;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$keywordPattern = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	A2(
		$elm$regex$Regex$fromStringWith,
		{ca: true, cy: false},
		'^(ACTION|ADD|AFTER|ALGORITHM|ALL|ALTER|ANALYZE|ANY|APPLY|AS|ASC|AUTHORIZATION|AUTO_INCREMENT|BACKUP|BDB|BEGIN|BERKELEYDB|BIGINT|BINARY|BIT|BLOB|BOOL|BOOLEAN|BREAK|BROWSE|BTREE|BULK|BY|CALL|CASCADED?|CASE|CHAIN|CHAR(?:ACTER|SET)?|CHECK(?:POINT)?|CLOSE|CLUSTERED|COALESCE|COLLATE|COLUMNS?|COMMENT|COMMIT(?:TED)?|COMPUTE|CONNECT|CONSISTENT|CONSTRAINT|CONTAINS(?:TABLE)?|CONTINUE|CONVERT|CREATE|CROSS|CURRENT(?:_DATE|_TIME|_TIMESTAMP|_USER)?|CURSOR|CYCLE|DATA(?:BASES?)?|DATE(?:TIME)?|DAY|DBCC|DEALLOCATE|DEC|DECIMAL|DECLARE|DEFAULT|DEFINER|DELAYED|DELETE|DELIMITERS?|DENY|DESC|DESCRIBE|DETERMINISTIC|DISABLE|DISCARD|DISK|DISTINCT|DISTINCTROW|DISTRIBUTED|DO|DOUBLE|DROP|DUMMY|DUMP(?:FILE)?|DUPLICATE|ELSE(?:IF)?|ENABLE|ENCLOSED|END|ENGINE|ENUM|ERRLVL|ERRORS|ESCAPED?|EXCEPT|EXEC(?:UTE)?|EXISTS|EXIT|EXPLAIN|EXTENDED|FETCH|FIELDS|FILE|FILLFACTOR|FIRST|FIXED|FLOAT|FOLLOWING|FOR(?: EACH ROW)?|FORCE|FOREIGN|FREETEXT(?:TABLE)?|FROM|FULL|FUNCTION|GEOMETRY(?:COLLECTION)?|GLOBAL|GOTO|GRANT|GROUP|HANDLER|HASH|HAVING|HOLDLOCK|HOUR|IDENTITY(?:_INSERT|COL)?|IF|IGNORE|IMPORT|INDEX|INFILE|INNER|INNODB|INOUT|INSERT|INT|INTEGER|INTERSECT|INTERVAL|INTO|INVOKER|ISOLATION|ITERATE|JOIN|KEYS?|KILL|LANGUAGE|LAST|LEAVE|LEFT|LEVEL|LIMIT|LINENO|LINES|LINESTRING|LOAD|LOCAL|LOCK|LONG(?:BLOB|TEXT)|LOOP|MATCH(?:ED)?|MEDIUM(?:BLOB|INT|TEXT)|MERGE|MIDDLEINT|MINUTE|MODE|MODIFIES|MODIFY|MONTH|MULTI(?:LINESTRING|POINT|POLYGON)|NATIONAL|NATURAL|NCHAR|NEXT|NO|NONCLUSTERED|NULLIF|NUMERIC|OFF?|OFFSETS?|ON|OPEN(?:DATASOURCE|QUERY|ROWSET)?|OPTIMIZE|OPTION(?:ALLY)?|ORDER|OUT(?:ER|FILE)?|OVER|PARTIAL|PARTITION|PERCENT|PIVOT|PLAN|POINT|POLYGON|PRECEDING|PRECISION|PREPARE|PREV|PRIMARY|PRINT|PRIVILEGES|PROC(?:EDURE)?|PUBLIC|PURGE|QUICK|RAISERROR|READS?|REAL|RECONFIGURE|REFERENCES|RELEASE|RENAME|REPEAT(?:ABLE)?|REPLACE|REPLICATION|REQUIRE|RESIGNAL|RESTORE|RESTRICT|RETURNS?|REVOKE|RIGHT|ROLLBACK|ROUTINE|ROW(?:COUNT|GUIDCOL|S)?|RTREE|RULE|SAVE(?:POINT)?|SCHEMA|SECOND|SELECT|SERIAL(?:IZABLE)?|SESSION(?:_USER)?|SET(?:USER)?|SHARE|SHOW|SHUTDOWN|SIMPLE|SMALLINT|SNAPSHOT|SOME|SONAME|SQL|START(?:ING)?|STATISTICS|STATUS|STRIPED|SYSTEM_USER|TABLES?|TABLESPACE|TEMP(?:ORARY|TABLE)?|TERMINATED|TEXT(?:SIZE)?|THEN|TIME(?:STAMP)?|TINY(?:BLOB|INT|TEXT)|TOP?|TRAN(?:SACTIONS?)?|TRIGGER|TRUNCATE|TSEQUAL|TYPES?|UNBOUNDED|UNCOMMITTED|UNDEFINED|UNION|UNIQUE|UNLOCK|UNPIVOT|UNSIGNED|UPDATE(?:TEXT)?|USAGE|USE|USER|USING|VALUES?|VAR(?:BINARY|CHAR|CHARACTER|YING)|VIEW|WAITFOR|WARNINGS|WHEN|WHERE|WHILE|WITH(?: ROLLUP|IN)?|WORK|WRITE(?:TEXT)?|YEAR)$'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isKeyword = $elm$regex$Regex$contains($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$keywordPattern);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$literalSet = $elm$core$Set$fromList(
	_List_fromArray(
		['TRUE', 'FALSE', 'NULL']));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isLiteral = function (str) {
	return A2(
		$elm$core$Set$member,
		$elm$core$String$toUpper(str),
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$literalSet);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$operatorPattern = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	A2(
		$elm$regex$Regex$fromStringWith,
		{ca: true, cy: false},
		'^([-+*\\/=%^~]|&&?|\\|\\|?|!=?|<(?:=>?|<|>)?|>[>=]?|AND|BETWEEN|IN|LIKE|NOT|OR|IS|DIV|REGEXP|RLIKE|SOUNDS LIKE|XOR)$'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isOperator = $elm$regex$Regex$contains($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$operatorPattern);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$keywordParser = F2(
	function (revTokens, s) {
		return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isOperator(s) ? $elm$parser$Parser$succeed(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(3),
					s),
				revTokens)) : ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isFunction(s) ? $elm$parser$Parser$succeed(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
					s),
				revTokens)) : ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isKeyword(s) ? $elm$parser$Parser$succeed(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(2),
					s),
				revTokens)) : ($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isLiteral(s) ? $elm$parser$Parser$succeed(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(6),
					s),
				revTokens)) : $elm$parser$Parser$succeed(
			A2(
				$elm$core$List$cons,
				_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, s),
				revTokens)))));
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$lineBreak = A2(
	$elm$parser$Parser$map,
	function (_v0) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak, '\n');
	},
	$elm$parser$Parser$symbol('\n'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$Number = 0;
var $elm$core$Char$isHexDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return ((48 <= code) && (code <= 57)) || (((65 <= code) && (code <= 70)) || ((97 <= code) && (code <= 102)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$hexNumber = A2(
	$elm$parser$Parser$ignorer,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed(0),
		$elm$parser$Parser$backtrackable(
			$elm$parser$Parser$symbol('0x'))),
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($elm$core$Char$isHexDigit));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$number = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(0),
			b);
	},
	$elm$parser$Parser$getChompedString(
		$elm$parser$Parser$oneOf(
			_List_fromArray(
				[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$hexNumber, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$number]))));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$Punctuation = 5;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$punctuationChar = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(5),
			b);
	},
	$elm$parser$Parser$getChompedString(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isPunctuationChar)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$space = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b);
	},
	$elm$parser$Parser$getChompedString(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$String = 1;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$sqlEscapable = A2(
	$elm$parser$Parser$map,
	function (b) {
		return _List_fromArray(
			[
				_Utils_Tuple2(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(4),
				b)
			]);
	},
	$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$escapable));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$quoteDelimiter = {
	ba: function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(1),
			b);
	},
	bb: '\'',
	bm: _List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$lineBreakList, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$sqlEscapable]),
	bo: false,
	bp: function (c) {
		return !($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c) || $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isEscapable(c));
	},
	aZ: '\''
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$doubleQuote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	_Utils_update(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$quoteDelimiter,
		{bb: '\"', aZ: '\"'}));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$quote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$quoteDelimiter);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$stringLiteral = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$quote, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$doubleQuote]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$checkContext = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$whitespaceOrCommentStep(revTokens),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$whitespaceOrCommentStep = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (s) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, s, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$space),
				A2(
				$elm$parser$Parser$andThen,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$checkContext,
				A2(
					$elm$parser$Parser$map,
					function (s) {
						return A2($elm$core$List$cons, s, revTokens);
					},
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$lineBreak)),
				A2(
				$elm$parser$Parser$map,
				function (s) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(s, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$comment)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$stringBody = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$whitespaceOrCommentStep(revTokens),
				A2(
				$elm$parser$Parser$map,
				function (s) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(s, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$stringLiteral),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$mainLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$space),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$lineBreak),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$punctuationChar),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$number),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$comment),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					function (n) {
						return A2(
							$elm$parser$Parser$loop,
							_Utils_ap(n, revTokens),
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$stringBody);
					},
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$stringLiteral)),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A2(
					$elm$parser$Parser$andThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$keywordParser(revTokens),
					$elm$parser$Parser$getChompedString(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$isIdentifierChar)))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$toRevTokens = A2($elm$parser$Parser$loop, _List_Nil, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$mainLoop);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$toLines = A2(
	$elm$core$Basics$composeR,
	$elm$parser$Parser$run($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$toRevTokens),
	$elm$core$Result$map(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLines($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$syntaxToStyle)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$sql = A2(
	$elm$core$Basics$composeR,
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Sql$toLines,
	$elm$core$Result$map($elm$core$Basics$identity));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$AttributeValue = 2;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$lineBreak = A2(
	$elm$parser$Parser$map,
	function (_v0) {
		return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$LineBreak, '\n');
	},
	$elm$parser$Parser$symbol('\n'));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$lineBreakList = A2($elm$parser$Parser$map, $elm$core$List$singleton, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$lineBreak);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$doubleQuoteDelimiter = {
	ba: function (b) {
		return _Utils_Tuple2(
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(2),
			b);
	},
	bb: '\"',
	bm: _List_fromArray(
		[$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$lineBreakList]),
	bo: false,
	bp: A2($elm$core$Basics$composeL, $elm$core$Basics$not, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak),
	aZ: '\"'
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$comment = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	_Utils_update(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$doubleQuoteDelimiter,
		{
			ba: function (b) {
				return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Comment, b);
			},
			bb: '-->',
			aZ: '<!--'
		}));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$openTagParser = A2(
	$elm$parser$Parser$ignorer,
	A2(
		$elm$parser$Parser$ignorer,
		$elm$parser$Parser$succeed(0),
		$elm$parser$Parser$chompIf(
			function (c) {
				return c === '<';
			})),
	$elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				$elm$parser$Parser$chompIf(
				function (c) {
					return (c === '/') || ((c === '!') || (c === '?'));
				}),
				$elm$parser$Parser$succeed(0)
			])));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$Tag = 0;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$Attribute = 1;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$doubleQuote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$doubleQuoteDelimiter);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$quote = $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$delimited(
	_Utils_update(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$doubleQuoteDelimiter,
		{bb: '\'', aZ: '\''}));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$attributeValue = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$doubleQuote,
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$quote,
			A2(
			$elm$parser$Parser$map,
			function (b) {
				return _List_fromArray(
					[
						_Utils_Tuple2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(2),
						b)
					]);
			},
			$elm$parser$Parser$getChompedString(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
					function (c) {
						return (!$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c)) && (c !== '>');
					})))
		]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$whitespace = $elm$parser$Parser$oneOf(
	_List_fromArray(
		[
			A2(
			$elm$parser$Parser$map,
			function (s) {
				return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, s);
			},
			$elm$parser$Parser$getChompedString(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isSpace))),
			$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$lineBreak
		]));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$attributeValueLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A3($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$attributeValueLoop, revTokens, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$whitespace),
				A3($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$addThen, $elm$parser$Parser$succeed, revTokens, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$attributeValue),
				$elm$parser$Parser$succeed(revTokens)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$attributeConfirm = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A3($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$attributeConfirm, revTokens, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$whitespace),
				A3(
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$attributeValueLoop,
				revTokens,
				A2(
					$elm$parser$Parser$map,
					function (_v0) {
						return _Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, '=');
					},
					$elm$parser$Parser$symbol('='))),
				$elm$parser$Parser$succeed(revTokens)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$isStartTagChar = function (c) {
	return $elm$core$Char$isUpper(c) || ($elm$core$Char$isLower(c) || $elm$core$Char$isDigit(c));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$isTagChar = function (c) {
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$isStartTagChar(c) || (c === '-');
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$isAttributeChar = function (c) {
	return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$isTagChar(c) || (c === '_');
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$attributeLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				A3(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$consThen,
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$attributeConfirm,
					revTokens,
					A2(
						$elm$parser$Parser$map,
						function (b) {
							return _Utils_Tuple2(
								$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(1),
								b);
						},
						$elm$parser$Parser$getChompedString(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$isAttributeChar))))),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$whitespace),
				A2(
				$elm$parser$Parser$map,
				function (b) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
						function (c) {
							return (!$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isWhitespace(c)) && (c !== '>');
						}))),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$tag = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$andThen,
				function (n) {
					return A2(
						$elm$parser$Parser$loop,
						A2($elm$core$List$cons, n, revTokens),
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$attributeLoop);
				},
				A2(
					$elm$parser$Parser$map,
					function (b) {
						return _Utils_Tuple2(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$C(0),
							b);
					},
					$elm$parser$Parser$getChompedString(
						A2(
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$thenChompWhile,
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$isTagChar,
							$elm$parser$Parser$chompIf($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$isStartTagChar))))),
				$elm$parser$Parser$succeed(revTokens)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$openTag = function (revTokens) {
	return A2(
		$elm$parser$Parser$andThen,
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$tag,
		A2(
			$elm$parser$Parser$map,
			function (b) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, b),
					revTokens);
			},
			$elm$parser$Parser$getChompedString($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$openTagParser)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$mainLoop = function (revTokens) {
	return $elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2($elm$core$List$cons, n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$whitespace),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						_Utils_ap(n, revTokens));
				},
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$comment),
				A2(
				$elm$parser$Parser$map,
				function (n) {
					return $elm$parser$Parser$Loop(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Type$Normal, n),
							revTokens));
				},
				$elm$parser$Parser$getChompedString(
					$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$chompIfThenWhile(
						function (c) {
							return (c !== '<') && (!$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Helpers$isLineBreak(c));
						}))),
				A2(
				$elm$parser$Parser$map,
				$elm$parser$Parser$Loop,
				$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$openTag(revTokens)),
				$elm$parser$Parser$succeed(
				$elm$parser$Parser$Done(revTokens))
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$toRevTokens = A2($elm$parser$Parser$loop, _List_Nil, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$mainLoop);
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$toLines = A2(
	$elm$core$Basics$composeR,
	$elm$parser$Parser$run($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$toRevTokens),
	$elm$core$Result$map(
		$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Helpers$toLines($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$syntaxToStyle)));
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$xml = A2(
	$elm$core$Basics$composeR,
	$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Language$Xml$toLines,
	$elm$core$Result$map($elm$core$Basics$identity));
var $jxxcarlson$elm_markdown$Markdown$Render$parserOfLanguage = function (lang_) {
	switch (lang_) {
		case 0:
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$elm;
		case 1:
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$css;
		case 2:
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$javascript;
		case 3:
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$json;
		case 4:
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$python;
		case 5:
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$sql;
		default:
			return $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$xml;
	}
};
var $elm$html$Html$pre = _VirtualDom_node('pre');
var $elm$html$Html$a = _VirtualDom_node('a');
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm$html$Html$em = _VirtualDom_node('em');
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$html$Html$img = _VirtualDom_node('img');
var $jxxcarlson$elm_markdown$Markdown$Render$idAttrWithLabel = F2(
	function (id, label) {
		return $elm$html$Html$Attributes$id(
			_Utils_ap(
				$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id),
				label));
	});
var $jxxcarlson$elm_markdown$Markdown$Render$inlineMathText = F2(
	function (id, str) {
		return A3(
			$elm$html$Html$Keyed$node,
			'span',
			_List_fromArray(
				[
					A2($jxxcarlson$elm_markdown$Markdown$Render$idAttrWithLabel, id, 'm')
				]),
			_List_fromArray(
				[
					_Utils_Tuple2(
					$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id) + 'm',
					$jxxcarlson$elm_markdown$Markdown$Render$mathText(
						'$ ' + ($elm$core$String$trim(str) + ' $ ')))
				]));
	});
var $elm$html$Html$p = _VirtualDom_node('p');
var $jxxcarlson$elm_markdown$Markdown$Render$renderStanza = F2(
	function (id, arg) {
		var poetryLine = function (line) {
			return A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(line)
					]));
		};
		var lines = A2($elm$core$String$split, '\n', arg);
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id),
					$elm$html$Html$Attributes$class('mm-poetry')
				]),
			A2($elm$core$List$map, poetryLine, lines));
	});
var $jxxcarlson$elm_markdown$Markdown$Render$highlightColor = '#d7d6ff';
var $jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_ = F2(
	function (targetId, currentId) {
		var _v0 = _Utils_eq(targetId, currentId);
		if (_v0) {
			return A2($elm$html$Html$Attributes$style, 'background-color', $jxxcarlson$elm_markdown$Markdown$Render$highlightColor);
		} else {
			return A2($elm$html$Html$Attributes$style, 'background-color', '#fff');
		}
	});
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $jxxcarlson$elm_markdown$Markdown$Render$strikethrough = function (str) {
	return A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('mm-strike-through')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(str)
			]));
};
var $elm$html$Html$strong = _VirtualDom_node('strong');
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $jxxcarlson$elm_markdown$Markdown$Render$joinLine = F4(
	function (selectedId, id, level, items) {
		var folder = F2(
			function (item, _v6) {
				var accString = _v6.a;
				var accElement = _v6.b;
				if (!item.$) {
					var str = item.a;
					return _Utils_Tuple2(
						A2($elm$core$List$cons, str, accString),
						accElement);
				} else {
					if (!_Utils_eq(accString, _List_Nil)) {
						var content = A2($elm$core$String$join, '', accString);
						var span = A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('innerJoin')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(content)
								]));
						return _Utils_Tuple2(
							_List_Nil,
							A2(
								$elm$core$List$cons,
								A4($jxxcarlson$elm_markdown$Markdown$Render$renderToHtmlMsg, selectedId, id, level, item),
								A2($elm$core$List$cons, span, accElement)));
					} else {
						return _Utils_Tuple2(
							_List_Nil,
							A2(
								$elm$core$List$cons,
								A4($jxxcarlson$elm_markdown$Markdown$Render$renderToHtmlMsg, selectedId, id, level, item),
								accElement));
					}
				}
			});
		var flush = function (_v4) {
			var accString = _v4.a;
			var accElement = _v4.b;
			if (!_Utils_eq(accString, _List_Nil)) {
				var content = A2($elm$core$String$join, '', accString);
				var span = A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(content)
						]));
				return A2($elm$core$List$cons, span, accElement);
			} else {
				return accElement;
			}
		};
		return $elm$core$List$reverse(
			flush(
				A3(
					$elm$core$List$foldl,
					folder,
					_Utils_Tuple2(_List_Nil, _List_Nil),
					items)));
	});
var $jxxcarlson$elm_markdown$Markdown$Render$renderToHtmlMsg = F4(
	function (selectedId, id, level, mmInline) {
		switch (mmInline.$) {
			case 0:
				var str = mmInline.a;
				return A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id),
							$elm$html$Html$Attributes$class('ordinary'),
							$jxxcarlson$elm_markdown$Markdown$Render$marginOfLevel(level)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(str)
						]));
			case 1:
				var str = mmInline.a;
				return A2(
					$elm$html$Html$em,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(str)
						]));
			case 2:
				var str = mmInline.a;
				return A2(
					$elm$html$Html$strong,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(str)
						]));
			case 3:
				var str = mmInline.a;
				return A2(
					$elm$html$Html$code,
					_List_fromArray(
						[
							$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(str)
						]));
			case 4:
				var str = mmInline.a;
				return A2($jxxcarlson$elm_markdown$Markdown$Render$inlineMathText, id, str);
			case 5:
				var str = mmInline.a;
				return $jxxcarlson$elm_markdown$Markdown$Render$strikethrough(str);
			case 6:
				var str = mmInline.a;
				return A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('bracketed')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('[' + (str + ']'))
						]));
			case 7:
				var url = mmInline.a;
				var label = mmInline.b;
				return A2(
					$elm$html$Html$a,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$href(url)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(label + ' ')
						]));
			case 8:
				var label_ = mmInline.a;
				var url = mmInline.b;
				var labelParts = A2(
					$elm$core$List$take,
					2,
					A2($elm$core$String$split, '::', label_));
				var _v1 = function () {
					var _v2 = _Utils_Tuple2(
						$elm$core$List$head(labelParts),
						$elm$core$List$head(
							A2($elm$core$List$drop, 1, labelParts)));
					if (!_v2.a.$) {
						if (!_v2.b.$) {
							var label__ = _v2.a.a;
							var class__ = _v2.b.a;
							return _Utils_Tuple2(label__, 'mm-image-' + class__);
						} else {
							var label__ = _v2.a.a;
							var _v3 = _v2.b;
							return _Utils_Tuple2(label__, 'mm-image');
						}
					} else {
						return _Utils_Tuple2('image', 'mm-image');
					}
				}();
				var label = _v1.a;
				var _class = _v1.b;
				return A2(
					$elm$html$Html$img,
					_List_fromArray(
						[
							$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id),
							$elm$html$Html$Attributes$src(url),
							$elm$html$Html$Attributes$class(_class)
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(label)
						]));
			case 9:
				var arg = mmInline.a;
				var joined = A4($jxxcarlson$elm_markdown$Markdown$Render$joinLine, selectedId, id, level, arg);
				return ($elm$core$List$length(joined) === 1) ? A2(
					$elm$core$Maybe$withDefault,
					A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('')
							])),
					$elm$core$List$head(joined)) : A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('line')
						]),
					joined);
			case 10:
				var arg = mmInline.a;
				var mapper = function (m) {
					return _Utils_Tuple2(
						$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id),
						A4($jxxcarlson$elm_markdown$Markdown$Render$renderToHtmlMsg, selectedId, id, level, m));
				};
				return A3(
					$elm$html$Html$Keyed$node,
					'p',
					_List_fromArray(
						[
							$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id),
							A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id),
							$elm$html$Html$Attributes$class('mm-paragraph'),
							$jxxcarlson$elm_markdown$Markdown$Render$blockLevelClass(level - 1)
						]),
					A2($elm$core$List$map, mapper, arg));
			case 11:
				var arg = mmInline.a;
				return A2($jxxcarlson$elm_markdown$Markdown$Render$renderStanza, id, arg);
			default:
				var arg = mmInline.a;
				return A2(
					$elm$html$Html$p,
					_List_Nil,
					A2(
						$elm$core$List$map,
						A3($jxxcarlson$elm_markdown$Markdown$Render$renderToHtmlMsg, selectedId, id, level),
						arg));
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent = F4(
	function (selectedId, id, level, blockContent) {
		if (!blockContent.$) {
			var mmInline = blockContent.a;
			return A4($jxxcarlson$elm_markdown$Markdown$Render$renderToHtmlMsg, selectedId, id, level, mmInline);
		} else {
			var str = blockContent.a;
			return A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id),
						$jxxcarlson$elm_markdown$Markdown$Render$blockLevelClass(level - 1),
						A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(str)
					]));
		}
	});
var $elm$html$Html$h1 = _VirtualDom_node('h1');
var $elm$html$Html$h2 = _VirtualDom_node('h2');
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $elm$html$Html$h4 = _VirtualDom_node('h4');
var $elm$html$Html$h5 = _VirtualDom_node('h5');
var $jxxcarlson$elm_markdown$Markdown$Render$nameFromBlockContent = function (blockContent) {
	if ((((((((!blockContent.$) && (blockContent.a.$ === 10)) && blockContent.a.a.b) && (blockContent.a.a.a.$ === 9)) && blockContent.a.a.a.a.b) && (!blockContent.a.a.a.a.a.$)) && (!blockContent.a.a.a.a.b.b)) && (!blockContent.a.a.b.b)) {
		var _v1 = blockContent.a.a;
		var _v2 = _v1.a.a;
		var str = _v2.a.a;
		return $elm$core$String$trim(str);
	} else {
		return '';
	}
};
var $jxxcarlson$elm_markdown$Markdown$Render$renderHeading = F5(
	function (selectedId, id, k, level, blockContent) {
		var name = $jxxcarlson$elm_markdown$Markdown$Render$nameFromBlockContent(blockContent);
		switch (k) {
			case 1:
				return A2(
					$elm$html$Html$h1,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(name),
							$elm$html$Html$Attributes$class('mm-h1'),
							A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
						]),
					_List_fromArray(
						[
							A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent)
						]));
			case 2:
				return A2(
					$elm$html$Html$h2,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(name),
							$elm$html$Html$Attributes$class('mm-h2'),
							A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
						]),
					_List_fromArray(
						[
							A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent)
						]));
			case 3:
				return A2(
					$elm$html$Html$h3,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(name),
							$elm$html$Html$Attributes$class('mm-h3'),
							A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
						]),
					_List_fromArray(
						[
							A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent)
						]));
			case 4:
				return A2(
					$elm$html$Html$h4,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(name),
							$elm$html$Html$Attributes$class('mm-h4'),
							A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
						]),
					_List_fromArray(
						[
							A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent)
						]));
			default:
				return A2(
					$elm$html$Html$h5,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$id(name),
							$elm$html$Html$Attributes$class('mm-h5'),
							A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
						]),
					_List_fromArray(
						[
							A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent)
						]));
		}
	});
var $jxxcarlson$elm_markdown$MDInline$OrdinaryText = function (a) {
	return {$: 0, a: a};
};
var $jxxcarlson$elm_markdown$Markdown$Render$alphabet = function (k) {
	var alpha = _List_fromArray(
		['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']);
	return A2(
		$elm$core$Maybe$withDefault,
		'zz',
		$elm$core$List$head(
			A2($elm$core$List$drop, k - 1, alpha)));
};
var $jxxcarlson$elm_markdown$Markdown$Parse$M = function (a) {
	return {$: 0, a: a};
};
var $jxxcarlson$elm_markdown$MDInline$Paragraph = function (a) {
	return {$: 10, a: a};
};
var $jxxcarlson$elm_markdown$Markdown$Render$prependToParagraph = F2(
	function (head, tail) {
		if (tail.$ === 1) {
			return tail;
		} else {
			var mmInLine = tail.a;
			if (mmInLine.$ === 10) {
				var lst = mmInLine.a;
				return $jxxcarlson$elm_markdown$Markdown$Parse$M(
					$jxxcarlson$elm_markdown$MDInline$Paragraph(
						A2($elm$core$List$cons, head, lst)));
			} else {
				return tail;
			}
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Render$romanNumeral = function (k) {
	var alpha = _List_fromArray(
		['i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x', 'xi', 'xii', 'xiii', 'xiv', 'xv', 'xvi', 'xvii', 'xviii', 'xix', 'xx', 'xxi', 'xxii', 'xxiii', 'xiv', 'xv', 'xvi']);
	return A2(
		$elm$core$Maybe$withDefault,
		'zz',
		$elm$core$List$head(
			A2($elm$core$List$drop, k - 1, alpha)));
};
var $jxxcarlson$elm_markdown$Markdown$Render$renderOListItem = F5(
	function (selectedId, id, index, level, blockContent) {
		var label = function () {
			switch (level) {
				case 1:
					return $elm$core$String$fromInt(index) + '. ';
				case 2:
					return $jxxcarlson$elm_markdown$Markdown$Render$alphabet(index) + '. ';
				case 3:
					return $jxxcarlson$elm_markdown$Markdown$Render$romanNumeral(index) + '. ';
				case 4:
					return $elm$core$String$fromInt(index) + '. ';
				default:
					return 'N. ';
			}
		}();
		return A2(
			$elm$html$Html$li,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('mm-olist-item'),
					$jxxcarlson$elm_markdown$Markdown$Render$blockLevelClass(level - 1),
					$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id),
					A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
				]),
			_List_fromArray(
				[
					A4(
					$jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent,
					selectedId,
					id,
					level,
					A2(
						$jxxcarlson$elm_markdown$Markdown$Render$prependToParagraph,
						$jxxcarlson$elm_markdown$MDInline$OrdinaryText(label),
						blockContent))
				]));
	});
var $jxxcarlson$elm_markdown$Markdown$Render$renderPoetry = F4(
	function (selectedId, id, level, blockContent) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('mm-poetry'),
					$jxxcarlson$elm_markdown$Markdown$Render$marginOfLevel(level),
					A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
				]),
			_List_fromArray(
				[
					A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent)
				]));
	});
var $jxxcarlson$elm_markdown$Markdown$Render$renderQuotation = F4(
	function (selectedId, id, level, blockContent) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('mm-quotation'),
					$jxxcarlson$elm_markdown$Markdown$Render$blockLevelClass(level),
					A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
				]),
			_List_fromArray(
				[
					A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent)
				]));
	});
var $jxxcarlson$elm_markdown$Markdown$Render$renderUListItem = F4(
	function (selectedId, id, level, blockContent) {
		var label = function () {
			switch (level) {
				case 1:
					return '• ';
				case 2:
					return '◊ ';
				case 3:
					return '† ';
				case 4:
					return '‡ ';
				default:
					return 'N. ';
			}
		}();
		return A2(
			$elm$html$Html$li,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('mm-ulist-item'),
					$jxxcarlson$elm_markdown$Markdown$Render$blockLevelClass(level - 1),
					$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id),
					A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
				]),
			_List_fromArray(
				[
					A4(
					$jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent,
					selectedId,
					id,
					level,
					A2(
						$jxxcarlson$elm_markdown$Markdown$Render$prependToParagraph,
						$jxxcarlson$elm_markdown$MDInline$OrdinaryText(label),
						blockContent))
				]));
	});
var $elm$html$Html$table = _VirtualDom_node('table');
var $elm$html$Html$td = _VirtualDom_node('td');
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Add = 1;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Del = 2;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$Line$Normal = 0;
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$requiredStyleToString = function (required) {
	return 'elmsh' + function () {
		switch (required) {
			case 0:
				return '0';
			case 1:
				return '-comm';
			case 2:
				return '1';
			case 3:
				return '2';
			case 4:
				return '3';
			case 5:
				return '4';
			case 6:
				return '5';
			case 7:
				return '6';
			default:
				return '7';
		}
	}();
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$fragmentView = function (_v0) {
	var text = _v0.a0;
	var requiredStyle = _v0.cP;
	var additionalClass = _v0.b4;
	return ((!requiredStyle) && $elm$core$String$isEmpty(additionalClass)) ? $elm$html$Html$text(text) : A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2(
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$requiredStyleToString(requiredStyle),
						!(!requiredStyle)),
						_Utils_Tuple2('elmsh-' + additionalClass, additionalClass !== '')
					]))
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(text)
			]));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$lineView = F3(
	function (start, index, _v0) {
		var fragments = _v0.co;
		var highlight = _v0.ab;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('elmsh-line', true),
							_Utils_Tuple2(
							'elmsh-hl',
							_Utils_eq(
								highlight,
								$elm$core$Maybe$Just(0))),
							_Utils_Tuple2(
							'elmsh-add',
							_Utils_eq(
								highlight,
								$elm$core$Maybe$Just(1))),
							_Utils_Tuple2(
							'elmsh-del',
							_Utils_eq(
								highlight,
								$elm$core$Maybe$Just(2)))
						])),
					A2(
					$elm$html$Html$Attributes$attribute,
					'data-elmsh-lc',
					$elm$core$String$fromInt(start + index))
				]),
			A2($elm$core$List$map, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$fragmentView, fragments));
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$toInlineHtml = function (lines) {
	return A2(
		$elm$html$Html$code,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('elmsh')
			]),
		$elm$core$List$concat(
			A2(
				$elm$core$List$map,
				function (_v0) {
					var highlight = _v0.ab;
					var fragments = _v0.co;
					return _Utils_eq(highlight, $elm$core$Maybe$Nothing) ? A2($elm$core$List$map, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$fragmentView, fragments) : _List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$classList(
									_List_fromArray(
										[
											_Utils_Tuple2(
											'elmsh-hl',
											_Utils_eq(
												highlight,
												$elm$core$Maybe$Just(0))),
											_Utils_Tuple2(
											'elmsh-add',
											_Utils_eq(
												highlight,
												$elm$core$Maybe$Just(1))),
											_Utils_Tuple2(
											'elmsh-del',
											_Utils_eq(
												highlight,
												$elm$core$Maybe$Just(2)))
										]))
								]),
							A2($elm$core$List$map, $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$fragmentView, fragments))
						]);
				},
				lines)));
};
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$toBlockHtml = F2(
	function (maybeStart, lines) {
		if (maybeStart.$ === 1) {
			return A2(
				$elm$html$Html$pre,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('elmsh')
					]),
				_List_fromArray(
					[
						$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$toInlineHtml(lines)
					]));
		} else {
			var start = maybeStart.a;
			return A2(
				$elm$html$Html$pre,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('elmsh')
					]),
				$elm$core$List$singleton(
					A2(
						$elm$html$Html$code,
						_List_Nil,
						A2(
							$elm$core$List$indexedMap,
							$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$lineView(start),
							lines))));
		}
	});
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$toBlockHtml = F2(
	function (maybeStart, _v0) {
		var lines = _v0;
		return A2($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$View$toBlockHtml, maybeStart, lines);
	});
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $elm$core$String$trimLeft = _String_trimLeft;
var $pablohirafuji$elm_syntax_highlight$SyntaxHighlight$useTheme = function (_v0) {
	var theme = _v0;
	return A3(
		$elm$html$Html$node,
		'style',
		_List_Nil,
		_List_fromArray(
			[
				$elm$html$Html$text(theme)
			]));
};
var $elm$core$Result$withDefault = F2(
	function (def, result) {
		if (!result.$) {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Render$renderBlock = F3(
	function (selectedId, id, block) {
		if (!block.a.$) {
			switch (block.a.a.$) {
				case 2:
					var _v9 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					if (blockContent.$ === 1) {
						var str = blockContent.a;
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id),
									$jxxcarlson$elm_markdown$Markdown$Render$marginOfLevel(level),
									A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
								]),
							_List_fromArray(
								[
									$jxxcarlson$elm_markdown$Markdown$Render$displayMathText(str)
								]));
					} else {
						return $jxxcarlson$elm_markdown$Markdown$Render$displayMathText('');
					}
				case 1:
					var _v11 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					if (blockContent.$ === 1) {
						var str = blockContent.a;
						return A2(
							$elm$html$Html$pre,
							_List_fromArray(
								[
									$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id),
									$jxxcarlson$elm_markdown$Markdown$Render$marginOfLevel(level),
									A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(str)
								]));
					} else {
						return $jxxcarlson$elm_markdown$Markdown$Render$displayMathText('');
					}
				default:
					var lang = block.a.a.a;
					var level = block.b;
					var blockContent = block.c;
					if (blockContent.$ === 1) {
						var str = blockContent.a;
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$jxxcarlson$elm_markdown$Markdown$Render$blockLevelClass(level - 1)
								]),
							_List_fromArray(
								[
									$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$useTheme($pablohirafuji$elm_syntax_highlight$SyntaxHighlight$monokai),
									A2(
									$elm$core$Result$withDefault,
									A2(
										$elm$html$Html$pre,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$elm$html$Html$code,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text(str)
													]))
											])),
									A2(
										$elm$core$Result$map,
										$pablohirafuji$elm_syntax_highlight$SyntaxHighlight$toBlockHtml(
											$elm$core$Maybe$Just(1)),
										A2(
											$jxxcarlson$elm_markdown$Markdown$Render$parserOfLanguage,
											lang,
											$elm$core$String$trimLeft(
												A2($jxxcarlson$elm_markdown$BlockType$deleteLangPrefix, lang, str)))))
								]));
					} else {
						return $jxxcarlson$elm_markdown$Markdown$Render$displayMathText('');
					}
			}
		} else {
			switch (block.a.a.$) {
				case 0:
					var _v1 = block.a.a;
					return A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id),
								A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
							]),
						_List_Nil);
				case 7:
					var _v2 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent);
				case 9:
					var _v3 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent);
				case 3:
					var k = block.a.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A5($jxxcarlson$elm_markdown$Markdown$Render$renderHeading, selectedId, id, k, level, blockContent);
				case 5:
					var _v4 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A4($jxxcarlson$elm_markdown$Markdown$Render$renderQuotation, selectedId, id, level, blockContent);
				case 6:
					var _v5 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A4($jxxcarlson$elm_markdown$Markdown$Render$renderPoetry, selectedId, id, level, blockContent);
				case 1:
					var _v6 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A4($jxxcarlson$elm_markdown$Markdown$Render$renderUListItem, selectedId, id, level, blockContent);
				case 2:
					var index = block.a.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A5($jxxcarlson$elm_markdown$Markdown$Render$renderOListItem, selectedId, id, index, level, blockContent);
				case 4:
					var _v7 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A2(
						$elm$html$Html$hr,
						_List_fromArray(
							[
								$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id),
								$elm$html$Html$Attributes$class('mm-thematic-break'),
								A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
							]),
						_List_Nil);
				case 8:
					var _v8 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent);
				case 10:
					var _v14 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A2(
						$elm$html$Html$td,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('mm-table-cell')
							]),
						_List_fromArray(
							[
								A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent)
							]));
				case 11:
					var _v15 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A2(
						$elm$html$Html$tr,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('mm-table-row')
							]),
						_List_fromArray(
							[
								A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent)
							]));
				default:
					var _v16 = block.a.a;
					var level = block.b;
					var blockContent = block.c;
					return A2(
						$elm$html$Html$table,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('mm-table'),
								$jxxcarlson$elm_markdown$Markdown$Render$marginOfLevel(level)
							]),
						_List_fromArray(
							[
								A4($jxxcarlson$elm_markdown$Markdown$Render$renderBlockContent, selectedId, id, level, blockContent)
							]));
			}
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Render$selectedStyle = F2(
	function (targetId, currentId) {
		var _v0 = _Utils_eq(targetId, currentId);
		if (_v0) {
			return _List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'background-color', $jxxcarlson$elm_markdown$Markdown$Render$highlightColor)
				]);
		} else {
			return _List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'background-color', '#fff')
				]);
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Render$mmBlockTreeToHtml = F2(
	function (selectedId, tree) {
		if (_Utils_eq(
			$zwilias$elm_rosetree$Tree$children(tree),
			_List_Nil)) {
			var _v0 = $zwilias$elm_rosetree$Tree$label(tree);
			var id = _v0.a;
			var bt = _v0.b;
			var lev = _v0.c;
			var content = _v0.d;
			if ((!bt.$) && (bt.a.$ === 2)) {
				var _v2 = bt.a;
				return A3(
					$elm$html$Html$Keyed$node,
					'spanXXX',
					_Utils_ap(
						A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle, selectedId, id),
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id))
							])),
					_List_fromArray(
						[
							_Utils_Tuple2(
							$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id),
							A3(
								$jxxcarlson$elm_markdown$Markdown$Render$renderBlock,
								selectedId,
								id,
								A3($jxxcarlson$elm_markdown$Markdown$Parse$MDBlock, bt, lev, content)))
						]));
			} else {
				return A3(
					$elm$html$Html$Keyed$node,
					'span',
					_Utils_ap(
						A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle, selectedId, id),
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id))
							])),
					_List_fromArray(
						[
							_Utils_Tuple2(
							$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id),
							A3(
								$jxxcarlson$elm_markdown$Markdown$Render$renderBlock,
								selectedId,
								id,
								A3($jxxcarlson$elm_markdown$Markdown$Parse$MDBlock, bt, lev, content)))
						]));
			}
		} else {
			var _v3 = $zwilias$elm_rosetree$Tree$label(tree);
			if (_v3.b.$ === 1) {
				switch (_v3.b.a.$) {
					case 11:
						var id = _v3.a;
						var _v4 = _v3.b.a;
						return A2(
							$elm$html$Html$tr,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('mm-table-row'),
									A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
								]),
							A2(
								$elm$core$List$map,
								$jxxcarlson$elm_markdown$Markdown$Render$mmBlockTreeToHtml(selectedId),
								$zwilias$elm_rosetree$Tree$children(tree)));
					case 12:
						var id = _v3.a;
						var _v5 = _v3.b.a;
						return A3(
							$elm$html$Html$Keyed$node,
							'table',
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('mm-table'),
									$elm$html$Html$Attributes$id(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id)),
									$elm$html$Html$Events$onClick(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id)),
									A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
								]),
							_List_fromArray(
								[
									_Utils_Tuple2(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id),
									A2(
										$elm$html$Html$div,
										_List_Nil,
										A2(
											$elm$core$List$map,
											$jxxcarlson$elm_markdown$Markdown$Render$mmBlockTreeToHtml(selectedId),
											$zwilias$elm_rosetree$Tree$children(tree))))
								]));
					case 7:
						var id = _v3.a;
						var _v6 = _v3.b.a;
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('mm-plain'),
									$elm$html$Html$Attributes$id(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id)),
									$elm$html$Html$Events$onClick(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id)),
									A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
								]),
							A2(
								$elm$core$List$map,
								$jxxcarlson$elm_markdown$Markdown$Render$mmBlockTreeToHtml(selectedId),
								$zwilias$elm_rosetree$Tree$children(tree)));
					default:
						var id = _v3.a;
						return A3(
							$elm$html$Html$Keyed$node,
							'div',
							_List_fromArray(
								[
									A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
								]),
							_List_fromArray(
								[
									_Utils_Tuple2(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id),
									A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$id(
												$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id)),
												$elm$html$Html$Events$onClick(
												$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id))
											]),
										_List_fromArray(
											[
												A3(
												$jxxcarlson$elm_markdown$Markdown$Render$renderBlock,
												selectedId,
												id,
												$jxxcarlson$elm_markdown$Markdown$Parse$project(
													$zwilias$elm_rosetree$Tree$label(tree))),
												A2(
												$elm$html$Html$div,
												_List_fromArray(
													[
														$jxxcarlson$elm_markdown$Markdown$Render$idAttr(id)
													]),
												A2(
													$elm$core$List$map,
													$jxxcarlson$elm_markdown$Markdown$Render$mmBlockTreeToHtml(selectedId),
													$zwilias$elm_rosetree$Tree$children(tree)))
											])))
								]));
				}
			} else {
				switch (_v3.b.a.$) {
					case 2:
						var id = _v3.a;
						var _v7 = _v3.b.a;
						var level = _v3.c;
						var content = _v3.d;
						return A3(
							$elm$html$Html$Keyed$node,
							'div',
							_List_fromArray(
								[
									$elm$html$Html$Attributes$id(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id)),
									$elm$html$Html$Events$onClick(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id)),
									A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
								]),
							_List_fromArray(
								[
									_Utils_Tuple2(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id),
									$jxxcarlson$elm_markdown$Markdown$Render$displayMathText(
										$jxxcarlson$elm_markdown$Markdown$Parse$projectedStringOfBlockContent(content)))
								]));
					case 1:
						var id = _v3.a;
						var _v8 = _v3.b.a;
						return A3(
							$elm$html$Html$Keyed$node,
							'pre',
							_List_fromArray(
								[
									$elm$html$Html$Attributes$id(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id)),
									$elm$html$Html$Events$onClick(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id)),
									A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
								]),
							_List_fromArray(
								[
									_Utils_Tuple2(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id),
									$elm$html$Html$text('OUF: Verbatim!'))
								]));
					default:
						var id = _v3.a;
						var lang = _v3.b.a.a;
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$id(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id)),
									$elm$html$Html$Events$onClick(
									$jxxcarlson$elm_markdown$Markdown$Parse$stringFromId(id)),
									A2($jxxcarlson$elm_markdown$Markdown$Render$selectedStyle_, selectedId, id)
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('OUF: Code!')
								]));
				}
			}
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Render$fromAST = F2(
	function (selectedId, blockTreeWithId) {
		return function (x) {
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[$jxxcarlson$elm_markdown$Markdown$Render$masterId]),
				x);
		}(
			A2(
				$elm$core$List$map,
				$jxxcarlson$elm_markdown$Markdown$Render$mmBlockTreeToHtml(selectedId),
				$zwilias$elm_rosetree$Tree$children(blockTreeWithId)));
	});
var $zwilias$elm_rosetree$Tree$Tree = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $zwilias$elm_rosetree$Tree$mapAccumulateHelp = F4(
	function (f, state, acc, stack) {
		mapAccumulateHelp:
		while (true) {
			var _v0 = acc.n;
			if (!_v0.b) {
				var node = A2(
					$zwilias$elm_rosetree$Tree$Tree,
					acc.q,
					$elm$core$List$reverse(acc.c));
				if (!stack.b) {
					return _Utils_Tuple2(state, node);
				} else {
					var top = stack.a;
					var rest = stack.b;
					var $temp$f = f,
						$temp$state = state,
						$temp$acc = _Utils_update(
						top,
						{
							c: A2($elm$core$List$cons, node, top.c)
						}),
						$temp$stack = rest;
					f = $temp$f;
					state = $temp$state;
					acc = $temp$acc;
					stack = $temp$stack;
					continue mapAccumulateHelp;
				}
			} else {
				if (!_v0.a.b.b) {
					var _v2 = _v0.a;
					var d = _v2.a;
					var rest = _v0.b;
					var _v3 = A2(f, state, d);
					var state_ = _v3.a;
					var label_ = _v3.b;
					var $temp$f = f,
						$temp$state = state_,
						$temp$acc = _Utils_update(
						acc,
						{
							c: A2(
								$elm$core$List$cons,
								A2($zwilias$elm_rosetree$Tree$Tree, label_, _List_Nil),
								acc.c),
							n: rest
						}),
						$temp$stack = stack;
					f = $temp$f;
					state = $temp$state;
					acc = $temp$acc;
					stack = $temp$stack;
					continue mapAccumulateHelp;
				} else {
					var _v4 = _v0.a;
					var d = _v4.a;
					var cs = _v4.b;
					var rest = _v0.b;
					var _v5 = A2(f, state, d);
					var state_ = _v5.a;
					var label_ = _v5.b;
					var $temp$f = f,
						$temp$state = state_,
						$temp$acc = {c: _List_Nil, q: label_, n: cs},
						$temp$stack = A2(
						$elm$core$List$cons,
						_Utils_update(
							acc,
							{n: rest}),
						stack);
					f = $temp$f;
					state = $temp$state;
					acc = $temp$acc;
					stack = $temp$stack;
					continue mapAccumulateHelp;
				}
			}
		}
	});
var $zwilias$elm_rosetree$Tree$mapAccumulate = F3(
	function (f, s, _v0) {
		var d = _v0.a;
		var cs = _v0.b;
		var _v1 = A2(f, s, d);
		var s_ = _v1.a;
		var d_ = _v1.b;
		return A4(
			$zwilias$elm_rosetree$Tree$mapAccumulateHelp,
			f,
			s_,
			{c: _List_Nil, q: d_, n: cs},
			_List_Nil);
	});
var $zwilias$elm_rosetree$Tree$indexedMap = F2(
	function (f, t) {
		return A3(
			$zwilias$elm_rosetree$Tree$mapAccumulate,
			F2(
				function (idx, elem) {
					return _Utils_Tuple2(
						idx + 1,
						A2(f, idx, elem));
				}),
			0,
			t).b;
	});
var $zwilias$elm_rosetree$Tree$map = F2(
	function (f, t) {
		return A3(
			$zwilias$elm_rosetree$Tree$mapAccumulate,
			F2(
				function (_v0, e) {
					return _Utils_Tuple2(
						0,
						f(e));
				}),
			0,
			t).b;
	});
var $jxxcarlson$elm_markdown$BlockType$BalancedBlock = function (a) {
	return {$: 0, a: a};
};
var $jxxcarlson$elm_markdown$BlockType$DisplayCode = function (a) {
	return {$: 0, a: a};
};
var $jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $jxxcarlson$elm_markdown$BlockType$MarkdownBlock = function (a) {
	return {$: 1, a: a};
};
var $jxxcarlson$elm_markdown$BlockType$Plain = {$: 7};
var $jxxcarlson$elm_markdown$MDInline$Stanza = function (a) {
	return {$: 11, a: a};
};
var $jxxcarlson$elm_markdown$Markdown$Parse$T = function (a) {
	return {$: 1, a: a};
};
var $jxxcarlson$elm_markdown$BlockType$Verbatim = {$: 1};
var $jxxcarlson$elm_markdown$MDInline$BoldText = function (a) {
	return {$: 2, a: a};
};
var $jxxcarlson$elm_markdown$MDInline$Expecting = $elm$core$Basics$identity;
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $elm$parser$Parser$Advanced$spaces = $elm$parser$Parser$Advanced$chompWhile(
	function (c) {
		return (c === ' ') || ((c === '\n') || (c === '\r'));
	});
var $jxxcarlson$elm_markdown$MDInline$boldText = A2(
	$elm$parser$Parser$Advanced$map,
	$jxxcarlson$elm_markdown$MDInline$BoldText,
	A2(
		$elm$parser$Parser$Advanced$map,
		A2($elm$core$String$replace, '**', ''),
		A2(
			$elm$parser$Parser$Advanced$map,
			$elm$core$String$dropLeft(2),
			$elm$parser$Parser$Advanced$getChompedString(
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							A2(
								$elm$parser$Parser$Advanced$ignorer,
								$elm$parser$Parser$Advanced$succeed(0),
								$elm$parser$Parser$Advanced$symbol(
									A2($elm$parser$Parser$Advanced$Token, '**', 'expecting \'**\' to begin bold text'))),
							$elm$parser$Parser$Advanced$chompWhile(
								function (c) {
									return c !== '*';
								})),
						$elm$parser$Parser$Advanced$symbol(
							A2($elm$parser$Parser$Advanced$Token, '**', 'expecting \'**\' to end bold text'))),
					$elm$parser$Parser$Advanced$spaces)))));
var $jxxcarlson$elm_markdown$MDInline$Code = function (a) {
	return {$: 3, a: a};
};
var $elm$core$String$dropRight = F2(
	function (n, string) {
		return (n < 1) ? string : A3($elm$core$String$slice, 0, -n, string);
	});
var $jxxcarlson$elm_markdown$MDInline$code = A2(
	$elm$parser$Parser$Advanced$map,
	$jxxcarlson$elm_markdown$MDInline$Code,
	A2(
		$elm$parser$Parser$Advanced$map,
		$elm$core$String$dropRight(1),
		A2(
			$elm$parser$Parser$Advanced$map,
			$elm$core$String$dropLeft(1),
			A2(
				$elm$parser$Parser$Advanced$map,
				$elm$core$String$trim,
				$elm$parser$Parser$Advanced$getChompedString(
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							A2(
								$elm$parser$Parser$Advanced$ignorer,
								A2(
									$elm$parser$Parser$Advanced$ignorer,
									$elm$parser$Parser$Advanced$succeed(0),
									$elm$parser$Parser$Advanced$symbol(
										A2($elm$parser$Parser$Advanced$Token, '`', 'Expecting \'``\' to begin inline code'))),
								$elm$parser$Parser$Advanced$chompWhile(
									function (c) {
										return c !== '`';
									})),
							$elm$parser$Parser$Advanced$symbol(
								A2($elm$parser$Parser$Advanced$Token, '`', 'Expecting \'``\' to end inline code'))),
						$elm$parser$Parser$Advanced$chompWhile(
							function (c) {
								return c !== ' ';
							})))))));
var $jxxcarlson$elm_markdown$MDInline$Image = F2(
	function (a, b) {
		return {$: 8, a: a, b: b};
	});
var $jxxcarlson$elm_markdown$MDInline$PrefixedString = F2(
	function (prefix, text) {
		return {aO: prefix, a0: text};
	});
var $elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3($elm$parser$Parser$Advanced$map2, $elm$core$Basics$apL, parseFunc, parseArg);
	});
var $jxxcarlson$elm_markdown$MDInline$parseWhile = function (accepting) {
	return $elm$parser$Parser$Advanced$getChompedString(
		$elm$parser$Parser$Advanced$chompWhile(accepting));
};
var $jxxcarlson$elm_markdown$MDInline$image = A2(
	$elm$parser$Parser$Advanced$map,
	function (ps) {
		return A2($jxxcarlson$elm_markdown$MDInline$Image, ps.aO, ps.a0);
	},
	A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed($jxxcarlson$elm_markdown$MDInline$PrefixedString),
				$elm$parser$Parser$Advanced$symbol(
					A2($elm$parser$Parser$Advanced$Token, '![', 'Expecting \'![\' to begin image block'))),
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$jxxcarlson$elm_markdown$MDInline$parseWhile(
					function (c) {
						return c !== ']';
					}),
				$elm$parser$Parser$Advanced$symbol(
					A2($elm$parser$Parser$Advanced$Token, '](', 'Expecting \'](\' in image block')))),
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$jxxcarlson$elm_markdown$MDInline$parseWhile(
					function (c) {
						return c !== ')';
					}),
				$elm$parser$Parser$Advanced$symbol(
					A2($elm$parser$Parser$Advanced$Token, ')', 'Expecting \')\' to end image block'))),
			$elm$parser$Parser$Advanced$chompWhile(
				function (c) {
					return c === '\n';
				}))));
var $jxxcarlson$elm_markdown$MDInline$ItalicText = function (a) {
	return {$: 1, a: a};
};
var $jxxcarlson$elm_markdown$MDInline$italicText = A2(
	$elm$parser$Parser$Advanced$map,
	$jxxcarlson$elm_markdown$MDInline$ItalicText,
	A2(
		$elm$parser$Parser$Advanced$map,
		A2($elm$core$String$replace, '*', ''),
		$elm$parser$Parser$Advanced$getChompedString(
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							$elm$parser$Parser$Advanced$succeed(0),
							$elm$parser$Parser$Advanced$symbol(
								A2($elm$parser$Parser$Advanced$Token, '*', 'Expecting \'*\' to begin italic text'))),
						$elm$parser$Parser$Advanced$chompWhile(
							function (c) {
								return c !== '*';
							})),
					$elm$parser$Parser$Advanced$symbol(
						A2($elm$parser$Parser$Advanced$Token, '*', 'Expecting \'*\' to end italic text'))),
				$elm$parser$Parser$Advanced$spaces))));
var $jxxcarlson$elm_markdown$MDInline$BracketedText = function (a) {
	return {$: 6, a: a};
};
var $jxxcarlson$elm_markdown$MDInline$Link = F2(
	function (a, b) {
		return {$: 7, a: a, b: b};
	});
var $jxxcarlson$elm_markdown$MDInline$linkOrBracket = function (ps) {
	var _v0 = ps.a0;
	if (_v0 === ' ') {
		return $jxxcarlson$elm_markdown$MDInline$BracketedText(ps.aO);
	} else {
		return A2($jxxcarlson$elm_markdown$MDInline$Link, ps.a0, ps.aO);
	}
};
var $jxxcarlson$elm_markdown$MDInline$linkUrl = A2(
	$elm$parser$Parser$Advanced$keeper,
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
		$elm$parser$Parser$Advanced$symbol(
			A2($elm$parser$Parser$Advanced$Token, '(', 'expecting \'(\' to begin link url'))),
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$jxxcarlson$elm_markdown$MDInline$parseWhile(
				function (c) {
					return c !== ')';
				}),
			$elm$parser$Parser$Advanced$symbol(
				A2($elm$parser$Parser$Advanced$Token, ')', 'expecting \')\' to end link url'))),
		$elm$parser$Parser$Advanced$spaces));
var $jxxcarlson$elm_markdown$MDInline$terminateBracket = A2(
	$elm$parser$Parser$Advanced$map,
	function (_v0) {
		return ' ';
	},
	$elm$parser$Parser$Advanced$succeed(0));
var $jxxcarlson$elm_markdown$MDInline$link = A2(
	$elm$parser$Parser$Advanced$map,
	function (ps) {
		return $jxxcarlson$elm_markdown$MDInline$linkOrBracket(ps);
	},
	A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$keeper,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed($jxxcarlson$elm_markdown$MDInline$PrefixedString),
				$elm$parser$Parser$Advanced$symbol(
					A2($elm$parser$Parser$Advanced$Token, '[', 'expecting \'[\' to begin label'))),
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$jxxcarlson$elm_markdown$MDInline$parseWhile(
					function (c) {
						return c !== ']';
					}),
				$elm$parser$Parser$Advanced$symbol(
					A2($elm$parser$Parser$Advanced$Token, ']', 'expecting \']\' to end first part of label')))),
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$oneOf(
				_List_fromArray(
					[$jxxcarlson$elm_markdown$MDInline$linkUrl, $jxxcarlson$elm_markdown$MDInline$terminateBracket])),
			$elm$parser$Parser$Advanced$spaces)));
var $jxxcarlson$elm_markdown$MDInline$isSpecialCharacter = function (c) {
	switch (c) {
		case '`':
			return true;
		case '[':
			return true;
		case '*':
			return true;
		case '\n':
			return true;
		default:
			return false;
	}
};
var $jxxcarlson$elm_markdown$MDInline$ordinaryTextParser = function (validStart) {
	var isRegular = function (c) {
		return (!(c === ']')) && validStart(c);
	};
	return A2(
		$elm$parser$Parser$Advanced$mapChompedString,
		F2(
			function (s, _v0) {
				return $jxxcarlson$elm_markdown$MDInline$OrdinaryText(s);
			}),
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2($elm$parser$Parser$Advanced$chompIf, validStart, 'expecting regular character to begin ordinary text line'),
			$elm$parser$Parser$Advanced$chompWhile(isRegular)));
};
var $jxxcarlson$elm_markdown$MDInline$ordinaryTextExtended = function () {
	var validStart = function (c) {
		return !((c === '~') || $jxxcarlson$elm_markdown$MDInline$isSpecialCharacter(c));
	};
	return $jxxcarlson$elm_markdown$MDInline$ordinaryTextParser(validStart);
}();
var $jxxcarlson$elm_markdown$MDInline$StrikeThroughText = function (a) {
	return {$: 5, a: a};
};
var $jxxcarlson$elm_markdown$MDInline$strikeThroughText = A2(
	$elm$parser$Parser$Advanced$map,
	$jxxcarlson$elm_markdown$MDInline$StrikeThroughText,
	A2(
		$elm$parser$Parser$Advanced$map,
		A2($elm$core$String$replace, '~~', ''),
		A2(
			$elm$parser$Parser$Advanced$map,
			$elm$core$String$dropLeft(2),
			$elm$parser$Parser$Advanced$getChompedString(
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							A2(
								$elm$parser$Parser$Advanced$ignorer,
								$elm$parser$Parser$Advanced$succeed(0),
								$elm$parser$Parser$Advanced$symbol(
									A2($elm$parser$Parser$Advanced$Token, '~~', 'expecting \'~~\' to begin strikethrough'))),
							$elm$parser$Parser$Advanced$chompWhile(
								function (c) {
									return c !== '~';
								})),
						$elm$parser$Parser$Advanced$symbol(
							A2($elm$parser$Parser$Advanced$Token, '~~', 'expecting \'~~\' to end strikethrough'))),
					$elm$parser$Parser$Advanced$spaces)))));
var $jxxcarlson$elm_markdown$MDInline$inlineExtended = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[$jxxcarlson$elm_markdown$MDInline$code, $jxxcarlson$elm_markdown$MDInline$image, $jxxcarlson$elm_markdown$MDInline$link, $jxxcarlson$elm_markdown$MDInline$boldText, $jxxcarlson$elm_markdown$MDInline$italicText, $jxxcarlson$elm_markdown$MDInline$strikeThroughText, $jxxcarlson$elm_markdown$MDInline$ordinaryTextExtended]));
var $jxxcarlson$elm_markdown$MDInline$InlineMath = function (a) {
	return {$: 4, a: a};
};
var $jxxcarlson$elm_markdown$MDInline$inlineMath = A2(
	$elm$parser$Parser$Advanced$map,
	$jxxcarlson$elm_markdown$MDInline$InlineMath,
	A2(
		$elm$parser$Parser$Advanced$map,
		$elm$core$String$dropRight(1),
		A2(
			$elm$parser$Parser$Advanced$map,
			$elm$core$String$dropLeft(1),
			A2(
				$elm$parser$Parser$Advanced$map,
				$elm$core$String$trim,
				$elm$parser$Parser$Advanced$getChompedString(
					A2(
						$elm$parser$Parser$Advanced$ignorer,
						A2(
							$elm$parser$Parser$Advanced$ignorer,
							A2(
								$elm$parser$Parser$Advanced$ignorer,
								A2(
									$elm$parser$Parser$Advanced$ignorer,
									$elm$parser$Parser$Advanced$succeed(0),
									$elm$parser$Parser$Advanced$symbol(
										A2($elm$parser$Parser$Advanced$Token, '$', 'Expecting \'$\' to begin inline math'))),
								$elm$parser$Parser$Advanced$chompWhile(
									function (c) {
										return c !== '$';
									})),
							$elm$parser$Parser$Advanced$symbol(
								A2($elm$parser$Parser$Advanced$Token, '$', 'Expecting \'$\' to end inline math'))),
						$elm$parser$Parser$Advanced$chompWhile(
							function (c) {
								return c === ' ';
							})))))));
var $jxxcarlson$elm_markdown$MDInline$ordinaryTextExtendedMath = function () {
	var validStart = function (c) {
		return !((c === '~') || ((c === '$') || $jxxcarlson$elm_markdown$MDInline$isSpecialCharacter(c)));
	};
	return $jxxcarlson$elm_markdown$MDInline$ordinaryTextParser(validStart);
}();
var $jxxcarlson$elm_markdown$MDInline$inlineExtendedMath = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[$jxxcarlson$elm_markdown$MDInline$code, $jxxcarlson$elm_markdown$MDInline$image, $jxxcarlson$elm_markdown$MDInline$link, $jxxcarlson$elm_markdown$MDInline$boldText, $jxxcarlson$elm_markdown$MDInline$italicText, $jxxcarlson$elm_markdown$MDInline$strikeThroughText, $jxxcarlson$elm_markdown$MDInline$inlineMath, $jxxcarlson$elm_markdown$MDInline$ordinaryTextExtendedMath]));
var $jxxcarlson$elm_markdown$MDInline$ordinaryTextStandard = function () {
	var validStart = A2($elm$core$Basics$composeL, $elm$core$Basics$not, $jxxcarlson$elm_markdown$MDInline$isSpecialCharacter);
	return $jxxcarlson$elm_markdown$MDInline$ordinaryTextParser(validStart);
}();
var $jxxcarlson$elm_markdown$MDInline$inlineStandard = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[$jxxcarlson$elm_markdown$MDInline$code, $jxxcarlson$elm_markdown$MDInline$image, $jxxcarlson$elm_markdown$MDInline$link, $jxxcarlson$elm_markdown$MDInline$boldText, $jxxcarlson$elm_markdown$MDInline$italicText, $jxxcarlson$elm_markdown$MDInline$ordinaryTextStandard]));
var $jxxcarlson$elm_markdown$MDInline$inline = function (option) {
	switch (option) {
		case 0:
			return $jxxcarlson$elm_markdown$MDInline$inlineStandard;
		case 1:
			return $jxxcarlson$elm_markdown$MDInline$inlineExtended;
		default:
			return $jxxcarlson$elm_markdown$MDInline$inlineExtendedMath;
	}
};
var $jxxcarlson$elm_markdown$MDInline$manyHelp = F2(
	function (p, vs) {
		return $elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$parser$Parser$Advanced$keeper,
					$elm$parser$Parser$Advanced$succeed(
						function (v) {
							return $elm$parser$Parser$Advanced$Loop(
								A2($elm$core$List$cons, v, vs));
						}),
					p),
					A2(
					$elm$parser$Parser$Advanced$map,
					function (_v0) {
						return $elm$parser$Parser$Advanced$Done(
							$elm$core$List$reverse(vs));
					},
					$elm$parser$Parser$Advanced$succeed(0))
				]));
	});
var $jxxcarlson$elm_markdown$MDInline$many = function (p) {
	return A2(
		$elm$parser$Parser$Advanced$loop,
		_List_Nil,
		$jxxcarlson$elm_markdown$MDInline$manyHelp(p));
};
var $jxxcarlson$elm_markdown$MDInline$inlineList = function (option) {
	return $jxxcarlson$elm_markdown$MDInline$many(
		$jxxcarlson$elm_markdown$MDInline$inline(option));
};
var $jxxcarlson$elm_markdown$MDInline$Line = function (a) {
	return {$: 9, a: a};
};
var $jxxcarlson$elm_markdown$MDInline$displayDeadEnd = function (deadend) {
	var _v0 = deadend.bA;
	var error = _v0;
	return error;
};
var $jxxcarlson$elm_markdown$MDInline$decodeInlineError = function (errorList) {
	var errorMessage = A2(
		$elm$core$String$join,
		';;\n\n',
		A2($elm$core$List$map, $jxxcarlson$elm_markdown$MDInline$displayDeadEnd, errorList));
	return $jxxcarlson$elm_markdown$MDInline$OrdinaryText(errorMessage);
};
var $jxxcarlson$elm_markdown$MDInline$resolveInlineResult = function (result) {
	if (!result.$) {
		var res_ = result.a;
		return $jxxcarlson$elm_markdown$MDInline$Line(res_);
	} else {
		var list = result.a;
		return $jxxcarlson$elm_markdown$MDInline$decodeInlineError(list);
	}
};
var $jxxcarlson$elm_markdown$MDInline$parseLine = F2(
	function (option, str) {
		return $jxxcarlson$elm_markdown$MDInline$resolveInlineResult(
			A2(
				$elm$parser$Parser$Advanced$run,
				$jxxcarlson$elm_markdown$MDInline$inlineList(option),
				str));
	});
var $elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			$elm$core$String$slice,
			-n,
			$elm$core$String$length(string),
			string);
	});
var $jxxcarlson$elm_markdown$MDInline$endsWithPunctuation = function (str) {
	return A2($elm$core$String$right, 1, str) === '.';
};
var $jxxcarlson$elm_markdown$MDInline$wrapper = F2(
	function (str, acc) {
		return (acc.K === '') ? {K: str, ag: _List_Nil} : ($jxxcarlson$elm_markdown$MDInline$endsWithPunctuation(acc.K) ? {
			K: str,
			ag: A2($elm$core$List$cons, acc.K, acc.ag)
		} : _Utils_update(
			acc,
			{K: acc.K + (' ' + str)}));
	});
var $jxxcarlson$elm_markdown$MDInline$wrap = function (strList) {
	return $elm$core$List$reverse(
		function (acc) {
			return A2($elm$core$List$cons, acc.K, acc.ag);
		}(
			A3(
				$elm$core$List$foldl,
				$jxxcarlson$elm_markdown$MDInline$wrapper,
				{K: '', ag: _List_Nil},
				strList)));
};
var $jxxcarlson$elm_markdown$MDInline$parse = F2(
	function (option, str) {
		return $jxxcarlson$elm_markdown$MDInline$Paragraph(
			A2(
				$elm$core$List$map,
				$jxxcarlson$elm_markdown$MDInline$parseLine(option),
				$jxxcarlson$elm_markdown$MDInline$wrap(
					A2($elm$core$String$split, '\n', str))));
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$extendedMDParser = F2(
	function (option_, _v0) {
		var id = _v0.a;
		var bt = _v0.b;
		var level_ = _v0.c;
		var content_ = _v0.d;
		if (bt.$ === 1) {
			var mt = bt.a;
			if (mt.$ === 6) {
				return A4(
					$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
					id,
					$jxxcarlson$elm_markdown$BlockType$MarkdownBlock(mt),
					level_,
					$jxxcarlson$elm_markdown$Markdown$Parse$M(
						$jxxcarlson$elm_markdown$MDInline$Stanza(content_)));
			} else {
				return A4(
					$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
					id,
					$jxxcarlson$elm_markdown$BlockType$MarkdownBlock(mt),
					level_,
					$jxxcarlson$elm_markdown$Markdown$Parse$M(
						A2($jxxcarlson$elm_markdown$MDInline$parse, option_, content_)));
			}
		} else {
			switch (bt.a.$) {
				case 0:
					var lang = bt.a.a;
					return A4(
						$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
						id,
						$jxxcarlson$elm_markdown$BlockType$BalancedBlock(
							$jxxcarlson$elm_markdown$BlockType$DisplayCode(lang)),
						level_,
						$jxxcarlson$elm_markdown$Markdown$Parse$T(content_));
				case 1:
					var _v3 = bt.a;
					return A4(
						$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
						id,
						$jxxcarlson$elm_markdown$BlockType$BalancedBlock($jxxcarlson$elm_markdown$BlockType$Verbatim),
						level_,
						$jxxcarlson$elm_markdown$Markdown$Parse$T(content_));
				default:
					return A4(
						$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
						id,
						$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Plain),
						level_,
						$jxxcarlson$elm_markdown$Markdown$Parse$M(
							A2($jxxcarlson$elm_markdown$MDInline$parse, option_, content_)));
			}
		}
	});
var $jxxcarlson$elm_markdown$BlockType$DisplayMath = {$: 2};
var $jxxcarlson$elm_markdown$Markdown$Parse$extendedMathMDParser = F2(
	function (option_, _v0) {
		var id = _v0.a;
		var bt = _v0.b;
		var level_ = _v0.c;
		var content_ = _v0.d;
		if (bt.$ === 1) {
			var mt = bt.a;
			if (mt.$ === 6) {
				return A4(
					$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
					id,
					$jxxcarlson$elm_markdown$BlockType$MarkdownBlock(mt),
					level_,
					$jxxcarlson$elm_markdown$Markdown$Parse$M(
						$jxxcarlson$elm_markdown$MDInline$Stanza(content_)));
			} else {
				return A4(
					$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
					id,
					$jxxcarlson$elm_markdown$BlockType$MarkdownBlock(mt),
					level_,
					$jxxcarlson$elm_markdown$Markdown$Parse$M(
						A2($jxxcarlson$elm_markdown$MDInline$parse, option_, content_)));
			}
		} else {
			switch (bt.a.$) {
				case 0:
					var lang = bt.a.a;
					return A4(
						$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
						id,
						$jxxcarlson$elm_markdown$BlockType$BalancedBlock(
							$jxxcarlson$elm_markdown$BlockType$DisplayCode(lang)),
						level_,
						$jxxcarlson$elm_markdown$Markdown$Parse$T(content_));
				case 1:
					var _v3 = bt.a;
					return A4(
						$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
						id,
						$jxxcarlson$elm_markdown$BlockType$BalancedBlock($jxxcarlson$elm_markdown$BlockType$Verbatim),
						level_,
						$jxxcarlson$elm_markdown$Markdown$Parse$T(content_));
				default:
					var _v4 = bt.a;
					return A4(
						$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
						id,
						$jxxcarlson$elm_markdown$BlockType$BalancedBlock($jxxcarlson$elm_markdown$BlockType$DisplayMath),
						level_,
						$jxxcarlson$elm_markdown$Markdown$Parse$T(content_));
			}
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$standardMDParser = F2(
	function (option_, _v0) {
		var id = _v0.a;
		var bt = _v0.b;
		var level_ = _v0.c;
		var content_ = _v0.d;
		if (bt.$ === 1) {
			var mt = bt.a;
			return A4(
				$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
				id,
				$jxxcarlson$elm_markdown$BlockType$MarkdownBlock(mt),
				level_,
				$jxxcarlson$elm_markdown$Markdown$Parse$M(
					A2($jxxcarlson$elm_markdown$MDInline$parse, option_, content_)));
		} else {
			if (!bt.a.$) {
				var lang = bt.a.a;
				return A4(
					$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
					id,
					$jxxcarlson$elm_markdown$BlockType$BalancedBlock(
						$jxxcarlson$elm_markdown$BlockType$DisplayCode(lang)),
					level_,
					$jxxcarlson$elm_markdown$Markdown$Parse$T(content_));
			} else {
				return A4(
					$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
					id,
					$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Plain),
					level_,
					$jxxcarlson$elm_markdown$Markdown$Parse$M(
						A2($jxxcarlson$elm_markdown$MDInline$parse, option_, content_)));
			}
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$selectParser = F2(
	function (option, block) {
		var id = block.a;
		var bt = block.b;
		var level_ = block.c;
		var content_ = block.d;
		switch (option) {
			case 0:
				return A2($jxxcarlson$elm_markdown$Markdown$Parse$standardMDParser, option, block);
			case 1:
				return A2($jxxcarlson$elm_markdown$Markdown$Parse$extendedMDParser, option, block);
			default:
				return A2($jxxcarlson$elm_markdown$Markdown$Parse$extendedMathMDParser, option, block);
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$setBlockIndex = F3(
	function (version, idx, _v0) {
		var id = _v0.a;
		var bt = _v0.b;
		var lev = _v0.c;
		var blockContent = _v0.d;
		return A4(
			$jxxcarlson$elm_markdown$Markdown$Parse$MDBlockWithId,
			_Utils_Tuple2(idx, version),
			bt,
			lev,
			blockContent);
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$blockLevel = function (_v0) {
	var k = _v0.c;
	return k;
};
var $jxxcarlson$elm_markdown$Markdown$Parse$Block = F4(
	function (a, b, c, d) {
		return {$: 0, a: a, b: b, c: c, d: d};
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$changeLevel = F2(
	function (k, _v0) {
		var id_ = _v0.a;
		var bt_ = _v0.b;
		var level_ = _v0.c;
		var content_ = _v0.d;
		return A4($jxxcarlson$elm_markdown$Markdown$Parse$Block, id_, bt_, level_ + k, content_);
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$blockListOfFSM = function (_v0) {
	var blockList_ = _v0.b;
	return blockList_;
};
var $jxxcarlson$elm_markdown$Markdown$Parse$stateOfFSM = function (_v0) {
	var state_ = _v0.a;
	return state_;
};
var $jxxcarlson$elm_markdown$Markdown$Parse$flush = function (fsm) {
	var _v0 = $jxxcarlson$elm_markdown$Markdown$Parse$stateOfFSM(fsm);
	switch (_v0.$) {
		case 0:
			return $elm$core$List$reverse(
				$jxxcarlson$elm_markdown$Markdown$Parse$blockListOfFSM(fsm));
		case 2:
			return $elm$core$List$reverse(
				$jxxcarlson$elm_markdown$Markdown$Parse$blockListOfFSM(fsm));
		default:
			var b = _v0.a;
			return $elm$core$List$reverse(
				A2(
					$elm$core$List$cons,
					b,
					$jxxcarlson$elm_markdown$Markdown$Parse$blockListOfFSM(fsm)));
	}
};
var $zwilias$elm_rosetree$Tree$Zipper$Zipper = $elm$core$Basics$identity;
var $zwilias$elm_rosetree$Tree$Zipper$fromTree = function (t) {
	return {g: _List_Nil, h: _List_Nil, u: _List_Nil, j: t};
};
var $zwilias$elm_rosetree$Tree$singleton = function (v) {
	return A2($zwilias$elm_rosetree$Tree$Tree, v, _List_Nil);
};
var $zwilias$elm_rosetree$Tree$appendChild = F2(
	function (c, _v0) {
		var v = _v0.a;
		var cs = _v0.b;
		return A2(
			$zwilias$elm_rosetree$Tree$Tree,
			v,
			_Utils_ap(
				cs,
				_List_fromArray(
					[c])));
	});
var $zwilias$elm_rosetree$Tree$Zipper$replaceTree = F2(
	function (t, _v0) {
		var zipper = _v0;
		return _Utils_update(
			zipper,
			{j: t});
	});
var $zwilias$elm_rosetree$Tree$Zipper$tree = function (_v0) {
	var focus = _v0.j;
	return focus;
};
var $jxxcarlson$htree$HTree$appendAtFocus = F2(
	function (s, z) {
		var t = $zwilias$elm_rosetree$Tree$Zipper$tree(z);
		var newTree = A2(
			$zwilias$elm_rosetree$Tree$appendChild,
			$zwilias$elm_rosetree$Tree$singleton(s),
			t);
		return A2($zwilias$elm_rosetree$Tree$Zipper$replaceTree, newTree, z);
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (!maybeValue.$) {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $jxxcarlson$htree$HTree$iterate = F3(
	function (remaining, f, accumulator) {
		iterate:
		while (true) {
			if (remaining > 0) {
				var $temp$remaining = remaining - 1,
					$temp$f = f,
					$temp$accumulator = f(accumulator);
				remaining = $temp$remaining;
				f = $temp$f;
				accumulator = $temp$accumulator;
				continue iterate;
			} else {
				return accumulator;
			}
		}
	});
var $zwilias$elm_rosetree$Tree$tree = $zwilias$elm_rosetree$Tree$Tree;
var $zwilias$elm_rosetree$Tree$Zipper$reconstruct = F4(
	function (focus, before, after, l) {
		return A2(
			$zwilias$elm_rosetree$Tree$tree,
			l,
			_Utils_ap(
				$elm$core$List$reverse(before),
				_Utils_ap(
					_List_fromArray(
						[focus]),
					after)));
	});
var $zwilias$elm_rosetree$Tree$Zipper$parent = function (_v0) {
	var zipper = _v0;
	var _v1 = zipper.u;
	if (!_v1.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var crumb = _v1.a;
		var rest = _v1.b;
		return $elm$core$Maybe$Just(
			{
				g: crumb.g,
				h: crumb.h,
				u: rest,
				j: A4($zwilias$elm_rosetree$Tree$Zipper$reconstruct, zipper.j, zipper.h, zipper.g, crumb.q)
			});
	}
};
var $jxxcarlson$htree$HTree$manyParent = F2(
	function (k, z) {
		var zz = $zwilias$elm_rosetree$Tree$Zipper$parent(z);
		return A3(
			$jxxcarlson$htree$HTree$iterate,
			k - 1,
			function (zi) {
				return A2($elm$core$Maybe$andThen, $zwilias$elm_rosetree$Tree$Zipper$parent, zi);
			},
			zz);
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (!maybe.$) {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $jxxcarlson$htree$HTree$addAtNthParent = F3(
	function (k, s, z) {
		return A2(
			$elm$core$Maybe$withDefault,
			z,
			A2(
				$elm$core$Maybe$map,
				$jxxcarlson$htree$HTree$appendAtFocus(s),
				A2($jxxcarlson$htree$HTree$manyParent, k, z)));
	});
var $zwilias$elm_rosetree$Tree$Zipper$lastChild = function (_v0) {
	var zipper = _v0;
	var _v1 = $elm$core$List$reverse(
		$zwilias$elm_rosetree$Tree$children(zipper.j));
	if (!_v1.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var c = _v1.a;
		var rest = _v1.b;
		return $elm$core$Maybe$Just(
			{
				g: _List_Nil,
				h: rest,
				u: A2(
					$elm$core$List$cons,
					{
						g: zipper.g,
						h: zipper.h,
						q: $zwilias$elm_rosetree$Tree$label(zipper.j)
					},
					zipper.u),
				j: c
			});
	}
};
var $jxxcarlson$htree$HTree$addChildAtFocus = F2(
	function (s, z) {
		return A2(
			$elm$core$Maybe$withDefault,
			z,
			A2(
				$elm$core$Maybe$map,
				$jxxcarlson$htree$HTree$appendAtFocus(s),
				$zwilias$elm_rosetree$Tree$Zipper$lastChild(z)));
	});
var $jxxcarlson$htree$HTree$levelOfLastChild = F2(
	function (level, z) {
		return A2(
			$elm$core$Maybe$map,
			level,
			A2(
				$elm$core$Maybe$map,
				$zwilias$elm_rosetree$Tree$label,
				A2(
					$elm$core$Maybe$map,
					$zwilias$elm_rosetree$Tree$Zipper$tree,
					$zwilias$elm_rosetree$Tree$Zipper$lastChild(z))));
	});
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 1) {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 1) {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $jxxcarlson$htree$HTree$levelDifference = F3(
	function (level, s, z) {
		return A3(
			$elm$core$Maybe$map2,
			$elm$core$Basics$sub,
			$elm$core$Maybe$Just(
				level(s)),
			A2($jxxcarlson$htree$HTree$levelOfLastChild, level, z));
	});
var $jxxcarlson$htree$HTree$step = F3(
	function (level, s, z) {
		var ld = A3($jxxcarlson$htree$HTree$levelDifference, level, s, z);
		if (ld.$ === 1) {
			return A2($jxxcarlson$htree$HTree$appendAtFocus, s, z);
		} else {
			switch (ld.a) {
				case 0:
					return A2($jxxcarlson$htree$HTree$appendAtFocus, s, z);
				case 1:
					return A2($jxxcarlson$htree$HTree$addChildAtFocus, s, z);
				default:
					var levelsBack = -A2($elm$core$Maybe$withDefault, 0, ld);
					return A3($jxxcarlson$htree$HTree$addAtNthParent, levelsBack, s, z);
			}
		}
	});
var $zwilias$elm_rosetree$Tree$Zipper$previousSibling = function (_v0) {
	var zipper = _v0;
	var _v1 = zipper.h;
	if (!_v1.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var previous = _v1.a;
		var rest = _v1.b;
		return $elm$core$Maybe$Just(
			{
				g: A2($elm$core$List$cons, zipper.j, zipper.g),
				h: rest,
				u: zipper.u,
				j: previous
			});
	}
};
var $zwilias$elm_rosetree$Tree$Zipper$firstSibling = function (zipper) {
	firstSibling:
	while (true) {
		var _v0 = $zwilias$elm_rosetree$Tree$Zipper$previousSibling(zipper);
		if (_v0.$ === 1) {
			return zipper;
		} else {
			var z = _v0.a;
			var $temp$zipper = z;
			zipper = $temp$zipper;
			continue firstSibling;
		}
	}
};
var $zwilias$elm_rosetree$Tree$Zipper$root = function (zipper) {
	root:
	while (true) {
		var _v0 = $zwilias$elm_rosetree$Tree$Zipper$parent(zipper);
		if (_v0.$ === 1) {
			return $zwilias$elm_rosetree$Tree$Zipper$firstSibling(zipper);
		} else {
			var z = _v0.a;
			var $temp$zipper = z;
			zipper = $temp$zipper;
			continue root;
		}
	}
};
var $zwilias$elm_rosetree$Tree$Zipper$toTree = A2($elm$core$Basics$composeL, $zwilias$elm_rosetree$Tree$Zipper$tree, $zwilias$elm_rosetree$Tree$Zipper$root);
var $jxxcarlson$htree$HTree$fromList = F3(
	function (rootLabel, level, lst) {
		return $zwilias$elm_rosetree$Tree$Zipper$toTree(
			A3(
				$elm$core$List$foldl,
				F2(
					function (s, z) {
						return A3($jxxcarlson$htree$HTree$step, level, s, z);
					}),
				$zwilias$elm_rosetree$Tree$Zipper$fromTree(
					$zwilias$elm_rosetree$Tree$singleton(rootLabel)),
				lst));
	});
var $jxxcarlson$elm_markdown$BlockType$Root = {$: 0};
var $jxxcarlson$elm_markdown$Markdown$Parse$rootBlock = A4(
	$jxxcarlson$elm_markdown$Markdown$Parse$Block,
	_Utils_Tuple2(0, 0),
	$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Root),
	0,
	'DOCUMENT');
var $jxxcarlson$elm_markdown$Markdown$Parse$FSM = F3(
	function (a, b, c) {
		return {$: 0, a: a, b: b, c: c};
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$Start = {$: 0};
var $jxxcarlson$elm_markdown$Markdown$Parse$emptyRegister = {
	t: _List_Nil,
	V: _List_Nil,
	ac: _Utils_Tuple2(0, 0),
	aq: 0,
	af: 0,
	X: 0,
	Q: 0,
	aL: 0
};
var $jxxcarlson$elm_markdown$Markdown$Parse$initialFSM = A3($jxxcarlson$elm_markdown$Markdown$Parse$FSM, $jxxcarlson$elm_markdown$Markdown$Parse$Start, _List_Nil, $jxxcarlson$elm_markdown$Markdown$Parse$emptyRegister);
var $jxxcarlson$elm_markdown$BlockType$Table = {$: 12};
var $jxxcarlson$elm_markdown$BlockType$TableRow = {$: 11};
var $jxxcarlson$elm_markdown$Markdown$Parse$clearBlockStack = function (register) {
	return _Utils_update(
		register,
		{t: _List_Nil});
};
var $jxxcarlson$elm_markdown$Markdown$Parse$editBlock = function (block) {
	var id = block.a;
	var bt = block.b;
	var lev = block.c;
	var content = block.d;
	return (_Utils_eq(
		bt,
		$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$TableRow)) && (content === 'row')) ? A4($jxxcarlson$elm_markdown$Markdown$Parse$Block, id, bt, lev, '') : block;
};
var $jxxcarlson$elm_markdown$Markdown$Parse$topOfBlockStack = function (register) {
	return $elm$core$List$head(register.t);
};
var $jxxcarlson$elm_markdown$Markdown$Parse$typeOfBlock = function (_v0) {
	var bt = _v0.b;
	return bt;
};
var $jxxcarlson$elm_markdown$Markdown$Parse$typeOfState = function (s) {
	switch (s.$) {
		case 0:
			return $elm$core$Maybe$Nothing;
		case 1:
			var b = s.a;
			return $elm$core$Maybe$Just(
				$jxxcarlson$elm_markdown$Markdown$Parse$typeOfBlock(b));
		default:
			return $elm$core$Maybe$Nothing;
	}
};
var $jxxcarlson$elm_markdown$Markdown$Parse$handleRegister = function (fsm) {
	var state = fsm.a;
	var blocks = fsm.b;
	var register = fsm.c;
	var _v0 = $jxxcarlson$elm_markdown$Markdown$Parse$topOfBlockStack(register);
	if (_v0.$ === 1) {
		return fsm;
	} else {
		var block = _v0.a;
		var _v1 = $jxxcarlson$elm_markdown$Markdown$Parse$typeOfState(state);
		if (((!_v1.$) && (_v1.a.$ === 1)) && (_v1.a.a.$ === 11)) {
			var _v2 = _v1.a.a;
			return fsm;
		} else {
			var tableBlock = A4(
				$jxxcarlson$elm_markdown$Markdown$Parse$Block,
				_Utils_Tuple2(-1, -1),
				$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Table),
				0,
				'tableRoot');
			var rowBlock = A4(
				$jxxcarlson$elm_markdown$Markdown$Parse$Block,
				_Utils_Tuple2(-1, -1),
				$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$TableRow),
				1,
				'row');
			var tableData = A2(
				$elm$core$List$map,
				$jxxcarlson$elm_markdown$Markdown$Parse$editBlock,
				function (x) {
					return _Utils_ap(
						x,
						_List_fromArray(
							[rowBlock, tableBlock]));
				}(
					$elm$core$List$reverse(register.t)));
			var newBlocks = A2(
				$elm$core$List$filter,
				function (_v3) {
					var content = _v3.d;
					return content !== 'deleteMe';
				},
				blocks);
			return A3(
				$jxxcarlson$elm_markdown$Markdown$Parse$FSM,
				$jxxcarlson$elm_markdown$Markdown$Parse$Start,
				_Utils_ap(tableData, newBlocks),
				$jxxcarlson$elm_markdown$Markdown$Parse$clearBlockStack(register));
		}
	}
};
var $jxxcarlson$elm_markdown$Markdown$Parse$Error = {$: 2};
var $jxxcarlson$elm_markdown$Markdown$Parse$InBlock = function (a) {
	return {$: 1, a: a};
};
var $jxxcarlson$elm_markdown$BlockType$Blank = {$: 9};
var $jxxcarlson$elm_markdown$BlockType$numberOfLeadingBlanks = A2(
	$elm$parser$Parser$Advanced$map,
	$elm$core$String$length,
	$elm$parser$Parser$Advanced$getChompedString(
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed(0),
			$elm$parser$Parser$Advanced$chompWhile(
				function (c) {
					return c === ' ';
				}))));
var $elm$core$Result$toMaybe = function (result) {
	if (!result.$) {
		var v = result.a;
		return $elm$core$Maybe$Just(v);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $jxxcarlson$elm_markdown$BlockType$getNumberOfLeadingBlanks = function (str) {
	return A2(
		$elm$core$Maybe$withDefault,
		0,
		$elm$core$Result$toMaybe(
			A2($elm$parser$Parser$Advanced$run, $jxxcarlson$elm_markdown$BlockType$numberOfLeadingBlanks, str)));
};
var $jxxcarlson$elm_markdown$BlockType$dropLeadingBlanks = function (str) {
	return A2(
		$elm$core$String$dropLeft,
		$jxxcarlson$elm_markdown$BlockType$getNumberOfLeadingBlanks(str),
		str);
};
var $jxxcarlson$elm_markdown$BlockType$levelIndentation = 4;
var $jxxcarlson$elm_markdown$BlockType$level = function (ln) {
	return A2(
		$elm$core$Maybe$withDefault,
		0,
		A2(
			$elm$core$Maybe$map,
			function (l) {
				return (l / $jxxcarlson$elm_markdown$BlockType$levelIndentation) | 0;
			},
			$elm$core$Result$toMaybe(
				A2($elm$parser$Parser$Advanced$run, $jxxcarlson$elm_markdown$BlockType$numberOfLeadingBlanks, ln))));
};
var $jxxcarlson$elm_markdown$BlockType$Expecting = $elm$core$Basics$identity;
var $jxxcarlson$elm_markdown$BlockType$CssLang = 1;
var $jxxcarlson$elm_markdown$BlockType$cssLang = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$succeed(1),
	$elm$parser$Parser$Advanced$symbol(
		A2($elm$parser$Parser$Advanced$Token, 'css', 'Expecting string for language')));
var $jxxcarlson$elm_markdown$BlockType$ElmLang = 0;
var $jxxcarlson$elm_markdown$BlockType$elmLang = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$succeed(0),
	$elm$parser$Parser$Advanced$symbol(
		A2($elm$parser$Parser$Advanced$Token, 'elm', 'Expecting string for language')));
var $jxxcarlson$elm_markdown$BlockType$JavascriptLang = 2;
var $jxxcarlson$elm_markdown$BlockType$javascriptLang = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$succeed(2),
	$elm$parser$Parser$Advanced$symbol(
		A2($elm$parser$Parser$Advanced$Token, 'javascript', 'Expecting string for language')));
var $jxxcarlson$elm_markdown$BlockType$JsonLang = 3;
var $jxxcarlson$elm_markdown$BlockType$jsonLang = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$succeed(3),
	$elm$parser$Parser$Advanced$symbol(
		A2($elm$parser$Parser$Advanced$Token, 'json', 'Expecting string for language')));
var $jxxcarlson$elm_markdown$BlockType$PythonLang = 4;
var $jxxcarlson$elm_markdown$BlockType$pythonLang = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$succeed(4),
	$elm$parser$Parser$Advanced$symbol(
		A2($elm$parser$Parser$Advanced$Token, 'python', 'Expecting string for language')));
var $jxxcarlson$elm_markdown$BlockType$SqlLang = 5;
var $jxxcarlson$elm_markdown$BlockType$sqlLang = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$succeed(5),
	$elm$parser$Parser$Advanced$symbol(
		A2($elm$parser$Parser$Advanced$Token, 'sql', 'Expecting string for language')));
var $jxxcarlson$elm_markdown$BlockType$XmlLang = 6;
var $jxxcarlson$elm_markdown$BlockType$xmlLang = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$succeed(6),
	$elm$parser$Parser$Advanced$symbol(
		A2($elm$parser$Parser$Advanced$Token, 'xml', 'Expecting string for language')));
var $jxxcarlson$elm_markdown$BlockType$codeBlock = A2(
	$elm$parser$Parser$Advanced$map,
	function (lang) {
		return $jxxcarlson$elm_markdown$BlockType$BalancedBlock(
			$jxxcarlson$elm_markdown$BlockType$DisplayCode(lang));
	},
	A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
			$elm$parser$Parser$Advanced$symbol(
				A2($elm$parser$Parser$Advanced$Token, '```', 'Expecting three ticks to begin code block'))),
		$elm$parser$Parser$Advanced$oneOf(
			_List_fromArray(
				[$jxxcarlson$elm_markdown$BlockType$cssLang, $jxxcarlson$elm_markdown$BlockType$elmLang, $jxxcarlson$elm_markdown$BlockType$javascriptLang, $jxxcarlson$elm_markdown$BlockType$jsonLang, $jxxcarlson$elm_markdown$BlockType$pythonLang, $jxxcarlson$elm_markdown$BlockType$sqlLang, $jxxcarlson$elm_markdown$BlockType$xmlLang]))));
var $jxxcarlson$elm_markdown$BlockType$Heading = function (a) {
	return {$: 3, a: a};
};
var $jxxcarlson$elm_markdown$BlockType$parseWhile = function (accepting) {
	return $elm$parser$Parser$Advanced$getChompedString(
		$elm$parser$Parser$Advanced$chompWhile(accepting));
};
var $jxxcarlson$elm_markdown$BlockType$headingBlock = A2(
	$elm$parser$Parser$Advanced$map,
	function (s) {
		return $jxxcarlson$elm_markdown$BlockType$MarkdownBlock(
			$jxxcarlson$elm_markdown$BlockType$Heading(
				$elm$core$String$length(s) + 1));
	},
	A2(
		$elm$parser$Parser$Advanced$keeper,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
				$elm$parser$Parser$Advanced$spaces),
			$elm$parser$Parser$Advanced$symbol(
				A2($elm$parser$Parser$Advanced$Token, '#', 'Expecting \'#\' to begin heading block'))),
		$jxxcarlson$elm_markdown$BlockType$parseWhile(
			function (c) {
				return c === '#';
			})));
var $jxxcarlson$elm_markdown$BlockType$HorizontalRule = {$: 4};
var $jxxcarlson$elm_markdown$BlockType$horizontalRuleBlock = A2(
	$elm$parser$Parser$Advanced$map,
	function (x) {
		return $jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$HorizontalRule);
	},
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			$elm$parser$Parser$Advanced$succeed(0),
			$elm$parser$Parser$Advanced$spaces),
		$elm$parser$Parser$Advanced$symbol(
			A2($elm$parser$Parser$Advanced$Token, '___', 'Expecting at least three underscores to begin thematic break'))));
var $jxxcarlson$elm_markdown$BlockType$Image = {$: 8};
var $jxxcarlson$elm_markdown$BlockType$imageBlock = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$succeed(
		$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Image)),
	$elm$parser$Parser$Advanced$symbol(
		A2($elm$parser$Parser$Advanced$Token, '![', 'Expecting \'![\' to begin image block')));
var $jxxcarlson$elm_markdown$BlockType$mathBlock = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$succeed(
		$jxxcarlson$elm_markdown$BlockType$BalancedBlock($jxxcarlson$elm_markdown$BlockType$DisplayMath)),
	$elm$parser$Parser$Advanced$symbol(
		A2($elm$parser$Parser$Advanced$Token, '$$', 'Expecting four ticks to begin verbatim block')));
var $jxxcarlson$elm_markdown$BlockType$OListItem = function (a) {
	return {$: 2, a: a};
};
var $jxxcarlson$elm_markdown$BlockType$orderedListItemBlock = A2(
	$elm$parser$Parser$Advanced$map,
	function (_v0) {
		return $jxxcarlson$elm_markdown$BlockType$MarkdownBlock(
			$jxxcarlson$elm_markdown$BlockType$OListItem(0));
	},
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		A2(
			$elm$parser$Parser$Advanced$ignorer,
			A2(
				$elm$parser$Parser$Advanced$ignorer,
				A2(
					$elm$parser$Parser$Advanced$ignorer,
					$elm$parser$Parser$Advanced$succeed(0),
					$jxxcarlson$elm_markdown$BlockType$parseWhile(
						function (c) {
							return c === ' ';
						})),
				A2(
					$elm$parser$Parser$Advanced$chompIf,
					function (c) {
						return $elm$core$Char$isDigit(c);
					},
					'Expecting digit to begin ordered list item')),
			$elm$parser$Parser$Advanced$chompWhile(
				function (c) {
					return $elm$core$Char$isDigit(c);
				})),
		$elm$parser$Parser$Advanced$symbol(
			A2($elm$parser$Parser$Advanced$Token, '. ', 'expecting period'))));
var $jxxcarlson$elm_markdown$BlockType$Poetry = {$: 6};
var $jxxcarlson$elm_markdown$BlockType$poetryBlock = A2(
	$elm$parser$Parser$Advanced$map,
	function (_v0) {
		return $jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Poetry);
	},
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$succeed(0),
		$elm$parser$Parser$Advanced$symbol(
			A2($elm$parser$Parser$Advanced$Token, '>> ', 'expecting \'>> \' to begin poetry block'))));
var $jxxcarlson$elm_markdown$BlockType$Quotation = {$: 5};
var $jxxcarlson$elm_markdown$BlockType$quotationBlock = A2(
	$elm$parser$Parser$Advanced$map,
	function (_v0) {
		return $jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Quotation);
	},
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$succeed(0),
		$elm$parser$Parser$Advanced$symbol(
			A2($elm$parser$Parser$Advanced$Token, '> ', 'expecting \'> \' to begin quotation'))));
var $jxxcarlson$elm_markdown$BlockType$tableBlock = A2(
	$elm$parser$Parser$Advanced$map,
	function (_v0) {
		return $jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$TableRow);
	},
	A2(
		$elm$parser$Parser$Advanced$ignorer,
		$elm$parser$Parser$Advanced$succeed(0),
		$elm$parser$Parser$Advanced$symbol(
			A2($elm$parser$Parser$Advanced$Token, '| ', 'expecting \'| \' to begin poetry block'))));
var $jxxcarlson$elm_markdown$BlockType$UListItem = {$: 1};
var $jxxcarlson$elm_markdown$BlockType$unorderedListItemBlock = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$succeed(
		$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$UListItem)),
	$elm$parser$Parser$Advanced$symbol(
		A2($elm$parser$Parser$Advanced$Token, '- ', 'Expecting \'-\' to begin list item')));
var $jxxcarlson$elm_markdown$BlockType$verbatimBlock = A2(
	$elm$parser$Parser$Advanced$ignorer,
	$elm$parser$Parser$Advanced$succeed(
		$jxxcarlson$elm_markdown$BlockType$BalancedBlock($jxxcarlson$elm_markdown$BlockType$Verbatim)),
	$elm$parser$Parser$Advanced$symbol(
		A2($elm$parser$Parser$Advanced$Token, '````', 'Expecting four ticks to begin verbatim block')));
var $jxxcarlson$elm_markdown$BlockType$parseExtended = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[
			$jxxcarlson$elm_markdown$BlockType$imageBlock,
			$jxxcarlson$elm_markdown$BlockType$mathBlock,
			$jxxcarlson$elm_markdown$BlockType$unorderedListItemBlock,
			$jxxcarlson$elm_markdown$BlockType$orderedListItemBlock,
			$jxxcarlson$elm_markdown$BlockType$quotationBlock,
			$jxxcarlson$elm_markdown$BlockType$poetryBlock,
			$elm$parser$Parser$Advanced$backtrackable($jxxcarlson$elm_markdown$BlockType$verbatimBlock),
			$jxxcarlson$elm_markdown$BlockType$codeBlock,
			$jxxcarlson$elm_markdown$BlockType$tableBlock,
			$jxxcarlson$elm_markdown$BlockType$headingBlock,
			$jxxcarlson$elm_markdown$BlockType$horizontalRuleBlock
		]));
var $jxxcarlson$elm_markdown$BlockType$parseStandard = $elm$parser$Parser$Advanced$oneOf(
	_List_fromArray(
		[$jxxcarlson$elm_markdown$BlockType$tableBlock, $jxxcarlson$elm_markdown$BlockType$imageBlock, $jxxcarlson$elm_markdown$BlockType$unorderedListItemBlock, $jxxcarlson$elm_markdown$BlockType$orderedListItemBlock, $jxxcarlson$elm_markdown$BlockType$quotationBlock, $jxxcarlson$elm_markdown$BlockType$codeBlock, $jxxcarlson$elm_markdown$BlockType$headingBlock, $jxxcarlson$elm_markdown$BlockType$horizontalRuleBlock]));
var $jxxcarlson$elm_markdown$BlockType$parse = function (option) {
	if (!option) {
		return $jxxcarlson$elm_markdown$BlockType$parseStandard;
	} else {
		return $jxxcarlson$elm_markdown$BlockType$parseExtended;
	}
};
var $jxxcarlson$elm_markdown$BlockType$get = F2(
	function (option, str) {
		if (str === '\n') {
			return _Utils_Tuple2(
				0,
				$elm$core$Maybe$Just(
					$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Blank)));
		} else {
			var _v0 = A2(
				$elm$parser$Parser$Advanced$run,
				$jxxcarlson$elm_markdown$BlockType$parse(option),
				$jxxcarlson$elm_markdown$BlockType$dropLeadingBlanks(str));
			if (!_v0.$) {
				var result = _v0.a;
				return _Utils_Tuple2(
					$jxxcarlson$elm_markdown$BlockType$level(str),
					$elm$core$Maybe$Just(result));
			} else {
				return _Utils_Tuple2(
					$jxxcarlson$elm_markdown$BlockType$level(str),
					$elm$core$Maybe$Just(
						$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Plain)));
			}
		}
	});
var $jxxcarlson$elm_markdown$BlockType$TableCell = {$: 10};
var $jxxcarlson$elm_markdown$Markdown$Parse$parseTableRow = F2(
	function (level, line) {
		return A2(
			$elm$core$List$map,
			function (s) {
				return A4(
					$jxxcarlson$elm_markdown$Markdown$Parse$Block,
					_Utils_Tuple2(-1, -1),
					$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$TableCell),
					level,
					s);
			},
			A2(
				$elm$core$List$filter,
				function (s) {
					return s !== '';
				},
				A2(
					$elm$core$List$map,
					$elm$core$String$trim,
					A2($elm$core$String$split, '|', line))));
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$handleTableStart = F6(
	function (blockTypeOfLine, level, line, state, blocks, register) {
		switch (state.$) {
			case 0:
				return A3($jxxcarlson$elm_markdown$Markdown$Parse$FSM, state, blocks, register);
			case 2:
				return A3($jxxcarlson$elm_markdown$Markdown$Parse$FSM, state, blocks, register);
			default:
				var currentBlock = state.a;
				var tableBlock = A4(
					$jxxcarlson$elm_markdown$Markdown$Parse$Block,
					_Utils_Tuple2(-1, -1),
					$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Table),
					level,
					'tableRoot');
				var rowBlock = A4(
					$jxxcarlson$elm_markdown$Markdown$Parse$Block,
					_Utils_Tuple2(-1, -1),
					blockTypeOfLine,
					level + 1,
					'row');
				var childrenOfNewBlock = A2($jxxcarlson$elm_markdown$Markdown$Parse$parseTableRow, level + 2, line);
				var newRow = _Utils_ap(
					childrenOfNewBlock,
					_List_fromArray(
						[rowBlock]));
				return A3(
					$jxxcarlson$elm_markdown$Markdown$Parse$FSM,
					$jxxcarlson$elm_markdown$Markdown$Parse$InBlock(rowBlock),
					blocks,
					_Utils_update(
						register,
						{t: newRow, aL: register.aL + 0}));
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$lineIsNotBlank = function (line) {
	return $elm$core$String$trim(line) !== '';
};
var $jxxcarlson$elm_markdown$Markdown$Parse$newBlockTypeIsDifferent = F2(
	function (blockType, state) {
		if (state.$ === 1) {
			var currentBlock = state.a;
			return !_Utils_eq(
				$jxxcarlson$elm_markdown$Markdown$Parse$typeOfBlock(currentBlock),
				blockType);
		} else {
			return false;
		}
	});
var $jxxcarlson$elm_markdown$BlockType$prefixOfBalancedType = function (bt) {
	switch (bt.$) {
		case 0:
			return '```';
		case 1:
			return '````';
		default:
			return '$$';
	}
};
var $elm$parser$Parser$Advanced$findSubString = _Parser_findSubString;
var $elm$parser$Parser$Advanced$fromInfo = F4(
	function (row, col, x, context) {
		return A2(
			$elm$parser$Parser$Advanced$AddRight,
			$elm$parser$Parser$Advanced$Empty,
			A4($elm$parser$Parser$Advanced$DeadEnd, row, col, x, context));
	});
var $elm$parser$Parser$Advanced$chompUntil = function (_v0) {
	var str = _v0.a;
	var expecting = _v0.b;
	return function (s) {
		var _v1 = A5($elm$parser$Parser$Advanced$findSubString, str, s.b, s.bK, s.a7, s.a);
		var newOffset = _v1.a;
		var newRow = _v1.b;
		var newCol = _v1.c;
		return _Utils_eq(newOffset, -1) ? A2(
			$elm$parser$Parser$Advanced$Bad,
			false,
			A4($elm$parser$Parser$Advanced$fromInfo, newRow, newCol, expecting, s.d)) : A3(
			$elm$parser$Parser$Advanced$Good,
			_Utils_cmp(s.b, newOffset) < 0,
			0,
			{a7: newCol, d: s.d, f: s.f, b: newOffset, bK: newRow, a: s.a});
	};
};
var $jxxcarlson$elm_markdown$BlockType$oListPrefix = A2(
	$elm$parser$Parser$Advanced$map,
	function (x) {
		return x + '. ';
	},
	$elm$parser$Parser$Advanced$getChompedString(
		A2(
			$elm$parser$Parser$Advanced$keeper,
			$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
			$elm$parser$Parser$Advanced$chompUntil(
				A2($elm$parser$Parser$Advanced$Token, '.', 'expecting \'.\' to begin OListItem block')))));
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			$elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var $elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3($elm$core$String$repeatHelp, n, chunk, '');
	});
var $jxxcarlson$elm_markdown$BlockType$uListPrefix = A2(
	$elm$parser$Parser$Advanced$map,
	function (s) {
		return s + '- ';
	},
	$elm$parser$Parser$Advanced$getChompedString(
		A2(
			$elm$parser$Parser$Advanced$keeper,
			$elm$parser$Parser$Advanced$succeed($elm$core$Basics$identity),
			$elm$parser$Parser$Advanced$chompUntil(
				A2($elm$parser$Parser$Advanced$Token, '-', 'expecting \'-\' to begin UListItem block')))));
var $jxxcarlson$elm_markdown$BlockType$prefixOfMarkdownType = F2(
	function (mdt, line) {
		var runPrefix = F2(
			function (prefixParser, str) {
				var _v1 = A2($elm$parser$Parser$Advanced$run, prefixParser, str);
				if (!_v1.$) {
					var prefix = _v1.a;
					return prefix;
				} else {
					return '';
				}
			});
		switch (mdt.$) {
			case 0:
				return '';
			case 1:
				return A2(runPrefix, $jxxcarlson$elm_markdown$BlockType$uListPrefix, line);
			case 2:
				return A2(runPrefix, $jxxcarlson$elm_markdown$BlockType$oListPrefix, line);
			case 3:
				var k = mdt.a;
				return A2($elm$core$String$repeat, k, '#') + ' ';
			case 4:
				return '___';
			case 5:
				return '> ';
			case 6:
				return '>> ';
			case 7:
				return '';
			case 8:
				return '';
			case 10:
				return '';
			case 11:
				return '';
			case 12:
				return '';
			default:
				return '';
		}
	});
var $jxxcarlson$elm_markdown$BlockType$prefixOfBlockType = F2(
	function (bt, line) {
		if (!bt.$) {
			var bb = bt.a;
			return $jxxcarlson$elm_markdown$BlockType$prefixOfBalancedType(bb);
		} else {
			var mdb = bt.a;
			return A2($jxxcarlson$elm_markdown$BlockType$prefixOfMarkdownType, mdb, line);
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$removePrefix = F2(
	function (blockType, line_) {
		var p = A2($jxxcarlson$elm_markdown$BlockType$prefixOfBlockType, blockType, line_);
		return A3($elm$core$String$replace, p, '', line_);
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$incrementRegisterLevel = F2(
	function (level, register) {
		var _v0 = level + 1;
		switch (_v0) {
			case 1:
				return _Utils_Tuple2(
					register.aq + 1,
					_Utils_update(
						register,
						{aq: register.aq + 1, af: 0, X: 0, Q: 0}));
			case 2:
				return _Utils_Tuple2(
					register.af + 1,
					_Utils_update(
						register,
						{af: register.af + 1, X: 0, Q: 0}));
			case 3:
				return _Utils_Tuple2(
					register.X + 1,
					_Utils_update(
						register,
						{X: register.X + 1, Q: 0}));
			case 4:
				return _Utils_Tuple2(
					register.Q + 1,
					_Utils_update(
						register,
						{Q: register.Q + 1}));
			default:
				return _Utils_Tuple2(0, register);
		}
	});
var $jxxcarlson$elm_markdown$BlockType$isCode = function (bt) {
	if ((!bt.$) && (!bt.a.$)) {
		return true;
	} else {
		return false;
	}
};
var $jxxcarlson$elm_markdown$BlockType$isOListItem = function (blockType) {
	if ((blockType.$ === 1) && (blockType.a.$ === 2)) {
		return true;
	} else {
		return false;
	}
};
var $jxxcarlson$elm_markdown$Markdown$Parse$updateRegisterAndBlockType = F3(
	function (blockType, level_, register) {
		if ($jxxcarlson$elm_markdown$BlockType$isOListItem(blockType)) {
			var _v0 = A2($jxxcarlson$elm_markdown$Markdown$Parse$incrementRegisterLevel, level_, register);
			var index = _v0.a;
			var newRegister = _v0.b;
			var newBlockType = $jxxcarlson$elm_markdown$BlockType$MarkdownBlock(
				$jxxcarlson$elm_markdown$BlockType$OListItem(index));
			return _Utils_Tuple2(newBlockType, newRegister);
		} else {
			if ($jxxcarlson$elm_markdown$BlockType$isCode(blockType)) {
				return _Utils_Tuple2(
					blockType,
					_Utils_update(
						register,
						{
							V: A2($elm$core$List$cons, blockType, register.V)
						}));
			} else {
				return _Utils_Tuple2(blockType, $jxxcarlson$elm_markdown$Markdown$Parse$emptyRegister);
			}
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$nextStateAtStart = F3(
	function (option, line, fsm) {
		var state = fsm.a;
		var blocks = fsm.b;
		var register = fsm.c;
		var _v0 = A2($jxxcarlson$elm_markdown$BlockType$get, option, line);
		if (_v0.b.$ === 1) {
			var _v1 = _v0.b;
			return A3($jxxcarlson$elm_markdown$Markdown$Parse$FSM, $jxxcarlson$elm_markdown$Markdown$Parse$Error, blocks, register);
		} else {
			var level = _v0.a;
			var blockType = _v0.b.a;
			var newLine = A2($jxxcarlson$elm_markdown$Markdown$Parse$removePrefix, blockType, line);
			var _v2 = A3($jxxcarlson$elm_markdown$Markdown$Parse$updateRegisterAndBlockType, blockType, level, register);
			var newBlockType = _v2.a;
			var newRegister = _v2.b;
			return (_Utils_eq(
				newBlockType,
				$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$TableRow)) && A2($jxxcarlson$elm_markdown$Markdown$Parse$newBlockTypeIsDifferent, newBlockType, state)) ? A6($jxxcarlson$elm_markdown$Markdown$Parse$handleTableStart, blockType, level, line, state, blocks, register) : ($jxxcarlson$elm_markdown$Markdown$Parse$lineIsNotBlank(line) ? A3(
				$jxxcarlson$elm_markdown$Markdown$Parse$FSM,
				$jxxcarlson$elm_markdown$Markdown$Parse$InBlock(
					A4(
						$jxxcarlson$elm_markdown$Markdown$Parse$Block,
						_Utils_Tuple2(-1, -1),
						newBlockType,
						level,
						newLine)),
				blocks,
				newRegister) : fsm);
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$getTopOfBlockTypeStack = function (_v0) {
	var register = _v0.c;
	return $elm$core$List$head(register.V);
};
var $jxxcarlson$elm_markdown$Markdown$Parse$isBalanced = F3(
	function (str, mbt, bt2) {
		if (mbt.$ === 1) {
			if (!bt2.$) {
				return true;
			} else {
				return false;
			}
		} else {
			var bt1 = mbt.a;
			var _v2 = _Utils_Tuple3(
				bt1,
				bt2,
				$elm$core$String$trimLeft(str) === '```\n');
			if (!_v2.b.$) {
				if ((!_v2.a.$) && (!_v2.a.a.$)) {
					return false;
				} else {
					return true;
				}
			} else {
				if ((!_v2.a.$) && (!_v2.a.a.$)) {
					if (!_v2.c) {
						return false;
					} else {
						return true;
					}
				} else {
					return false;
				}
			}
		}
	});
var $jxxcarlson$elm_markdown$BlockType$isMarkDown = function (bt) {
	if (!bt.$) {
		return false;
	} else {
		return true;
	}
};
var $jxxcarlson$elm_markdown$Markdown$Parse$addLineToBlock = F2(
	function (line, _v0) {
		var id = _v0.a;
		var blockType_ = _v0.b;
		var level_ = _v0.c;
		var content_ = _v0.d;
		return A4(
			$jxxcarlson$elm_markdown$Markdown$Parse$Block,
			id,
			blockType_,
			level_,
			_Utils_ap(content_, line));
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$trimBalancedBlock = function (_v0) {
	var id = _v0.a;
	var bt = _v0.b;
	var lev = _v0.c;
	var content = _v0.d;
	return A4(
		$jxxcarlson$elm_markdown$Markdown$Parse$Block,
		id,
		bt,
		lev,
		$elm$core$String$trim(content));
};
var $jxxcarlson$elm_markdown$Markdown$Parse$processBalancedBlock = F3(
	function (blockType, line, fsm) {
		var state_ = fsm.a;
		var blocks_ = fsm.b;
		var register = fsm.c;
		if (_Utils_eq(
			$elm$core$Maybe$Just(blockType),
			$jxxcarlson$elm_markdown$Markdown$Parse$typeOfState(
				$jxxcarlson$elm_markdown$Markdown$Parse$stateOfFSM(fsm)))) {
			var _v0 = $jxxcarlson$elm_markdown$Markdown$Parse$stateOfFSM(fsm);
			if (_v0.$ === 1) {
				var block_ = _v0.a;
				var line_ = A2($jxxcarlson$elm_markdown$Markdown$Parse$removePrefix, blockType, line);
				var block__ = function () {
					if ((!blockType.$) && (blockType.a.$ === 1)) {
						var _v2 = blockType.a;
						return block_;
					} else {
						return $jxxcarlson$elm_markdown$Markdown$Parse$trimBalancedBlock(block_);
					}
				}();
				return A3(
					$jxxcarlson$elm_markdown$Markdown$Parse$FSM,
					$jxxcarlson$elm_markdown$Markdown$Parse$Start,
					A2(
						$elm$core$List$cons,
						A2($jxxcarlson$elm_markdown$Markdown$Parse$addLineToBlock, line_, block__),
						blocks_),
					register);
			} else {
				return fsm;
			}
		} else {
			var _v3 = $jxxcarlson$elm_markdown$Markdown$Parse$stateOfFSM(fsm);
			if (_v3.$ === 1) {
				var block_ = _v3.a;
				var line_ = ($elm$core$String$trimLeft(line) === '```\n') ? '\n' : line;
				var block__ = $jxxcarlson$elm_markdown$Markdown$Parse$trimBalancedBlock(block_);
				return A3(
					$jxxcarlson$elm_markdown$Markdown$Parse$FSM,
					$jxxcarlson$elm_markdown$Markdown$Parse$InBlock(
						A4(
							$jxxcarlson$elm_markdown$Markdown$Parse$Block,
							register.ac,
							blockType,
							$jxxcarlson$elm_markdown$BlockType$level(line_),
							line_)),
					A2($elm$core$List$cons, block__, blocks_),
					_Utils_update(
						register,
						{
							V: A2($elm$core$List$drop, 1, register.V)
						}));
			} else {
				return fsm;
			}
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$addLineToState = F2(
	function (line, state_) {
		switch (state_.$) {
			case 0:
				return $jxxcarlson$elm_markdown$Markdown$Parse$Start;
			case 2:
				return $jxxcarlson$elm_markdown$Markdown$Parse$Error;
			default:
				var block_ = state_.a;
				return $jxxcarlson$elm_markdown$Markdown$Parse$InBlock(
					A2($jxxcarlson$elm_markdown$Markdown$Parse$addLineToBlock, line, block_));
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$addLineToFSM = F2(
	function (line, _v0) {
		var state_ = _v0.a;
		var blocks_ = _v0.b;
		var register = _v0.c;
		switch (state_.$) {
			case 0:
				return A3($jxxcarlson$elm_markdown$Markdown$Parse$FSM, state_, blocks_, register);
			case 2:
				return A3($jxxcarlson$elm_markdown$Markdown$Parse$FSM, state_, blocks_, register);
			default:
				var _v2 = $elm$core$List$head(register.t);
				if (_v2.$ === 1) {
					return A3(
						$jxxcarlson$elm_markdown$Markdown$Parse$FSM,
						A2($jxxcarlson$elm_markdown$Markdown$Parse$addLineToState, line, state_),
						blocks_,
						register);
				} else {
					var block = _v2.a;
					return A3(
						$jxxcarlson$elm_markdown$Markdown$Parse$FSM,
						A2($jxxcarlson$elm_markdown$Markdown$Parse$addLineToState, line, state_),
						A2($elm$core$List$cons, block, blocks_),
						_Utils_update(
							register,
							{
								t: A2($elm$core$List$drop, 1, register.t)
							}));
				}
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$adjustLevel = function (block) {
	var id = block.a;
	var blockType = block.b;
	var level = block.c;
	var content = block.d;
	if (_Utils_eq(
		blockType,
		$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Plain))) {
		var newLevel = $jxxcarlson$elm_markdown$BlockType$level(content);
		return A4($jxxcarlson$elm_markdown$Markdown$Parse$Block, id, blockType, newLevel, content);
	} else {
		return block;
	}
};
var $jxxcarlson$elm_markdown$Markdown$Parse$addNewMarkdownBlock = F4(
	function (option, currentBlock, line, fsm) {
		var id = currentBlock.a;
		var typeOfCurrentBlock = currentBlock.b;
		var levelOfCurrentBlock = currentBlock.c;
		var state = fsm.a;
		var blocks = fsm.b;
		var register = fsm.c;
		var _v0 = A2($jxxcarlson$elm_markdown$BlockType$get, option, line);
		if (_v0.b.$ === 1) {
			var _v1 = _v0.b;
			return fsm;
		} else {
			var level = _v0.a;
			var newBlockType_ = _v0.b.a;
			var newLine = A2($jxxcarlson$elm_markdown$Markdown$Parse$removePrefix, typeOfCurrentBlock, line);
			var _v2 = A3($jxxcarlson$elm_markdown$Markdown$Parse$updateRegisterAndBlockType, newBlockType_, level, register);
			var newBlockType = _v2.a;
			var newRegister = _v2.b;
			var newBlock = A4(
				$jxxcarlson$elm_markdown$Markdown$Parse$Block,
				id,
				newBlockType,
				level,
				A2($jxxcarlson$elm_markdown$Markdown$Parse$removePrefix, newBlockType, newLine));
			return A3(
				$jxxcarlson$elm_markdown$Markdown$Parse$FSM,
				$jxxcarlson$elm_markdown$Markdown$Parse$InBlock(newBlock),
				A2(
					$elm$core$List$cons,
					$jxxcarlson$elm_markdown$Markdown$Parse$adjustLevel(currentBlock),
					blocks),
				newRegister);
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$handleInnerTableRow = F6(
	function (blockTypeOfLine, level, line, state, blocks, register) {
		switch (state.$) {
			case 0:
				return A3($jxxcarlson$elm_markdown$Markdown$Parse$FSM, state, blocks, register);
			case 2:
				return A3($jxxcarlson$elm_markdown$Markdown$Parse$FSM, state, blocks, register);
			default:
				var currentBlock = state.a;
				var tableMarker = A4(
					$jxxcarlson$elm_markdown$Markdown$Parse$Block,
					_Utils_Tuple2(-1, -1),
					$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$TableRow),
					level + 1,
					'deleteMe');
				var rowBlock = A4(
					$jxxcarlson$elm_markdown$Markdown$Parse$Block,
					_Utils_Tuple2(-1, -1),
					blockTypeOfLine,
					level + 1,
					'row');
				var childrenOfNewBlock = A2($jxxcarlson$elm_markdown$Markdown$Parse$parseTableRow, level + 2, line);
				var newRow = _Utils_ap(
					childrenOfNewBlock,
					_List_fromArray(
						[rowBlock]));
				return A3(
					$jxxcarlson$elm_markdown$Markdown$Parse$FSM,
					$jxxcarlson$elm_markdown$Markdown$Parse$InBlock(tableMarker),
					blocks,
					_Utils_update(
						register,
						{
							t: _Utils_ap(register.t, newRow)
						}));
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$handleTableRow = F6(
	function (blockTypeOfLine, level, line, state, blocks, register) {
		return A2($jxxcarlson$elm_markdown$Markdown$Parse$newBlockTypeIsDifferent, blockTypeOfLine, state) ? A6($jxxcarlson$elm_markdown$Markdown$Parse$handleTableStart, blockTypeOfLine, level, line, state, blocks, register) : A6($jxxcarlson$elm_markdown$Markdown$Parse$handleInnerTableRow, blockTypeOfLine, level, line, state, blocks, register);
	});
var $jxxcarlson$elm_markdown$BlockType$isBalanced = function (bt) {
	if (!bt.$) {
		return true;
	} else {
		return false;
	}
};
var $jxxcarlson$elm_markdown$Markdown$Parse$processMarkDownBlock = F5(
	function (option, level, blockTypeOfLine, line, fsm) {
		var state = fsm.a;
		var blocks = fsm.b;
		var register = fsm.c;
		if (state.$ === 1) {
			var currentBlock = state.a;
			var id = currentBlock.a;
			var typeOfCurrentBlock = currentBlock.b;
			var levelOfCurrentBlock = currentBlock.c;
			return $jxxcarlson$elm_markdown$BlockType$isBalanced(typeOfCurrentBlock) ? A2($jxxcarlson$elm_markdown$Markdown$Parse$addLineToFSM, line, fsm) : (_Utils_eq(
				blockTypeOfLine,
				$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Blank)) ? A3(
				$jxxcarlson$elm_markdown$Markdown$Parse$FSM,
				$jxxcarlson$elm_markdown$Markdown$Parse$Start,
				A2(
					$elm$core$List$cons,
					$jxxcarlson$elm_markdown$Markdown$Parse$adjustLevel(currentBlock),
					blocks),
				register) : ((_Utils_eq(
				blockTypeOfLine,
				$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$Plain)) && ((!_Utils_eq(
				typeOfCurrentBlock,
				$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$TableRow))) && $jxxcarlson$elm_markdown$Markdown$Parse$lineIsNotBlank(line))) ? A2($jxxcarlson$elm_markdown$Markdown$Parse$addLineToFSM, line, fsm) : (_Utils_eq(
				blockTypeOfLine,
				$jxxcarlson$elm_markdown$BlockType$MarkdownBlock($jxxcarlson$elm_markdown$BlockType$TableRow)) ? A6($jxxcarlson$elm_markdown$Markdown$Parse$handleTableRow, blockTypeOfLine, level, line, state, blocks, register) : A4($jxxcarlson$elm_markdown$Markdown$Parse$addNewMarkdownBlock, option, currentBlock, line, fsm))));
		} else {
			return fsm;
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$nextStateInBlock = F3(
	function (option, line, fsm) {
		var state_ = fsm.a;
		var blocks_ = fsm.b;
		var register = fsm.c;
		var _v0 = A2($jxxcarlson$elm_markdown$BlockType$get, option, line);
		if (_v0.b.$ === 1) {
			var _v1 = _v0.b;
			return A3(
				$jxxcarlson$elm_markdown$Markdown$Parse$FSM,
				$jxxcarlson$elm_markdown$Markdown$Parse$Error,
				$jxxcarlson$elm_markdown$Markdown$Parse$blockListOfFSM(fsm),
				register);
		} else {
			var level = _v0.a;
			var blockType = _v0.b.a;
			return A3(
				$jxxcarlson$elm_markdown$Markdown$Parse$isBalanced,
				line,
				$jxxcarlson$elm_markdown$Markdown$Parse$getTopOfBlockTypeStack(fsm),
				blockType) ? A3($jxxcarlson$elm_markdown$Markdown$Parse$processBalancedBlock, blockType, line, fsm) : ($jxxcarlson$elm_markdown$BlockType$isMarkDown(blockType) ? A5($jxxcarlson$elm_markdown$Markdown$Parse$processMarkDownBlock, option, level, blockType, line, fsm) : fsm);
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$nextState = F3(
	function (option, line, fsm_) {
		var state = fsm_.a;
		var blocks = fsm_.b;
		var register = fsm_.c;
		var fsm = $jxxcarlson$elm_markdown$Markdown$Parse$handleRegister(fsm_);
		var _v0 = $jxxcarlson$elm_markdown$Markdown$Parse$stateOfFSM(fsm);
		switch (_v0.$) {
			case 0:
				return A3($jxxcarlson$elm_markdown$Markdown$Parse$nextStateAtStart, option, line, fsm);
			case 1:
				return A3($jxxcarlson$elm_markdown$Markdown$Parse$nextStateInBlock, option, line, fsm);
			default:
				return fsm;
		}
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$runFSM = F2(
	function (option, lines) {
		var folder = F2(
			function (line, fsm) {
				return A3($jxxcarlson$elm_markdown$Markdown$Parse$nextState, option, line, fsm);
			});
		return A3($elm$core$List$foldl, folder, $jxxcarlson$elm_markdown$Markdown$Parse$initialFSM, lines);
	});
var $elm$core$String$lines = _String_lines;
var $jxxcarlson$elm_markdown$Markdown$Parse$splitIntoLines = function (str) {
	return A2(
		$elm$core$List$map,
		function (l) {
			return l + '\n';
		},
		$elm$core$String$lines(str));
};
var $jxxcarlson$elm_markdown$Markdown$Parse$toBlockTree = F2(
	function (option, document) {
		return A3(
			$jxxcarlson$htree$HTree$fromList,
			$jxxcarlson$elm_markdown$Markdown$Parse$rootBlock,
			$jxxcarlson$elm_markdown$Markdown$Parse$blockLevel,
			A2(
				$elm$core$List$map,
				$jxxcarlson$elm_markdown$Markdown$Parse$changeLevel(1),
				$jxxcarlson$elm_markdown$Markdown$Parse$flush(
					A2(
						$jxxcarlson$elm_markdown$Markdown$Parse$runFSM,
						option,
						$jxxcarlson$elm_markdown$Markdown$Parse$splitIntoLines(document)))));
	});
var $jxxcarlson$elm_markdown$Markdown$Parse$toMDBlockTree = F3(
	function (version, option, document) {
		return A2(
			$zwilias$elm_rosetree$Tree$indexedMap,
			F2(
				function (idx, block) {
					return A3($jxxcarlson$elm_markdown$Markdown$Parse$setBlockIndex, version, idx, block);
				}),
			A2(
				$zwilias$elm_rosetree$Tree$map,
				$jxxcarlson$elm_markdown$Markdown$Parse$selectParser(option),
				A2($jxxcarlson$elm_markdown$Markdown$Parse$toBlockTree, option, document)));
	});
var $jxxcarlson$elm_markdown$Markdown$Render$toHtml = F2(
	function (option, str) {
		return A2(
			$jxxcarlson$elm_markdown$Markdown$Render$fromAST,
			_Utils_Tuple2(0, 0),
			A3($jxxcarlson$elm_markdown$Markdown$Parse$toMDBlockTree, 0, option, str));
	});
var $elm$html$Html$ul = _VirtualDom_node('ul');
var $author$project$Main$articleView = F2(
	function (zone, post) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$ul,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'font-size', 'large')
								]),
							_List_fromArray(
								[
									$elm$html$Html$text(post.bW)
								])),
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									'投稿日:' + A2($author$project$Main$showTime, zone, post.c0))
								])),
							A2(
							$elm$html$Html$ul,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$li,
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$style, 'list-style', 'none')
										]),
									A2(
										$elm$core$List$cons,
										$elm$html$Html$text('タグ: '),
										A2(
											$elm$core$List$map,
											A2($elm$core$Basics$composeL, $elm$html$Html$text, $author$project$Models$showTag),
											post.cZ)))
								])),
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$map,
									function (_v0) {
										return $author$project$Main$NoOp;
									},
									A2($jxxcarlson$elm_markdown$Markdown$Render$toHtml, 2, post.b7))
								]))
						]))
				]));
	});
var $author$project$Main$articlesView = F2(
	function (zone, articles) {
		return A2(
			$elm$html$Html$ul,
			_List_Nil,
			A2(
				$elm$core$List$map,
				$author$project$Main$articleView(zone),
				articles));
	});
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 0, a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $author$project$Main$jst = A2($elm$time$Time$customZone, 9 * 60, _List_Nil);
var $author$project$Main$mainMarginLeft = A2($elm$html$Html$Attributes$style, 'margin-left', '300px');
var $author$project$Main$myProfile = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			A2($elm$html$Html$Attributes$style, 'height', '100%'),
			A2($elm$html$Html$Attributes$style, 'background-color', '#F0F0F0'),
			A2($elm$html$Html$Attributes$style, 'position', 'fixed'),
			A2($elm$html$Html$Attributes$style, 'z-index', '1'),
			A2($elm$html$Html$Attributes$style, 'left', '0'),
			A2($elm$html$Html$Attributes$style, 'top', '0'),
			A2($elm$html$Html$Attributes$style, 'overflow-x', 'hidden'),
			A2($elm$html$Html$Attributes$style, 'padding-top', '60px'),
			A2($elm$html$Html$Attributes$style, 'padding-right', '60px'),
			A2($elm$html$Html$Attributes$style, 'padding-left', '30px'),
			A2($elm$html$Html$Attributes$style, 'transition', '0.5s')
		]),
	_List_fromArray(
		[
			A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					$elm$html$Html$text('profile')
				])),
			A2(
			$elm$html$Html$ul,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$li,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('なまえ: tsukimizake')
						])),
					A2(
					$elm$html$Html$li,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('最近の趣味: 柔術')
						])),
					A2(
					$elm$html$Html$li,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('将来の夢: 農家')
						]))
				]))
		]));
var $author$project$Main$pageTitle = A2(
	$elm$html$Html$div,
	_List_fromArray(
		[
			A2($elm$html$Html$Attributes$style, 'font-size', 'x-large')
		]),
	_List_fromArray(
		[
			$elm$html$Html$text('ブログ予定地')
		]));
var $author$project$Main$view = function (model) {
	return {
		b8: _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						$author$project$Main$myProfile,
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[$author$project$Main$mainMarginLeft]),
						_List_fromArray(
							[
								$author$project$Main$pageTitle,
								A2($author$project$Main$articlesView, $author$project$Main$jst, model.aG)
							]))
					]))
			]),
		bW: 'ブログ予定地'
	};
};
var $author$project$Main$main = $elm$browser$Browser$document(
	{
		ct: function (_v0) {
			return _Utils_Tuple2(
				{aG: _List_Nil},
				$elm$http$Http$get(
					{
						cj: A2($elm$http$Http$expectJson, $author$project$Main$GotArticles, $author$project$ArticlesDecoder$articlesDecoder),
						c1: 'https://tsukimizake.github.io/articles.json'
					}));
		},
		cW: $author$project$Main$subscriptions,
		c$: $author$project$Main$update,
		c2: $author$project$Main$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(0))(0)}});}(this));