var Elm = Elm || { Native: {} };
Elm.Native.Basics = {};
Elm.Native.Basics.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Basics = localRuntime.Native.Basics || {};
	if (localRuntime.Native.Basics.values)
	{
		return localRuntime.Native.Basics.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	function div(a, b)
	{
		return (a / b) | 0;
	}
	function rem(a, b)
	{
		return a % b;
	}
	function mod(a, b)
	{
		if (b === 0)
		{
			throw new Error('Cannot perform mod 0. Division by zero error.');
		}
		var r = a % b;
		var m = a === 0 ? 0 : (b > 0 ? (a >= 0 ? r : r + b) : -mod(-a, -b));

		return m === b ? 0 : m;
	}
	function logBase(base, n)
	{
		return Math.log(n) / Math.log(base);
	}
	function negate(n)
	{
		return -n;
	}
	function abs(n)
	{
		return n < 0 ? -n : n;
	}

	function min(a, b)
	{
		return Utils.cmp(a, b) < 0 ? a : b;
	}
	function max(a, b)
	{
		return Utils.cmp(a, b) > 0 ? a : b;
	}
	function clamp(lo, hi, n)
	{
		return Utils.cmp(n, lo) < 0 ? lo : Utils.cmp(n, hi) > 0 ? hi : n;
	}

	function xor(a, b)
	{
		return a !== b;
	}
	function not(b)
	{
		return !b;
	}
	function isInfinite(n)
	{
		return n === Infinity || n === -Infinity;
	}

	function truncate(n)
	{
		return n | 0;
	}

	function degrees(d)
	{
		return d * Math.PI / 180;
	}
	function turns(t)
	{
		return 2 * Math.PI * t;
	}
	function fromPolar(point)
	{
		var r = point._0;
		var t = point._1;
		return Utils.Tuple2(r * Math.cos(t), r * Math.sin(t));
	}
	function toPolar(point)
	{
		var x = point._0;
		var y = point._1;
		return Utils.Tuple2(Math.sqrt(x * x + y * y), Math.atan2(y, x));
	}

	return localRuntime.Native.Basics.values = {
		div: F2(div),
		rem: F2(rem),
		mod: F2(mod),

		pi: Math.PI,
		e: Math.E,
		cos: Math.cos,
		sin: Math.sin,
		tan: Math.tan,
		acos: Math.acos,
		asin: Math.asin,
		atan: Math.atan,
		atan2: F2(Math.atan2),

		degrees: degrees,
		turns: turns,
		fromPolar: fromPolar,
		toPolar: toPolar,

		sqrt: Math.sqrt,
		logBase: F2(logBase),
		negate: negate,
		abs: abs,
		min: F2(min),
		max: F2(max),
		clamp: F3(clamp),
		compare: Utils.compare,

		xor: F2(xor),
		not: not,

		truncate: truncate,
		ceiling: Math.ceil,
		floor: Math.floor,
		round: Math.round,
		toFloat: function(x) { return x; },
		isNaN: isNaN,
		isInfinite: isInfinite
	};
};

Elm.Native.Port = {};

Elm.Native.Port.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Port = localRuntime.Native.Port || {};
	if (localRuntime.Native.Port.values)
	{
		return localRuntime.Native.Port.values;
	}

	var NS;

	// INBOUND

	function inbound(name, type, converter)
	{
		if (!localRuntime.argsTracker[name])
		{
			throw new Error(
				'Port Error:\n' +
				'No argument was given for the port named \'' + name + '\' with type:\n\n' +
				'    ' + type.split('\n').join('\n        ') + '\n\n' +
				'You need to provide an initial value!\n\n' +
				'Find out more about ports here <http://elm-lang.org/learn/Ports.elm>'
			);
		}
		var arg = localRuntime.argsTracker[name];
		arg.used = true;

		return jsToElm(name, type, converter, arg.value);
	}


	function inboundSignal(name, type, converter)
	{
		var initialValue = inbound(name, type, converter);

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		var signal = NS.input('inbound-port-' + name, initialValue);

		function send(jsValue)
		{
			var elmValue = jsToElm(name, type, converter, jsValue);
			setTimeout(function() {
				localRuntime.notify(signal.id, elmValue);
			}, 0);
		}

		localRuntime.ports[name] = { send: send };

		return signal;
	}


	function jsToElm(name, type, converter, value)
	{
		try
		{
			return converter(value);
		}
		catch(e)
		{
			throw new Error(
				'Port Error:\n' +
				'Regarding the port named \'' + name + '\' with type:\n\n' +
				'    ' + type.split('\n').join('\n        ') + '\n\n' +
				'You just sent the value:\n\n' +
				'    ' + JSON.stringify(value) + '\n\n' +
				'but it cannot be converted to the necessary type.\n' +
				e.message
			);
		}
	}


	// OUTBOUND

	function outbound(name, converter, elmValue)
	{
		localRuntime.ports[name] = converter(elmValue);
	}


	function outboundSignal(name, converter, signal)
	{
		var subscribers = [];

		function subscribe(handler)
		{
			subscribers.push(handler);
		}
		function unsubscribe(handler)
		{
			subscribers.pop(subscribers.indexOf(handler));
		}

		function notify(elmValue)
		{
			var jsValue = converter(elmValue);
			var len = subscribers.length;
			for (var i = 0; i < len; ++i)
			{
				subscribers[i](jsValue);
			}
		}

		if (!NS)
		{
			NS = Elm.Native.Signal.make(localRuntime);
		}
		NS.output('outbound-port-' + name, notify, signal);

		localRuntime.ports[name] = {
			subscribe: subscribe,
			unsubscribe: unsubscribe
		};

		return signal;
	}


	return localRuntime.Native.Port.values = {
		inbound: inbound,
		outbound: outbound,
		inboundSignal: inboundSignal,
		outboundSignal: outboundSignal
	};
};

if (!Elm.fullscreen) {
	(function() {
		'use strict';

		var Display = {
			FULLSCREEN: 0,
			COMPONENT: 1,
			NONE: 2
		};

		Elm.fullscreen = function(module, args)
		{
			var container = document.createElement('div');
			document.body.appendChild(container);
			return init(Display.FULLSCREEN, container, module, args || {});
		};

		Elm.embed = function(module, container, args)
		{
			var tag = container.tagName;
			if (tag !== 'DIV')
			{
				throw new Error('Elm.node must be given a DIV, not a ' + tag + '.');
			}
			return init(Display.COMPONENT, container, module, args || {});
		};

		Elm.worker = function(module, args)
		{
			return init(Display.NONE, {}, module, args || {});
		};

		function init(display, container, module, args, moduleToReplace)
		{
			// defining state needed for an instance of the Elm RTS
			var inputs = [];

			/* OFFSET
			 * Elm's time traveling debugger lets you pause time. This means
			 * "now" may be shifted a bit into the past. By wrapping Date.now()
			 * we can manage this.
			 */
			var timer = {
				programStart: Date.now(),
				now: function()
				{
					return Date.now();
				}
			};

			var updateInProgress = false;
			function notify(id, v)
			{
				if (updateInProgress)
				{
					throw new Error(
						'The notify function has been called synchronously!\n' +
						'This can lead to frames being dropped.\n' +
						'Definitely report this to <https://github.com/elm-lang/Elm/issues>\n');
				}
				updateInProgress = true;
				var timestep = timer.now();
				for (var i = inputs.length; i--; )
				{
					inputs[i].notify(timestep, id, v);
				}
				updateInProgress = false;
			}
			function setTimeout(func, delay)
			{
				return window.setTimeout(func, delay);
			}

			var listeners = [];
			function addListener(relevantInputs, domNode, eventName, func)
			{
				domNode.addEventListener(eventName, func);
				var listener = {
					relevantInputs: relevantInputs,
					domNode: domNode,
					eventName: eventName,
					func: func
				};
				listeners.push(listener);
			}

			var argsTracker = {};
			for (var name in args)
			{
				argsTracker[name] = {
					value: args[name],
					used: false
				};
			}

			// create the actual RTS. Any impure modules will attach themselves to this
			// object. This permits many Elm programs to be embedded per document.
			var elm = {
				notify: notify,
				setTimeout: setTimeout,
				node: container,
				addListener: addListener,
				inputs: inputs,
				timer: timer,
				argsTracker: argsTracker,
				ports: {},

				isFullscreen: function() { return display === Display.FULLSCREEN; },
				isEmbed: function() { return display === Display.COMPONENT; },
				isWorker: function() { return display === Display.NONE; }
			};

			function swap(newModule)
			{
				removeListeners(listeners);
				var div = document.createElement('div');
				var newElm = init(display, div, newModule, args, elm);
				inputs = [];

				return newElm;
			}

			function dispose()
			{
				removeListeners(listeners);
				inputs = [];
			}

			var Module = {};
			try
			{
				Module = module.make(elm);
				checkInputs(elm);
			}
			catch (error)
			{
				if (typeof container.appendChild === "function")
				{
					container.appendChild(errorNode(error.message));
				}
				else
				{
					console.error(error.message);
				}
				throw error;
			}

			if (display !== Display.NONE)
			{
				var graphicsNode = initGraphics(elm, Module);
			}

			var rootNode = { kids: inputs };
			trimDeadNodes(rootNode);
			inputs = rootNode.kids;
			filterListeners(inputs, listeners);

			addReceivers(elm.ports);

			if (typeof moduleToReplace !== 'undefined')
			{
				hotSwap(moduleToReplace, elm);

				// rerender scene if graphics are enabled.
				if (typeof graphicsNode !== 'undefined')
				{
					graphicsNode.notify(0, true, 0);
				}
			}

			return {
				swap: swap,
				ports: elm.ports,
				dispose: dispose
			};
		}

		function checkInputs(elm)
		{
			var argsTracker = elm.argsTracker;
			for (var name in argsTracker)
			{
				if (!argsTracker[name].used)
				{
					throw new Error(
						"Port Error:\nYou provided an argument named '" + name +
						"' but there is no corresponding port!\n\n" +
						"Maybe add a port '" + name + "' to your Elm module?\n" +
						"Maybe remove the '" + name + "' argument from your initialization code in JS?"
					);
				}
			}
		}

		function errorNode(message)
		{
			var code = document.createElement('code');

			var lines = message.split('\n');
			code.appendChild(document.createTextNode(lines[0]));
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createElement('br'));
			for (var i = 1; i < lines.length; ++i)
			{
				code.appendChild(document.createTextNode('\u00A0 \u00A0 ' + lines[i].replace(/  /g, '\u00A0 ')));
				code.appendChild(document.createElement('br'));
			}
			code.appendChild(document.createElement('br'));
			code.appendChild(document.createTextNode('Open the developer console for more details.'));
			return code;
		}


		//// FILTER SIGNALS ////

		// TODO: move this code into the signal module and create a function
		// Signal.initializeGraph that actually instantiates everything.

		function filterListeners(inputs, listeners)
		{
			loop:
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				for (var j = inputs.length; j--; )
				{
					if (listener.relevantInputs.indexOf(inputs[j].id) >= 0)
					{
						continue loop;
					}
				}
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		function removeListeners(listeners)
		{
			for (var i = listeners.length; i--; )
			{
				var listener = listeners[i];
				listener.domNode.removeEventListener(listener.eventName, listener.func);
			}
		}

		// add receivers for built-in ports if they are defined
		function addReceivers(ports)
		{
			if ('title' in ports)
			{
				if (typeof ports.title === 'string')
				{
					document.title = ports.title;
				}
				else
				{
					ports.title.subscribe(function(v) { document.title = v; });
				}
			}
			if ('redirect' in ports)
			{
				ports.redirect.subscribe(function(v) {
					if (v.length > 0)
					{
						window.location = v;
					}
				});
			}
		}


		// returns a boolean representing whether the node is alive or not.
		function trimDeadNodes(node)
		{
			if (node.isOutput)
			{
				return true;
			}

			var liveKids = [];
			for (var i = node.kids.length; i--; )
			{
				var kid = node.kids[i];
				if (trimDeadNodes(kid))
				{
					liveKids.push(kid);
				}
			}
			node.kids = liveKids;

			return liveKids.length > 0;
		}


		////  RENDERING  ////

		function initGraphics(elm, Module)
		{
			if (!('main' in Module))
			{
				throw new Error("'main' is missing! What do I display?!");
			}

			var signalGraph = Module.main;

			// make sure the signal graph is actually a signal & extract the visual model
			if (!('notify' in signalGraph))
			{
				signalGraph = Elm.Signal.make(elm).constant(signalGraph);
			}
			var initialScene = signalGraph.value;

			// Figure out what the render functions should be
			var render;
			var update;
			if (initialScene.ctor === 'Element_elm_builtin')
			{
				var Element = Elm.Native.Graphics.Element.make(elm);
				render = Element.render;
				update = Element.updateAndReplace;
			}
			else
			{
				var VirtualDom = Elm.Native.VirtualDom.make(elm);
				render = VirtualDom.render;
				update = VirtualDom.updateAndReplace;
			}

			// Add the initialScene to the DOM
			var container = elm.node;
			var node = render(initialScene);
			while (container.firstChild)
			{
				container.removeChild(container.firstChild);
			}
			container.appendChild(node);

			var _requestAnimationFrame =
				typeof requestAnimationFrame !== 'undefined'
					? requestAnimationFrame
					: function(cb) { setTimeout(cb, 1000 / 60); }
					;

			// domUpdate is called whenever the main Signal changes.
			//
			// domUpdate and drawCallback implement a small state machine in order
			// to schedule only 1 draw per animation frame. This enforces that
			// once draw has been called, it will not be called again until the
			// next frame.
			//
			// drawCallback is scheduled whenever
			// 1. The state transitions from PENDING_REQUEST to EXTRA_REQUEST, or
			// 2. The state transitions from NO_REQUEST to PENDING_REQUEST
			//
			// Invariants:
			// 1. In the NO_REQUEST state, there is never a scheduled drawCallback.
			// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly 1
			//    scheduled drawCallback.
			var NO_REQUEST = 0;
			var PENDING_REQUEST = 1;
			var EXTRA_REQUEST = 2;
			var state = NO_REQUEST;
			var savedScene = initialScene;
			var scheduledScene = initialScene;

			function domUpdate(newScene)
			{
				scheduledScene = newScene;

				switch (state)
				{
					case NO_REQUEST:
						_requestAnimationFrame(drawCallback);
						state = PENDING_REQUEST;
						return;
					case PENDING_REQUEST:
						state = PENDING_REQUEST;
						return;
					case EXTRA_REQUEST:
						state = PENDING_REQUEST;
						return;
				}
			}

			function drawCallback()
			{
				switch (state)
				{
					case NO_REQUEST:
						// This state should not be possible. How can there be no
						// request, yet somehow we are actively fulfilling a
						// request?
						throw new Error(
							'Unexpected draw callback.\n' +
							'Please report this to <https://github.com/elm-lang/core/issues>.'
						);

					case PENDING_REQUEST:
						// At this point, we do not *know* that another frame is
						// needed, but we make an extra request to rAF just in
						// case. It's possible to drop a frame if rAF is called
						// too late, so we just do it preemptively.
						_requestAnimationFrame(drawCallback);
						state = EXTRA_REQUEST;

						// There's also stuff we definitely need to draw.
						draw();
						return;

					case EXTRA_REQUEST:
						// Turns out the extra request was not needed, so we will
						// stop calling rAF. No reason to call it all the time if
						// no one needs it.
						state = NO_REQUEST;
						return;
				}
			}

			function draw()
			{
				update(elm.node.firstChild, savedScene, scheduledScene);
				if (elm.Native.Window)
				{
					elm.Native.Window.values.resizeIfNeeded();
				}
				savedScene = scheduledScene;
			}

			var renderer = Elm.Native.Signal.make(elm).output('main', domUpdate, signalGraph);

			// must check for resize after 'renderer' is created so
			// that changes show up.
			if (elm.Native.Window)
			{
				elm.Native.Window.values.resizeIfNeeded();
			}

			return renderer;
		}

		//// HOT SWAPPING ////

		// Returns boolean indicating if the swap was successful.
		// Requires that the two signal graphs have exactly the same
		// structure.
		function hotSwap(from, to)
		{
			function similar(nodeOld, nodeNew)
			{
				if (nodeOld.id !== nodeNew.id)
				{
					return false;
				}
				if (nodeOld.isOutput)
				{
					return nodeNew.isOutput;
				}
				return nodeOld.kids.length === nodeNew.kids.length;
			}
			function swap(nodeOld, nodeNew)
			{
				nodeNew.value = nodeOld.value;
				return true;
			}
			var canSwap = depthFirstTraversals(similar, from.inputs, to.inputs);
			if (canSwap)
			{
				depthFirstTraversals(swap, from.inputs, to.inputs);
			}
			from.node.parentNode.replaceChild(to.node, from.node);

			return canSwap;
		}

		// Returns false if the node operation f ever fails.
		function depthFirstTraversals(f, queueOld, queueNew)
		{
			if (queueOld.length !== queueNew.length)
			{
				return false;
			}
			queueOld = queueOld.slice(0);
			queueNew = queueNew.slice(0);

			var seen = [];
			while (queueOld.length > 0 && queueNew.length > 0)
			{
				var nodeOld = queueOld.pop();
				var nodeNew = queueNew.pop();
				if (seen.indexOf(nodeOld.id) < 0)
				{
					if (!f(nodeOld, nodeNew))
					{
						return false;
					}
					queueOld = queueOld.concat(nodeOld.kids || []);
					queueNew = queueNew.concat(nodeNew.kids || []);
					seen.push(nodeOld.id);
				}
			}
			return true;
		}
	}());

	function F2(fun)
	{
		function wrapper(a) { return function(b) { return fun(a,b); }; }
		wrapper.arity = 2;
		wrapper.func = fun;
		return wrapper;
	}

	function F3(fun)
	{
		function wrapper(a) {
			return function(b) { return function(c) { return fun(a, b, c); }; };
		}
		wrapper.arity = 3;
		wrapper.func = fun;
		return wrapper;
	}

	function F4(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return fun(a, b, c, d); }; }; };
		}
		wrapper.arity = 4;
		wrapper.func = fun;
		return wrapper;
	}

	function F5(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
		}
		wrapper.arity = 5;
		wrapper.func = fun;
		return wrapper;
	}

	function F6(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return fun(a, b, c, d, e, f); }; }; }; }; };
		}
		wrapper.arity = 6;
		wrapper.func = fun;
		return wrapper;
	}

	function F7(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
		}
		wrapper.arity = 7;
		wrapper.func = fun;
		return wrapper;
	}

	function F8(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) {
			return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
		}
		wrapper.arity = 8;
		wrapper.func = fun;
		return wrapper;
	}

	function F9(fun)
	{
		function wrapper(a) { return function(b) { return function(c) {
			return function(d) { return function(e) { return function(f) {
			return function(g) { return function(h) { return function(i) {
			return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
		}
		wrapper.arity = 9;
		wrapper.func = fun;
		return wrapper;
	}

	function A2(fun, a, b)
	{
		return fun.arity === 2
			? fun.func(a, b)
			: fun(a)(b);
	}
	function A3(fun, a, b, c)
	{
		return fun.arity === 3
			? fun.func(a, b, c)
			: fun(a)(b)(c);
	}
	function A4(fun, a, b, c, d)
	{
		return fun.arity === 4
			? fun.func(a, b, c, d)
			: fun(a)(b)(c)(d);
	}
	function A5(fun, a, b, c, d, e)
	{
		return fun.arity === 5
			? fun.func(a, b, c, d, e)
			: fun(a)(b)(c)(d)(e);
	}
	function A6(fun, a, b, c, d, e, f)
	{
		return fun.arity === 6
			? fun.func(a, b, c, d, e, f)
			: fun(a)(b)(c)(d)(e)(f);
	}
	function A7(fun, a, b, c, d, e, f, g)
	{
		return fun.arity === 7
			? fun.func(a, b, c, d, e, f, g)
			: fun(a)(b)(c)(d)(e)(f)(g);
	}
	function A8(fun, a, b, c, d, e, f, g, h)
	{
		return fun.arity === 8
			? fun.func(a, b, c, d, e, f, g, h)
			: fun(a)(b)(c)(d)(e)(f)(g)(h);
	}
	function A9(fun, a, b, c, d, e, f, g, h, i)
	{
		return fun.arity === 9
			? fun.func(a, b, c, d, e, f, g, h, i)
			: fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
	}
}

Elm.Native = Elm.Native || {};
Elm.Native.Utils = {};
Elm.Native.Utils.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Utils = localRuntime.Native.Utils || {};
	if (localRuntime.Native.Utils.values)
	{
		return localRuntime.Native.Utils.values;
	}


	// COMPARISONS

	function eq(l, r)
	{
		var stack = [{'x': l, 'y': r}];
		while (stack.length > 0)
		{
			var front = stack.pop();
			var x = front.x;
			var y = front.y;
			if (x === y)
			{
				continue;
			}
			if (typeof x === 'object')
			{
				var c = 0;
				for (var i in x)
				{
					++c;
					if (i in y)
					{
						if (i !== 'ctor')
						{
							stack.push({ 'x': x[i], 'y': y[i] });
						}
					}
					else
					{
						return false;
					}
				}
				if ('ctor' in x)
				{
					stack.push({'x': x.ctor, 'y': y.ctor});
				}
				if (c !== Object.keys(y).length)
				{
					return false;
				}
			}
			else if (typeof x === 'function')
			{
				throw new Error('Equality error: general function equality is ' +
								'undecidable, and therefore, unsupported');
			}
			else
			{
				return false;
			}
		}
		return true;
	}

	// code in Generate/JavaScript.hs depends on the particular
	// integer values assigned to LT, EQ, and GT
	var LT = -1, EQ = 0, GT = 1, ord = ['LT', 'EQ', 'GT'];

	function compare(x, y)
	{
		return {
			ctor: ord[cmp(x, y) + 1]
		};
	}

	function cmp(x, y) {
		var ord;
		if (typeof x !== 'object')
		{
			return x === y ? EQ : x < y ? LT : GT;
		}
		else if (x.isChar)
		{
			var a = x.toString();
			var b = y.toString();
			return a === b
				? EQ
				: a < b
					? LT
					: GT;
		}
		else if (x.ctor === '::' || x.ctor === '[]')
		{
			while (true)
			{
				if (x.ctor === '[]' && y.ctor === '[]')
				{
					return EQ;
				}
				if (x.ctor !== y.ctor)
				{
					return x.ctor === '[]' ? LT : GT;
				}
				ord = cmp(x._0, y._0);
				if (ord !== EQ)
				{
					return ord;
				}
				x = x._1;
				y = y._1;
			}
		}
		else if (x.ctor.slice(0, 6) === '_Tuple')
		{
			var n = x.ctor.slice(6) - 0;
			var err = 'cannot compare tuples with more than 6 elements.';
			if (n === 0) return EQ;
			if (n >= 1) { ord = cmp(x._0, y._0); if (ord !== EQ) return ord;
			if (n >= 2) { ord = cmp(x._1, y._1); if (ord !== EQ) return ord;
			if (n >= 3) { ord = cmp(x._2, y._2); if (ord !== EQ) return ord;
			if (n >= 4) { ord = cmp(x._3, y._3); if (ord !== EQ) return ord;
			if (n >= 5) { ord = cmp(x._4, y._4); if (ord !== EQ) return ord;
			if (n >= 6) { ord = cmp(x._5, y._5); if (ord !== EQ) return ord;
			if (n >= 7) throw new Error('Comparison error: ' + err); } } } } } }
			return EQ;
		}
		else
		{
			throw new Error('Comparison error: comparison is only defined on ints, ' +
							'floats, times, chars, strings, lists of comparable values, ' +
							'and tuples of comparable values.');
		}
	}


	// TUPLES

	var Tuple0 = {
		ctor: '_Tuple0'
	};

	function Tuple2(x, y)
	{
		return {
			ctor: '_Tuple2',
			_0: x,
			_1: y
		};
	}


	// LITERALS

	function chr(c)
	{
		var x = new String(c);
		x.isChar = true;
		return x;
	}

	function txt(str)
	{
		var t = new String(str);
		t.text = true;
		return t;
	}


	// GUID

	var count = 0;
	function guid(_)
	{
		return count++;
	}


	// RECORDS

	function update(oldRecord, updatedFields)
	{
		var newRecord = {};
		for (var key in oldRecord)
		{
			var value = (key in updatedFields) ? updatedFields[key] : oldRecord[key];
			newRecord[key] = value;
		}
		return newRecord;
	}


	// MOUSE COORDINATES

	function getXY(e)
	{
		var posx = 0;
		var posy = 0;
		if (e.pageX || e.pageY)
		{
			posx = e.pageX;
			posy = e.pageY;
		}
		else if (e.clientX || e.clientY)
		{
			posx = e.clientX + document.body.scrollLeft + document.documentElement.scrollLeft;
			posy = e.clientY + document.body.scrollTop + document.documentElement.scrollTop;
		}

		if (localRuntime.isEmbed())
		{
			var rect = localRuntime.node.getBoundingClientRect();
			var relx = rect.left + document.body.scrollLeft + document.documentElement.scrollLeft;
			var rely = rect.top + document.body.scrollTop + document.documentElement.scrollTop;
			// TODO: figure out if there is a way to avoid rounding here
			posx = posx - Math.round(relx) - localRuntime.node.clientLeft;
			posy = posy - Math.round(rely) - localRuntime.node.clientTop;
		}
		return Tuple2(posx, posy);
	}


	//// LIST STUFF ////

	var Nil = { ctor: '[]' };

	function Cons(hd, tl)
	{
		return {
			ctor: '::',
			_0: hd,
			_1: tl
		};
	}

	function list(arr)
	{
		var out = Nil;
		for (var i = arr.length; i--; )
		{
			out = Cons(arr[i], out);
		}
		return out;
	}

	function range(lo, hi)
	{
		var list = Nil;
		if (lo <= hi)
		{
			do
			{
				list = Cons(hi, list);
			}
			while (hi-- > lo);
		}
		return list;
	}

	function append(xs, ys)
	{
		// append Strings
		if (typeof xs === 'string')
		{
			return xs + ys;
		}

		// append Text
		if (xs.ctor.slice(0, 5) === 'Text:')
		{
			return {
				ctor: 'Text:Append',
				_0: xs,
				_1: ys
			};
		}


		// append Lists
		if (xs.ctor === '[]')
		{
			return ys;
		}
		var root = Cons(xs._0, Nil);
		var curr = root;
		xs = xs._1;
		while (xs.ctor !== '[]')
		{
			curr._1 = Cons(xs._0, Nil);
			xs = xs._1;
			curr = curr._1;
		}
		curr._1 = ys;
		return root;
	}


	// CRASHES

	function crash(moduleName, region)
	{
		return function(message) {
			throw new Error(
				'Ran into a `Debug.crash` in module `' + moduleName + '` ' + regionToString(region) + '\n'
				+ 'The message provided by the code author is:\n\n    '
				+ message
			);
		};
	}

	function crashCase(moduleName, region, value)
	{
		return function(message) {
			throw new Error(
				'Ran into a `Debug.crash` in module `' + moduleName + '`\n\n'
				+ 'This was caused by the `case` expression ' + regionToString(region) + '.\n'
				+ 'One of the branches ended with a crash and the following value got through:\n\n    ' + toString(value) + '\n\n'
				+ 'The message provided by the code author is:\n\n    '
				+ message
			);
		};
	}

	function regionToString(region)
	{
		if (region.start.line == region.end.line)
		{
			return 'on line ' + region.start.line;
		}
		return 'between lines ' + region.start.line + ' and ' + region.end.line;
	}


	// BAD PORTS

	function badPort(expected, received)
	{
		throw new Error(
			'Runtime error when sending values through a port.\n\n'
			+ 'Expecting ' + expected + ' but was given ' + formatValue(received)
		);
	}

	function formatValue(value)
	{
		// Explicity format undefined values as "undefined"
		// because JSON.stringify(undefined) unhelpfully returns ""
		return (value === undefined) ? "undefined" : JSON.stringify(value);
	}


	// TO STRING

	var _Array;
	var Dict;
	var List;

	var toString = function(v)
	{
		var type = typeof v;
		if (type === 'function')
		{
			var name = v.func ? v.func.name : v.name;
			return '<function' + (name === '' ? '' : ': ') + name + '>';
		}
		else if (type === 'boolean')
		{
			return v ? 'True' : 'False';
		}
		else if (type === 'number')
		{
			return v + '';
		}
		else if ((v instanceof String) && v.isChar)
		{
			return '\'' + addSlashes(v, true) + '\'';
		}
		else if (type === 'string')
		{
			return '"' + addSlashes(v, false) + '"';
		}
		else if (type === 'object' && 'ctor' in v)
		{
			if (v.ctor.substring(0, 6) === '_Tuple')
			{
				var output = [];
				for (var k in v)
				{
					if (k === 'ctor') continue;
					output.push(toString(v[k]));
				}
				return '(' + output.join(',') + ')';
			}
			else if (v.ctor === '_Array')
			{
				if (!_Array)
				{
					_Array = Elm.Array.make(localRuntime);
				}
				var list = _Array.toList(v);
				return 'Array.fromList ' + toString(list);
			}
			else if (v.ctor === '::')
			{
				var output = '[' + toString(v._0);
				v = v._1;
				while (v.ctor === '::')
				{
					output += ',' + toString(v._0);
					v = v._1;
				}
				return output + ']';
			}
			else if (v.ctor === '[]')
			{
				return '[]';
			}
			else if (v.ctor === 'RBNode_elm_builtin' || v.ctor === 'RBEmpty_elm_builtin' || v.ctor === 'Set_elm_builtin')
			{
				if (!Dict)
				{
					Dict = Elm.Dict.make(localRuntime);
				}
				var list;
				var name;
				if (v.ctor === 'Set_elm_builtin')
				{
					if (!List)
					{
						List = Elm.List.make(localRuntime);
					}
					name = 'Set';
					list = A2(List.map, function(x) {return x._0; }, Dict.toList(v._0));
				}
				else
				{
					name = 'Dict';
					list = Dict.toList(v);
				}
				return name + '.fromList ' + toString(list);
			}
			else if (v.ctor.slice(0, 5) === 'Text:')
			{
				return '<text>';
			}
			else if (v.ctor === 'Element_elm_builtin')
			{
				return '<element>'
			}
			else if (v.ctor === 'Form_elm_builtin')
			{
				return '<form>'
			}
			else
			{
				var output = '';
				for (var i in v)
				{
					if (i === 'ctor') continue;
					var str = toString(v[i]);
					var parenless = str[0] === '{' || str[0] === '<' || str.indexOf(' ') < 0;
					output += ' ' + (parenless ? str : '(' + str + ')');
				}
				return v.ctor + output;
			}
		}
		else if (type === 'object' && 'notify' in v && 'id' in v)
		{
			return '<signal>';
		}
		else if (type === 'object')
		{
			var output = [];
			for (var k in v)
			{
				output.push(k + ' = ' + toString(v[k]));
			}
			if (output.length === 0)
			{
				return '{}';
			}
			return '{ ' + output.join(', ') + ' }';
		}
		return '<internal structure>';
	};

	function addSlashes(str, isChar)
	{
		var s = str.replace(/\\/g, '\\\\')
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


	return localRuntime.Native.Utils.values = {
		eq: eq,
		cmp: cmp,
		compare: F2(compare),
		Tuple0: Tuple0,
		Tuple2: Tuple2,
		chr: chr,
		txt: txt,
		update: update,
		guid: guid,
		getXY: getXY,

		Nil: Nil,
		Cons: Cons,
		list: list,
		range: range,
		append: F2(append),

		crash: crash,
		crashCase: crashCase,
		badPort: badPort,

		toString: toString
	};
};

Elm.Basics = Elm.Basics || {};
Elm.Basics.make = function (_elm) {
   "use strict";
   _elm.Basics = _elm.Basics || {};
   if (_elm.Basics.values) return _elm.Basics.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Native$Basics = Elm.Native.Basics.make(_elm),
   $Native$Utils = Elm.Native.Utils.make(_elm);
   var _op = {};
   var uncurry = F2(function (f,_p0) {
      var _p1 = _p0;
      return A2(f,_p1._0,_p1._1);
   });
   var curry = F3(function (f,a,b) {
      return f({ctor: "_Tuple2",_0: a,_1: b});
   });
   var flip = F3(function (f,b,a) {    return A2(f,a,b);});
   var snd = function (_p2) {    var _p3 = _p2;return _p3._1;};
   var fst = function (_p4) {    var _p5 = _p4;return _p5._0;};
   var always = F2(function (a,_p6) {    return a;});
   var identity = function (x) {    return x;};
   _op["<|"] = F2(function (f,x) {    return f(x);});
   _op["|>"] = F2(function (x,f) {    return f(x);});
   _op[">>"] = F3(function (f,g,x) {    return g(f(x));});
   _op["<<"] = F3(function (g,f,x) {    return g(f(x));});
   _op["++"] = $Native$Utils.append;
   var toString = $Native$Utils.toString;
   var isInfinite = $Native$Basics.isInfinite;
   var isNaN = $Native$Basics.isNaN;
   var toFloat = $Native$Basics.toFloat;
   var ceiling = $Native$Basics.ceiling;
   var floor = $Native$Basics.floor;
   var truncate = $Native$Basics.truncate;
   var round = $Native$Basics.round;
   var not = $Native$Basics.not;
   var xor = $Native$Basics.xor;
   _op["||"] = $Native$Basics.or;
   _op["&&"] = $Native$Basics.and;
   var max = $Native$Basics.max;
   var min = $Native$Basics.min;
   var GT = {ctor: "GT"};
   var EQ = {ctor: "EQ"};
   var LT = {ctor: "LT"};
   var compare = $Native$Basics.compare;
   _op[">="] = $Native$Basics.ge;
   _op["<="] = $Native$Basics.le;
   _op[">"] = $Native$Basics.gt;
   _op["<"] = $Native$Basics.lt;
   _op["/="] = $Native$Basics.neq;
   _op["=="] = $Native$Basics.eq;
   var e = $Native$Basics.e;
   var pi = $Native$Basics.pi;
   var clamp = $Native$Basics.clamp;
   var logBase = $Native$Basics.logBase;
   var abs = $Native$Basics.abs;
   var negate = $Native$Basics.negate;
   var sqrt = $Native$Basics.sqrt;
   var atan2 = $Native$Basics.atan2;
   var atan = $Native$Basics.atan;
   var asin = $Native$Basics.asin;
   var acos = $Native$Basics.acos;
   var tan = $Native$Basics.tan;
   var sin = $Native$Basics.sin;
   var cos = $Native$Basics.cos;
   _op["^"] = $Native$Basics.exp;
   _op["%"] = $Native$Basics.mod;
   var rem = $Native$Basics.rem;
   _op["//"] = $Native$Basics.div;
   _op["/"] = $Native$Basics.floatDiv;
   _op["*"] = $Native$Basics.mul;
   _op["-"] = $Native$Basics.sub;
   _op["+"] = $Native$Basics.add;
   var toPolar = $Native$Basics.toPolar;
   var fromPolar = $Native$Basics.fromPolar;
   var turns = $Native$Basics.turns;
   var degrees = $Native$Basics.degrees;
   var radians = function (t) {    return t;};
   return _elm.Basics.values = {_op: _op
                               ,max: max
                               ,min: min
                               ,compare: compare
                               ,not: not
                               ,xor: xor
                               ,rem: rem
                               ,negate: negate
                               ,abs: abs
                               ,sqrt: sqrt
                               ,clamp: clamp
                               ,logBase: logBase
                               ,e: e
                               ,pi: pi
                               ,cos: cos
                               ,sin: sin
                               ,tan: tan
                               ,acos: acos
                               ,asin: asin
                               ,atan: atan
                               ,atan2: atan2
                               ,round: round
                               ,floor: floor
                               ,ceiling: ceiling
                               ,truncate: truncate
                               ,toFloat: toFloat
                               ,degrees: degrees
                               ,radians: radians
                               ,turns: turns
                               ,toPolar: toPolar
                               ,fromPolar: fromPolar
                               ,isNaN: isNaN
                               ,isInfinite: isInfinite
                               ,toString: toString
                               ,fst: fst
                               ,snd: snd
                               ,identity: identity
                               ,always: always
                               ,flip: flip
                               ,curry: curry
                               ,uncurry: uncurry
                               ,LT: LT
                               ,EQ: EQ
                               ,GT: GT};
};
Elm.Maybe = Elm.Maybe || {};
Elm.Maybe.make = function (_elm) {
   "use strict";
   _elm.Maybe = _elm.Maybe || {};
   if (_elm.Maybe.values) return _elm.Maybe.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var withDefault = F2(function ($default,maybe) {
      var _p0 = maybe;
      if (_p0.ctor === "Just") {
            return _p0._0;
         } else {
            return $default;
         }
   });
   var Nothing = {ctor: "Nothing"};
   var oneOf = function (maybes) {
      oneOf: while (true) {
         var _p1 = maybes;
         if (_p1.ctor === "[]") {
               return Nothing;
            } else {
               var _p3 = _p1._0;
               var _p2 = _p3;
               if (_p2.ctor === "Nothing") {
                     var _v3 = _p1._1;
                     maybes = _v3;
                     continue oneOf;
                  } else {
                     return _p3;
                  }
            }
      }
   };
   var andThen = F2(function (maybeValue,callback) {
      var _p4 = maybeValue;
      if (_p4.ctor === "Just") {
            return callback(_p4._0);
         } else {
            return Nothing;
         }
   });
   var Just = function (a) {    return {ctor: "Just",_0: a};};
   var map = F2(function (f,maybe) {
      var _p5 = maybe;
      if (_p5.ctor === "Just") {
            return Just(f(_p5._0));
         } else {
            return Nothing;
         }
   });
   var map2 = F3(function (func,ma,mb) {
      var _p6 = {ctor: "_Tuple2",_0: ma,_1: mb};
      if (_p6.ctor === "_Tuple2" && _p6._0.ctor === "Just" && _p6._1.ctor === "Just")
      {
            return Just(A2(func,_p6._0._0,_p6._1._0));
         } else {
            return Nothing;
         }
   });
   var map3 = F4(function (func,ma,mb,mc) {
      var _p7 = {ctor: "_Tuple3",_0: ma,_1: mb,_2: mc};
      if (_p7.ctor === "_Tuple3" && _p7._0.ctor === "Just" && _p7._1.ctor === "Just" && _p7._2.ctor === "Just")
      {
            return Just(A3(func,_p7._0._0,_p7._1._0,_p7._2._0));
         } else {
            return Nothing;
         }
   });
   var map4 = F5(function (func,ma,mb,mc,md) {
      var _p8 = {ctor: "_Tuple4",_0: ma,_1: mb,_2: mc,_3: md};
      if (_p8.ctor === "_Tuple4" && _p8._0.ctor === "Just" && _p8._1.ctor === "Just" && _p8._2.ctor === "Just" && _p8._3.ctor === "Just")
      {
            return Just(A4(func,
            _p8._0._0,
            _p8._1._0,
            _p8._2._0,
            _p8._3._0));
         } else {
            return Nothing;
         }
   });
   var map5 = F6(function (func,ma,mb,mc,md,me) {
      var _p9 = {ctor: "_Tuple5"
                ,_0: ma
                ,_1: mb
                ,_2: mc
                ,_3: md
                ,_4: me};
      if (_p9.ctor === "_Tuple5" && _p9._0.ctor === "Just" && _p9._1.ctor === "Just" && _p9._2.ctor === "Just" && _p9._3.ctor === "Just" && _p9._4.ctor === "Just")
      {
            return Just(A5(func,
            _p9._0._0,
            _p9._1._0,
            _p9._2._0,
            _p9._3._0,
            _p9._4._0));
         } else {
            return Nothing;
         }
   });
   return _elm.Maybe.values = {_op: _op
                              ,andThen: andThen
                              ,map: map
                              ,map2: map2
                              ,map3: map3
                              ,map4: map4
                              ,map5: map5
                              ,withDefault: withDefault
                              ,oneOf: oneOf
                              ,Just: Just
                              ,Nothing: Nothing};
};
Elm.Native.List = {};
Elm.Native.List.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.List = localRuntime.Native.List || {};
	if (localRuntime.Native.List.values)
	{
		return localRuntime.Native.List.values;
	}
	if ('values' in Elm.Native.List)
	{
		return localRuntime.Native.List.values = Elm.Native.List.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	var Nil = Utils.Nil;
	var Cons = Utils.Cons;

	var fromArray = Utils.list;

	function toArray(xs)
	{
		var out = [];
		while (xs.ctor !== '[]')
		{
			out.push(xs._0);
			xs = xs._1;
		}
		return out;
	}

	// f defined similarly for both foldl and foldr (NB: different from Haskell)
	// ie, foldl : (a -> b -> b) -> b -> [a] -> b
	function foldl(f, b, xs)
	{
		var acc = b;
		while (xs.ctor !== '[]')
		{
			acc = A2(f, xs._0, acc);
			xs = xs._1;
		}
		return acc;
	}

	function foldr(f, b, xs)
	{
		var arr = toArray(xs);
		var acc = b;
		for (var i = arr.length; i--; )
		{
			acc = A2(f, arr[i], acc);
		}
		return acc;
	}

	function map2(f, xs, ys)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]')
		{
			arr.push(A2(f, xs._0, ys._0));
			xs = xs._1;
			ys = ys._1;
		}
		return fromArray(arr);
	}

	function map3(f, xs, ys, zs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && ys.ctor !== '[]' && zs.ctor !== '[]')
		{
			arr.push(A3(f, xs._0, ys._0, zs._0));
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map4(f, ws, xs, ys, zs)
	{
		var arr = [];
		while (   ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A4(f, ws._0, xs._0, ys._0, zs._0));
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function map5(f, vs, ws, xs, ys, zs)
	{
		var arr = [];
		while (   vs.ctor !== '[]'
			   && ws.ctor !== '[]'
			   && xs.ctor !== '[]'
			   && ys.ctor !== '[]'
			   && zs.ctor !== '[]')
		{
			arr.push(A5(f, vs._0, ws._0, xs._0, ys._0, zs._0));
			vs = vs._1;
			ws = ws._1;
			xs = xs._1;
			ys = ys._1;
			zs = zs._1;
		}
		return fromArray(arr);
	}

	function sortBy(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a, b) {
			return Utils.cmp(f(a), f(b));
		}));
	}

	function sortWith(f, xs)
	{
		return fromArray(toArray(xs).sort(function(a, b) {
			var ord = f(a)(b).ctor;
			return ord === 'EQ' ? 0 : ord === 'LT' ? -1 : 1;
		}));
	}

	function take(n, xs)
	{
		var arr = [];
		while (xs.ctor !== '[]' && n > 0)
		{
			arr.push(xs._0);
			xs = xs._1;
			--n;
		}
		return fromArray(arr);
	}


	Elm.Native.List.values = {
		Nil: Nil,
		Cons: Cons,
		cons: F2(Cons),
		toArray: toArray,
		fromArray: fromArray,

		foldl: F3(foldl),
		foldr: F3(foldr),

		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		sortBy: F2(sortBy),
		sortWith: F2(sortWith),
		take: F2(take)
	};
	return localRuntime.Native.List.values = Elm.Native.List.values;
};

Elm.List = Elm.List || {};
Elm.List.make = function (_elm) {
   "use strict";
   _elm.List = _elm.List || {};
   if (_elm.List.values) return _elm.List.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$List = Elm.Native.List.make(_elm);
   var _op = {};
   var sortWith = $Native$List.sortWith;
   var sortBy = $Native$List.sortBy;
   var sort = function (xs) {
      return A2(sortBy,$Basics.identity,xs);
   };
   var drop = F2(function (n,list) {
      drop: while (true) if (_U.cmp(n,0) < 1) return list; else {
            var _p0 = list;
            if (_p0.ctor === "[]") {
                  return list;
               } else {
                  var _v1 = n - 1,_v2 = _p0._1;
                  n = _v1;
                  list = _v2;
                  continue drop;
               }
         }
   });
   var take = $Native$List.take;
   var map5 = $Native$List.map5;
   var map4 = $Native$List.map4;
   var map3 = $Native$List.map3;
   var map2 = $Native$List.map2;
   var any = F2(function (isOkay,list) {
      any: while (true) {
         var _p1 = list;
         if (_p1.ctor === "[]") {
               return false;
            } else {
               if (isOkay(_p1._0)) return true; else {
                     var _v4 = isOkay,_v5 = _p1._1;
                     isOkay = _v4;
                     list = _v5;
                     continue any;
                  }
            }
      }
   });
   var all = F2(function (isOkay,list) {
      return $Basics.not(A2(any,
      function (_p2) {
         return $Basics.not(isOkay(_p2));
      },
      list));
   });
   var foldr = $Native$List.foldr;
   var foldl = $Native$List.foldl;
   var length = function (xs) {
      return A3(foldl,
      F2(function (_p3,i) {    return i + 1;}),
      0,
      xs);
   };
   var sum = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {    return x + y;}),
      0,
      numbers);
   };
   var product = function (numbers) {
      return A3(foldl,
      F2(function (x,y) {    return x * y;}),
      1,
      numbers);
   };
   var maximum = function (list) {
      var _p4 = list;
      if (_p4.ctor === "::") {
            return $Maybe.Just(A3(foldl,$Basics.max,_p4._0,_p4._1));
         } else {
            return $Maybe.Nothing;
         }
   };
   var minimum = function (list) {
      var _p5 = list;
      if (_p5.ctor === "::") {
            return $Maybe.Just(A3(foldl,$Basics.min,_p5._0,_p5._1));
         } else {
            return $Maybe.Nothing;
         }
   };
   var indexedMap = F2(function (f,xs) {
      return A3(map2,f,_U.range(0,length(xs) - 1),xs);
   });
   var member = F2(function (x,xs) {
      return A2(any,function (a) {    return _U.eq(a,x);},xs);
   });
   var isEmpty = function (xs) {
      var _p6 = xs;
      if (_p6.ctor === "[]") {
            return true;
         } else {
            return false;
         }
   };
   var tail = function (list) {
      var _p7 = list;
      if (_p7.ctor === "::") {
            return $Maybe.Just(_p7._1);
         } else {
            return $Maybe.Nothing;
         }
   };
   var head = function (list) {
      var _p8 = list;
      if (_p8.ctor === "::") {
            return $Maybe.Just(_p8._0);
         } else {
            return $Maybe.Nothing;
         }
   };
   _op["::"] = $Native$List.cons;
   var map = F2(function (f,xs) {
      return A3(foldr,
      F2(function (x,acc) {    return A2(_op["::"],f(x),acc);}),
      _U.list([]),
      xs);
   });
   var filter = F2(function (pred,xs) {
      var conditionalCons = F2(function (x,xs$) {
         return pred(x) ? A2(_op["::"],x,xs$) : xs$;
      });
      return A3(foldr,conditionalCons,_U.list([]),xs);
   });
   var maybeCons = F3(function (f,mx,xs) {
      var _p9 = f(mx);
      if (_p9.ctor === "Just") {
            return A2(_op["::"],_p9._0,xs);
         } else {
            return xs;
         }
   });
   var filterMap = F2(function (f,xs) {
      return A3(foldr,maybeCons(f),_U.list([]),xs);
   });
   var reverse = function (list) {
      return A3(foldl,
      F2(function (x,y) {    return A2(_op["::"],x,y);}),
      _U.list([]),
      list);
   };
   var scanl = F3(function (f,b,xs) {
      var scan1 = F2(function (x,accAcc) {
         var _p10 = accAcc;
         if (_p10.ctor === "::") {
               return A2(_op["::"],A2(f,x,_p10._0),accAcc);
            } else {
               return _U.list([]);
            }
      });
      return reverse(A3(foldl,scan1,_U.list([b]),xs));
   });
   var append = F2(function (xs,ys) {
      var _p11 = ys;
      if (_p11.ctor === "[]") {
            return xs;
         } else {
            return A3(foldr,
            F2(function (x,y) {    return A2(_op["::"],x,y);}),
            ys,
            xs);
         }
   });
   var concat = function (lists) {
      return A3(foldr,append,_U.list([]),lists);
   };
   var concatMap = F2(function (f,list) {
      return concat(A2(map,f,list));
   });
   var partition = F2(function (pred,list) {
      var step = F2(function (x,_p12) {
         var _p13 = _p12;
         var _p15 = _p13._0;
         var _p14 = _p13._1;
         return pred(x) ? {ctor: "_Tuple2"
                          ,_0: A2(_op["::"],x,_p15)
                          ,_1: _p14} : {ctor: "_Tuple2"
                                       ,_0: _p15
                                       ,_1: A2(_op["::"],x,_p14)};
      });
      return A3(foldr,
      step,
      {ctor: "_Tuple2",_0: _U.list([]),_1: _U.list([])},
      list);
   });
   var unzip = function (pairs) {
      var step = F2(function (_p17,_p16) {
         var _p18 = _p17;
         var _p19 = _p16;
         return {ctor: "_Tuple2"
                ,_0: A2(_op["::"],_p18._0,_p19._0)
                ,_1: A2(_op["::"],_p18._1,_p19._1)};
      });
      return A3(foldr,
      step,
      {ctor: "_Tuple2",_0: _U.list([]),_1: _U.list([])},
      pairs);
   };
   var intersperse = F2(function (sep,xs) {
      var _p20 = xs;
      if (_p20.ctor === "[]") {
            return _U.list([]);
         } else {
            var step = F2(function (x,rest) {
               return A2(_op["::"],sep,A2(_op["::"],x,rest));
            });
            var spersed = A3(foldr,step,_U.list([]),_p20._1);
            return A2(_op["::"],_p20._0,spersed);
         }
   });
   var repeatHelp = F3(function (result,n,value) {
      repeatHelp: while (true) if (_U.cmp(n,0) < 1) return result;
      else {
            var _v18 = A2(_op["::"],value,result),
            _v19 = n - 1,
            _v20 = value;
            result = _v18;
            n = _v19;
            value = _v20;
            continue repeatHelp;
         }
   });
   var repeat = F2(function (n,value) {
      return A3(repeatHelp,_U.list([]),n,value);
   });
   return _elm.List.values = {_op: _op
                             ,isEmpty: isEmpty
                             ,length: length
                             ,reverse: reverse
                             ,member: member
                             ,head: head
                             ,tail: tail
                             ,filter: filter
                             ,take: take
                             ,drop: drop
                             ,repeat: repeat
                             ,append: append
                             ,concat: concat
                             ,intersperse: intersperse
                             ,partition: partition
                             ,unzip: unzip
                             ,map: map
                             ,map2: map2
                             ,map3: map3
                             ,map4: map4
                             ,map5: map5
                             ,filterMap: filterMap
                             ,concatMap: concatMap
                             ,indexedMap: indexedMap
                             ,foldr: foldr
                             ,foldl: foldl
                             ,sum: sum
                             ,product: product
                             ,maximum: maximum
                             ,minimum: minimum
                             ,all: all
                             ,any: any
                             ,scanl: scanl
                             ,sort: sort
                             ,sortBy: sortBy
                             ,sortWith: sortWith};
};
Elm.Native.Transform2D = {};
Elm.Native.Transform2D.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Transform2D = localRuntime.Native.Transform2D || {};
	if (localRuntime.Native.Transform2D.values)
	{
		return localRuntime.Native.Transform2D.values;
	}

	var A;
	if (typeof Float32Array === 'undefined')
	{
		A = function(arr)
		{
			this.length = arr.length;
			this[0] = arr[0];
			this[1] = arr[1];
			this[2] = arr[2];
			this[3] = arr[3];
			this[4] = arr[4];
			this[5] = arr[5];
		};
	}
	else
	{
		A = Float32Array;
	}

	// layout of matrix in an array is
	//
	//   | m11 m12 dx |
	//   | m21 m22 dy |
	//   |  0   0   1 |
	//
	//  new A([ m11, m12, dx, m21, m22, dy ])

	var identity = new A([1, 0, 0, 0, 1, 0]);
	function matrix(m11, m12, m21, m22, dx, dy)
	{
		return new A([m11, m12, dx, m21, m22, dy]);
	}

	function rotation(t)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		return new A([c, -s, 0, s, c, 0]);
	}

	function rotate(t, m)
	{
		var c = Math.cos(t);
		var s = Math.sin(t);
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11 * c + m12 * s, -m11 * s + m12 * c, m[2],
					  m21 * c + m22 * s, -m21 * s + m22 * c, m[5]]);
	}
	/*
	function move(xy,m) {
		var x = xy._0;
		var y = xy._1;
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4];
		return new A([m11, m12, m11*x + m12*y + m[2],
					  m21, m22, m21*x + m22*y + m[5]]);
	}
	function scale(s,m) { return new A([m[0]*s, m[1]*s, m[2], m[3]*s, m[4]*s, m[5]]); }
	function scaleX(x,m) { return new A([m[0]*x, m[1], m[2], m[3]*x, m[4], m[5]]); }
	function scaleY(y,m) { return new A([m[0], m[1]*y, m[2], m[3], m[4]*y, m[5]]); }
	function reflectX(m) { return new A([-m[0], m[1], m[2], -m[3], m[4], m[5]]); }
	function reflectY(m) { return new A([m[0], -m[1], m[2], m[3], -m[4], m[5]]); }

	function transform(m11, m21, m12, m22, mdx, mdy, n) {
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11*n11 + m12*n21,
					  m11*n12 + m12*n22,
					  m11*ndx + m12*ndy + mdx,
					  m21*n11 + m22*n21,
					  m21*n12 + m22*n22,
					  m21*ndx + m22*ndy + mdy]);
	}
	*/
	function multiply(m, n)
	{
		var m11 = m[0], m12 = m[1], m21 = m[3], m22 = m[4], mdx = m[2], mdy = m[5];
		var n11 = n[0], n12 = n[1], n21 = n[3], n22 = n[4], ndx = n[2], ndy = n[5];
		return new A([m11 * n11 + m12 * n21,
					  m11 * n12 + m12 * n22,
					  m11 * ndx + m12 * ndy + mdx,
					  m21 * n11 + m22 * n21,
					  m21 * n12 + m22 * n22,
					  m21 * ndx + m22 * ndy + mdy]);
	}

	return localRuntime.Native.Transform2D.values = {
		identity: identity,
		matrix: F6(matrix),
		rotation: rotation,
		multiply: F2(multiply)
		/*
		transform: F7(transform),
		rotate: F2(rotate),
		move: F2(move),
		scale: F2(scale),
		scaleX: F2(scaleX),
		scaleY: F2(scaleY),
		reflectX: reflectX,
		reflectY: reflectY
		*/
	};
};

Elm.Transform2D = Elm.Transform2D || {};
Elm.Transform2D.make = function (_elm) {
   "use strict";
   _elm.Transform2D = _elm.Transform2D || {};
   if (_elm.Transform2D.values) return _elm.Transform2D.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Native$Transform2D = Elm.Native.Transform2D.make(_elm);
   var _op = {};
   var multiply = $Native$Transform2D.multiply;
   var rotation = $Native$Transform2D.rotation;
   var matrix = $Native$Transform2D.matrix;
   var translation = F2(function (x,y) {
      return A6(matrix,1,0,0,1,x,y);
   });
   var scale = function (s) {    return A6(matrix,s,0,0,s,0,0);};
   var scaleX = function (x) {    return A6(matrix,x,0,0,1,0,0);};
   var scaleY = function (y) {    return A6(matrix,1,0,0,y,0,0);};
   var identity = $Native$Transform2D.identity;
   var Transform2D = {ctor: "Transform2D"};
   return _elm.Transform2D.values = {_op: _op
                                    ,identity: identity
                                    ,matrix: matrix
                                    ,multiply: multiply
                                    ,rotation: rotation
                                    ,translation: translation
                                    ,scale: scale
                                    ,scaleX: scaleX
                                    ,scaleY: scaleY};
};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Collage = Elm.Native.Graphics.Collage || {};

// definition
Elm.Native.Graphics.Collage.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Collage = localRuntime.Native.Graphics.Collage || {};
	if ('values' in localRuntime.Native.Graphics.Collage)
	{
		return localRuntime.Native.Graphics.Collage.values;
	}

	// okay, we cannot short-ciruit, so now we define everything
	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var NativeElement = Elm.Native.Graphics.Element.make(localRuntime);
	var Transform = Elm.Transform2D.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function setStrokeStyle(ctx, style)
	{
		ctx.lineWidth = style.width;

		var cap = style.cap.ctor;
		ctx.lineCap = cap === 'Flat'
			? 'butt'
			: cap === 'Round'
				? 'round'
				: 'square';

		var join = style.join.ctor;
		ctx.lineJoin = join === 'Smooth'
			? 'round'
			: join === 'Sharp'
				? 'miter'
				: 'bevel';

		ctx.miterLimit = style.join._0 || 10;
		ctx.strokeStyle = Color.toCss(style.color);
	}

	function setFillStyle(redo, ctx, style)
	{
		var sty = style.ctor;
		ctx.fillStyle = sty === 'Solid'
			? Color.toCss(style._0)
			: sty === 'Texture'
				? texture(redo, ctx, style._0)
				: gradient(ctx, style._0);
	}

	function trace(ctx, path)
	{
		var points = List.toArray(path);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		ctx.moveTo(points[i]._0, points[i]._1);
		while (i--)
		{
			ctx.lineTo(points[i]._0, points[i]._1);
		}
		if (path.closed)
		{
			i = points.length - 1;
			ctx.lineTo(points[i]._0, points[i]._1);
		}
	}

	function line(ctx, style, path)
	{
		if (style.dashing.ctor === '[]')
		{
			trace(ctx, path);
		}
		else
		{
			customLineHelp(ctx, style, path);
		}
		ctx.scale(1, -1);
		ctx.stroke();
	}

	function customLineHelp(ctx, style, path)
	{
		var points = List.toArray(path);
		if (path.closed)
		{
			points.push(points[0]);
		}
		var pattern = List.toArray(style.dashing);
		var i = points.length - 1;
		if (i <= 0)
		{
			return;
		}
		var x0 = points[i]._0, y0 = points[i]._1;
		var x1 = 0, y1 = 0, dx = 0, dy = 0, remaining = 0;
		var pindex = 0, plen = pattern.length;
		var draw = true, segmentLength = pattern[0];
		ctx.moveTo(x0, y0);
		while (i--)
		{
			x1 = points[i]._0;
			y1 = points[i]._1;
			dx = x1 - x0;
			dy = y1 - y0;
			remaining = Math.sqrt(dx * dx + dy * dy);
			while (segmentLength <= remaining)
			{
				x0 += dx * segmentLength / remaining;
				y0 += dy * segmentLength / remaining;
				ctx[draw ? 'lineTo' : 'moveTo'](x0, y0);
				// update starting position
				dx = x1 - x0;
				dy = y1 - y0;
				remaining = Math.sqrt(dx * dx + dy * dy);
				// update pattern
				draw = !draw;
				pindex = (pindex + 1) % plen;
				segmentLength = pattern[pindex];
			}
			if (remaining > 0)
			{
				ctx[draw ? 'lineTo' : 'moveTo'](x1, y1);
				segmentLength -= remaining;
			}
			x0 = x1;
			y0 = y1;
		}
	}

	function drawLine(ctx, style, path)
	{
		setStrokeStyle(ctx, style);
		return line(ctx, style, path);
	}

	function texture(redo, ctx, src)
	{
		var img = new Image();
		img.src = src;
		img.onload = redo;
		return ctx.createPattern(img, 'repeat');
	}

	function gradient(ctx, grad)
	{
		var g;
		var stops = [];
		if (grad.ctor === 'Linear')
		{
			var p0 = grad._0, p1 = grad._1;
			g = ctx.createLinearGradient(p0._0, -p0._1, p1._0, -p1._1);
			stops = List.toArray(grad._2);
		}
		else
		{
			var p0 = grad._0, p2 = grad._2;
			g = ctx.createRadialGradient(p0._0, -p0._1, grad._1, p2._0, -p2._1, grad._3);
			stops = List.toArray(grad._4);
		}
		var len = stops.length;
		for (var i = 0; i < len; ++i)
		{
			var stop = stops[i];
			g.addColorStop(stop._0, Color.toCss(stop._1));
		}
		return g;
	}

	function drawShape(redo, ctx, style, path)
	{
		trace(ctx, path);
		setFillStyle(redo, ctx, style);
		ctx.scale(1, -1);
		ctx.fill();
	}


	// TEXT RENDERING

	function fillText(redo, ctx, text)
	{
		drawText(ctx, text, ctx.fillText);
	}

	function strokeText(redo, ctx, style, text)
	{
		setStrokeStyle(ctx, style);
		// Use native canvas API for dashes only for text for now
		// Degrades to non-dashed on IE 9 + 10
		if (style.dashing.ctor !== '[]' && ctx.setLineDash)
		{
			var pattern = List.toArray(style.dashing);
			ctx.setLineDash(pattern);
		}
		drawText(ctx, text, ctx.strokeText);
	}

	function drawText(ctx, text, canvasDrawFn)
	{
		var textChunks = chunkText(defaultContext, text);

		var totalWidth = 0;
		var maxHeight = 0;
		var numChunks = textChunks.length;

		ctx.scale(1,-1);

		for (var i = numChunks; i--; )
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			var metrics = ctx.measureText(chunk.text);
			chunk.width = metrics.width;
			totalWidth += chunk.width;
			if (chunk.height > maxHeight)
			{
				maxHeight = chunk.height;
			}
		}

		var x = -totalWidth / 2.0;
		for (var i = 0; i < numChunks; ++i)
		{
			var chunk = textChunks[i];
			ctx.font = chunk.font;
			ctx.fillStyle = chunk.color;
			canvasDrawFn.call(ctx, chunk.text, x, maxHeight / 2);
			x += chunk.width;
		}
	}

	function toFont(props)
	{
		return [
			props['font-style'],
			props['font-variant'],
			props['font-weight'],
			props['font-size'],
			props['font-family']
		].join(' ');
	}


	// Convert the object returned by the text module
	// into something we can use for styling canvas text
	function chunkText(context, text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			var leftChunks = chunkText(context, text._0);
			var rightChunks = chunkText(context, text._1);
			return leftChunks.concat(rightChunks);
		}
		if (tag === 'Text:Text')
		{
			return [{
				text: text._0,
				color: context.color,
				height: context['font-size'].slice(0, -2) | 0,
				font: toFont(context)
			}];
		}
		if (tag === 'Text:Meta')
		{
			var newContext = freshContext(text._0, context);
			return chunkText(newContext, text._1);
		}
	}

	function freshContext(props, ctx)
	{
		return {
			'font-style': props['font-style'] || ctx['font-style'],
			'font-variant': props['font-variant'] || ctx['font-variant'],
			'font-weight': props['font-weight'] || ctx['font-weight'],
			'font-size': props['font-size'] || ctx['font-size'],
			'font-family': props['font-family'] || ctx['font-family'],
			'color': props['color'] || ctx['color']
		};
	}

	var defaultContext = {
		'font-style': 'normal',
		'font-variant': 'normal',
		'font-weight': 'normal',
		'font-size': '12px',
		'font-family': 'sans-serif',
		'color': 'black'
	};


	// IMAGES

	function drawImage(redo, ctx, form)
	{
		var img = new Image();
		img.onload = redo;
		img.src = form._3;
		var w = form._0,
			h = form._1,
			pos = form._2,
			srcX = pos._0,
			srcY = pos._1,
			srcW = w,
			srcH = h,
			destX = -w / 2,
			destY = -h / 2,
			destW = w,
			destH = h;

		ctx.scale(1, -1);
		ctx.drawImage(img, srcX, srcY, srcW, srcH, destX, destY, destW, destH);
	}

	function renderForm(redo, ctx, form)
	{
		ctx.save();

		var x = form.x,
			y = form.y,
			theta = form.theta,
			scale = form.scale;

		if (x !== 0 || y !== 0)
		{
			ctx.translate(x, y);
		}
		if (theta !== 0)
		{
			ctx.rotate(theta % (Math.PI * 2));
		}
		if (scale !== 1)
		{
			ctx.scale(scale, scale);
		}
		if (form.alpha !== 1)
		{
			ctx.globalAlpha = ctx.globalAlpha * form.alpha;
		}

		ctx.beginPath();
		var f = form.form;
		switch (f.ctor)
		{
			case 'FPath':
				drawLine(ctx, f._0, f._1);
				break;

			case 'FImage':
				drawImage(redo, ctx, f);
				break;

			case 'FShape':
				if (f._0.ctor === 'Line')
				{
					f._1.closed = true;
					drawLine(ctx, f._0._0, f._1);
				}
				else
				{
					drawShape(redo, ctx, f._0._0, f._1);
				}
				break;

			case 'FText':
				fillText(redo, ctx, f._0);
				break;

			case 'FOutlinedText':
				strokeText(redo, ctx, f._0, f._1);
				break;
		}
		ctx.restore();
	}

	function formToMatrix(form)
	{
	   var scale = form.scale;
	   var matrix = A6( Transform.matrix, scale, 0, 0, scale, form.x, form.y );

	   var theta = form.theta;
	   if (theta !== 0)
	   {
		   matrix = A2( Transform.multiply, matrix, Transform.rotation(theta) );
	   }

	   return matrix;
	}

	function str(n)
	{
		if (n < 0.00001 && n > -0.00001)
		{
			return 0;
		}
		return n;
	}

	function makeTransform(w, h, form, matrices)
	{
		var props = form.form._0._0.props;
		var m = A6( Transform.matrix, 1, 0, 0, -1,
					(w - props.width ) / 2,
					(h - props.height) / 2 );
		var len = matrices.length;
		for (var i = 0; i < len; ++i)
		{
			m = A2( Transform.multiply, m, matrices[i] );
		}
		m = A2( Transform.multiply, m, formToMatrix(form) );

		return 'matrix(' +
			str( m[0]) + ', ' + str( m[3]) + ', ' +
			str(-m[1]) + ', ' + str(-m[4]) + ', ' +
			str( m[2]) + ', ' + str( m[5]) + ')';
	}

	function stepperHelp(list)
	{
		var arr = List.toArray(list);
		var i = 0;
		function peekNext()
		{
			return i < arr.length ? arr[i]._0.form.ctor : '';
		}
		// assumes that there is a next element
		function next()
		{
			var out = arr[i]._0;
			++i;
			return out;
		}
		return {
			peekNext: peekNext,
			next: next
		};
	}

	function formStepper(forms)
	{
		var ps = [stepperHelp(forms)];
		var matrices = [];
		var alphas = [];
		function peekNext()
		{
			var len = ps.length;
			var formType = '';
			for (var i = 0; i < len; ++i )
			{
				if (formType = ps[i].peekNext()) return formType;
			}
			return '';
		}
		// assumes that there is a next element
		function next(ctx)
		{
			while (!ps[0].peekNext())
			{
				ps.shift();
				matrices.pop();
				alphas.shift();
				if (ctx)
				{
					ctx.restore();
				}
			}
			var out = ps[0].next();
			var f = out.form;
			if (f.ctor === 'FGroup')
			{
				ps.unshift(stepperHelp(f._1));
				var m = A2(Transform.multiply, f._0, formToMatrix(out));
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
				matrices.push(m);

				var alpha = (alphas[0] || 1) * out.alpha;
				alphas.unshift(alpha);
				ctx.globalAlpha = alpha;
			}
			return out;
		}
		function transforms()
		{
			return matrices;
		}
		function alpha()
		{
			return alphas[0] || 1;
		}
		return {
			peekNext: peekNext,
			next: next,
			transforms: transforms,
			alpha: alpha
		};
	}

	function makeCanvas(w, h)
	{
		var canvas = NativeElement.createNode('canvas');
		canvas.style.width  = w + 'px';
		canvas.style.height = h + 'px';
		canvas.style.display = 'block';
		canvas.style.position = 'absolute';
		var ratio = window.devicePixelRatio || 1;
		canvas.width  = w * ratio;
		canvas.height = h * ratio;
		return canvas;
	}

	function render(model)
	{
		var div = NativeElement.createNode('div');
		div.style.overflow = 'hidden';
		div.style.position = 'relative';
		update(div, model, model);
		return div;
	}

	function nodeStepper(w, h, div)
	{
		var kids = div.childNodes;
		var i = 0;
		var ratio = window.devicePixelRatio || 1;

		function transform(transforms, ctx)
		{
			ctx.translate( w / 2 * ratio, h / 2 * ratio );
			ctx.scale( ratio, -ratio );
			var len = transforms.length;
			for (var i = 0; i < len; ++i)
			{
				var m = transforms[i];
				ctx.save();
				ctx.transform(m[0], m[3], m[1], m[4], m[2], m[5]);
			}
			return ctx;
		}
		function nextContext(transforms)
		{
			while (i < kids.length)
			{
				var node = kids[i];
				if (node.getContext)
				{
					node.width = w * ratio;
					node.height = h * ratio;
					node.style.width = w + 'px';
					node.style.height = h + 'px';
					++i;
					return transform(transforms, node.getContext('2d'));
				}
				div.removeChild(node);
			}
			var canvas = makeCanvas(w, h);
			div.appendChild(canvas);
			// we have added a new node, so we must step our position
			++i;
			return transform(transforms, canvas.getContext('2d'));
		}
		function addElement(matrices, alpha, form)
		{
			var kid = kids[i];
			var elem = form.form._0;

			var node = (!kid || kid.getContext)
				? NativeElement.render(elem)
				: NativeElement.update(kid, kid.oldElement, elem);

			node.style.position = 'absolute';
			node.style.opacity = alpha * form.alpha * elem._0.props.opacity;
			NativeElement.addTransform(node.style, makeTransform(w, h, form, matrices));
			node.oldElement = elem;
			++i;
			if (!kid)
			{
				div.appendChild(node);
			}
			else
			{
				div.insertBefore(node, kid);
			}
		}
		function clearRest()
		{
			while (i < kids.length)
			{
				div.removeChild(kids[i]);
			}
		}
		return {
			nextContext: nextContext,
			addElement: addElement,
			clearRest: clearRest
		};
	}


	function update(div, _, model)
	{
		var w = model.w;
		var h = model.h;

		var forms = formStepper(model.forms);
		var nodes = nodeStepper(w, h, div);
		var ctx = null;
		var formType = '';

		while (formType = forms.peekNext())
		{
			// make sure we have context if we need it
			if (ctx === null && formType !== 'FElement')
			{
				ctx = nodes.nextContext(forms.transforms());
				ctx.globalAlpha = forms.alpha();
			}

			var form = forms.next(ctx);
			// if it is FGroup, all updates are made within formStepper when next is called.
			if (formType === 'FElement')
			{
				// update or insert an element, get a new context
				nodes.addElement(forms.transforms(), forms.alpha(), form);
				ctx = null;
			}
			else if (formType !== 'FGroup')
			{
				renderForm(function() { update(div, model, model); }, ctx, form);
			}
		}
		nodes.clearRest();
		return div;
	}


	function collage(w, h, forms)
	{
		return A3(NativeElement.newElement, w, h, {
			ctor: 'Custom',
			type: 'Collage',
			render: render,
			update: update,
			model: {w: w, h: h, forms: forms}
		});
	}

	return localRuntime.Native.Graphics.Collage.values = {
		collage: F3(collage)
	};
};

Elm.Native.Color = {};
Elm.Native.Color.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Color = localRuntime.Native.Color || {};
	if (localRuntime.Native.Color.values)
	{
		return localRuntime.Native.Color.values;
	}

	function toCss(c)
	{
		var format = '';
		var colors = '';
		if (c.ctor === 'RGBA')
		{
			format = 'rgb';
			colors = c._0 + ', ' + c._1 + ', ' + c._2;
		}
		else
		{
			format = 'hsl';
			colors = (c._0 * 180 / Math.PI) + ', ' +
					 (c._1 * 100) + '%, ' +
					 (c._2 * 100) + '%';
		}
		if (c._3 === 1)
		{
			return format + '(' + colors + ')';
		}
		else
		{
			return format + 'a(' + colors + ', ' + c._3 + ')';
		}
	}

	return localRuntime.Native.Color.values = {
		toCss: toCss
	};
};

Elm.Color = Elm.Color || {};
Elm.Color.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   if (_elm.Color.values) return _elm.Color.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm);
   var _op = {};
   var Radial = F5(function (a,b,c,d,e) {
      return {ctor: "Radial",_0: a,_1: b,_2: c,_3: d,_4: e};
   });
   var radial = Radial;
   var Linear = F3(function (a,b,c) {
      return {ctor: "Linear",_0: a,_1: b,_2: c};
   });
   var linear = Linear;
   var fmod = F2(function (f,n) {
      var integer = $Basics.floor(f);
      return $Basics.toFloat(A2($Basics._op["%"],
      integer,
      n)) + f - $Basics.toFloat(integer);
   });
   var rgbToHsl = F3(function (red,green,blue) {
      var b = $Basics.toFloat(blue) / 255;
      var g = $Basics.toFloat(green) / 255;
      var r = $Basics.toFloat(red) / 255;
      var cMax = A2($Basics.max,A2($Basics.max,r,g),b);
      var cMin = A2($Basics.min,A2($Basics.min,r,g),b);
      var c = cMax - cMin;
      var lightness = (cMax + cMin) / 2;
      var saturation = _U.eq(lightness,
      0) ? 0 : c / (1 - $Basics.abs(2 * lightness - 1));
      var hue = $Basics.degrees(60) * (_U.eq(cMax,r) ? A2(fmod,
      (g - b) / c,
      6) : _U.eq(cMax,g) ? (b - r) / c + 2 : (r - g) / c + 4);
      return {ctor: "_Tuple3",_0: hue,_1: saturation,_2: lightness};
   });
   var hslToRgb = F3(function (hue,saturation,lightness) {
      var hue$ = hue / $Basics.degrees(60);
      var chroma = (1 - $Basics.abs(2 * lightness - 1)) * saturation;
      var x = chroma * (1 - $Basics.abs(A2(fmod,hue$,2) - 1));
      var _p0 = _U.cmp(hue$,0) < 0 ? {ctor: "_Tuple3"
                                     ,_0: 0
                                     ,_1: 0
                                     ,_2: 0} : _U.cmp(hue$,1) < 0 ? {ctor: "_Tuple3"
                                                                    ,_0: chroma
                                                                    ,_1: x
                                                                    ,_2: 0} : _U.cmp(hue$,2) < 0 ? {ctor: "_Tuple3"
                                                                                                   ,_0: x
                                                                                                   ,_1: chroma
                                                                                                   ,_2: 0} : _U.cmp(hue$,3) < 0 ? {ctor: "_Tuple3"
                                                                                                                                  ,_0: 0
                                                                                                                                  ,_1: chroma
                                                                                                                                  ,_2: x} : _U.cmp(hue$,
      4) < 0 ? {ctor: "_Tuple3",_0: 0,_1: x,_2: chroma} : _U.cmp(hue$,
      5) < 0 ? {ctor: "_Tuple3",_0: x,_1: 0,_2: chroma} : _U.cmp(hue$,
      6) < 0 ? {ctor: "_Tuple3"
               ,_0: chroma
               ,_1: 0
               ,_2: x} : {ctor: "_Tuple3",_0: 0,_1: 0,_2: 0};
      var r = _p0._0;
      var g = _p0._1;
      var b = _p0._2;
      var m = lightness - chroma / 2;
      return {ctor: "_Tuple3",_0: r + m,_1: g + m,_2: b + m};
   });
   var toRgb = function (color) {
      var _p1 = color;
      if (_p1.ctor === "RGBA") {
            return {red: _p1._0
                   ,green: _p1._1
                   ,blue: _p1._2
                   ,alpha: _p1._3};
         } else {
            var _p2 = A3(hslToRgb,_p1._0,_p1._1,_p1._2);
            var r = _p2._0;
            var g = _p2._1;
            var b = _p2._2;
            return {red: $Basics.round(255 * r)
                   ,green: $Basics.round(255 * g)
                   ,blue: $Basics.round(255 * b)
                   ,alpha: _p1._3};
         }
   };
   var toHsl = function (color) {
      var _p3 = color;
      if (_p3.ctor === "HSLA") {
            return {hue: _p3._0
                   ,saturation: _p3._1
                   ,lightness: _p3._2
                   ,alpha: _p3._3};
         } else {
            var _p4 = A3(rgbToHsl,_p3._0,_p3._1,_p3._2);
            var h = _p4._0;
            var s = _p4._1;
            var l = _p4._2;
            return {hue: h,saturation: s,lightness: l,alpha: _p3._3};
         }
   };
   var HSLA = F4(function (a,b,c,d) {
      return {ctor: "HSLA",_0: a,_1: b,_2: c,_3: d};
   });
   var hsla = F4(function (hue,saturation,lightness,alpha) {
      return A4(HSLA,
      hue - $Basics.turns($Basics.toFloat($Basics.floor(hue / (2 * $Basics.pi)))),
      saturation,
      lightness,
      alpha);
   });
   var hsl = F3(function (hue,saturation,lightness) {
      return A4(hsla,hue,saturation,lightness,1);
   });
   var complement = function (color) {
      var _p5 = color;
      if (_p5.ctor === "HSLA") {
            return A4(hsla,
            _p5._0 + $Basics.degrees(180),
            _p5._1,
            _p5._2,
            _p5._3);
         } else {
            var _p6 = A3(rgbToHsl,_p5._0,_p5._1,_p5._2);
            var h = _p6._0;
            var s = _p6._1;
            var l = _p6._2;
            return A4(hsla,h + $Basics.degrees(180),s,l,_p5._3);
         }
   };
   var grayscale = function (p) {    return A4(HSLA,0,0,1 - p,1);};
   var greyscale = function (p) {    return A4(HSLA,0,0,1 - p,1);};
   var RGBA = F4(function (a,b,c,d) {
      return {ctor: "RGBA",_0: a,_1: b,_2: c,_3: d};
   });
   var rgba = RGBA;
   var rgb = F3(function (r,g,b) {    return A4(RGBA,r,g,b,1);});
   var lightRed = A4(RGBA,239,41,41,1);
   var red = A4(RGBA,204,0,0,1);
   var darkRed = A4(RGBA,164,0,0,1);
   var lightOrange = A4(RGBA,252,175,62,1);
   var orange = A4(RGBA,245,121,0,1);
   var darkOrange = A4(RGBA,206,92,0,1);
   var lightYellow = A4(RGBA,255,233,79,1);
   var yellow = A4(RGBA,237,212,0,1);
   var darkYellow = A4(RGBA,196,160,0,1);
   var lightGreen = A4(RGBA,138,226,52,1);
   var green = A4(RGBA,115,210,22,1);
   var darkGreen = A4(RGBA,78,154,6,1);
   var lightBlue = A4(RGBA,114,159,207,1);
   var blue = A4(RGBA,52,101,164,1);
   var darkBlue = A4(RGBA,32,74,135,1);
   var lightPurple = A4(RGBA,173,127,168,1);
   var purple = A4(RGBA,117,80,123,1);
   var darkPurple = A4(RGBA,92,53,102,1);
   var lightBrown = A4(RGBA,233,185,110,1);
   var brown = A4(RGBA,193,125,17,1);
   var darkBrown = A4(RGBA,143,89,2,1);
   var black = A4(RGBA,0,0,0,1);
   var white = A4(RGBA,255,255,255,1);
   var lightGrey = A4(RGBA,238,238,236,1);
   var grey = A4(RGBA,211,215,207,1);
   var darkGrey = A4(RGBA,186,189,182,1);
   var lightGray = A4(RGBA,238,238,236,1);
   var gray = A4(RGBA,211,215,207,1);
   var darkGray = A4(RGBA,186,189,182,1);
   var lightCharcoal = A4(RGBA,136,138,133,1);
   var charcoal = A4(RGBA,85,87,83,1);
   var darkCharcoal = A4(RGBA,46,52,54,1);
   return _elm.Color.values = {_op: _op
                              ,rgb: rgb
                              ,rgba: rgba
                              ,hsl: hsl
                              ,hsla: hsla
                              ,greyscale: greyscale
                              ,grayscale: grayscale
                              ,complement: complement
                              ,linear: linear
                              ,radial: radial
                              ,toRgb: toRgb
                              ,toHsl: toHsl
                              ,red: red
                              ,orange: orange
                              ,yellow: yellow
                              ,green: green
                              ,blue: blue
                              ,purple: purple
                              ,brown: brown
                              ,lightRed: lightRed
                              ,lightOrange: lightOrange
                              ,lightYellow: lightYellow
                              ,lightGreen: lightGreen
                              ,lightBlue: lightBlue
                              ,lightPurple: lightPurple
                              ,lightBrown: lightBrown
                              ,darkRed: darkRed
                              ,darkOrange: darkOrange
                              ,darkYellow: darkYellow
                              ,darkGreen: darkGreen
                              ,darkBlue: darkBlue
                              ,darkPurple: darkPurple
                              ,darkBrown: darkBrown
                              ,white: white
                              ,lightGrey: lightGrey
                              ,grey: grey
                              ,darkGrey: darkGrey
                              ,lightCharcoal: lightCharcoal
                              ,charcoal: charcoal
                              ,darkCharcoal: darkCharcoal
                              ,black: black
                              ,lightGray: lightGray
                              ,gray: gray
                              ,darkGray: darkGray};
};

// setup
Elm.Native = Elm.Native || {};
Elm.Native.Graphics = Elm.Native.Graphics || {};
Elm.Native.Graphics.Element = Elm.Native.Graphics.Element || {};

// definition
Elm.Native.Graphics.Element.make = function(localRuntime) {
	'use strict';

	// attempt to short-circuit
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Graphics = localRuntime.Native.Graphics || {};
	localRuntime.Native.Graphics.Element = localRuntime.Native.Graphics.Element || {};
	if ('values' in localRuntime.Native.Graphics.Element)
	{
		return localRuntime.Native.Graphics.Element.values;
	}

	var Color = Elm.Native.Color.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Text = Elm.Native.Text.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CREATION

	var createNode =
		typeof document === 'undefined'
			?
				function(_)
				{
					return {
						style: {},
						appendChild: function() {}
					};
				}
			:
				function(elementType)
				{
					var node = document.createElement(elementType);
					node.style.padding = '0';
					node.style.margin = '0';
					return node;
				}
			;


	function newElement(width, height, elementPrim)
	{
		return {
			ctor: 'Element_elm_builtin',
			_0: {
				element: elementPrim,
				props: {
					id: Utils.guid(),
					width: width,
					height: height,
					opacity: 1,
					color: Maybe.Nothing,
					href: '',
					tag: '',
					hover: Utils.Tuple0,
					click: Utils.Tuple0
				}
			}
		};
	}


	// PROPERTIES

	function setProps(elem, node)
	{
		var props = elem.props;

		var element = elem.element;
		var width = props.width - (element.adjustWidth || 0);
		var height = props.height - (element.adjustHeight || 0);
		node.style.width  = (width | 0) + 'px';
		node.style.height = (height | 0) + 'px';

		if (props.opacity !== 1)
		{
			node.style.opacity = props.opacity;
		}

		if (props.color.ctor === 'Just')
		{
			node.style.backgroundColor = Color.toCss(props.color._0);
		}

		if (props.tag !== '')
		{
			node.id = props.tag;
		}

		if (props.hover.ctor !== '_Tuple0')
		{
			addHover(node, props.hover);
		}

		if (props.click.ctor !== '_Tuple0')
		{
			addClick(node, props.click);
		}

		if (props.href !== '')
		{
			var anchor = createNode('a');
			anchor.href = props.href;
			anchor.style.display = 'block';
			anchor.style.pointerEvents = 'auto';
			anchor.appendChild(node);
			node = anchor;
		}

		return node;
	}

	function addClick(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_click_handler = handler;
		function trigger(ev)
		{
			e.elm_click_handler(Utils.Tuple0);
			ev.stopPropagation();
		}
		e.elm_click_trigger = trigger;
		e.addEventListener('click', trigger);
	}

	function removeClick(e, handler)
	{
		if (e.elm_click_trigger)
		{
			e.removeEventListener('click', e.elm_click_trigger);
			e.elm_click_trigger = null;
			e.elm_click_handler = null;
		}
	}

	function addHover(e, handler)
	{
		e.style.pointerEvents = 'auto';
		e.elm_hover_handler = handler;
		e.elm_hover_count = 0;

		function over(evt)
		{
			if (e.elm_hover_count++ > 0) return;
			e.elm_hover_handler(true);
			evt.stopPropagation();
		}
		function out(evt)
		{
			if (e.contains(evt.toElement || evt.relatedTarget)) return;
			e.elm_hover_count = 0;
			e.elm_hover_handler(false);
			evt.stopPropagation();
		}
		e.elm_hover_over = over;
		e.elm_hover_out = out;
		e.addEventListener('mouseover', over);
		e.addEventListener('mouseout', out);
	}

	function removeHover(e)
	{
		e.elm_hover_handler = null;
		if (e.elm_hover_over)
		{
			e.removeEventListener('mouseover', e.elm_hover_over);
			e.elm_hover_over = null;
		}
		if (e.elm_hover_out)
		{
			e.removeEventListener('mouseout', e.elm_hover_out);
			e.elm_hover_out = null;
		}
	}


	// IMAGES

	function image(props, img)
	{
		switch (img._0.ctor)
		{
			case 'Plain':
				return plainImage(img._3);

			case 'Fitted':
				return fittedImage(props.width, props.height, img._3);

			case 'Cropped':
				return croppedImage(img, props.width, props.height, img._3);

			case 'Tiled':
				return tiledImage(img._3);
		}
	}

	function plainImage(src)
	{
		var img = createNode('img');
		img.src = src;
		img.name = src;
		img.style.display = 'block';
		return img;
	}

	function tiledImage(src)
	{
		var div = createNode('div');
		div.style.backgroundImage = 'url(' + src + ')';
		return div;
	}

	function fittedImage(w, h, src)
	{
		var div = createNode('div');
		div.style.background = 'url(' + src + ') no-repeat center';
		div.style.webkitBackgroundSize = 'cover';
		div.style.MozBackgroundSize = 'cover';
		div.style.OBackgroundSize = 'cover';
		div.style.backgroundSize = 'cover';
		return div;
	}

	function croppedImage(elem, w, h, src)
	{
		var pos = elem._0._0;
		var e = createNode('div');
		e.style.overflow = 'hidden';

		var img = createNode('img');
		img.onload = function() {
			var sw = w / elem._1, sh = h / elem._2;
			img.style.width = ((this.width * sw) | 0) + 'px';
			img.style.height = ((this.height * sh) | 0) + 'px';
			img.style.marginLeft = ((- pos._0 * sw) | 0) + 'px';
			img.style.marginTop = ((- pos._1 * sh) | 0) + 'px';
		};
		img.src = src;
		img.name = src;
		e.appendChild(img);
		return e;
	}


	// FLOW

	function goOut(node)
	{
		node.style.position = 'absolute';
		return node;
	}
	function goDown(node)
	{
		return node;
	}
	function goRight(node)
	{
		node.style.styleFloat = 'left';
		node.style.cssFloat = 'left';
		return node;
	}

	var directionTable = {
		DUp: goDown,
		DDown: goDown,
		DLeft: goRight,
		DRight: goRight,
		DIn: goOut,
		DOut: goOut
	};
	function needsReversal(dir)
	{
		return dir === 'DUp' || dir === 'DLeft' || dir === 'DIn';
	}

	function flow(dir, elist)
	{
		var array = List.toArray(elist);
		var container = createNode('div');
		var goDir = directionTable[dir];
		if (goDir === goOut)
		{
			container.style.pointerEvents = 'none';
		}
		if (needsReversal(dir))
		{
			array.reverse();
		}
		var len = array.length;
		for (var i = 0; i < len; ++i)
		{
			container.appendChild(goDir(render(array[i])));
		}
		return container;
	}


	// CONTAINER

	function toPos(pos)
	{
		return pos.ctor === 'Absolute'
			? pos._0 + 'px'
			: (pos._0 * 100) + '%';
	}

	// must clear right, left, top, bottom, and transform
	// before calling this function
	function setPos(pos, wrappedElement, e)
	{
		var elem = wrappedElement._0;
		var element = elem.element;
		var props = elem.props;
		var w = props.width + (element.adjustWidth ? element.adjustWidth : 0);
		var h = props.height + (element.adjustHeight ? element.adjustHeight : 0);

		e.style.position = 'absolute';
		e.style.margin = 'auto';
		var transform = '';

		switch (pos.horizontal.ctor)
		{
			case 'P':
				e.style.right = toPos(pos.x);
				e.style.removeProperty('left');
				break;

			case 'Z':
				transform = 'translateX(' + ((-w / 2) | 0) + 'px) ';

			case 'N':
				e.style.left = toPos(pos.x);
				e.style.removeProperty('right');
				break;
		}
		switch (pos.vertical.ctor)
		{
			case 'N':
				e.style.bottom = toPos(pos.y);
				e.style.removeProperty('top');
				break;

			case 'Z':
				transform += 'translateY(' + ((-h / 2) | 0) + 'px)';

			case 'P':
				e.style.top = toPos(pos.y);
				e.style.removeProperty('bottom');
				break;
		}
		if (transform !== '')
		{
			addTransform(e.style, transform);
		}
		return e;
	}

	function addTransform(style, transform)
	{
		style.transform       = transform;
		style.msTransform     = transform;
		style.MozTransform    = transform;
		style.webkitTransform = transform;
		style.OTransform      = transform;
	}

	function container(pos, elem)
	{
		var e = render(elem);
		setPos(pos, elem, e);
		var div = createNode('div');
		div.style.position = 'relative';
		div.style.overflow = 'hidden';
		div.appendChild(e);
		return div;
	}


	function rawHtml(elem)
	{
		var html = elem.html;
		var align = elem.align;

		var div = createNode('div');
		div.innerHTML = html;
		div.style.visibility = 'hidden';
		if (align)
		{
			div.style.textAlign = align;
		}
		div.style.visibility = 'visible';
		div.style.pointerEvents = 'auto';
		return div;
	}


	// RENDER

	function render(wrappedElement)
	{
		var elem = wrappedElement._0;
		return setProps(elem, makeElement(elem));
	}

	function makeElement(e)
	{
		var elem = e.element;
		switch (elem.ctor)
		{
			case 'Image':
				return image(e.props, elem);

			case 'Flow':
				return flow(elem._0.ctor, elem._1);

			case 'Container':
				return container(elem._0, elem._1);

			case 'Spacer':
				return createNode('div');

			case 'RawHtml':
				return rawHtml(elem);

			case 'Custom':
				return elem.render(elem.model);
		}
	}

	function updateAndReplace(node, curr, next)
	{
		var newNode = update(node, curr, next);
		if (newNode !== node)
		{
			node.parentNode.replaceChild(newNode, node);
		}
		return newNode;
	}


	// UPDATE

	function update(node, wrappedCurrent, wrappedNext)
	{
		var curr = wrappedCurrent._0;
		var next = wrappedNext._0;
		var rootNode = node;
		if (node.tagName === 'A')
		{
			node = node.firstChild;
		}
		if (curr.props.id === next.props.id)
		{
			updateProps(node, curr, next);
			return rootNode;
		}
		if (curr.element.ctor !== next.element.ctor)
		{
			return render(wrappedNext);
		}
		var nextE = next.element;
		var currE = curr.element;
		switch (nextE.ctor)
		{
			case 'Spacer':
				updateProps(node, curr, next);
				return rootNode;

			case 'RawHtml':
				if(currE.html.valueOf() !== nextE.html.valueOf())
				{
					node.innerHTML = nextE.html;
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Image':
				if (nextE._0.ctor === 'Plain')
				{
					if (nextE._3 !== currE._3)
					{
						node.src = nextE._3;
					}
				}
				else if (!Utils.eq(nextE, currE)
					|| next.props.width !== curr.props.width
					|| next.props.height !== curr.props.height)
				{
					return render(wrappedNext);
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Flow':
				var arr = List.toArray(nextE._1);
				for (var i = arr.length; i--; )
				{
					arr[i] = arr[i]._0.element.ctor;
				}
				if (nextE._0.ctor !== currE._0.ctor)
				{
					return render(wrappedNext);
				}
				var nexts = List.toArray(nextE._1);
				var kids = node.childNodes;
				if (nexts.length !== kids.length)
				{
					return render(wrappedNext);
				}
				var currs = List.toArray(currE._1);
				var dir = nextE._0.ctor;
				var goDir = directionTable[dir];
				var toReverse = needsReversal(dir);
				var len = kids.length;
				for (var i = len; i--; )
				{
					var subNode = kids[toReverse ? len - i - 1 : i];
					goDir(updateAndReplace(subNode, currs[i], nexts[i]));
				}
				updateProps(node, curr, next);
				return rootNode;

			case 'Container':
				var subNode = node.firstChild;
				var newSubNode = updateAndReplace(subNode, currE._1, nextE._1);
				setPos(nextE._0, nextE._1, newSubNode);
				updateProps(node, curr, next);
				return rootNode;

			case 'Custom':
				if (currE.type === nextE.type)
				{
					var updatedNode = nextE.update(node, currE.model, nextE.model);
					updateProps(updatedNode, curr, next);
					return updatedNode;
				}
				return render(wrappedNext);
		}
	}

	function updateProps(node, curr, next)
	{
		var nextProps = next.props;
		var currProps = curr.props;

		var element = next.element;
		var width = nextProps.width - (element.adjustWidth || 0);
		var height = nextProps.height - (element.adjustHeight || 0);
		if (width !== currProps.width)
		{
			node.style.width = (width | 0) + 'px';
		}
		if (height !== currProps.height)
		{
			node.style.height = (height | 0) + 'px';
		}

		if (nextProps.opacity !== currProps.opacity)
		{
			node.style.opacity = nextProps.opacity;
		}

		var nextColor = nextProps.color.ctor === 'Just'
			? Color.toCss(nextProps.color._0)
			: '';
		if (node.style.backgroundColor !== nextColor)
		{
			node.style.backgroundColor = nextColor;
		}

		if (nextProps.tag !== currProps.tag)
		{
			node.id = nextProps.tag;
		}

		if (nextProps.href !== currProps.href)
		{
			if (currProps.href === '')
			{
				// add a surrounding href
				var anchor = createNode('a');
				anchor.href = nextProps.href;
				anchor.style.display = 'block';
				anchor.style.pointerEvents = 'auto';

				node.parentNode.replaceChild(anchor, node);
				anchor.appendChild(node);
			}
			else if (nextProps.href === '')
			{
				// remove the surrounding href
				var anchor = node.parentNode;
				anchor.parentNode.replaceChild(node, anchor);
			}
			else
			{
				// just update the link
				node.parentNode.href = nextProps.href;
			}
		}

		// update click and hover handlers
		var removed = false;

		// update hover handlers
		if (currProps.hover.ctor === '_Tuple0')
		{
			if (nextProps.hover.ctor !== '_Tuple0')
			{
				addHover(node, nextProps.hover);
			}
		}
		else
		{
			if (nextProps.hover.ctor === '_Tuple0')
			{
				removed = true;
				removeHover(node);
			}
			else
			{
				node.elm_hover_handler = nextProps.hover;
			}
		}

		// update click handlers
		if (currProps.click.ctor === '_Tuple0')
		{
			if (nextProps.click.ctor !== '_Tuple0')
			{
				addClick(node, nextProps.click);
			}
		}
		else
		{
			if (nextProps.click.ctor === '_Tuple0')
			{
				removed = true;
				removeClick(node);
			}
			else
			{
				node.elm_click_handler = nextProps.click;
			}
		}

		// stop capturing clicks if
		if (removed
			&& nextProps.hover.ctor === '_Tuple0'
			&& nextProps.click.ctor === '_Tuple0')
		{
			node.style.pointerEvents = 'none';
		}
	}


	// TEXT

	function block(align)
	{
		return function(text)
		{
			var raw = {
				ctor: 'RawHtml',
				html: Text.renderHtml(text),
				align: align
			};
			var pos = htmlHeight(0, raw);
			return newElement(pos._0, pos._1, raw);
		};
	}

	function markdown(text)
	{
		var raw = {
			ctor: 'RawHtml',
			html: text,
			align: null
		};
		var pos = htmlHeight(0, raw);
		return newElement(pos._0, pos._1, raw);
	}

	var htmlHeight =
		typeof document !== 'undefined'
			? realHtmlHeight
			: function(a, b) { return Utils.Tuple2(0, 0); };

	function realHtmlHeight(width, rawHtml)
	{
		// create dummy node
		var temp = document.createElement('div');
		temp.innerHTML = rawHtml.html;
		if (width > 0)
		{
			temp.style.width = width + 'px';
		}
		temp.style.visibility = 'hidden';
		temp.style.styleFloat = 'left';
		temp.style.cssFloat = 'left';

		document.body.appendChild(temp);

		// get dimensions
		var style = window.getComputedStyle(temp, null);
		var w = Math.ceil(style.getPropertyValue('width').slice(0, -2) - 0);
		var h = Math.ceil(style.getPropertyValue('height').slice(0, -2) - 0);
		document.body.removeChild(temp);
		return Utils.Tuple2(w, h);
	}


	return localRuntime.Native.Graphics.Element.values = {
		render: render,
		update: update,
		updateAndReplace: updateAndReplace,

		createNode: createNode,
		newElement: F3(newElement),
		addTransform: addTransform,
		htmlHeight: F2(htmlHeight),
		guid: Utils.guid,

		block: block,
		markdown: markdown
	};
};

Elm.Native.Text = {};
Elm.Native.Text.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Text = localRuntime.Native.Text || {};
	if (localRuntime.Native.Text.values)
	{
		return localRuntime.Native.Text.values;
	}

	var toCss = Elm.Native.Color.make(localRuntime).toCss;
	var List = Elm.Native.List.make(localRuntime);


	// CONSTRUCTORS

	function fromString(str)
	{
		return {
			ctor: 'Text:Text',
			_0: str
		};
	}

	function append(a, b)
	{
		return {
			ctor: 'Text:Append',
			_0: a,
			_1: b
		};
	}

	function addMeta(field, value, text)
	{
		var newProps = {};
		var newText = {
			ctor: 'Text:Meta',
			_0: newProps,
			_1: text
		};

		if (text.ctor === 'Text:Meta')
		{
			newText._1 = text._1;
			var props = text._0;
			for (var i = metaKeys.length; i--; )
			{
				var key = metaKeys[i];
				var val = props[key];
				if (val)
				{
					newProps[key] = val;
				}
			}
		}
		newProps[field] = value;
		return newText;
	}

	var metaKeys = [
		'font-size',
		'font-family',
		'font-style',
		'font-weight',
		'href',
		'text-decoration',
		'color'
	];


	// conversions from Elm values to CSS

	function toTypefaces(list)
	{
		var typefaces = List.toArray(list);
		for (var i = typefaces.length; i--; )
		{
			var typeface = typefaces[i];
			if (typeface.indexOf(' ') > -1)
			{
				typefaces[i] = "'" + typeface + "'";
			}
		}
		return typefaces.join(',');
	}

	function toLine(line)
	{
		var ctor = line.ctor;
		return ctor === 'Under'
			? 'underline'
			: ctor === 'Over'
				? 'overline'
				: 'line-through';
	}

	// setting styles of Text

	function style(style, text)
	{
		var newText = addMeta('color', toCss(style.color), text);
		var props = newText._0;

		if (style.typeface.ctor !== '[]')
		{
			props['font-family'] = toTypefaces(style.typeface);
		}
		if (style.height.ctor !== 'Nothing')
		{
			props['font-size'] = style.height._0 + 'px';
		}
		if (style.bold)
		{
			props['font-weight'] = 'bold';
		}
		if (style.italic)
		{
			props['font-style'] = 'italic';
		}
		if (style.line.ctor !== 'Nothing')
		{
			props['text-decoration'] = toLine(style.line._0);
		}
		return newText;
	}

	function height(px, text)
	{
		return addMeta('font-size', px + 'px', text);
	}

	function typeface(names, text)
	{
		return addMeta('font-family', toTypefaces(names), text);
	}

	function monospace(text)
	{
		return addMeta('font-family', 'monospace', text);
	}

	function italic(text)
	{
		return addMeta('font-style', 'italic', text);
	}

	function bold(text)
	{
		return addMeta('font-weight', 'bold', text);
	}

	function link(href, text)
	{
		return addMeta('href', href, text);
	}

	function line(line, text)
	{
		return addMeta('text-decoration', toLine(line), text);
	}

	function color(color, text)
	{
		return addMeta('color', toCss(color), text);
	}


	// RENDER

	function renderHtml(text)
	{
		var tag = text.ctor;
		if (tag === 'Text:Append')
		{
			return renderHtml(text._0) + renderHtml(text._1);
		}
		if (tag === 'Text:Text')
		{
			return properEscape(text._0);
		}
		if (tag === 'Text:Meta')
		{
			return renderMeta(text._0, renderHtml(text._1));
		}
	}

	function renderMeta(metas, string)
	{
		var href = metas.href;
		if (href)
		{
			string = '<a href="' + href + '">' + string + '</a>';
		}
		var styles = '';
		for (var key in metas)
		{
			if (key === 'href')
			{
				continue;
			}
			styles += key + ':' + metas[key] + ';';
		}
		if (styles)
		{
			string = '<span style="' + styles + '">' + string + '</span>';
		}
		return string;
	}

	function properEscape(str)
	{
		if (str.length === 0)
		{
			return str;
		}
		str = str //.replace(/&/g,  '&#38;')
			.replace(/"/g,  '&#34;')
			.replace(/'/g,  '&#39;')
			.replace(/</g,  '&#60;')
			.replace(/>/g,  '&#62;');
		var arr = str.split('\n');
		for (var i = arr.length; i--; )
		{
			arr[i] = makeSpaces(arr[i]);
		}
		return arr.join('<br/>');
	}

	function makeSpaces(s)
	{
		if (s.length === 0)
		{
			return s;
		}
		var arr = s.split('');
		if (arr[0] === ' ')
		{
			arr[0] = '&nbsp;';
		}
		for (var i = arr.length; --i; )
		{
			if (arr[i][0] === ' ' && arr[i - 1] === ' ')
			{
				arr[i - 1] = arr[i - 1] + arr[i];
				arr[i] = '';
			}
		}
		for (var i = arr.length; i--; )
		{
			if (arr[i].length > 1 && arr[i][0] === ' ')
			{
				var spaces = arr[i].split('');
				for (var j = spaces.length - 2; j >= 0; j -= 2)
				{
					spaces[j] = '&nbsp;';
				}
				arr[i] = spaces.join('');
			}
		}
		arr = arr.join('');
		if (arr[arr.length - 1] === ' ')
		{
			return arr.slice(0, -1) + '&nbsp;';
		}
		return arr;
	}


	return localRuntime.Native.Text.values = {
		fromString: fromString,
		append: F2(append),

		height: F2(height),
		italic: italic,
		bold: bold,
		line: F2(line),
		monospace: monospace,
		typeface: F2(typeface),
		color: F2(color),
		link: F2(link),
		style: F2(style),

		toTypefaces: toTypefaces,
		toLine: toLine,
		renderHtml: renderHtml
	};
};

Elm.Text = Elm.Text || {};
Elm.Text.make = function (_elm) {
   "use strict";
   _elm.Text = _elm.Text || {};
   if (_elm.Text.values) return _elm.Text.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Text = Elm.Native.Text.make(_elm);
   var _op = {};
   var line = $Native$Text.line;
   var italic = $Native$Text.italic;
   var bold = $Native$Text.bold;
   var color = $Native$Text.color;
   var height = $Native$Text.height;
   var link = $Native$Text.link;
   var monospace = $Native$Text.monospace;
   var typeface = $Native$Text.typeface;
   var style = $Native$Text.style;
   var append = $Native$Text.append;
   var fromString = $Native$Text.fromString;
   var empty = fromString("");
   var concat = function (texts) {
      return A3($List.foldr,append,empty,texts);
   };
   var join = F2(function (seperator,texts) {
      return concat(A2($List.intersperse,seperator,texts));
   });
   var defaultStyle = {typeface: _U.list([])
                      ,height: $Maybe.Nothing
                      ,color: $Color.black
                      ,bold: false
                      ,italic: false
                      ,line: $Maybe.Nothing};
   var Style = F6(function (a,b,c,d,e,f) {
      return {typeface: a
             ,height: b
             ,color: c
             ,bold: d
             ,italic: e
             ,line: f};
   });
   var Through = {ctor: "Through"};
   var Over = {ctor: "Over"};
   var Under = {ctor: "Under"};
   var Text = {ctor: "Text"};
   return _elm.Text.values = {_op: _op
                             ,fromString: fromString
                             ,empty: empty
                             ,append: append
                             ,concat: concat
                             ,join: join
                             ,link: link
                             ,style: style
                             ,defaultStyle: defaultStyle
                             ,typeface: typeface
                             ,monospace: monospace
                             ,height: height
                             ,color: color
                             ,bold: bold
                             ,italic: italic
                             ,line: line
                             ,Style: Style
                             ,Under: Under
                             ,Over: Over
                             ,Through: Through};
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Element = Elm.Graphics.Element || {};
Elm.Graphics.Element.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Element = _elm.Graphics.Element || {};
   if (_elm.Graphics.Element.values)
   return _elm.Graphics.Element.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Graphics$Element = Elm.Native.Graphics.Element.make(_elm),
   $Text = Elm.Text.make(_elm);
   var _op = {};
   var DOut = {ctor: "DOut"};
   var outward = DOut;
   var DIn = {ctor: "DIn"};
   var inward = DIn;
   var DRight = {ctor: "DRight"};
   var right = DRight;
   var DLeft = {ctor: "DLeft"};
   var left = DLeft;
   var DDown = {ctor: "DDown"};
   var down = DDown;
   var DUp = {ctor: "DUp"};
   var up = DUp;
   var RawPosition = F4(function (a,b,c,d) {
      return {horizontal: a,vertical: b,x: c,y: d};
   });
   var Position = function (a) {
      return {ctor: "Position",_0: a};
   };
   var Relative = function (a) {
      return {ctor: "Relative",_0: a};
   };
   var relative = Relative;
   var Absolute = function (a) {
      return {ctor: "Absolute",_0: a};
   };
   var absolute = Absolute;
   var N = {ctor: "N"};
   var bottomLeft = Position({horizontal: N
                             ,vertical: N
                             ,x: Absolute(0)
                             ,y: Absolute(0)});
   var bottomLeftAt = F2(function (x,y) {
      return Position({horizontal: N,vertical: N,x: x,y: y});
   });
   var Z = {ctor: "Z"};
   var middle = Position({horizontal: Z
                         ,vertical: Z
                         ,x: Relative(0.5)
                         ,y: Relative(0.5)});
   var midLeft = Position({horizontal: N
                          ,vertical: Z
                          ,x: Absolute(0)
                          ,y: Relative(0.5)});
   var midBottom = Position({horizontal: Z
                            ,vertical: N
                            ,x: Relative(0.5)
                            ,y: Absolute(0)});
   var middleAt = F2(function (x,y) {
      return Position({horizontal: Z,vertical: Z,x: x,y: y});
   });
   var midLeftAt = F2(function (x,y) {
      return Position({horizontal: N,vertical: Z,x: x,y: y});
   });
   var midBottomAt = F2(function (x,y) {
      return Position({horizontal: Z,vertical: N,x: x,y: y});
   });
   var P = {ctor: "P"};
   var topLeft = Position({horizontal: N
                          ,vertical: P
                          ,x: Absolute(0)
                          ,y: Absolute(0)});
   var topRight = Position({horizontal: P
                           ,vertical: P
                           ,x: Absolute(0)
                           ,y: Absolute(0)});
   var bottomRight = Position({horizontal: P
                              ,vertical: N
                              ,x: Absolute(0)
                              ,y: Absolute(0)});
   var midRight = Position({horizontal: P
                           ,vertical: Z
                           ,x: Absolute(0)
                           ,y: Relative(0.5)});
   var midTop = Position({horizontal: Z
                         ,vertical: P
                         ,x: Relative(0.5)
                         ,y: Absolute(0)});
   var topLeftAt = F2(function (x,y) {
      return Position({horizontal: N,vertical: P,x: x,y: y});
   });
   var topRightAt = F2(function (x,y) {
      return Position({horizontal: P,vertical: P,x: x,y: y});
   });
   var bottomRightAt = F2(function (x,y) {
      return Position({horizontal: P,vertical: N,x: x,y: y});
   });
   var midRightAt = F2(function (x,y) {
      return Position({horizontal: P,vertical: Z,x: x,y: y});
   });
   var midTopAt = F2(function (x,y) {
      return Position({horizontal: Z,vertical: P,x: x,y: y});
   });
   var justified = $Native$Graphics$Element.block("justify");
   var centered = $Native$Graphics$Element.block("center");
   var rightAligned = $Native$Graphics$Element.block("right");
   var leftAligned = $Native$Graphics$Element.block("left");
   var show = function (value) {
      return leftAligned($Text.monospace($Text.fromString($Basics.toString(value))));
   };
   var Tiled = {ctor: "Tiled"};
   var Cropped = function (a) {
      return {ctor: "Cropped",_0: a};
   };
   var Fitted = {ctor: "Fitted"};
   var Plain = {ctor: "Plain"};
   var Custom = {ctor: "Custom"};
   var RawHtml = {ctor: "RawHtml"};
   var Spacer = {ctor: "Spacer"};
   var Flow = F2(function (a,b) {
      return {ctor: "Flow",_0: a,_1: b};
   });
   var Container = F2(function (a,b) {
      return {ctor: "Container",_0: a,_1: b};
   });
   var Image = F4(function (a,b,c,d) {
      return {ctor: "Image",_0: a,_1: b,_2: c,_3: d};
   });
   var newElement = $Native$Graphics$Element.newElement;
   var image = F3(function (w,h,src) {
      return A3(newElement,w,h,A4(Image,Plain,w,h,src));
   });
   var fittedImage = F3(function (w,h,src) {
      return A3(newElement,w,h,A4(Image,Fitted,w,h,src));
   });
   var croppedImage = F4(function (pos,w,h,src) {
      return A3(newElement,w,h,A4(Image,Cropped(pos),w,h,src));
   });
   var tiledImage = F3(function (w,h,src) {
      return A3(newElement,w,h,A4(Image,Tiled,w,h,src));
   });
   var container = F4(function (w,h,_p0,e) {
      var _p1 = _p0;
      return A3(newElement,w,h,A2(Container,_p1._0,e));
   });
   var spacer = F2(function (w,h) {
      return A3(newElement,w,h,Spacer);
   });
   var sizeOf = function (_p2) {
      var _p3 = _p2;
      var _p4 = _p3._0;
      return {ctor: "_Tuple2"
             ,_0: _p4.props.width
             ,_1: _p4.props.height};
   };
   var heightOf = function (_p5) {
      var _p6 = _p5;
      return _p6._0.props.height;
   };
   var widthOf = function (_p7) {
      var _p8 = _p7;
      return _p8._0.props.width;
   };
   var above = F2(function (hi,lo) {
      return A3(newElement,
      A2($Basics.max,widthOf(hi),widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,DDown,_U.list([hi,lo])));
   });
   var below = F2(function (lo,hi) {
      return A3(newElement,
      A2($Basics.max,widthOf(hi),widthOf(lo)),
      heightOf(hi) + heightOf(lo),
      A2(Flow,DDown,_U.list([hi,lo])));
   });
   var beside = F2(function (lft,rht) {
      return A3(newElement,
      widthOf(lft) + widthOf(rht),
      A2($Basics.max,heightOf(lft),heightOf(rht)),
      A2(Flow,right,_U.list([lft,rht])));
   });
   var layers = function (es) {
      var hs = A2($List.map,heightOf,es);
      var ws = A2($List.map,widthOf,es);
      return A3(newElement,
      A2($Maybe.withDefault,0,$List.maximum(ws)),
      A2($Maybe.withDefault,0,$List.maximum(hs)),
      A2(Flow,DOut,es));
   };
   var empty = A2(spacer,0,0);
   var flow = F2(function (dir,es) {
      var newFlow = F2(function (w,h) {
         return A3(newElement,w,h,A2(Flow,dir,es));
      });
      var maxOrZero = function (list) {
         return A2($Maybe.withDefault,0,$List.maximum(list));
      };
      var hs = A2($List.map,heightOf,es);
      var ws = A2($List.map,widthOf,es);
      if (_U.eq(es,_U.list([]))) return empty; else {
            var _p9 = dir;
            switch (_p9.ctor)
            {case "DUp": return A2(newFlow,maxOrZero(ws),$List.sum(hs));
               case "DDown": return A2(newFlow,maxOrZero(ws),$List.sum(hs));
               case "DLeft": return A2(newFlow,$List.sum(ws),maxOrZero(hs));
               case "DRight": return A2(newFlow,$List.sum(ws),maxOrZero(hs));
               case "DIn": return A2(newFlow,maxOrZero(ws),maxOrZero(hs));
               default: return A2(newFlow,maxOrZero(ws),maxOrZero(hs));}
         }
   });
   var Properties = F9(function (a,b,c,d,e,f,g,h,i) {
      return {id: a
             ,width: b
             ,height: c
             ,opacity: d
             ,color: e
             ,href: f
             ,tag: g
             ,hover: h
             ,click: i};
   });
   var Element_elm_builtin = function (a) {
      return {ctor: "Element_elm_builtin",_0: a};
   };
   var width = F2(function (newWidth,_p10) {
      var _p11 = _p10;
      var _p14 = _p11._0.props;
      var _p13 = _p11._0.element;
      var newHeight = function () {
         var _p12 = _p13;
         switch (_p12.ctor)
         {case "Image":
            return $Basics.round($Basics.toFloat(_p12._2) / $Basics.toFloat(_p12._1) * $Basics.toFloat(newWidth));
            case "RawHtml":
            return $Basics.snd(A2($Native$Graphics$Element.htmlHeight,
              newWidth,
              _p13));
            default: return _p14.height;}
      }();
      return Element_elm_builtin({element: _p13
                                 ,props: _U.update(_p14,{width: newWidth,height: newHeight})});
   });
   var height = F2(function (newHeight,_p15) {
      var _p16 = _p15;
      return Element_elm_builtin({element: _p16._0.element
                                 ,props: _U.update(_p16._0.props,{height: newHeight})});
   });
   var size = F3(function (w,h,e) {
      return A2(height,h,A2(width,w,e));
   });
   var opacity = F2(function (givenOpacity,_p17) {
      var _p18 = _p17;
      return Element_elm_builtin({element: _p18._0.element
                                 ,props: _U.update(_p18._0.props,{opacity: givenOpacity})});
   });
   var color = F2(function (clr,_p19) {
      var _p20 = _p19;
      return Element_elm_builtin({element: _p20._0.element
                                 ,props: _U.update(_p20._0.props,{color: $Maybe.Just(clr)})});
   });
   var tag = F2(function (name,_p21) {
      var _p22 = _p21;
      return Element_elm_builtin({element: _p22._0.element
                                 ,props: _U.update(_p22._0.props,{tag: name})});
   });
   var link = F2(function (href,_p23) {
      var _p24 = _p23;
      return Element_elm_builtin({element: _p24._0.element
                                 ,props: _U.update(_p24._0.props,{href: href})});
   });
   return _elm.Graphics.Element.values = {_op: _op
                                         ,image: image
                                         ,fittedImage: fittedImage
                                         ,croppedImage: croppedImage
                                         ,tiledImage: tiledImage
                                         ,leftAligned: leftAligned
                                         ,rightAligned: rightAligned
                                         ,centered: centered
                                         ,justified: justified
                                         ,show: show
                                         ,width: width
                                         ,height: height
                                         ,size: size
                                         ,color: color
                                         ,opacity: opacity
                                         ,link: link
                                         ,tag: tag
                                         ,widthOf: widthOf
                                         ,heightOf: heightOf
                                         ,sizeOf: sizeOf
                                         ,flow: flow
                                         ,up: up
                                         ,down: down
                                         ,left: left
                                         ,right: right
                                         ,inward: inward
                                         ,outward: outward
                                         ,layers: layers
                                         ,above: above
                                         ,below: below
                                         ,beside: beside
                                         ,empty: empty
                                         ,spacer: spacer
                                         ,container: container
                                         ,middle: middle
                                         ,midTop: midTop
                                         ,midBottom: midBottom
                                         ,midLeft: midLeft
                                         ,midRight: midRight
                                         ,topLeft: topLeft
                                         ,topRight: topRight
                                         ,bottomLeft: bottomLeft
                                         ,bottomRight: bottomRight
                                         ,absolute: absolute
                                         ,relative: relative
                                         ,middleAt: middleAt
                                         ,midTopAt: midTopAt
                                         ,midBottomAt: midBottomAt
                                         ,midLeftAt: midLeftAt
                                         ,midRightAt: midRightAt
                                         ,topLeftAt: topLeftAt
                                         ,topRightAt: topRightAt
                                         ,bottomLeftAt: bottomLeftAt
                                         ,bottomRightAt: bottomRightAt};
};
Elm.Graphics = Elm.Graphics || {};
Elm.Graphics.Collage = Elm.Graphics.Collage || {};
Elm.Graphics.Collage.make = function (_elm) {
   "use strict";
   _elm.Graphics = _elm.Graphics || {};
   _elm.Graphics.Collage = _elm.Graphics.Collage || {};
   if (_elm.Graphics.Collage.values)
   return _elm.Graphics.Collage.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Native$Graphics$Collage = Elm.Native.Graphics.Collage.make(_elm),
   $Text = Elm.Text.make(_elm),
   $Transform2D = Elm.Transform2D.make(_elm);
   var _op = {};
   var Shape = function (a) {    return {ctor: "Shape",_0: a};};
   var polygon = function (points) {    return Shape(points);};
   var rect = F2(function (w,h) {
      var hh = h / 2;
      var hw = w / 2;
      return Shape(_U.list([{ctor: "_Tuple2",_0: 0 - hw,_1: 0 - hh}
                           ,{ctor: "_Tuple2",_0: 0 - hw,_1: hh}
                           ,{ctor: "_Tuple2",_0: hw,_1: hh}
                           ,{ctor: "_Tuple2",_0: hw,_1: 0 - hh}]));
   });
   var square = function (n) {    return A2(rect,n,n);};
   var oval = F2(function (w,h) {
      var hh = h / 2;
      var hw = w / 2;
      var n = 50;
      var t = 2 * $Basics.pi / n;
      var f = function (i) {
         return {ctor: "_Tuple2"
                ,_0: hw * $Basics.cos(t * i)
                ,_1: hh * $Basics.sin(t * i)};
      };
      return Shape(A2($List.map,f,_U.range(0,n - 1)));
   });
   var circle = function (r) {    return A2(oval,2 * r,2 * r);};
   var ngon = F2(function (n,r) {
      var m = $Basics.toFloat(n);
      var t = 2 * $Basics.pi / m;
      var f = function (i) {
         return {ctor: "_Tuple2"
                ,_0: r * $Basics.cos(t * i)
                ,_1: r * $Basics.sin(t * i)};
      };
      return Shape(A2($List.map,f,_U.range(0,m - 1)));
   });
   var Path = function (a) {    return {ctor: "Path",_0: a};};
   var path = function (ps) {    return Path(ps);};
   var segment = F2(function (p1,p2) {
      return Path(_U.list([p1,p2]));
   });
   var collage = $Native$Graphics$Collage.collage;
   var Fill = function (a) {    return {ctor: "Fill",_0: a};};
   var Line = function (a) {    return {ctor: "Line",_0: a};};
   var FGroup = F2(function (a,b) {
      return {ctor: "FGroup",_0: a,_1: b};
   });
   var FElement = function (a) {
      return {ctor: "FElement",_0: a};
   };
   var FImage = F4(function (a,b,c,d) {
      return {ctor: "FImage",_0: a,_1: b,_2: c,_3: d};
   });
   var FText = function (a) {    return {ctor: "FText",_0: a};};
   var FOutlinedText = F2(function (a,b) {
      return {ctor: "FOutlinedText",_0: a,_1: b};
   });
   var FShape = F2(function (a,b) {
      return {ctor: "FShape",_0: a,_1: b};
   });
   var FPath = F2(function (a,b) {
      return {ctor: "FPath",_0: a,_1: b};
   });
   var LineStyle = F6(function (a,b,c,d,e,f) {
      return {color: a
             ,width: b
             ,cap: c
             ,join: d
             ,dashing: e
             ,dashOffset: f};
   });
   var Clipped = {ctor: "Clipped"};
   var Sharp = function (a) {    return {ctor: "Sharp",_0: a};};
   var Smooth = {ctor: "Smooth"};
   var Padded = {ctor: "Padded"};
   var Round = {ctor: "Round"};
   var Flat = {ctor: "Flat"};
   var defaultLine = {color: $Color.black
                     ,width: 1
                     ,cap: Flat
                     ,join: Sharp(10)
                     ,dashing: _U.list([])
                     ,dashOffset: 0};
   var solid = function (clr) {
      return _U.update(defaultLine,{color: clr});
   };
   var dashed = function (clr) {
      return _U.update(defaultLine,
      {color: clr,dashing: _U.list([8,4])});
   };
   var dotted = function (clr) {
      return _U.update(defaultLine,
      {color: clr,dashing: _U.list([3,3])});
   };
   var Grad = function (a) {    return {ctor: "Grad",_0: a};};
   var Texture = function (a) {
      return {ctor: "Texture",_0: a};
   };
   var Solid = function (a) {    return {ctor: "Solid",_0: a};};
   var Form_elm_builtin = function (a) {
      return {ctor: "Form_elm_builtin",_0: a};
   };
   var form = function (f) {
      return Form_elm_builtin({theta: 0
                              ,scale: 1
                              ,x: 0
                              ,y: 0
                              ,alpha: 1
                              ,form: f});
   };
   var fill = F2(function (style,_p0) {
      var _p1 = _p0;
      return form(A2(FShape,Fill(style),_p1._0));
   });
   var filled = F2(function (color,shape) {
      return A2(fill,Solid(color),shape);
   });
   var textured = F2(function (src,shape) {
      return A2(fill,Texture(src),shape);
   });
   var gradient = F2(function (grad,shape) {
      return A2(fill,Grad(grad),shape);
   });
   var outlined = F2(function (style,_p2) {
      var _p3 = _p2;
      return form(A2(FShape,Line(style),_p3._0));
   });
   var traced = F2(function (style,_p4) {
      var _p5 = _p4;
      return form(A2(FPath,style,_p5._0));
   });
   var sprite = F4(function (w,h,pos,src) {
      return form(A4(FImage,w,h,pos,src));
   });
   var toForm = function (e) {    return form(FElement(e));};
   var group = function (fs) {
      return form(A2(FGroup,$Transform2D.identity,fs));
   };
   var groupTransform = F2(function (matrix,fs) {
      return form(A2(FGroup,matrix,fs));
   });
   var text = function (t) {    return form(FText(t));};
   var outlinedText = F2(function (ls,t) {
      return form(A2(FOutlinedText,ls,t));
   });
   var move = F2(function (_p7,_p6) {
      var _p8 = _p7;
      var _p9 = _p6;
      var _p10 = _p9._0;
      return Form_elm_builtin(_U.update(_p10,
      {x: _p10.x + _p8._0,y: _p10.y + _p8._1}));
   });
   var moveX = F2(function (x,_p11) {
      var _p12 = _p11;
      var _p13 = _p12._0;
      return Form_elm_builtin(_U.update(_p13,{x: _p13.x + x}));
   });
   var moveY = F2(function (y,_p14) {
      var _p15 = _p14;
      var _p16 = _p15._0;
      return Form_elm_builtin(_U.update(_p16,{y: _p16.y + y}));
   });
   var scale = F2(function (s,_p17) {
      var _p18 = _p17;
      var _p19 = _p18._0;
      return Form_elm_builtin(_U.update(_p19,
      {scale: _p19.scale * s}));
   });
   var rotate = F2(function (t,_p20) {
      var _p21 = _p20;
      var _p22 = _p21._0;
      return Form_elm_builtin(_U.update(_p22,
      {theta: _p22.theta + t}));
   });
   var alpha = F2(function (a,_p23) {
      var _p24 = _p23;
      return Form_elm_builtin(_U.update(_p24._0,{alpha: a}));
   });
   return _elm.Graphics.Collage.values = {_op: _op
                                         ,collage: collage
                                         ,toForm: toForm
                                         ,filled: filled
                                         ,textured: textured
                                         ,gradient: gradient
                                         ,outlined: outlined
                                         ,traced: traced
                                         ,text: text
                                         ,outlinedText: outlinedText
                                         ,move: move
                                         ,moveX: moveX
                                         ,moveY: moveY
                                         ,scale: scale
                                         ,rotate: rotate
                                         ,alpha: alpha
                                         ,group: group
                                         ,groupTransform: groupTransform
                                         ,rect: rect
                                         ,oval: oval
                                         ,square: square
                                         ,circle: circle
                                         ,ngon: ngon
                                         ,polygon: polygon
                                         ,segment: segment
                                         ,path: path
                                         ,solid: solid
                                         ,dashed: dashed
                                         ,dotted: dotted
                                         ,defaultLine: defaultLine
                                         ,LineStyle: LineStyle
                                         ,Flat: Flat
                                         ,Round: Round
                                         ,Padded: Padded
                                         ,Smooth: Smooth
                                         ,Sharp: Sharp
                                         ,Clipped: Clipped};
};
Elm.Native.Debug = {};
Elm.Native.Debug.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Debug = localRuntime.Native.Debug || {};
	if (localRuntime.Native.Debug.values)
	{
		return localRuntime.Native.Debug.values;
	}

	var toString = Elm.Native.Utils.make(localRuntime).toString;

	function log(tag, value)
	{
		var msg = tag + ': ' + toString(value);
		var process = process || {};
		if (process.stdout)
		{
			process.stdout.write(msg);
		}
		else
		{
			console.log(msg);
		}
		return value;
	}

	function crash(message)
	{
		throw new Error(message);
	}

	function tracePath(tag, form)
	{
		if (localRuntime.debug)
		{
			return localRuntime.debug.trace(tag, form);
		}
		return form;
	}

	function watch(tag, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, value);
		}
		return value;
	}

	function watchSummary(tag, summarize, value)
	{
		if (localRuntime.debug)
		{
			localRuntime.debug.watch(tag, summarize(value));
		}
		return value;
	}

	return localRuntime.Native.Debug.values = {
		crash: crash,
		tracePath: F2(tracePath),
		log: F2(log),
		watch: F2(watch),
		watchSummary: F3(watchSummary)
	};
};

Elm.Debug = Elm.Debug || {};
Elm.Debug.make = function (_elm) {
   "use strict";
   _elm.Debug = _elm.Debug || {};
   if (_elm.Debug.values) return _elm.Debug.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Graphics$Collage = Elm.Graphics.Collage.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm);
   var _op = {};
   var trace = $Native$Debug.tracePath;
   var watchSummary = $Native$Debug.watchSummary;
   var watch = $Native$Debug.watch;
   var crash = $Native$Debug.crash;
   var log = $Native$Debug.log;
   return _elm.Debug.values = {_op: _op
                              ,log: log
                              ,crash: crash
                              ,watch: watch
                              ,watchSummary: watchSummary
                              ,trace: trace};
};
Elm.Result = Elm.Result || {};
Elm.Result.make = function (_elm) {
   "use strict";
   _elm.Result = _elm.Result || {};
   if (_elm.Result.values) return _elm.Result.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Maybe = Elm.Maybe.make(_elm);
   var _op = {};
   var toMaybe = function (result) {
      var _p0 = result;
      if (_p0.ctor === "Ok") {
            return $Maybe.Just(_p0._0);
         } else {
            return $Maybe.Nothing;
         }
   };
   var withDefault = F2(function (def,result) {
      var _p1 = result;
      if (_p1.ctor === "Ok") {
            return _p1._0;
         } else {
            return def;
         }
   });
   var Err = function (a) {    return {ctor: "Err",_0: a};};
   var andThen = F2(function (result,callback) {
      var _p2 = result;
      if (_p2.ctor === "Ok") {
            return callback(_p2._0);
         } else {
            return Err(_p2._0);
         }
   });
   var Ok = function (a) {    return {ctor: "Ok",_0: a};};
   var map = F2(function (func,ra) {
      var _p3 = ra;
      if (_p3.ctor === "Ok") {
            return Ok(func(_p3._0));
         } else {
            return Err(_p3._0);
         }
   });
   var map2 = F3(function (func,ra,rb) {
      var _p4 = {ctor: "_Tuple2",_0: ra,_1: rb};
      if (_p4._0.ctor === "Ok") {
            if (_p4._1.ctor === "Ok") {
                  return Ok(A2(func,_p4._0._0,_p4._1._0));
               } else {
                  return Err(_p4._1._0);
               }
         } else {
            return Err(_p4._0._0);
         }
   });
   var map3 = F4(function (func,ra,rb,rc) {
      var _p5 = {ctor: "_Tuple3",_0: ra,_1: rb,_2: rc};
      if (_p5._0.ctor === "Ok") {
            if (_p5._1.ctor === "Ok") {
                  if (_p5._2.ctor === "Ok") {
                        return Ok(A3(func,_p5._0._0,_p5._1._0,_p5._2._0));
                     } else {
                        return Err(_p5._2._0);
                     }
               } else {
                  return Err(_p5._1._0);
               }
         } else {
            return Err(_p5._0._0);
         }
   });
   var map4 = F5(function (func,ra,rb,rc,rd) {
      var _p6 = {ctor: "_Tuple4",_0: ra,_1: rb,_2: rc,_3: rd};
      if (_p6._0.ctor === "Ok") {
            if (_p6._1.ctor === "Ok") {
                  if (_p6._2.ctor === "Ok") {
                        if (_p6._3.ctor === "Ok") {
                              return Ok(A4(func,_p6._0._0,_p6._1._0,_p6._2._0,_p6._3._0));
                           } else {
                              return Err(_p6._3._0);
                           }
                     } else {
                        return Err(_p6._2._0);
                     }
               } else {
                  return Err(_p6._1._0);
               }
         } else {
            return Err(_p6._0._0);
         }
   });
   var map5 = F6(function (func,ra,rb,rc,rd,re) {
      var _p7 = {ctor: "_Tuple5"
                ,_0: ra
                ,_1: rb
                ,_2: rc
                ,_3: rd
                ,_4: re};
      if (_p7._0.ctor === "Ok") {
            if (_p7._1.ctor === "Ok") {
                  if (_p7._2.ctor === "Ok") {
                        if (_p7._3.ctor === "Ok") {
                              if (_p7._4.ctor === "Ok") {
                                    return Ok(A5(func,
                                    _p7._0._0,
                                    _p7._1._0,
                                    _p7._2._0,
                                    _p7._3._0,
                                    _p7._4._0));
                                 } else {
                                    return Err(_p7._4._0);
                                 }
                           } else {
                              return Err(_p7._3._0);
                           }
                     } else {
                        return Err(_p7._2._0);
                     }
               } else {
                  return Err(_p7._1._0);
               }
         } else {
            return Err(_p7._0._0);
         }
   });
   var formatError = F2(function (f,result) {
      var _p8 = result;
      if (_p8.ctor === "Ok") {
            return Ok(_p8._0);
         } else {
            return Err(f(_p8._0));
         }
   });
   var fromMaybe = F2(function (err,maybe) {
      var _p9 = maybe;
      if (_p9.ctor === "Just") {
            return Ok(_p9._0);
         } else {
            return Err(err);
         }
   });
   return _elm.Result.values = {_op: _op
                               ,withDefault: withDefault
                               ,map: map
                               ,map2: map2
                               ,map3: map3
                               ,map4: map4
                               ,map5: map5
                               ,andThen: andThen
                               ,toMaybe: toMaybe
                               ,fromMaybe: fromMaybe
                               ,formatError: formatError
                               ,Ok: Ok
                               ,Err: Err};
};
Elm.Native.Signal = {};

Elm.Native.Signal.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Signal = localRuntime.Native.Signal || {};
	if (localRuntime.Native.Signal.values)
	{
		return localRuntime.Native.Signal.values;
	}


	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function broadcastToKids(node, timestamp, update)
	{
		var kids = node.kids;
		for (var i = kids.length; i--; )
		{
			kids[i].notify(timestamp, update, node.id);
		}
	}


	// INPUT

	function input(name, base)
	{
		var node = {
			id: Utils.guid(),
			name: 'input-' + name,
			value: base,
			parents: [],
			kids: []
		};

		node.notify = function(timestamp, targetId, value) {
			var update = targetId === node.id;
			if (update)
			{
				node.value = value;
			}
			broadcastToKids(node, timestamp, update);
			return update;
		};

		localRuntime.inputs.push(node);

		return node;
	}

	function constant(value)
	{
		return input('constant', value);
	}


	// MAILBOX

	function mailbox(base)
	{
		var signal = input('mailbox', base);

		function send(value) {
			return Task.asyncFunction(function(callback) {
				localRuntime.setTimeout(function() {
					localRuntime.notify(signal.id, value);
				}, 0);
				callback(Task.succeed(Utils.Tuple0));
			});
		}

		return {
			signal: signal,
			address: {
				ctor: 'Address',
				_0: send
			}
		};
	}

	function sendMessage(message)
	{
		Task.perform(message._0);
	}


	// OUTPUT

	function output(name, handler, parent)
	{
		var node = {
			id: Utils.guid(),
			name: 'output-' + name,
			parents: [parent],
			isOutput: true
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				handler(parent.value);
			}
		};

		parent.kids.push(node);

		return node;
	}


	// MAP

	function mapMany(refreshValue, args)
	{
		var node = {
			id: Utils.guid(),
			name: 'map' + args.length,
			value: refreshValue(),
			parents: args,
			kids: []
		};

		var numberOfParents = args.length;
		var count = 0;
		var update = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			++count;

			update = update || parentUpdate;

			if (count === numberOfParents)
			{
				if (update)
				{
					node.value = refreshValue();
				}
				broadcastToKids(node, timestamp, update);
				update = false;
				count = 0;
			}
		};

		for (var i = numberOfParents; i--; )
		{
			args[i].kids.push(node);
		}

		return node;
	}


	function map(func, a)
	{
		function refreshValue()
		{
			return func(a.value);
		}
		return mapMany(refreshValue, [a]);
	}


	function map2(func, a, b)
	{
		function refreshValue()
		{
			return A2( func, a.value, b.value );
		}
		return mapMany(refreshValue, [a, b]);
	}


	function map3(func, a, b, c)
	{
		function refreshValue()
		{
			return A3( func, a.value, b.value, c.value );
		}
		return mapMany(refreshValue, [a, b, c]);
	}


	function map4(func, a, b, c, d)
	{
		function refreshValue()
		{
			return A4( func, a.value, b.value, c.value, d.value );
		}
		return mapMany(refreshValue, [a, b, c, d]);
	}


	function map5(func, a, b, c, d, e)
	{
		function refreshValue()
		{
			return A5( func, a.value, b.value, c.value, d.value, e.value );
		}
		return mapMany(refreshValue, [a, b, c, d, e]);
	}


	// FOLD

	function foldp(update, state, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'foldp',
			parents: [signal],
			kids: [],
			value: state
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = A2( update, signal.value, node.value );
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	// TIME

	function timestamp(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'timestamp',
			value: Utils.Tuple2(localRuntime.timer.programStart, signal.value),
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentUpdate)
			{
				node.value = Utils.Tuple2(timestamp, signal.value);
			}
			broadcastToKids(node, timestamp, parentUpdate);
		};

		signal.kids.push(node);

		return node;
	}


	function delay(time, signal)
	{
		var delayed = input('delay-input-' + time, signal.value);

		function handler(value)
		{
			setTimeout(function() {
				localRuntime.notify(delayed.id, value);
			}, time);
		}

		output('delay-output-' + time, handler, signal);

		return delayed;
	}


	// MERGING

	function genericMerge(tieBreaker, leftStream, rightStream)
	{
		var node = {
			id: Utils.guid(),
			name: 'merge',
			value: A2(tieBreaker, leftStream.value, rightStream.value),
			parents: [leftStream, rightStream],
			kids: []
		};

		var left = { touched: false, update: false, value: null };
		var right = { touched: false, update: false, value: null };

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === leftStream.id)
			{
				left.touched = true;
				left.update = parentUpdate;
				left.value = leftStream.value;
			}
			if (parentID === rightStream.id)
			{
				right.touched = true;
				right.update = parentUpdate;
				right.value = rightStream.value;
			}

			if (left.touched && right.touched)
			{
				var update = false;
				if (left.update && right.update)
				{
					node.value = A2(tieBreaker, left.value, right.value);
					update = true;
				}
				else if (left.update)
				{
					node.value = left.value;
					update = true;
				}
				else if (right.update)
				{
					node.value = right.value;
					update = true;
				}
				left.touched = false;
				right.touched = false;

				broadcastToKids(node, timestamp, update);
			}
		};

		leftStream.kids.push(node);
		rightStream.kids.push(node);

		return node;
	}


	// FILTERING

	function filterMap(toMaybe, base, signal)
	{
		var maybe = toMaybe(signal.value);
		var node = {
			id: Utils.guid(),
			name: 'filterMap',
			value: maybe.ctor === 'Nothing' ? base : maybe._0,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate)
			{
				var maybe = toMaybe(signal.value);
				if (maybe.ctor === 'Just')
				{
					update = true;
					node.value = maybe._0;
				}
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	// SAMPLING

	function sampleOn(ticker, signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'sampleOn',
			value: signal.value,
			parents: [ticker, signal],
			kids: []
		};

		var signalTouch = false;
		var tickerTouch = false;
		var tickerUpdate = false;

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			if (parentID === ticker.id)
			{
				tickerTouch = true;
				tickerUpdate = parentUpdate;
			}
			if (parentID === signal.id)
			{
				signalTouch = true;
			}

			if (tickerTouch && signalTouch)
			{
				if (tickerUpdate)
				{
					node.value = signal.value;
				}
				tickerTouch = false;
				signalTouch = false;

				broadcastToKids(node, timestamp, tickerUpdate);
			}
		};

		ticker.kids.push(node);
		signal.kids.push(node);

		return node;
	}


	// DROP REPEATS

	function dropRepeats(signal)
	{
		var node = {
			id: Utils.guid(),
			name: 'dropRepeats',
			value: signal.value,
			parents: [signal],
			kids: []
		};

		node.notify = function(timestamp, parentUpdate, parentID)
		{
			var update = false;
			if (parentUpdate && !Utils.eq(node.value, signal.value))
			{
				node.value = signal.value;
				update = true;
			}
			broadcastToKids(node, timestamp, update);
		};

		signal.kids.push(node);

		return node;
	}


	return localRuntime.Native.Signal.values = {
		input: input,
		constant: constant,
		mailbox: mailbox,
		sendMessage: sendMessage,
		output: output,
		map: F2(map),
		map2: F3(map2),
		map3: F4(map3),
		map4: F5(map4),
		map5: F6(map5),
		foldp: F3(foldp),
		genericMerge: F3(genericMerge),
		filterMap: F3(filterMap),
		sampleOn: F2(sampleOn),
		dropRepeats: dropRepeats,
		timestamp: timestamp,
		delay: F2(delay)
	};
};

Elm.Native.Task = {};

Elm.Native.Task.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Task = localRuntime.Native.Task || {};
	if (localRuntime.Native.Task.values)
	{
		return localRuntime.Native.Task.values;
	}

	var Result = Elm.Result.make(localRuntime);
	var Signal;
	var Utils = Elm.Native.Utils.make(localRuntime);


	// CONSTRUCTORS

	function succeed(value)
	{
		return {
			tag: 'Succeed',
			value: value
		};
	}

	function fail(error)
	{
		return {
			tag: 'Fail',
			value: error
		};
	}

	function asyncFunction(func)
	{
		return {
			tag: 'Async',
			asyncFunction: func
		};
	}

	function andThen(task, callback)
	{
		return {
			tag: 'AndThen',
			task: task,
			callback: callback
		};
	}

	function catch_(task, callback)
	{
		return {
			tag: 'Catch',
			task: task,
			callback: callback
		};
	}


	// RUNNER

	function perform(task) {
		runTask({ task: task }, function() {});
	}

	function performSignal(name, signal)
	{
		var workQueue = [];

		function onComplete()
		{
			workQueue.shift();

			if (workQueue.length > 0)
			{
				var task = workQueue[0];

				setTimeout(function() {
					runTask(task, onComplete);
				}, 0);
			}
		}

		function register(task)
		{
			var root = { task: task };
			workQueue.push(root);
			if (workQueue.length === 1)
			{
				runTask(root, onComplete);
			}
		}

		if (!Signal)
		{
			Signal = Elm.Native.Signal.make(localRuntime);
		}
		Signal.output('perform-tasks-' + name, register, signal);

		register(signal.value);

		return signal;
	}

	function mark(status, task)
	{
		return { status: status, task: task };
	}

	function runTask(root, onComplete)
	{
		var result = mark('runnable', root.task);
		while (result.status === 'runnable')
		{
			result = stepTask(onComplete, root, result.task);
		}

		if (result.status === 'done')
		{
			root.task = result.task;
			onComplete();
		}

		if (result.status === 'blocked')
		{
			root.task = result.task;
		}
	}

	function stepTask(onComplete, root, task)
	{
		var tag = task.tag;

		if (tag === 'Succeed' || tag === 'Fail')
		{
			return mark('done', task);
		}

		if (tag === 'Async')
		{
			var placeHolder = {};
			var couldBeSync = true;
			var wasSync = false;

			task.asyncFunction(function(result) {
				placeHolder.tag = result.tag;
				placeHolder.value = result.value;
				if (couldBeSync)
				{
					wasSync = true;
				}
				else
				{
					runTask(root, onComplete);
				}
			});
			couldBeSync = false;
			return mark(wasSync ? 'done' : 'blocked', placeHolder);
		}

		if (tag === 'AndThen' || tag === 'Catch')
		{
			var result = mark('runnable', task.task);
			while (result.status === 'runnable')
			{
				result = stepTask(onComplete, root, result.task);
			}

			if (result.status === 'done')
			{
				var activeTask = result.task;
				var activeTag = activeTask.tag;

				var succeedChain = activeTag === 'Succeed' && tag === 'AndThen';
				var failChain = activeTag === 'Fail' && tag === 'Catch';

				return (succeedChain || failChain)
					? mark('runnable', task.callback(activeTask.value))
					: mark('runnable', activeTask);
			}
			if (result.status === 'blocked')
			{
				return mark('blocked', {
					tag: tag,
					task: result.task,
					callback: task.callback
				});
			}
		}
	}


	// THREADS

	function sleep(time) {
		return asyncFunction(function(callback) {
			setTimeout(function() {
				callback(succeed(Utils.Tuple0));
			}, time);
		});
	}

	function spawn(task) {
		return asyncFunction(function(callback) {
			var id = setTimeout(function() {
				perform(task);
			}, 0);
			callback(succeed(id));
		});
	}


	return localRuntime.Native.Task.values = {
		succeed: succeed,
		fail: fail,
		asyncFunction: asyncFunction,
		andThen: F2(andThen),
		catch_: F2(catch_),
		perform: perform,
		performSignal: performSignal,
		spawn: spawn,
		sleep: sleep
	};
};

Elm.Task = Elm.Task || {};
Elm.Task.make = function (_elm) {
   "use strict";
   _elm.Task = _elm.Task || {};
   if (_elm.Task.values) return _elm.Task.values;
   var _U = Elm.Native.Utils.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Task = Elm.Native.Task.make(_elm),
   $Result = Elm.Result.make(_elm);
   var _op = {};
   var sleep = $Native$Task.sleep;
   var spawn = $Native$Task.spawn;
   var ThreadID = function (a) {
      return {ctor: "ThreadID",_0: a};
   };
   var onError = $Native$Task.catch_;
   var andThen = $Native$Task.andThen;
   var fail = $Native$Task.fail;
   var mapError = F2(function (f,task) {
      return A2(onError,
      task,
      function (err) {
         return fail(f(err));
      });
   });
   var succeed = $Native$Task.succeed;
   var map = F2(function (func,taskA) {
      return A2(andThen,
      taskA,
      function (a) {
         return succeed(func(a));
      });
   });
   var map2 = F3(function (func,taskA,taskB) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return succeed(A2(func,a,b));
         });
      });
   });
   var map3 = F4(function (func,taskA,taskB,taskC) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return A2(andThen,
            taskC,
            function (c) {
               return succeed(A3(func,a,b,c));
            });
         });
      });
   });
   var map4 = F5(function (func,taskA,taskB,taskC,taskD) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return A2(andThen,
            taskC,
            function (c) {
               return A2(andThen,
               taskD,
               function (d) {
                  return succeed(A4(func,a,b,c,d));
               });
            });
         });
      });
   });
   var map5 = F6(function (func,taskA,taskB,taskC,taskD,taskE) {
      return A2(andThen,
      taskA,
      function (a) {
         return A2(andThen,
         taskB,
         function (b) {
            return A2(andThen,
            taskC,
            function (c) {
               return A2(andThen,
               taskD,
               function (d) {
                  return A2(andThen,
                  taskE,
                  function (e) {
                     return succeed(A5(func,a,b,c,d,e));
                  });
               });
            });
         });
      });
   });
   var andMap = F2(function (taskFunc,taskValue) {
      return A2(andThen,
      taskFunc,
      function (func) {
         return A2(andThen,
         taskValue,
         function (value) {
            return succeed(func(value));
         });
      });
   });
   var sequence = function (tasks) {
      var _p0 = tasks;
      if (_p0.ctor === "[]") {
            return succeed(_U.list([]));
         } else {
            return A3(map2,
            F2(function (x,y) {    return A2($List._op["::"],x,y);}),
            _p0._0,
            sequence(_p0._1));
         }
   };
   var toMaybe = function (task) {
      return A2(onError,
      A2(map,$Maybe.Just,task),
      function (_p1) {
         return succeed($Maybe.Nothing);
      });
   };
   var fromMaybe = F2(function ($default,maybe) {
      var _p2 = maybe;
      if (_p2.ctor === "Just") {
            return succeed(_p2._0);
         } else {
            return fail($default);
         }
   });
   var toResult = function (task) {
      return A2(onError,
      A2(map,$Result.Ok,task),
      function (msg) {
         return succeed($Result.Err(msg));
      });
   };
   var fromResult = function (result) {
      var _p3 = result;
      if (_p3.ctor === "Ok") {
            return succeed(_p3._0);
         } else {
            return fail(_p3._0);
         }
   };
   var Task = {ctor: "Task"};
   return _elm.Task.values = {_op: _op
                             ,succeed: succeed
                             ,fail: fail
                             ,map: map
                             ,map2: map2
                             ,map3: map3
                             ,map4: map4
                             ,map5: map5
                             ,andMap: andMap
                             ,sequence: sequence
                             ,andThen: andThen
                             ,onError: onError
                             ,mapError: mapError
                             ,toMaybe: toMaybe
                             ,fromMaybe: fromMaybe
                             ,toResult: toResult
                             ,fromResult: fromResult
                             ,spawn: spawn
                             ,sleep: sleep};
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   if (_elm.Signal.values) return _elm.Signal.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var send = F2(function (_p0,value) {
      var _p1 = _p0;
      return A2($Task.onError,
      _p1._0(value),
      function (_p2) {
         return $Task.succeed({ctor: "_Tuple0"});
      });
   });
   var Message = function (a) {
      return {ctor: "Message",_0: a};
   };
   var message = F2(function (_p3,value) {
      var _p4 = _p3;
      return Message(_p4._0(value));
   });
   var mailbox = $Native$Signal.mailbox;
   var Address = function (a) {
      return {ctor: "Address",_0: a};
   };
   var forwardTo = F2(function (_p5,f) {
      var _p6 = _p5;
      return Address(function (x) {    return _p6._0(f(x));});
   });
   var Mailbox = F2(function (a,b) {
      return {address: a,signal: b};
   });
   var sampleOn = $Native$Signal.sampleOn;
   var dropRepeats = $Native$Signal.dropRepeats;
   var filterMap = $Native$Signal.filterMap;
   var filter = F3(function (isOk,base,signal) {
      return A3(filterMap,
      function (value) {
         return isOk(value) ? $Maybe.Just(value) : $Maybe.Nothing;
      },
      base,
      signal);
   });
   var merge = F2(function (left,right) {
      return A3($Native$Signal.genericMerge,
      $Basics.always,
      left,
      right);
   });
   var mergeMany = function (signalList) {
      var _p7 = $List.reverse(signalList);
      if (_p7.ctor === "[]") {
            return _U.crashCase("Signal",
            {start: {line: 184,column: 3},end: {line: 189,column: 40}},
            _p7)("mergeMany was given an empty list!");
         } else {
            return A3($List.foldl,merge,_p7._0,_p7._1);
         }
   };
   var foldp = $Native$Signal.foldp;
   var map5 = $Native$Signal.map5;
   var map4 = $Native$Signal.map4;
   var map3 = $Native$Signal.map3;
   var map2 = $Native$Signal.map2;
   var map = $Native$Signal.map;
   var constant = $Native$Signal.constant;
   var Signal = {ctor: "Signal"};
   return _elm.Signal.values = {_op: _op
                               ,merge: merge
                               ,mergeMany: mergeMany
                               ,map: map
                               ,map2: map2
                               ,map3: map3
                               ,map4: map4
                               ,map5: map5
                               ,constant: constant
                               ,dropRepeats: dropRepeats
                               ,filter: filter
                               ,filterMap: filterMap
                               ,sampleOn: sampleOn
                               ,foldp: foldp
                               ,mailbox: mailbox
                               ,send: send
                               ,message: message
                               ,forwardTo: forwardTo
                               ,Mailbox: Mailbox};
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.Discrete = Elm.Signal.Discrete || {};
Elm.Signal.Discrete.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   _elm.Signal.Discrete = _elm.Signal.Discrete || {};
   if (_elm.Signal.Discrete.values)
   return _elm.Signal.Discrete.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var folde = F3(function (step,base,evt) {
      return A3($Signal.foldp,
      F2(function (_p0,b) {    return step(b);}),
      base,
      evt);
   });
   var es = $Signal.map($Basics.always({ctor: "_Tuple0"}));
   var whenEqual = F2(function (value,input) {
      var keepIf = $Signal.filter;
      var matchEvent = A3(keepIf,
      F2(function (x,y) {    return _U.eq(x,y);})(value),
      value,
      input);
      return es(matchEvent);
   });
   var whenChangeTo = F2(function (value,input) {
      return A2(whenEqual,value,$Signal.dropRepeats(input));
   });
   var whenChange = function (input) {
      return es($Signal.dropRepeats(input));
   };
   return _elm.Signal.Discrete.values = {_op: _op
                                        ,es: es
                                        ,whenEqual: whenEqual
                                        ,whenChange: whenChange
                                        ,whenChangeTo: whenChangeTo
                                        ,folde: folde};
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.Extra = Elm.Signal.Extra || {};
Elm.Signal.Extra.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   _elm.Signal.Extra = _elm.Signal.Extra || {};
   if (_elm.Signal.Extra.values) return _elm.Signal.Extra.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var unsafeFromJust = function (maybe) {
      var _p0 = maybe;
      if (_p0.ctor === "Just") {
            return _p0._0;
         } else {
            return _U.crashCase("Signal.Extra",
            {start: {line: 510,column: 3},end: {line: 515,column: 59}},
            _p0)("This case should have been unreachable");
         }
   };
   var passiveMap2 = F2(function (func,a) {
      return function (_p2) {
         return A3($Signal.map2,func,a,A2($Signal.sampleOn,a,_p2));
      };
   });
   var withPassive = passiveMap2(F2(function (x,y) {
      return x(y);
   }));
   var combine = A2($List.foldr,
   $Signal.map2(F2(function (x,y) {
      return A2($List._op["::"],x,y);
   })),
   $Signal.constant(_U.list([])));
   var mergeMany = F2(function (original,others) {
      return A3($List.foldl,$Signal.merge,original,others);
   });
   var filter = function (initial) {
      return A2($Signal.filterMap,$Basics.identity,initial);
   };
   var keepIf = $Signal.filter;
   var runBuffer$ = F3(function (l,n,input) {
      var f = F2(function (inp,prev) {
         var l = $List.length(prev);
         return _U.cmp(l,n) < 0 ? A2($Basics._op["++"],
         prev,
         _U.list([inp])) : A2($Basics._op["++"],
         A2($List.drop,l - n + 1,prev),
         _U.list([inp]));
      });
      return A3($Signal.foldp,f,l,input);
   });
   var runBuffer = runBuffer$(_U.list([]));
   var initSignal = function (s) {
      return A2($Signal.sampleOn,
      $Signal.constant({ctor: "_Tuple0"}),
      s);
   };
   var zip4 = $Signal.map4(F4(function (v0,v1,v2,v3) {
      return {ctor: "_Tuple4",_0: v0,_1: v1,_2: v2,_3: v3};
   }));
   var zip3 = $Signal.map3(F3(function (v0,v1,v2) {
      return {ctor: "_Tuple3",_0: v0,_1: v1,_2: v2};
   }));
   var zip = $Signal.map2(F2(function (v0,v1) {
      return {ctor: "_Tuple2",_0: v0,_1: v1};
   }));
   var keepWhen = F3(function (boolSig,a,aSig) {
      return A2($Signal.map,
      $Basics.snd,
      A3(keepIf,
      $Basics.fst,
      {ctor: "_Tuple2",_0: true,_1: a},
      A2($Signal.sampleOn,aSig,A2(zip,boolSig,aSig))));
   });
   var sampleWhen = F3(function (bs,def,sig) {
      return A2($Signal.map,
      $Basics.snd,
      A3(keepIf,
      $Basics.fst,
      {ctor: "_Tuple2",_0: true,_1: def},
      A2(zip,bs,sig)));
   });
   var andMap = $Signal.map2(F2(function (x,y) {
      return x(y);
   }));
   _op["~"] = andMap;
   var applyMany = F2(function (fs,l) {
      return A2(_op["~"],fs,combine(l));
   });
   _op["~>"] = $Basics.flip($Signal.map);
   var foldpWith = F4(function (unpack,step,init,input) {
      var step$ = F2(function (a,_p3) {
         var _p4 = _p3;
         return unpack(A2(step,a,_p4._1));
      });
      return A2(_op["~>"],
      A3($Signal.foldp,step$,init,input),
      $Basics.fst);
   });
   _op["<~"] = $Signal.map;
   var unzip = function (pairS) {
      return {ctor: "_Tuple2"
             ,_0: A2(_op["<~"],$Basics.fst,pairS)
             ,_1: A2(_op["<~"],$Basics.snd,pairS)};
   };
   var unzip3 = function (pairS) {
      return {ctor: "_Tuple3"
             ,_0: A2(_op["<~"],
             function (_p5) {
                var _p6 = _p5;
                return _p6._0;
             },
             pairS)
             ,_1: A2(_op["<~"],
             function (_p7) {
                var _p8 = _p7;
                return _p8._1;
             },
             pairS)
             ,_2: A2(_op["<~"],
             function (_p9) {
                var _p10 = _p9;
                return _p10._2;
             },
             pairS)};
   };
   var unzip4 = function (pairS) {
      return {ctor: "_Tuple4"
             ,_0: A2(_op["<~"],
             function (_p11) {
                var _p12 = _p11;
                return _p12._0;
             },
             pairS)
             ,_1: A2(_op["<~"],
             function (_p13) {
                var _p14 = _p13;
                return _p14._1;
             },
             pairS)
             ,_2: A2(_op["<~"],
             function (_p15) {
                var _p16 = _p15;
                return _p16._2;
             },
             pairS)
             ,_3: A2(_op["<~"],
             function (_p17) {
                var _p18 = _p17;
                return _p18._3;
             },
             pairS)};
   };
   var foldp$ = F3(function (fun,initFun,input) {
      var fun$ = F2(function (_p19,mb) {
         var _p20 = _p19;
         return $Maybe.Just(A2(fun,
         _p20._0,
         A2($Maybe.withDefault,_p20._1,mb)));
      });
      var initial = A2(_op["~>"],initSignal(input),initFun);
      var rest = A3($Signal.foldp,
      fun$,
      $Maybe.Nothing,
      A2(zip,input,initial));
      return A2(_op["<~"],
      unsafeFromJust,
      A2($Signal.merge,A2(_op["<~"],$Maybe.Just,initial),rest));
   });
   var deltas = function (signal) {
      var initial = function (value) {
         return {ctor: "_Tuple2",_0: value,_1: value};
      };
      var step = F2(function (value,delta) {
         return {ctor: "_Tuple2",_0: $Basics.snd(delta),_1: value};
      });
      return A3(foldp$,step,initial,signal);
   };
   var foldps = F3(function (f,bs,aS) {
      return A2(_op["<~"],
      $Basics.fst,
      A3($Signal.foldp,
      F2(function (a,_p21) {
         var _p22 = _p21;
         return A2(f,a,_p22._1);
      }),
      bs,
      aS));
   });
   var delayRound = F2(function (b,bS) {
      return A3(foldps,
      F2(function ($new,old) {
         return {ctor: "_Tuple2",_0: old,_1: $new};
      }),
      {ctor: "_Tuple2",_0: b,_1: b},
      bS);
   });
   var filterFold = F2(function (f,initial) {
      var f$ = F2(function (a,s) {
         var res = A2(f,a,s);
         return {ctor: "_Tuple2"
                ,_0: res
                ,_1: A2($Maybe.withDefault,s,res)};
      });
      return function (_p23) {
         return A2(filter,
         initial,
         A3(foldps,
         f$,
         {ctor: "_Tuple2",_0: $Maybe.Just(initial),_1: initial},
         _p23));
      };
   });
   var foldps$ = F3(function (f,iF,aS) {
      return A2(_op["<~"],
      $Basics.fst,
      A3(foldp$,
      F2(function (a,_p24) {
         var _p25 = _p24;
         return A2(f,a,_p25._1);
      }),
      iF,
      aS));
   });
   var switchHelper = F4(function (filter,b,l,r) {
      var lAndR = A2($Signal.merge,
      A3(filter,b,$Maybe.Nothing,A2(_op["<~"],$Maybe.Just,l)),
      A3(filter,
      A2(_op["<~"],$Basics.not,b),
      $Maybe.Nothing,
      A2(_op["<~"],$Maybe.Just,r)));
      var base = A2(_op["~"],
      A2(_op["~"],
      A2(_op["<~"],
      F3(function (bi,li,ri) {    return $Maybe.Just(bi ? li : ri);}),
      initSignal(b)),
      initSignal(l)),
      initSignal(r));
      return A2(_op["<~"],
      unsafeFromJust,
      A2($Signal.merge,base,lAndR));
   });
   var switchWhen = F3(function (b,l,r) {
      return A4(switchHelper,keepWhen,b,l,r);
   });
   var switchSample = F3(function (b,l,r) {
      return A4(switchHelper,sampleWhen,b,l,r);
   });
   var keepThen = F3(function (choice,base,signal) {
      return A3(switchSample,choice,signal,$Signal.constant(base));
   });
   var keepWhenI = F2(function (fs,s) {
      return A2(_op["~>"],
      A3(keepWhen,
      A2($Signal.merge,$Signal.constant(true),fs),
      $Maybe.Nothing,
      A2(_op["<~"],$Maybe.Just,s)),
      unsafeFromJust);
   });
   var fairMerge = F3(function (resolve,left,right) {
      var merged = A2($Signal.merge,left,right);
      var boolRight = A2(_op["<~"],$Basics.always(false),right);
      var boolLeft = A2(_op["<~"],$Basics.always(true),left);
      var bothUpdated = A2(_op["~"],
      A2(_op["<~"],
      F2(function (x,y) {    return !_U.eq(x,y);}),
      A2($Signal.merge,boolLeft,boolRight)),
      A2($Signal.merge,boolRight,boolLeft));
      var keep = keepWhenI(bothUpdated);
      var resolved = A2(_op["~"],
      A2(_op["<~"],resolve,keep(left)),
      keep(right));
      return A2($Signal.merge,resolved,merged);
   });
   var mapMany = F2(function (f,l) {
      return A2(_op["<~"],f,combine(l));
   });
   return _elm.Signal.Extra.values = {_op: _op
                                     ,andMap: andMap
                                     ,zip: zip
                                     ,zip3: zip3
                                     ,zip4: zip4
                                     ,unzip: unzip
                                     ,unzip3: unzip3
                                     ,unzip4: unzip4
                                     ,foldp$: foldp$
                                     ,foldps: foldps
                                     ,foldps$: foldps$
                                     ,runBuffer: runBuffer
                                     ,runBuffer$: runBuffer$
                                     ,deltas: deltas
                                     ,delayRound: delayRound
                                     ,keepIf: keepIf
                                     ,keepWhen: keepWhen
                                     ,sampleWhen: sampleWhen
                                     ,switchWhen: switchWhen
                                     ,keepWhenI: keepWhenI
                                     ,switchSample: switchSample
                                     ,keepThen: keepThen
                                     ,filter: filter
                                     ,filterFold: filterFold
                                     ,fairMerge: fairMerge
                                     ,mergeMany: mergeMany
                                     ,combine: combine
                                     ,mapMany: mapMany
                                     ,applyMany: applyMany
                                     ,passiveMap2: passiveMap2
                                     ,withPassive: withPassive};
};
Elm.Native.Time = {};

Elm.Native.Time.make = function(localRuntime)
{
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Time = localRuntime.Native.Time || {};
	if (localRuntime.Native.Time.values)
	{
		return localRuntime.Native.Time.values;
	}

	var NS = Elm.Native.Signal.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);


	// FRAMES PER SECOND

	function fpsWhen(desiredFPS, isOn)
	{
		var msPerFrame = 1000 / desiredFPS;
		var ticker = NS.input('fps-' + desiredFPS, null);

		function notifyTicker()
		{
			localRuntime.notify(ticker.id, null);
		}

		function firstArg(x, y)
		{
			return x;
		}

		// input fires either when isOn changes, or when ticker fires.
		// Its value is a tuple with the current timestamp, and the state of isOn
		var input = NS.timestamp(A3(NS.map2, F2(firstArg), NS.dropRepeats(isOn), ticker));

		var initialState = {
			isOn: false,
			time: localRuntime.timer.programStart,
			delta: 0
		};

		var timeoutId;

		function update(input, state)
		{
			var currentTime = input._0;
			var isOn = input._1;
			var wasOn = state.isOn;
			var previousTime = state.time;

			if (isOn)
			{
				timeoutId = localRuntime.setTimeout(notifyTicker, msPerFrame);
			}
			else if (wasOn)
			{
				clearTimeout(timeoutId);
			}

			return {
				isOn: isOn,
				time: currentTime,
				delta: (isOn && !wasOn) ? 0 : currentTime - previousTime
			};
		}

		return A2(
			NS.map,
			function(state) { return state.delta; },
			A3(NS.foldp, F2(update), update(input.value, initialState), input)
		);
	}


	// EVERY

	function every(t)
	{
		var ticker = NS.input('every-' + t, null);
		function tellTime()
		{
			localRuntime.notify(ticker.id, null);
		}
		var clock = A2(NS.map, fst, NS.timestamp(ticker));
		setInterval(tellTime, t);
		return clock;
	}


	function fst(pair)
	{
		return pair._0;
	}


	function read(s)
	{
		var t = Date.parse(s);
		return isNaN(t) ? Maybe.Nothing : Maybe.Just(t);
	}

	return localRuntime.Native.Time.values = {
		fpsWhen: F2(fpsWhen),
		every: every,
		toDate: function(t) { return new Date(t); },
		read: read
	};
};

Elm.Time = Elm.Time || {};
Elm.Time.make = function (_elm) {
   "use strict";
   _elm.Time = _elm.Time || {};
   if (_elm.Time.values) return _elm.Time.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Native$Signal = Elm.Native.Signal.make(_elm),
   $Native$Time = Elm.Native.Time.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var delay = $Native$Signal.delay;
   var since = F2(function (time,signal) {
      var stop = A2($Signal.map,
      $Basics.always(-1),
      A2(delay,time,signal));
      var start = A2($Signal.map,$Basics.always(1),signal);
      var delaydiff = A3($Signal.foldp,
      F2(function (x,y) {    return x + y;}),
      0,
      A2($Signal.merge,start,stop));
      return A2($Signal.map,
      F2(function (x,y) {    return !_U.eq(x,y);})(0),
      delaydiff);
   });
   var timestamp = $Native$Signal.timestamp;
   var every = $Native$Time.every;
   var fpsWhen = $Native$Time.fpsWhen;
   var fps = function (targetFrames) {
      return A2(fpsWhen,targetFrames,$Signal.constant(true));
   };
   var inMilliseconds = function (t) {    return t;};
   var millisecond = 1;
   var second = 1000 * millisecond;
   var minute = 60 * second;
   var hour = 60 * minute;
   var inHours = function (t) {    return t / hour;};
   var inMinutes = function (t) {    return t / minute;};
   var inSeconds = function (t) {    return t / second;};
   return _elm.Time.values = {_op: _op
                             ,millisecond: millisecond
                             ,second: second
                             ,minute: minute
                             ,hour: hour
                             ,inMilliseconds: inMilliseconds
                             ,inSeconds: inSeconds
                             ,inMinutes: inMinutes
                             ,inHours: inHours
                             ,fps: fps
                             ,fpsWhen: fpsWhen
                             ,every: every
                             ,timestamp: timestamp
                             ,delay: delay
                             ,since: since};
};
Elm.Signal = Elm.Signal || {};
Elm.Signal.Time = Elm.Signal.Time || {};
Elm.Signal.Time.make = function (_elm) {
   "use strict";
   _elm.Signal = _elm.Signal || {};
   _elm.Signal.Time = _elm.Signal.Time || {};
   if (_elm.Signal.Time.values) return _elm.Signal.Time.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Signal$Discrete = Elm.Signal.Discrete.make(_elm),
   $Signal$Extra = Elm.Signal.Extra.make(_elm),
   $Time = Elm.Time.make(_elm);
   var _op = {};
   var timestamp = $Time.timestamp;
   var delay = $Time.delay;
   var since = $Time.since;
   var settledAfter = F2(function (delay,sig) {
      var trailing = A2($Signal$Discrete.whenChangeTo,
      false,
      A2(since,delay,sig));
      return A2($Signal.sampleOn,trailing,sig);
   });
   var dropWithin = F2(function (delay,sig) {
      var leading = A2($Signal$Discrete.whenChangeTo,
      true,
      A2(since,delay,sig));
      return A2($Signal.sampleOn,leading,sig);
   });
   var timestamps = function (s) {
      return A2($Signal$Extra._op["~>"],timestamp(s),$Basics.fst);
   };
   var limitRate = F2(function (period,sig) {
      var within = F2(function (newt,oldt) {
         return _U.cmp(newt - oldt,period) > 0 ? newt : oldt;
      });
      var windowStart = A3($Signal.foldp,within,0,timestamps(sig));
      return A2($Signal.sampleOn,
      $Signal$Discrete.whenChange(windowStart),
      sig);
   });
   var startTime = timestamps($Signal.constant({ctor: "_Tuple0"}));
   var relativeTime = function (s) {
      return A2($Signal$Extra._op["~"],
      A2($Signal$Extra._op["<~"],
      F2(function (x,y) {    return x - y;}),
      s),
      startTime);
   };
   return _elm.Signal.Time.values = {_op: _op
                                    ,limitRate: limitRate
                                    ,dropWithin: dropWithin
                                    ,settledAfter: settledAfter
                                    ,startTime: startTime
                                    ,relativeTime: relativeTime
                                    ,since: since
                                    ,delay: delay
                                    ,timestamp: timestamp};
};
Elm.Native.Array = {};
Elm.Native.Array.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Array = localRuntime.Native.Array || {};
	if (localRuntime.Native.Array.values)
	{
		return localRuntime.Native.Array.values;
	}
	if ('values' in Elm.Native.Array)
	{
		return localRuntime.Native.Array.values = Elm.Native.Array.values;
	}

	var List = Elm.Native.List.make(localRuntime);

	// A RRB-Tree has two distinct data types.
	// Leaf -> "height"  is always 0
	//         "table"   is an array of elements
	// Node -> "height"  is always greater than 0
	//         "table"   is an array of child nodes
	//         "lengths" is an array of accumulated lengths of the child nodes

	// M is the maximal table size. 32 seems fast. E is the allowed increase
	// of search steps when concatting to find an index. Lower values will
	// decrease balancing, but will increase search steps.
	var M = 32;
	var E = 2;

	// An empty array.
	var empty = {
		ctor: '_Array',
		height: 0,
		table: []
	};


	function get(i, array)
	{
		if (i < 0 || i >= length(array))
		{
			throw new Error(
				'Index ' + i + ' is out of range. Check the length of ' +
				'your array first or use getMaybe or getWithDefault.');
		}
		return unsafeGet(i, array);
	}


	function unsafeGet(i, array)
	{
		for (var x = array.height; x > 0; x--)
		{
			var slot = i >> (x * 5);
			while (array.lengths[slot] <= i)
			{
				slot++;
			}
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array = array.table[slot];
		}
		return array.table[i];
	}


	// Sets the value at the index i. Only the nodes leading to i will get
	// copied and updated.
	function set(i, item, array)
	{
		if (i < 0 || length(array) <= i)
		{
			return array;
		}
		return unsafeSet(i, item, array);
	}


	function unsafeSet(i, item, array)
	{
		array = nodeCopy(array);

		if (array.height === 0)
		{
			array.table[i] = item;
		}
		else
		{
			var slot = getSlot(i, array);
			if (slot > 0)
			{
				i -= array.lengths[slot - 1];
			}
			array.table[slot] = unsafeSet(i, item, array.table[slot]);
		}
		return array;
	}


	function initialize(len, f)
	{
		if (len <= 0)
		{
			return empty;
		}
		var h = Math.floor( Math.log(len) / Math.log(M) );
		return initialize_(f, h, 0, len);
	}

	function initialize_(f, h, from, to)
	{
		if (h === 0)
		{
			var table = new Array((to - from) % (M + 1));
			for (var i = 0; i < table.length; i++)
			{
			  table[i] = f(from + i);
			}
			return {
				ctor: '_Array',
				height: 0,
				table: table
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = initialize_(f, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i-1] : 0);
		}
		return {
			ctor: '_Array',
			height: h,
			table: table,
			lengths: lengths
		};
	}

	function fromList(list)
	{
		if (list === List.Nil)
		{
			return empty;
		}

		// Allocate M sized blocks (table) and write list elements to it.
		var table = new Array(M);
		var nodes = [];
		var i = 0;

		while (list.ctor !== '[]')
		{
			table[i] = list._0;
			list = list._1;
			i++;

			// table is full, so we can push a leaf containing it into the
			// next node.
			if (i === M)
			{
				var leaf = {
					ctor: '_Array',
					height: 0,
					table: table
				};
				fromListPush(leaf, nodes);
				table = new Array(M);
				i = 0;
			}
		}

		// Maybe there is something left on the table.
		if (i > 0)
		{
			var leaf = {
				ctor: '_Array',
				height: 0,
				table: table.splice(0, i)
			};
			fromListPush(leaf, nodes);
		}

		// Go through all of the nodes and eventually push them into higher nodes.
		for (var h = 0; h < nodes.length - 1; h++)
		{
			if (nodes[h].table.length > 0)
			{
				fromListPush(nodes[h], nodes);
			}
		}

		var head = nodes[nodes.length - 1];
		if (head.height > 0 && head.table.length === 1)
		{
			return head.table[0];
		}
		else
		{
			return head;
		}
	}

	// Push a node into a higher node as a child.
	function fromListPush(toPush, nodes)
	{
		var h = toPush.height;

		// Maybe the node on this height does not exist.
		if (nodes.length === h)
		{
			var node = {
				ctor: '_Array',
				height: h + 1,
				table: [],
				lengths: []
			};
			nodes.push(node);
		}

		nodes[h].table.push(toPush);
		var len = length(toPush);
		if (nodes[h].lengths.length > 0)
		{
			len += nodes[h].lengths[nodes[h].lengths.length - 1];
		}
		nodes[h].lengths.push(len);

		if (nodes[h].table.length === M)
		{
			fromListPush(nodes[h], nodes);
			nodes[h] = {
				ctor: '_Array',
				height: h + 1,
				table: [],
				lengths: []
			};
		}
	}

	// Pushes an item via push_ to the bottom right of a tree.
	function push(item, a)
	{
		var pushed = push_(item, a);
		if (pushed !== null)
		{
			return pushed;
		}

		var newTree = create(item, a.height);
		return siblise(a, newTree);
	}

	// Recursively tries to push an item to the bottom-right most
	// tree possible. If there is no space left for the item,
	// null will be returned.
	function push_(item, a)
	{
		// Handle resursion stop at leaf level.
		if (a.height === 0)
		{
			if (a.table.length < M)
			{
				var newA = {
					ctor: '_Array',
					height: 0,
					table: a.table.slice()
				};
				newA.table.push(item);
				return newA;
			}
			else
			{
			  return null;
			}
		}

		// Recursively push
		var pushed = push_(item, botRight(a));

		// There was space in the bottom right tree, so the slot will
		// be updated.
		if (pushed !== null)
		{
			var newA = nodeCopy(a);
			newA.table[newA.table.length - 1] = pushed;
			newA.lengths[newA.lengths.length - 1]++;
			return newA;
		}

		// When there was no space left, check if there is space left
		// for a new slot with a tree which contains only the item
		// at the bottom.
		if (a.table.length < M)
		{
			var newSlot = create(item, a.height - 1);
			var newA = nodeCopy(a);
			newA.table.push(newSlot);
			newA.lengths.push(newA.lengths[newA.lengths.length - 1] + length(newSlot));
			return newA;
		}
		else
		{
			return null;
		}
	}

	// Converts an array into a list of elements.
	function toList(a)
	{
		return toList_(List.Nil, a);
	}

	function toList_(list, a)
	{
		for (var i = a.table.length - 1; i >= 0; i--)
		{
			list =
				a.height === 0
					? List.Cons(a.table[i], list)
					: toList_(list, a.table[i]);
		}
		return list;
	}

	// Maps a function over the elements of an array.
	function map(f, a)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height === 0
					? f(a.table[i])
					: map(f, a.table[i]);
		}
		return newA;
	}

	// Maps a function over the elements with their index as first argument.
	function indexedMap(f, a)
	{
		return indexedMap_(f, a, 0);
	}

	function indexedMap_(f, a, from)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: new Array(a.table.length)
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths;
		}
		for (var i = 0; i < a.table.length; i++)
		{
			newA.table[i] =
				a.height === 0
					? A2(f, from + i, a.table[i])
					: indexedMap_(f, a.table[i], i == 0 ? from : from + a.lengths[i - 1]);
		}
		return newA;
	}

	function foldl(f, b, a)
	{
		if (a.height === 0)
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = 0; i < a.table.length; i++)
			{
				b = foldl(f, b, a.table[i]);
			}
		}
		return b;
	}

	function foldr(f, b, a)
	{
		if (a.height === 0)
		{
			for (var i = a.table.length; i--; )
			{
				b = A2(f, a.table[i], b);
			}
		}
		else
		{
			for (var i = a.table.length; i--; )
			{
				b = foldr(f, b, a.table[i]);
			}
		}
		return b;
	}

	// TODO: currently, it slices the right, then the left. This can be
	// optimized.
	function slice(from, to, a)
	{
		if (from < 0)
		{
			from += length(a);
		}
		if (to < 0)
		{
			to += length(a);
		}
		return sliceLeft(from, sliceRight(to, a));
	}

	function sliceRight(to, a)
	{
		if (to === length(a))
		{
			return a;
		}

		// Handle leaf level.
		if (a.height === 0)
		{
			var newA = { ctor:'_Array', height:0 };
			newA.table = a.table.slice(0, to);
			return newA;
		}

		// Slice the right recursively.
		var right = getSlot(to, a);
		var sliced = sliceRight(to - (right > 0 ? a.lengths[right - 1] : 0), a.table[right]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (right === 0)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice(0, right),
			lengths: a.lengths.slice(0, right)
		};
		if (sliced.table.length > 0)
		{
			newA.table[right] = sliced;
			newA.lengths[right] = length(sliced) + (right > 0 ? newA.lengths[right - 1] : 0);
		}
		return newA;
	}

	function sliceLeft(from, a)
	{
		if (from === 0)
		{
			return a;
		}

		// Handle leaf level.
		if (a.height === 0)
		{
			var newA = { ctor:'_Array', height:0 };
			newA.table = a.table.slice(from, a.table.length + 1);
			return newA;
		}

		// Slice the left recursively.
		var left = getSlot(from, a);
		var sliced = sliceLeft(from - (left > 0 ? a.lengths[left - 1] : 0), a.table[left]);

		// Maybe the a node is not even needed, as sliced contains the whole slice.
		if (left === a.table.length - 1)
		{
			return sliced;
		}

		// Create new node.
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice(left, a.table.length + 1),
			lengths: new Array(a.table.length - left)
		};
		newA.table[0] = sliced;
		var len = 0;
		for (var i = 0; i < newA.table.length; i++)
		{
			len += length(newA.table[i]);
			newA.lengths[i] = len;
		}

		return newA;
	}

	// Appends two trees.
	function append(a,b)
	{
		if (a.table.length === 0)
		{
			return b;
		}
		if (b.table.length === 0)
		{
			return a;
		}

		var c = append_(a, b);

		// Check if both nodes can be crunshed together.
		if (c[0].table.length + c[1].table.length <= M)
		{
			if (c[0].table.length === 0)
			{
				return c[1];
			}
			if (c[1].table.length === 0)
			{
				return c[0];
			}

			// Adjust .table and .lengths
			c[0].table = c[0].table.concat(c[1].table);
			if (c[0].height > 0)
			{
				var len = length(c[0]);
				for (var i = 0; i < c[1].lengths.length; i++)
				{
					c[1].lengths[i] += len;
				}
				c[0].lengths = c[0].lengths.concat(c[1].lengths);
			}

			return c[0];
		}

		if (c[0].height > 0)
		{
			var toRemove = calcToRemove(a, b);
			if (toRemove > E)
			{
				c = shuffle(c[0], c[1], toRemove);
			}
		}

		return siblise(c[0], c[1]);
	}

	// Returns an array of two nodes; right and left. One node _may_ be empty.
	function append_(a, b)
	{
		if (a.height === 0 && b.height === 0)
		{
			return [a, b];
		}

		if (a.height !== 1 || b.height !== 1)
		{
			if (a.height === b.height)
			{
				a = nodeCopy(a);
				b = nodeCopy(b);
				var appended = append_(botRight(a), botLeft(b));

				insertRight(a, appended[1]);
				insertLeft(b, appended[0]);
			}
			else if (a.height > b.height)
			{
				a = nodeCopy(a);
				var appended = append_(botRight(a), b);

				insertRight(a, appended[0]);
				b = parentise(appended[1], appended[1].height + 1);
			}
			else
			{
				b = nodeCopy(b);
				var appended = append_(a, botLeft(b));

				var left = appended[0].table.length === 0 ? 0 : 1;
				var right = left === 0 ? 1 : 0;
				insertLeft(b, appended[left]);
				a = parentise(appended[right], appended[right].height + 1);
			}
		}

		// Check if balancing is needed and return based on that.
		if (a.table.length === 0 || b.table.length === 0)
		{
			return [a, b];
		}

		var toRemove = calcToRemove(a, b);
		if (toRemove <= E)
		{
			return [a, b];
		}
		return shuffle(a, b, toRemove);
	}

	// Helperfunctions for append_. Replaces a child node at the side of the parent.
	function insertRight(parent, node)
	{
		var index = parent.table.length - 1;
		parent.table[index] = node;
		parent.lengths[index] = length(node);
		parent.lengths[index] += index > 0 ? parent.lengths[index - 1] : 0;
	}

	function insertLeft(parent, node)
	{
		if (node.table.length > 0)
		{
			parent.table[0] = node;
			parent.lengths[0] = length(node);

			var len = length(parent.table[0]);
			for (var i = 1; i < parent.lengths.length; i++)
			{
				len += length(parent.table[i]);
				parent.lengths[i] = len;
			}
		}
		else
		{
			parent.table.shift();
			for (var i = 1; i < parent.lengths.length; i++)
			{
				parent.lengths[i] = parent.lengths[i] - parent.lengths[0];
			}
			parent.lengths.shift();
		}
	}

	// Returns the extra search steps for E. Refer to the paper.
	function calcToRemove(a, b)
	{
		var subLengths = 0;
		for (var i = 0; i < a.table.length; i++)
		{
			subLengths += a.table[i].table.length;
		}
		for (var i = 0; i < b.table.length; i++)
		{
			subLengths += b.table[i].table.length;
		}

		var toRemove = a.table.length + b.table.length;
		return toRemove - (Math.floor((subLengths - 1) / M) + 1);
	}

	// get2, set2 and saveSlot are helpers for accessing elements over two arrays.
	function get2(a, b, index)
	{
		return index < a.length
			? a[index]
			: b[index - a.length];
	}

	function set2(a, b, index, value)
	{
		if (index < a.length)
		{
			a[index] = value;
		}
		else
		{
			b[index - a.length] = value;
		}
	}

	function saveSlot(a, b, index, slot)
	{
		set2(a.table, b.table, index, slot);

		var l = (index === 0 || index === a.lengths.length)
			? 0
			: get2(a.lengths, a.lengths, index - 1);

		set2(a.lengths, b.lengths, index, l + length(slot));
	}

	// Creates a node or leaf with a given length at their arrays for perfomance.
	// Is only used by shuffle.
	function createNode(h, length)
	{
		if (length < 0)
		{
			length = 0;
		}
		var a = {
			ctor: '_Array',
			height: h,
			table: new Array(length)
		};
		if (h > 0)
		{
			a.lengths = new Array(length);
		}
		return a;
	}

	// Returns an array of two balanced nodes.
	function shuffle(a, b, toRemove)
	{
		var newA = createNode(a.height, Math.min(M, a.table.length + b.table.length - toRemove));
		var newB = createNode(a.height, newA.table.length - (a.table.length + b.table.length - toRemove));

		// Skip the slots with size M. More precise: copy the slot references
		// to the new node
		var read = 0;
		while (get2(a.table, b.table, read).table.length % M === 0)
		{
			set2(newA.table, newB.table, read, get2(a.table, b.table, read));
			set2(newA.lengths, newB.lengths, read, get2(a.lengths, b.lengths, read));
			read++;
		}

		// Pulling items from left to right, caching in a slot before writing
		// it into the new nodes.
		var write = read;
		var slot = new createNode(a.height - 1, 0);
		var from = 0;

		// If the current slot is still containing data, then there will be at
		// least one more write, so we do not break this loop yet.
		while (read - write - (slot.table.length > 0 ? 1 : 0) < toRemove)
		{
			// Find out the max possible items for copying.
			var source = get2(a.table, b.table, read);
			var to = Math.min(M - slot.table.length, source.table.length);

			// Copy and adjust size table.
			slot.table = slot.table.concat(source.table.slice(from, to));
			if (slot.height > 0)
			{
				var len = slot.lengths.length;
				for (var i = len; i < len + to - from; i++)
				{
					slot.lengths[i] = length(slot.table[i]);
					slot.lengths[i] += (i > 0 ? slot.lengths[i - 1] : 0);
				}
			}

			from += to;

			// Only proceed to next slots[i] if the current one was
			// fully copied.
			if (source.table.length <= to)
			{
				read++; from = 0;
			}

			// Only create a new slot if the current one is filled up.
			if (slot.table.length === M)
			{
				saveSlot(newA, newB, write, slot);
				slot = createNode(a.height - 1, 0);
				write++;
			}
		}

		// Cleanup after the loop. Copy the last slot into the new nodes.
		if (slot.table.length > 0)
		{
			saveSlot(newA, newB, write, slot);
			write++;
		}

		// Shift the untouched slots to the left
		while (read < a.table.length + b.table.length )
		{
			saveSlot(newA, newB, write, get2(a.table, b.table, read));
			read++;
			write++;
		}

		return [newA, newB];
	}

	// Navigation functions
	function botRight(a)
	{
		return a.table[a.table.length - 1];
	}
	function botLeft(a)
	{
		return a.table[0];
	}

	// Copies a node for updating. Note that you should not use this if
	// only updating only one of "table" or "lengths" for performance reasons.
	function nodeCopy(a)
	{
		var newA = {
			ctor: '_Array',
			height: a.height,
			table: a.table.slice()
		};
		if (a.height > 0)
		{
			newA.lengths = a.lengths.slice();
		}
		return newA;
	}

	// Returns how many items are in the tree.
	function length(array)
	{
		if (array.height === 0)
		{
			return array.table.length;
		}
		else
		{
			return array.lengths[array.lengths.length - 1];
		}
	}

	// Calculates in which slot of "table" the item probably is, then
	// find the exact slot via forward searching in  "lengths". Returns the index.
	function getSlot(i, a)
	{
		var slot = i >> (5 * a.height);
		while (a.lengths[slot] <= i)
		{
			slot++;
		}
		return slot;
	}

	// Recursively creates a tree with a given height containing
	// only the given item.
	function create(item, h)
	{
		if (h === 0)
		{
			return {
				ctor: '_Array',
				height: 0,
				table: [item]
			};
		}
		return {
			ctor: '_Array',
			height: h,
			table: [create(item, h - 1)],
			lengths: [1]
		};
	}

	// Recursively creates a tree that contains the given tree.
	function parentise(tree, h)
	{
		if (h === tree.height)
		{
			return tree;
		}

		return {
			ctor: '_Array',
			height: h,
			table: [parentise(tree, h - 1)],
			lengths: [length(tree)]
		};
	}

	// Emphasizes blood brotherhood beneath two trees.
	function siblise(a, b)
	{
		return {
			ctor: '_Array',
			height: a.height + 1,
			table: [a, b],
			lengths: [length(a), length(a) + length(b)]
		};
	}

	function toJSArray(a)
	{
		var jsArray = new Array(length(a));
		toJSArray_(jsArray, 0, a);
		return jsArray;
	}

	function toJSArray_(jsArray, i, a)
	{
		for (var t = 0; t < a.table.length; t++)
		{
			if (a.height === 0)
			{
				jsArray[i + t] = a.table[t];
			}
			else
			{
				var inc = t === 0 ? 0 : a.lengths[t - 1];
				toJSArray_(jsArray, i + inc, a.table[t]);
			}
		}
	}

	function fromJSArray(jsArray)
	{
		if (jsArray.length === 0)
		{
			return empty;
		}
		var h = Math.floor(Math.log(jsArray.length) / Math.log(M));
		return fromJSArray_(jsArray, h, 0, jsArray.length);
	}

	function fromJSArray_(jsArray, h, from, to)
	{
		if (h === 0)
		{
			return {
				ctor: '_Array',
				height: 0,
				table: jsArray.slice(from, to)
			};
		}

		var step = Math.pow(M, h);
		var table = new Array(Math.ceil((to - from) / step));
		var lengths = new Array(table.length);
		for (var i = 0; i < table.length; i++)
		{
			table[i] = fromJSArray_(jsArray, h - 1, from + (i * step), Math.min(from + ((i + 1) * step), to));
			lengths[i] = length(table[i]) + (i > 0 ? lengths[i - 1] : 0);
		}
		return {
			ctor: '_Array',
			height: h,
			table: table,
			lengths: lengths
		};
	}

	Elm.Native.Array.values = {
		empty: empty,
		fromList: fromList,
		toList: toList,
		initialize: F2(initialize),
		append: F2(append),
		push: F2(push),
		slice: F3(slice),
		get: F2(get),
		set: F3(set),
		map: F2(map),
		indexedMap: F2(indexedMap),
		foldl: F3(foldl),
		foldr: F3(foldr),
		length: length,

		toJSArray: toJSArray,
		fromJSArray: fromJSArray
	};

	return localRuntime.Native.Array.values = Elm.Native.Array.values;
};

Elm.Array = Elm.Array || {};
Elm.Array.make = function (_elm) {
   "use strict";
   _elm.Array = _elm.Array || {};
   if (_elm.Array.values) return _elm.Array.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Array = Elm.Native.Array.make(_elm);
   var _op = {};
   var append = $Native$Array.append;
   var length = $Native$Array.length;
   var isEmpty = function (array) {
      return _U.eq(length(array),0);
   };
   var slice = $Native$Array.slice;
   var set = $Native$Array.set;
   var get = F2(function (i,array) {
      return _U.cmp(0,i) < 1 && _U.cmp(i,
      $Native$Array.length(array)) < 0 ? $Maybe.Just(A2($Native$Array.get,
      i,
      array)) : $Maybe.Nothing;
   });
   var push = $Native$Array.push;
   var empty = $Native$Array.empty;
   var filter = F2(function (isOkay,arr) {
      var update = F2(function (x,xs) {
         return isOkay(x) ? A2($Native$Array.push,x,xs) : xs;
      });
      return A3($Native$Array.foldl,update,$Native$Array.empty,arr);
   });
   var foldr = $Native$Array.foldr;
   var foldl = $Native$Array.foldl;
   var indexedMap = $Native$Array.indexedMap;
   var map = $Native$Array.map;
   var toIndexedList = function (array) {
      return A3($List.map2,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2",_0: v0,_1: v1};
      }),
      _U.range(0,$Native$Array.length(array) - 1),
      $Native$Array.toList(array));
   };
   var toList = $Native$Array.toList;
   var fromList = $Native$Array.fromList;
   var initialize = $Native$Array.initialize;
   var repeat = F2(function (n,e) {
      return A2(initialize,n,$Basics.always(e));
   });
   var Array = {ctor: "Array"};
   return _elm.Array.values = {_op: _op
                              ,empty: empty
                              ,repeat: repeat
                              ,initialize: initialize
                              ,fromList: fromList
                              ,isEmpty: isEmpty
                              ,length: length
                              ,push: push
                              ,append: append
                              ,get: get
                              ,set: set
                              ,slice: slice
                              ,toList: toList
                              ,toIndexedList: toIndexedList
                              ,map: map
                              ,indexedMap: indexedMap
                              ,filter: filter
                              ,foldl: foldl
                              ,foldr: foldr};
};
Elm.Native.Bitwise = {};
Elm.Native.Bitwise.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Bitwise = localRuntime.Native.Bitwise || {};
	if (localRuntime.Native.Bitwise.values)
	{
		return localRuntime.Native.Bitwise.values;
	}

	function and(a, b) { return a & b; }
	function or(a, b) { return a | b; }
	function xor(a, b) { return a ^ b; }
	function not(a) { return ~a; }
	function sll(a, offset) { return a << offset; }
	function sra(a, offset) { return a >> offset; }
	function srl(a, offset) { return a >>> offset; }

	return localRuntime.Native.Bitwise.values = {
		and: F2(and),
		or: F2(or),
		xor: F2(xor),
		complement: not,
		shiftLeft: F2(sll),
		shiftRightArithmatic: F2(sra),
		shiftRightLogical: F2(srl)
	};
};

Elm.Bitwise = Elm.Bitwise || {};
Elm.Bitwise.make = function (_elm) {
   "use strict";
   _elm.Bitwise = _elm.Bitwise || {};
   if (_elm.Bitwise.values) return _elm.Bitwise.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Native$Bitwise = Elm.Native.Bitwise.make(_elm);
   var _op = {};
   var shiftRightLogical = $Native$Bitwise.shiftRightLogical;
   var shiftRight = $Native$Bitwise.shiftRightArithmatic;
   var shiftLeft = $Native$Bitwise.shiftLeft;
   var complement = $Native$Bitwise.complement;
   var xor = $Native$Bitwise.xor;
   var or = $Native$Bitwise.or;
   var and = $Native$Bitwise.and;
   return _elm.Bitwise.values = {_op: _op
                                ,and: and
                                ,or: or
                                ,xor: xor
                                ,complement: complement
                                ,shiftLeft: shiftLeft
                                ,shiftRight: shiftRight
                                ,shiftRightLogical: shiftRightLogical};
};
Elm.Native.Char = {};
Elm.Native.Char.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Char = localRuntime.Native.Char || {};
	if (localRuntime.Native.Char.values)
	{
		return localRuntime.Native.Char.values;
	}

	var Utils = Elm.Native.Utils.make(localRuntime);

	return localRuntime.Native.Char.values = {
		fromCode: function(c) { return Utils.chr(String.fromCharCode(c)); },
		toCode: function(c) { return c.charCodeAt(0); },
		toUpper: function(c) { return Utils.chr(c.toUpperCase()); },
		toLower: function(c) { return Utils.chr(c.toLowerCase()); },
		toLocaleUpper: function(c) { return Utils.chr(c.toLocaleUpperCase()); },
		toLocaleLower: function(c) { return Utils.chr(c.toLocaleLowerCase()); }
	};
};

Elm.Char = Elm.Char || {};
Elm.Char.make = function (_elm) {
   "use strict";
   _elm.Char = _elm.Char || {};
   if (_elm.Char.values) return _elm.Char.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Native$Char = Elm.Native.Char.make(_elm);
   var _op = {};
   var fromCode = $Native$Char.fromCode;
   var toCode = $Native$Char.toCode;
   var toLocaleLower = $Native$Char.toLocaleLower;
   var toLocaleUpper = $Native$Char.toLocaleUpper;
   var toLower = $Native$Char.toLower;
   var toUpper = $Native$Char.toUpper;
   var isBetween = F3(function (low,high,$char) {
      var code = toCode($char);
      return _U.cmp(code,toCode(low)) > -1 && _U.cmp(code,
      toCode(high)) < 1;
   });
   var isUpper = A2(isBetween,_U.chr("A"),_U.chr("Z"));
   var isLower = A2(isBetween,_U.chr("a"),_U.chr("z"));
   var isDigit = A2(isBetween,_U.chr("0"),_U.chr("9"));
   var isOctDigit = A2(isBetween,_U.chr("0"),_U.chr("7"));
   var isHexDigit = function ($char) {
      return isDigit($char) || (A3(isBetween,
      _U.chr("a"),
      _U.chr("f"),
      $char) || A3(isBetween,_U.chr("A"),_U.chr("F"),$char));
   };
   return _elm.Char.values = {_op: _op
                             ,isUpper: isUpper
                             ,isLower: isLower
                             ,isDigit: isDigit
                             ,isOctDigit: isOctDigit
                             ,isHexDigit: isHexDigit
                             ,toUpper: toUpper
                             ,toLower: toLower
                             ,toLocaleUpper: toLocaleUpper
                             ,toLocaleLower: toLocaleLower
                             ,toCode: toCode
                             ,fromCode: fromCode};
};
Elm.Native.String = {};

Elm.Native.String.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.String = localRuntime.Native.String || {};
	if (localRuntime.Native.String.values)
	{
		return localRuntime.Native.String.values;
	}
	if ('values' in Elm.Native.String)
	{
		return localRuntime.Native.String.values = Elm.Native.String.values;
	}


	var Char = Elm.Char.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);

	function isEmpty(str)
	{
		return str.length === 0;
	}
	function cons(chr, str)
	{
		return chr + str;
	}
	function uncons(str)
	{
		var hd = str[0];
		if (hd)
		{
			return Maybe.Just(Utils.Tuple2(Utils.chr(hd), str.slice(1)));
		}
		return Maybe.Nothing;
	}
	function append(a, b)
	{
		return a + b;
	}
	function concat(strs)
	{
		return List.toArray(strs).join('');
	}
	function length(str)
	{
		return str.length;
	}
	function map(f, str)
	{
		var out = str.split('');
		for (var i = out.length; i--; )
		{
			out[i] = f(Utils.chr(out[i]));
		}
		return out.join('');
	}
	function filter(pred, str)
	{
		return str.split('').map(Utils.chr).filter(pred).join('');
	}
	function reverse(str)
	{
		return str.split('').reverse().join('');
	}
	function foldl(f, b, str)
	{
		var len = str.length;
		for (var i = 0; i < len; ++i)
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function foldr(f, b, str)
	{
		for (var i = str.length; i--; )
		{
			b = A2(f, Utils.chr(str[i]), b);
		}
		return b;
	}
	function split(sep, str)
	{
		return List.fromArray(str.split(sep));
	}
	function join(sep, strs)
	{
		return List.toArray(strs).join(sep);
	}
	function repeat(n, str)
	{
		var result = '';
		while (n > 0)
		{
			if (n & 1)
			{
				result += str;
			}
			n >>= 1, str += str;
		}
		return result;
	}
	function slice(start, end, str)
	{
		return str.slice(start, end);
	}
	function left(n, str)
	{
		return n < 1 ? '' : str.slice(0, n);
	}
	function right(n, str)
	{
		return n < 1 ? '' : str.slice(-n);
	}
	function dropLeft(n, str)
	{
		return n < 1 ? str : str.slice(n);
	}
	function dropRight(n, str)
	{
		return n < 1 ? str : str.slice(0, -n);
	}
	function pad(n, chr, str)
	{
		var half = (n - str.length) / 2;
		return repeat(Math.ceil(half), chr) + str + repeat(half | 0, chr);
	}
	function padRight(n, chr, str)
	{
		return str + repeat(n - str.length, chr);
	}
	function padLeft(n, chr, str)
	{
		return repeat(n - str.length, chr) + str;
	}

	function trim(str)
	{
		return str.trim();
	}
	function trimLeft(str)
	{
		return str.replace(/^\s+/, '');
	}
	function trimRight(str)
	{
		return str.replace(/\s+$/, '');
	}

	function words(str)
	{
		return List.fromArray(str.trim().split(/\s+/g));
	}
	function lines(str)
	{
		return List.fromArray(str.split(/\r\n|\r|\n/g));
	}

	function toUpper(str)
	{
		return str.toUpperCase();
	}
	function toLower(str)
	{
		return str.toLowerCase();
	}

	function any(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (pred(Utils.chr(str[i])))
			{
				return true;
			}
		}
		return false;
	}
	function all(pred, str)
	{
		for (var i = str.length; i--; )
		{
			if (!pred(Utils.chr(str[i])))
			{
				return false;
			}
		}
		return true;
	}

	function contains(sub, str)
	{
		return str.indexOf(sub) > -1;
	}
	function startsWith(sub, str)
	{
		return str.indexOf(sub) === 0;
	}
	function endsWith(sub, str)
	{
		return str.length >= sub.length &&
			str.lastIndexOf(sub) === str.length - sub.length;
	}
	function indexes(sub, str)
	{
		var subLen = sub.length;
		var i = 0;
		var is = [];
		while ((i = str.indexOf(sub, i)) > -1)
		{
			is.push(i);
			i = i + subLen;
		}
		return List.fromArray(is);
	}

	function toInt(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to an Int" );
		}
		var start = 0;
		if (s[0] === '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
			start = 1;
		}
		for (var i = start; i < len; ++i)
		{
			if (!Char.isDigit(s[i]))
			{
				return Result.Err("could not convert string '" + s + "' to an Int" );
			}
		}
		return Result.Ok(parseInt(s, 10));
	}

	function toFloat(s)
	{
		var len = s.length;
		if (len === 0)
		{
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		var start = 0;
		if (s[0] === '-')
		{
			if (len === 1)
			{
				return Result.Err("could not convert string '" + s + "' to a Float" );
			}
			start = 1;
		}
		var dotCount = 0;
		for (var i = start; i < len; ++i)
		{
			if (Char.isDigit(s[i]))
			{
				continue;
			}
			if (s[i] === '.')
			{
				dotCount += 1;
				if (dotCount <= 1)
				{
					continue;
				}
			}
			return Result.Err("could not convert string '" + s + "' to a Float" );
		}
		return Result.Ok(parseFloat(s));
	}

	function toList(str)
	{
		return List.fromArray(str.split('').map(Utils.chr));
	}
	function fromList(chars)
	{
		return List.toArray(chars).join('');
	}

	return Elm.Native.String.values = {
		isEmpty: isEmpty,
		cons: F2(cons),
		uncons: uncons,
		append: F2(append),
		concat: concat,
		length: length,
		map: F2(map),
		filter: F2(filter),
		reverse: reverse,
		foldl: F3(foldl),
		foldr: F3(foldr),

		split: F2(split),
		join: F2(join),
		repeat: F2(repeat),

		slice: F3(slice),
		left: F2(left),
		right: F2(right),
		dropLeft: F2(dropLeft),
		dropRight: F2(dropRight),

		pad: F3(pad),
		padLeft: F3(padLeft),
		padRight: F3(padRight),

		trim: trim,
		trimLeft: trimLeft,
		trimRight: trimRight,

		words: words,
		lines: lines,

		toUpper: toUpper,
		toLower: toLower,

		any: F2(any),
		all: F2(all),

		contains: F2(contains),
		startsWith: F2(startsWith),
		endsWith: F2(endsWith),
		indexes: F2(indexes),

		toInt: toInt,
		toFloat: toFloat,
		toList: toList,
		fromList: fromList
	};
};

Elm.String = Elm.String || {};
Elm.String.make = function (_elm) {
   "use strict";
   _elm.String = _elm.String || {};
   if (_elm.String.values) return _elm.String.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$String = Elm.Native.String.make(_elm),
   $Result = Elm.Result.make(_elm);
   var _op = {};
   var fromList = $Native$String.fromList;
   var toList = $Native$String.toList;
   var toFloat = $Native$String.toFloat;
   var toInt = $Native$String.toInt;
   var indices = $Native$String.indexes;
   var indexes = $Native$String.indexes;
   var endsWith = $Native$String.endsWith;
   var startsWith = $Native$String.startsWith;
   var contains = $Native$String.contains;
   var all = $Native$String.all;
   var any = $Native$String.any;
   var toLower = $Native$String.toLower;
   var toUpper = $Native$String.toUpper;
   var lines = $Native$String.lines;
   var words = $Native$String.words;
   var trimRight = $Native$String.trimRight;
   var trimLeft = $Native$String.trimLeft;
   var trim = $Native$String.trim;
   var padRight = $Native$String.padRight;
   var padLeft = $Native$String.padLeft;
   var pad = $Native$String.pad;
   var dropRight = $Native$String.dropRight;
   var dropLeft = $Native$String.dropLeft;
   var right = $Native$String.right;
   var left = $Native$String.left;
   var slice = $Native$String.slice;
   var repeat = $Native$String.repeat;
   var join = $Native$String.join;
   var split = $Native$String.split;
   var foldr = $Native$String.foldr;
   var foldl = $Native$String.foldl;
   var reverse = $Native$String.reverse;
   var filter = $Native$String.filter;
   var map = $Native$String.map;
   var length = $Native$String.length;
   var concat = $Native$String.concat;
   var append = $Native$String.append;
   var uncons = $Native$String.uncons;
   var cons = $Native$String.cons;
   var fromChar = function ($char) {    return A2(cons,$char,"");};
   var isEmpty = $Native$String.isEmpty;
   return _elm.String.values = {_op: _op
                               ,isEmpty: isEmpty
                               ,length: length
                               ,reverse: reverse
                               ,repeat: repeat
                               ,cons: cons
                               ,uncons: uncons
                               ,fromChar: fromChar
                               ,append: append
                               ,concat: concat
                               ,split: split
                               ,join: join
                               ,words: words
                               ,lines: lines
                               ,slice: slice
                               ,left: left
                               ,right: right
                               ,dropLeft: dropLeft
                               ,dropRight: dropRight
                               ,contains: contains
                               ,startsWith: startsWith
                               ,endsWith: endsWith
                               ,indexes: indexes
                               ,indices: indices
                               ,toInt: toInt
                               ,toFloat: toFloat
                               ,toList: toList
                               ,fromList: fromList
                               ,toUpper: toUpper
                               ,toLower: toLower
                               ,pad: pad
                               ,padLeft: padLeft
                               ,padRight: padRight
                               ,trim: trim
                               ,trimLeft: trimLeft
                               ,trimRight: trimRight
                               ,map: map
                               ,filter: filter
                               ,foldl: foldl
                               ,foldr: foldr
                               ,any: any
                               ,all: all};
};
Elm.Dict = Elm.Dict || {};
Elm.Dict.make = function (_elm) {
   "use strict";
   _elm.Dict = _elm.Dict || {};
   if (_elm.Dict.values) return _elm.Dict.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Debug = Elm.Native.Debug.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var foldr = F3(function (f,acc,t) {
      foldr: while (true) {
         var _p0 = t;
         if (_p0.ctor === "RBEmpty_elm_builtin") {
               return acc;
            } else {
               var _v1 = f,
               _v2 = A3(f,_p0._1,_p0._2,A3(foldr,f,acc,_p0._4)),
               _v3 = _p0._3;
               f = _v1;
               acc = _v2;
               t = _v3;
               continue foldr;
            }
      }
   });
   var keys = function (dict) {
      return A3(foldr,
      F3(function (key,value,keyList) {
         return A2($List._op["::"],key,keyList);
      }),
      _U.list([]),
      dict);
   };
   var values = function (dict) {
      return A3(foldr,
      F3(function (key,value,valueList) {
         return A2($List._op["::"],value,valueList);
      }),
      _U.list([]),
      dict);
   };
   var toList = function (dict) {
      return A3(foldr,
      F3(function (key,value,list) {
         return A2($List._op["::"],
         {ctor: "_Tuple2",_0: key,_1: value},
         list);
      }),
      _U.list([]),
      dict);
   };
   var foldl = F3(function (f,acc,dict) {
      foldl: while (true) {
         var _p1 = dict;
         if (_p1.ctor === "RBEmpty_elm_builtin") {
               return acc;
            } else {
               var _v5 = f,
               _v6 = A3(f,_p1._1,_p1._2,A3(foldl,f,acc,_p1._3)),
               _v7 = _p1._4;
               f = _v5;
               acc = _v6;
               dict = _v7;
               continue foldl;
            }
      }
   });
   var reportRemBug = F4(function (msg,c,lgot,rgot) {
      return $Native$Debug.crash($String.concat(_U.list(["Internal red-black tree invariant violated, expected "
                                                        ,msg
                                                        ," and got "
                                                        ,$Basics.toString(c)
                                                        ,"/"
                                                        ,lgot
                                                        ,"/"
                                                        ,rgot
                                                        ,"\nPlease report this bug to <https://github.com/elm-lang/core/issues>"])));
   });
   var isBBlack = function (dict) {
      var _p2 = dict;
      _v8_2: do {
         if (_p2.ctor === "RBNode_elm_builtin") {
               if (_p2._0.ctor === "BBlack") {
                     return true;
                  } else {
                     break _v8_2;
                  }
            } else {
               if (_p2._0.ctor === "LBBlack") {
                     return true;
                  } else {
                     break _v8_2;
                  }
            }
      } while (false);
      return false;
   };
   var Same = {ctor: "Same"};
   var Remove = {ctor: "Remove"};
   var Insert = {ctor: "Insert"};
   var sizeHelp = F2(function (n,dict) {
      sizeHelp: while (true) {
         var _p3 = dict;
         if (_p3.ctor === "RBEmpty_elm_builtin") {
               return n;
            } else {
               var _v10 = A2(sizeHelp,n + 1,_p3._4),_v11 = _p3._3;
               n = _v10;
               dict = _v11;
               continue sizeHelp;
            }
      }
   });
   var size = function (dict) {    return A2(sizeHelp,0,dict);};
   var get = F2(function (targetKey,dict) {
      get: while (true) {
         var _p4 = dict;
         if (_p4.ctor === "RBEmpty_elm_builtin") {
               return $Maybe.Nothing;
            } else {
               var _p5 = A2($Basics.compare,targetKey,_p4._1);
               switch (_p5.ctor)
               {case "LT": var _v14 = targetKey,_v15 = _p4._3;
                    targetKey = _v14;
                    dict = _v15;
                    continue get;
                  case "EQ": return $Maybe.Just(_p4._2);
                  default: var _v16 = targetKey,_v17 = _p4._4;
                    targetKey = _v16;
                    dict = _v17;
                    continue get;}
            }
      }
   });
   var member = F2(function (key,dict) {
      var _p6 = A2(get,key,dict);
      if (_p6.ctor === "Just") {
            return true;
         } else {
            return false;
         }
   });
   var maxWithDefault = F3(function (k,v,r) {
      maxWithDefault: while (true) {
         var _p7 = r;
         if (_p7.ctor === "RBEmpty_elm_builtin") {
               return {ctor: "_Tuple2",_0: k,_1: v};
            } else {
               var _v20 = _p7._1,_v21 = _p7._2,_v22 = _p7._4;
               k = _v20;
               v = _v21;
               r = _v22;
               continue maxWithDefault;
            }
      }
   });
   var RBEmpty_elm_builtin = function (a) {
      return {ctor: "RBEmpty_elm_builtin",_0: a};
   };
   var RBNode_elm_builtin = F5(function (a,b,c,d,e) {
      return {ctor: "RBNode_elm_builtin"
             ,_0: a
             ,_1: b
             ,_2: c
             ,_3: d
             ,_4: e};
   });
   var LBBlack = {ctor: "LBBlack"};
   var LBlack = {ctor: "LBlack"};
   var empty = RBEmpty_elm_builtin(LBlack);
   var isEmpty = function (dict) {    return _U.eq(dict,empty);};
   var map = F2(function (f,dict) {
      var _p8 = dict;
      if (_p8.ctor === "RBEmpty_elm_builtin") {
            return RBEmpty_elm_builtin(LBlack);
         } else {
            var _p9 = _p8._1;
            return A5(RBNode_elm_builtin,
            _p8._0,
            _p9,
            A2(f,_p9,_p8._2),
            A2(map,f,_p8._3),
            A2(map,f,_p8._4));
         }
   });
   var NBlack = {ctor: "NBlack"};
   var BBlack = {ctor: "BBlack"};
   var Black = {ctor: "Black"};
   var ensureBlackRoot = function (dict) {
      var _p10 = dict;
      if (_p10.ctor === "RBNode_elm_builtin" && _p10._0.ctor === "Red")
      {
            return A5(RBNode_elm_builtin,
            Black,
            _p10._1,
            _p10._2,
            _p10._3,
            _p10._4);
         } else {
            return dict;
         }
   };
   var blackish = function (t) {
      var _p11 = t;
      if (_p11.ctor === "RBNode_elm_builtin") {
            var _p12 = _p11._0;
            return _U.eq(_p12,Black) || _U.eq(_p12,BBlack);
         } else {
            return true;
         }
   };
   var blacken = function (t) {
      var _p13 = t;
      if (_p13.ctor === "RBEmpty_elm_builtin") {
            return RBEmpty_elm_builtin(LBlack);
         } else {
            return A5(RBNode_elm_builtin,
            Black,
            _p13._1,
            _p13._2,
            _p13._3,
            _p13._4);
         }
   };
   var Red = {ctor: "Red"};
   var moreBlack = function (color) {
      var _p14 = color;
      switch (_p14.ctor)
      {case "Black": return BBlack;
         case "Red": return Black;
         case "NBlack": return Red;
         default:
         return $Native$Debug.crash("Can\'t make a double black node more black!");}
   };
   var lessBlack = function (color) {
      var _p15 = color;
      switch (_p15.ctor)
      {case "BBlack": return Black;
         case "Black": return Red;
         case "Red": return NBlack;
         default:
         return $Native$Debug.crash("Can\'t make a negative black node less black!");}
   };
   var lessBlackTree = function (dict) {
      var _p16 = dict;
      if (_p16.ctor === "RBNode_elm_builtin") {
            return A5(RBNode_elm_builtin,
            lessBlack(_p16._0),
            _p16._1,
            _p16._2,
            _p16._3,
            _p16._4);
         } else {
            return RBEmpty_elm_builtin(LBlack);
         }
   };
   var balancedTree = function (col) {
      return function (xk) {
         return function (xv) {
            return function (yk) {
               return function (yv) {
                  return function (zk) {
                     return function (zv) {
                        return function (a) {
                           return function (b) {
                              return function (c) {
                                 return function (d) {
                                    return A5(RBNode_elm_builtin,
                                    lessBlack(col),
                                    yk,
                                    yv,
                                    A5(RBNode_elm_builtin,Black,xk,xv,a,b),
                                    A5(RBNode_elm_builtin,Black,zk,zv,c,d));
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   var redden = function (t) {
      var _p17 = t;
      if (_p17.ctor === "RBEmpty_elm_builtin") {
            return $Native$Debug.crash("can\'t make a Leaf red");
         } else {
            return A5(RBNode_elm_builtin,
            Red,
            _p17._1,
            _p17._2,
            _p17._3,
            _p17._4);
         }
   };
   var balanceHelp = function (tree) {
      var _p18 = tree;
      _v31_6: do {
         _v31_5: do {
            _v31_4: do {
               _v31_3: do {
                  _v31_2: do {
                     _v31_1: do {
                        _v31_0: do {
                           if (_p18.ctor === "RBNode_elm_builtin") {
                                 if (_p18._3.ctor === "RBNode_elm_builtin") {
                                       if (_p18._4.ctor === "RBNode_elm_builtin") {
                                             switch (_p18._3._0.ctor)
                                             {case "Red": switch (_p18._4._0.ctor)
                                                  {case "Red":
                                                     if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red")
                                                       {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red")
                                                             {
                                                                   break _v31_1;
                                                                } else {
                                                                   if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red")
                                                                   {
                                                                         break _v31_2;
                                                                      } else {
                                                                         if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red")
                                                                         {
                                                                               break _v31_3;
                                                                            } else {
                                                                               break _v31_6;
                                                                            }
                                                                      }
                                                                }
                                                          }
                                                     case "NBlack":
                                                     if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red")
                                                       {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red")
                                                             {
                                                                   break _v31_1;
                                                                } else {
                                                                   if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_4;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          }
                                                     default:
                                                     if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red")
                                                       {
                                                             break _v31_0;
                                                          } else {
                                                             if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red")
                                                             {
                                                                   break _v31_1;
                                                                } else {
                                                                   break _v31_6;
                                                                }
                                                          }}
                                                case "NBlack": switch (_p18._4._0.ctor)
                                                  {case "Red":
                                                     if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red")
                                                       {
                                                             break _v31_2;
                                                          } else {
                                                             if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red")
                                                             {
                                                                   break _v31_3;
                                                                } else {
                                                                   if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_5;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          }
                                                     case "NBlack": if (_p18._0.ctor === "BBlack") {
                                                             if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                             {
                                                                   break _v31_4;
                                                                } else {
                                                                   if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                                   {
                                                                         break _v31_5;
                                                                      } else {
                                                                         break _v31_6;
                                                                      }
                                                                }
                                                          } else {
                                                             break _v31_6;
                                                          }
                                                     default:
                                                     if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                       {
                                                             break _v31_5;
                                                          } else {
                                                             break _v31_6;
                                                          }}
                                                default: switch (_p18._4._0.ctor)
                                                  {case "Red":
                                                     if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red")
                                                       {
                                                             break _v31_2;
                                                          } else {
                                                             if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red")
                                                             {
                                                                   break _v31_3;
                                                                } else {
                                                                   break _v31_6;
                                                                }
                                                          }
                                                     case "NBlack":
                                                     if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                       {
                                                             break _v31_4;
                                                          } else {
                                                             break _v31_6;
                                                          }
                                                     default: break _v31_6;}}
                                          } else {
                                             switch (_p18._3._0.ctor)
                                             {case "Red":
                                                if (_p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Red")
                                                  {
                                                        break _v31_0;
                                                     } else {
                                                        if (_p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Red")
                                                        {
                                                              break _v31_1;
                                                           } else {
                                                              break _v31_6;
                                                           }
                                                     }
                                                case "NBlack":
                                                if (_p18._0.ctor === "BBlack" && _p18._3._3.ctor === "RBNode_elm_builtin" && _p18._3._3._0.ctor === "Black" && _p18._3._4.ctor === "RBNode_elm_builtin" && _p18._3._4._0.ctor === "Black")
                                                  {
                                                        break _v31_5;
                                                     } else {
                                                        break _v31_6;
                                                     }
                                                default: break _v31_6;}
                                          }
                                    } else {
                                       if (_p18._4.ctor === "RBNode_elm_builtin") {
                                             switch (_p18._4._0.ctor)
                                             {case "Red":
                                                if (_p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Red")
                                                  {
                                                        break _v31_2;
                                                     } else {
                                                        if (_p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Red")
                                                        {
                                                              break _v31_3;
                                                           } else {
                                                              break _v31_6;
                                                           }
                                                     }
                                                case "NBlack":
                                                if (_p18._0.ctor === "BBlack" && _p18._4._3.ctor === "RBNode_elm_builtin" && _p18._4._3._0.ctor === "Black" && _p18._4._4.ctor === "RBNode_elm_builtin" && _p18._4._4._0.ctor === "Black")
                                                  {
                                                        break _v31_4;
                                                     } else {
                                                        break _v31_6;
                                                     }
                                                default: break _v31_6;}
                                          } else {
                                             break _v31_6;
                                          }
                                    }
                              } else {
                                 break _v31_6;
                              }
                        } while (false);
                        return balancedTree(_p18._0)(_p18._3._3._1)(_p18._3._3._2)(_p18._3._1)(_p18._3._2)(_p18._1)(_p18._2)(_p18._3._3._3)(_p18._3._3._4)(_p18._3._4)(_p18._4);
                     } while (false);
                     return balancedTree(_p18._0)(_p18._3._1)(_p18._3._2)(_p18._3._4._1)(_p18._3._4._2)(_p18._1)(_p18._2)(_p18._3._3)(_p18._3._4._3)(_p18._3._4._4)(_p18._4);
                  } while (false);
                  return balancedTree(_p18._0)(_p18._1)(_p18._2)(_p18._4._3._1)(_p18._4._3._2)(_p18._4._1)(_p18._4._2)(_p18._3)(_p18._4._3._3)(_p18._4._3._4)(_p18._4._4);
               } while (false);
               return balancedTree(_p18._0)(_p18._1)(_p18._2)(_p18._4._1)(_p18._4._2)(_p18._4._4._1)(_p18._4._4._2)(_p18._3)(_p18._4._3)(_p18._4._4._3)(_p18._4._4._4);
            } while (false);
            return A5(RBNode_elm_builtin,
            Black,
            _p18._4._3._1,
            _p18._4._3._2,
            A5(RBNode_elm_builtin,
            Black,
            _p18._1,
            _p18._2,
            _p18._3,
            _p18._4._3._3),
            A5(balance,
            Black,
            _p18._4._1,
            _p18._4._2,
            _p18._4._3._4,
            redden(_p18._4._4)));
         } while (false);
         return A5(RBNode_elm_builtin,
         Black,
         _p18._3._4._1,
         _p18._3._4._2,
         A5(balance,
         Black,
         _p18._3._1,
         _p18._3._2,
         redden(_p18._3._3),
         _p18._3._4._3),
         A5(RBNode_elm_builtin,
         Black,
         _p18._1,
         _p18._2,
         _p18._3._4._4,
         _p18._4));
      } while (false);
      return tree;
   };
   var balance = F5(function (c,k,v,l,r) {
      var tree = A5(RBNode_elm_builtin,c,k,v,l,r);
      return blackish(tree) ? balanceHelp(tree) : tree;
   });
   var bubble = F5(function (c,k,v,l,r) {
      return isBBlack(l) || isBBlack(r) ? A5(balance,
      moreBlack(c),
      k,
      v,
      lessBlackTree(l),
      lessBlackTree(r)) : A5(RBNode_elm_builtin,c,k,v,l,r);
   });
   var removeMax = F5(function (c,k,v,l,r) {
      var _p19 = r;
      if (_p19.ctor === "RBEmpty_elm_builtin") {
            return A3(rem,c,l,r);
         } else {
            return A5(bubble,
            c,
            k,
            v,
            l,
            A5(removeMax,_p19._0,_p19._1,_p19._2,_p19._3,_p19._4));
         }
   });
   var rem = F3(function (c,l,r) {
      var _p20 = {ctor: "_Tuple2",_0: l,_1: r};
      if (_p20._0.ctor === "RBEmpty_elm_builtin") {
            if (_p20._1.ctor === "RBEmpty_elm_builtin") {
                  var _p21 = c;
                  switch (_p21.ctor)
                  {case "Red": return RBEmpty_elm_builtin(LBlack);
                     case "Black": return RBEmpty_elm_builtin(LBBlack);
                     default:
                     return $Native$Debug.crash("cannot have bblack or nblack nodes at this point");}
               } else {
                  var _p24 = _p20._1._0;
                  var _p23 = _p20._0._0;
                  var _p22 = {ctor: "_Tuple3",_0: c,_1: _p23,_2: _p24};
                  if (_p22.ctor === "_Tuple3" && _p22._0.ctor === "Black" && _p22._1.ctor === "LBlack" && _p22._2.ctor === "Red")
                  {
                        return A5(RBNode_elm_builtin,
                        Black,
                        _p20._1._1,
                        _p20._1._2,
                        _p20._1._3,
                        _p20._1._4);
                     } else {
                        return A4(reportRemBug,
                        "Black/LBlack/Red",
                        c,
                        $Basics.toString(_p23),
                        $Basics.toString(_p24));
                     }
               }
         } else {
            if (_p20._1.ctor === "RBEmpty_elm_builtin") {
                  var _p27 = _p20._1._0;
                  var _p26 = _p20._0._0;
                  var _p25 = {ctor: "_Tuple3",_0: c,_1: _p26,_2: _p27};
                  if (_p25.ctor === "_Tuple3" && _p25._0.ctor === "Black" && _p25._1.ctor === "Red" && _p25._2.ctor === "LBlack")
                  {
                        return A5(RBNode_elm_builtin,
                        Black,
                        _p20._0._1,
                        _p20._0._2,
                        _p20._0._3,
                        _p20._0._4);
                     } else {
                        return A4(reportRemBug,
                        "Black/Red/LBlack",
                        c,
                        $Basics.toString(_p26),
                        $Basics.toString(_p27));
                     }
               } else {
                  var _p31 = _p20._0._2;
                  var _p30 = _p20._0._4;
                  var _p29 = _p20._0._1;
                  var l$ = A5(removeMax,_p20._0._0,_p29,_p31,_p20._0._3,_p30);
                  var _p28 = A3(maxWithDefault,_p29,_p31,_p30);
                  var k = _p28._0;
                  var v = _p28._1;
                  return A5(bubble,c,k,v,l$,r);
               }
         }
   });
   var update = F3(function (k,alter,dict) {
      var up = function (dict) {
         var _p32 = dict;
         if (_p32.ctor === "RBEmpty_elm_builtin") {
               var _p33 = alter($Maybe.Nothing);
               if (_p33.ctor === "Nothing") {
                     return {ctor: "_Tuple2",_0: Same,_1: empty};
                  } else {
                     return {ctor: "_Tuple2"
                            ,_0: Insert
                            ,_1: A5(RBNode_elm_builtin,Red,k,_p33._0,empty,empty)};
                  }
            } else {
               var _p44 = _p32._2;
               var _p43 = _p32._4;
               var _p42 = _p32._3;
               var _p41 = _p32._1;
               var _p40 = _p32._0;
               var _p34 = A2($Basics.compare,k,_p41);
               switch (_p34.ctor)
               {case "EQ": var _p35 = alter($Maybe.Just(_p44));
                    if (_p35.ctor === "Nothing") {
                          return {ctor: "_Tuple2"
                                 ,_0: Remove
                                 ,_1: A3(rem,_p40,_p42,_p43)};
                       } else {
                          return {ctor: "_Tuple2"
                                 ,_0: Same
                                 ,_1: A5(RBNode_elm_builtin,_p40,_p41,_p35._0,_p42,_p43)};
                       }
                  case "LT": var _p36 = up(_p42);
                    var flag = _p36._0;
                    var newLeft = _p36._1;
                    var _p37 = flag;
                    switch (_p37.ctor)
                    {case "Same": return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode_elm_builtin,_p40,_p41,_p44,newLeft,_p43)};
                       case "Insert": return {ctor: "_Tuple2"
                                             ,_0: Insert
                                             ,_1: A5(balance,_p40,_p41,_p44,newLeft,_p43)};
                       default: return {ctor: "_Tuple2"
                                       ,_0: Remove
                                       ,_1: A5(bubble,_p40,_p41,_p44,newLeft,_p43)};}
                  default: var _p38 = up(_p43);
                    var flag = _p38._0;
                    var newRight = _p38._1;
                    var _p39 = flag;
                    switch (_p39.ctor)
                    {case "Same": return {ctor: "_Tuple2"
                                         ,_0: Same
                                         ,_1: A5(RBNode_elm_builtin,_p40,_p41,_p44,_p42,newRight)};
                       case "Insert": return {ctor: "_Tuple2"
                                             ,_0: Insert
                                             ,_1: A5(balance,_p40,_p41,_p44,_p42,newRight)};
                       default: return {ctor: "_Tuple2"
                                       ,_0: Remove
                                       ,_1: A5(bubble,_p40,_p41,_p44,_p42,newRight)};}}
            }
      };
      var _p45 = up(dict);
      var flag = _p45._0;
      var updatedDict = _p45._1;
      var _p46 = flag;
      switch (_p46.ctor)
      {case "Same": return updatedDict;
         case "Insert": return ensureBlackRoot(updatedDict);
         default: return blacken(updatedDict);}
   });
   var insert = F3(function (key,value,dict) {
      return A3(update,
      key,
      $Basics.always($Maybe.Just(value)),
      dict);
   });
   var singleton = F2(function (key,value) {
      return A3(insert,key,value,empty);
   });
   var union = F2(function (t1,t2) {
      return A3(foldl,insert,t2,t1);
   });
   var fromList = function (assocs) {
      return A3($List.foldl,
      F2(function (_p47,dict) {
         var _p48 = _p47;
         return A3(insert,_p48._0,_p48._1,dict);
      }),
      empty,
      assocs);
   };
   var filter = F2(function (predicate,dictionary) {
      var add = F3(function (key,value,dict) {
         return A2(predicate,key,value) ? A3(insert,
         key,
         value,
         dict) : dict;
      });
      return A3(foldl,add,empty,dictionary);
   });
   var intersect = F2(function (t1,t2) {
      return A2(filter,
      F2(function (k,_p49) {    return A2(member,k,t2);}),
      t1);
   });
   var partition = F2(function (predicate,dict) {
      var add = F3(function (key,value,_p50) {
         var _p51 = _p50;
         var _p53 = _p51._1;
         var _p52 = _p51._0;
         return A2(predicate,key,value) ? {ctor: "_Tuple2"
                                          ,_0: A3(insert,key,value,_p52)
                                          ,_1: _p53} : {ctor: "_Tuple2"
                                                       ,_0: _p52
                                                       ,_1: A3(insert,key,value,_p53)};
      });
      return A3(foldl,add,{ctor: "_Tuple2",_0: empty,_1: empty},dict);
   });
   var remove = F2(function (key,dict) {
      return A3(update,key,$Basics.always($Maybe.Nothing),dict);
   });
   var diff = F2(function (t1,t2) {
      return A3(foldl,
      F3(function (k,v,t) {    return A2(remove,k,t);}),
      t1,
      t2);
   });
   return _elm.Dict.values = {_op: _op
                             ,empty: empty
                             ,singleton: singleton
                             ,insert: insert
                             ,update: update
                             ,isEmpty: isEmpty
                             ,get: get
                             ,remove: remove
                             ,member: member
                             ,size: size
                             ,filter: filter
                             ,partition: partition
                             ,foldl: foldl
                             ,foldr: foldr
                             ,map: map
                             ,union: union
                             ,intersect: intersect
                             ,diff: diff
                             ,keys: keys
                             ,values: values
                             ,toList: toList
                             ,fromList: fromList};
};
Elm.Native.Json = {};

Elm.Native.Json.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Json = localRuntime.Native.Json || {};
	if (localRuntime.Native.Json.values) {
		return localRuntime.Native.Json.values;
	}

	var ElmArray = Elm.Native.Array.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);
	var Result = Elm.Result.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);


	function crash(expected, actual) {
		throw new Error(
			'expecting ' + expected + ' but got ' + JSON.stringify(actual)
		);
	}


	// PRIMITIVE VALUES

	function decodeNull(successValue) {
		return function(value) {
			if (value === null) {
				return successValue;
			}
			crash('null', value);
		};
	}


	function decodeString(value) {
		if (typeof value === 'string' || value instanceof String) {
			return value;
		}
		crash('a String', value);
	}


	function decodeFloat(value) {
		if (typeof value === 'number') {
			return value;
		}
		crash('a Float', value);
	}


	function decodeInt(value) {
		if (typeof value !== 'number') {
			crash('an Int', value);
		}

		if (value < 2147483647 && value > -2147483647 && (value | 0) === value) {
			return value;
		}

		if (isFinite(value) && !(value % 1)) {
			return value;
		}

		crash('an Int', value);
	}


	function decodeBool(value) {
		if (typeof value === 'boolean') {
			return value;
		}
		crash('a Bool', value);
	}


	// ARRAY

	function decodeArray(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var array = new Array(len);
				for (var i = len; i--; ) {
					array[i] = decoder(value[i]);
				}
				return ElmArray.fromJSArray(array);
			}
			crash('an Array', value);
		};
	}


	// LIST

	function decodeList(decoder) {
		return function(value) {
			if (value instanceof Array) {
				var len = value.length;
				var list = List.Nil;
				for (var i = len; i--; ) {
					list = List.Cons( decoder(value[i]), list );
				}
				return list;
			}
			crash('a List', value);
		};
	}


	// MAYBE

	function decodeMaybe(decoder) {
		return function(value) {
			try {
				return Maybe.Just(decoder(value));
			} catch(e) {
				return Maybe.Nothing;
			}
		};
	}


	// FIELDS

	function decodeField(field, decoder) {
		return function(value) {
			var subValue = value[field];
			if (subValue !== undefined) {
				return decoder(subValue);
			}
			crash("an object with field '" + field + "'", value);
		};
	}


	// OBJECTS

	function decodeKeyValuePairs(decoder) {
		return function(value) {
			var isObject =
				typeof value === 'object'
					&& value !== null
					&& !(value instanceof Array);

			if (isObject) {
				var keyValuePairs = List.Nil;
				for (var key in value)
				{
					var elmValue = decoder(value[key]);
					var pair = Utils.Tuple2(key, elmValue);
					keyValuePairs = List.Cons(pair, keyValuePairs);
				}
				return keyValuePairs;
			}

			crash('an object', value);
		};
	}

	function decodeObject1(f, d1) {
		return function(value) {
			return f(d1(value));
		};
	}

	function decodeObject2(f, d1, d2) {
		return function(value) {
			return A2( f, d1(value), d2(value) );
		};
	}

	function decodeObject3(f, d1, d2, d3) {
		return function(value) {
			return A3( f, d1(value), d2(value), d3(value) );
		};
	}

	function decodeObject4(f, d1, d2, d3, d4) {
		return function(value) {
			return A4( f, d1(value), d2(value), d3(value), d4(value) );
		};
	}

	function decodeObject5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			return A5( f, d1(value), d2(value), d3(value), d4(value), d5(value) );
		};
	}

	function decodeObject6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			return A6( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value)
			);
		};
	}

	function decodeObject7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			return A7( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value)
			);
		};
	}

	function decodeObject8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			return A8( f,
				d1(value),
				d2(value),
				d3(value),
				d4(value),
				d5(value),
				d6(value),
				d7(value),
				d8(value)
			);
		};
	}


	// TUPLES

	function decodeTuple1(f, d1) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 1 ) {
				crash('a Tuple of length 1', value);
			}
			return f( d1(value[0]) );
		};
	}

	function decodeTuple2(f, d1, d2) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 2 ) {
				crash('a Tuple of length 2', value);
			}
			return A2( f, d1(value[0]), d2(value[1]) );
		};
	}

	function decodeTuple3(f, d1, d2, d3) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 3 ) {
				crash('a Tuple of length 3', value);
			}
			return A3( f, d1(value[0]), d2(value[1]), d3(value[2]) );
		};
	}


	function decodeTuple4(f, d1, d2, d3, d4) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 4 ) {
				crash('a Tuple of length 4', value);
			}
			return A4( f, d1(value[0]), d2(value[1]), d3(value[2]), d4(value[3]) );
		};
	}


	function decodeTuple5(f, d1, d2, d3, d4, d5) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 5 ) {
				crash('a Tuple of length 5', value);
			}
			return A5( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4])
			);
		};
	}


	function decodeTuple6(f, d1, d2, d3, d4, d5, d6) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 6 ) {
				crash('a Tuple of length 6', value);
			}
			return A6( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5])
			);
		};
	}

	function decodeTuple7(f, d1, d2, d3, d4, d5, d6, d7) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 7 ) {
				crash('a Tuple of length 7', value);
			}
			return A7( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6])
			);
		};
	}


	function decodeTuple8(f, d1, d2, d3, d4, d5, d6, d7, d8) {
		return function(value) {
			if ( !(value instanceof Array) || value.length !== 8 ) {
				crash('a Tuple of length 8', value);
			}
			return A8( f,
				d1(value[0]),
				d2(value[1]),
				d3(value[2]),
				d4(value[3]),
				d5(value[4]),
				d6(value[5]),
				d7(value[6]),
				d8(value[7])
			);
		};
	}


	// CUSTOM DECODERS

	function decodeValue(value) {
		return value;
	}

	function runDecoderValue(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function customDecoder(decoder, callback) {
		return function(value) {
			var result = callback(decoder(value));
			if (result.ctor === 'Err') {
				throw new Error('custom decoder failed: ' + result._0);
			}
			return result._0;
		};
	}

	function andThen(decode, callback) {
		return function(value) {
			var result = decode(value);
			return callback(result)(value);
		};
	}

	function fail(msg) {
		return function(value) {
			throw new Error(msg);
		};
	}

	function succeed(successValue) {
		return function(value) {
			return successValue;
		};
	}


	// ONE OF MANY

	function oneOf(decoders) {
		return function(value) {
			var errors = [];
			var temp = decoders;
			while (temp.ctor !== '[]') {
				try {
					return temp._0(value);
				} catch(e) {
					errors.push(e.message);
				}
				temp = temp._1;
			}
			throw new Error('expecting one of the following:\n    ' + errors.join('\n    '));
		};
	}

	function get(decoder, value) {
		try {
			return Result.Ok(decoder(value));
		} catch(e) {
			return Result.Err(e.message);
		}
	}


	// ENCODE / DECODE

	function runDecoderString(decoder, string) {
		try {
			return Result.Ok(decoder(JSON.parse(string)));
		} catch(e) {
			return Result.Err(e.message);
		}
	}

	function encode(indentLevel, value) {
		return JSON.stringify(value, null, indentLevel);
	}

	function identity(value) {
		return value;
	}

	function encodeObject(keyValuePairs) {
		var obj = {};
		while (keyValuePairs.ctor !== '[]') {
			var pair = keyValuePairs._0;
			obj[pair._0] = pair._1;
			keyValuePairs = keyValuePairs._1;
		}
		return obj;
	}

	return localRuntime.Native.Json.values = {
		encode: F2(encode),
		runDecoderString: F2(runDecoderString),
		runDecoderValue: F2(runDecoderValue),

		get: F2(get),
		oneOf: oneOf,

		decodeNull: decodeNull,
		decodeInt: decodeInt,
		decodeFloat: decodeFloat,
		decodeString: decodeString,
		decodeBool: decodeBool,

		decodeMaybe: decodeMaybe,

		decodeList: decodeList,
		decodeArray: decodeArray,

		decodeField: F2(decodeField),

		decodeObject1: F2(decodeObject1),
		decodeObject2: F3(decodeObject2),
		decodeObject3: F4(decodeObject3),
		decodeObject4: F5(decodeObject4),
		decodeObject5: F6(decodeObject5),
		decodeObject6: F7(decodeObject6),
		decodeObject7: F8(decodeObject7),
		decodeObject8: F9(decodeObject8),
		decodeKeyValuePairs: decodeKeyValuePairs,

		decodeTuple1: F2(decodeTuple1),
		decodeTuple2: F3(decodeTuple2),
		decodeTuple3: F4(decodeTuple3),
		decodeTuple4: F5(decodeTuple4),
		decodeTuple5: F6(decodeTuple5),
		decodeTuple6: F7(decodeTuple6),
		decodeTuple7: F8(decodeTuple7),
		decodeTuple8: F9(decodeTuple8),

		andThen: F2(andThen),
		decodeValue: decodeValue,
		customDecoder: F2(customDecoder),
		fail: fail,
		succeed: succeed,

		identity: identity,
		encodeNull: null,
		encodeArray: ElmArray.toJSArray,
		encodeList: List.toArray,
		encodeObject: encodeObject

	};
};

Elm.Json = Elm.Json || {};
Elm.Json.Encode = Elm.Json.Encode || {};
Elm.Json.Encode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Encode = _elm.Json.Encode || {};
   if (_elm.Json.Encode.values) return _elm.Json.Encode.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm);
   var _op = {};
   var list = $Native$Json.encodeList;
   var array = $Native$Json.encodeArray;
   var object = $Native$Json.encodeObject;
   var $null = $Native$Json.encodeNull;
   var bool = $Native$Json.identity;
   var $float = $Native$Json.identity;
   var $int = $Native$Json.identity;
   var string = $Native$Json.identity;
   var encode = $Native$Json.encode;
   var Value = {ctor: "Value"};
   return _elm.Json.Encode.values = {_op: _op
                                    ,encode: encode
                                    ,string: string
                                    ,$int: $int
                                    ,$float: $float
                                    ,bool: bool
                                    ,$null: $null
                                    ,list: list
                                    ,array: array
                                    ,object: object};
};
Elm.Json = Elm.Json || {};
Elm.Json.Decode = Elm.Json.Decode || {};
Elm.Json.Decode.make = function (_elm) {
   "use strict";
   _elm.Json = _elm.Json || {};
   _elm.Json.Decode = _elm.Json.Decode || {};
   if (_elm.Json.Decode.values) return _elm.Json.Decode.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Dict = Elm.Dict.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Json = Elm.Native.Json.make(_elm),
   $Result = Elm.Result.make(_elm);
   var _op = {};
   var tuple8 = $Native$Json.decodeTuple8;
   var tuple7 = $Native$Json.decodeTuple7;
   var tuple6 = $Native$Json.decodeTuple6;
   var tuple5 = $Native$Json.decodeTuple5;
   var tuple4 = $Native$Json.decodeTuple4;
   var tuple3 = $Native$Json.decodeTuple3;
   var tuple2 = $Native$Json.decodeTuple2;
   var tuple1 = $Native$Json.decodeTuple1;
   var succeed = $Native$Json.succeed;
   var fail = $Native$Json.fail;
   var andThen = $Native$Json.andThen;
   var customDecoder = $Native$Json.customDecoder;
   var decodeValue = $Native$Json.runDecoderValue;
   var value = $Native$Json.decodeValue;
   var maybe = $Native$Json.decodeMaybe;
   var $null = $Native$Json.decodeNull;
   var array = $Native$Json.decodeArray;
   var list = $Native$Json.decodeList;
   var bool = $Native$Json.decodeBool;
   var $int = $Native$Json.decodeInt;
   var $float = $Native$Json.decodeFloat;
   var string = $Native$Json.decodeString;
   var oneOf = $Native$Json.oneOf;
   var keyValuePairs = $Native$Json.decodeKeyValuePairs;
   var object8 = $Native$Json.decodeObject8;
   var object7 = $Native$Json.decodeObject7;
   var object6 = $Native$Json.decodeObject6;
   var object5 = $Native$Json.decodeObject5;
   var object4 = $Native$Json.decodeObject4;
   var object3 = $Native$Json.decodeObject3;
   var object2 = $Native$Json.decodeObject2;
   var object1 = $Native$Json.decodeObject1;
   _op[":="] = $Native$Json.decodeField;
   var at = F2(function (fields,decoder) {
      return A3($List.foldr,
      F2(function (x,y) {    return A2(_op[":="],x,y);}),
      decoder,
      fields);
   });
   var decodeString = $Native$Json.runDecoderString;
   var map = $Native$Json.decodeObject1;
   var dict = function (decoder) {
      return A2(map,$Dict.fromList,keyValuePairs(decoder));
   };
   var Decoder = {ctor: "Decoder"};
   return _elm.Json.Decode.values = {_op: _op
                                    ,decodeString: decodeString
                                    ,decodeValue: decodeValue
                                    ,string: string
                                    ,$int: $int
                                    ,$float: $float
                                    ,bool: bool
                                    ,$null: $null
                                    ,list: list
                                    ,array: array
                                    ,tuple1: tuple1
                                    ,tuple2: tuple2
                                    ,tuple3: tuple3
                                    ,tuple4: tuple4
                                    ,tuple5: tuple5
                                    ,tuple6: tuple6
                                    ,tuple7: tuple7
                                    ,tuple8: tuple8
                                    ,at: at
                                    ,object1: object1
                                    ,object2: object2
                                    ,object3: object3
                                    ,object4: object4
                                    ,object5: object5
                                    ,object6: object6
                                    ,object7: object7
                                    ,object8: object8
                                    ,keyValuePairs: keyValuePairs
                                    ,dict: dict
                                    ,maybe: maybe
                                    ,oneOf: oneOf
                                    ,map: map
                                    ,fail: fail
                                    ,succeed: succeed
                                    ,andThen: andThen
                                    ,value: value
                                    ,customDecoder: customDecoder};
};
Elm.Native.Regex = {};
Elm.Native.Regex.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Regex = localRuntime.Native.Regex || {};
	if (localRuntime.Native.Regex.values)
	{
		return localRuntime.Native.Regex.values;
	}
	if ('values' in Elm.Native.Regex)
	{
		return localRuntime.Native.Regex.values = Elm.Native.Regex.values;
	}

	var List = Elm.Native.List.make(localRuntime);
	var Maybe = Elm.Maybe.make(localRuntime);

	function escape(str)
	{
		return str.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
	}
	function caseInsensitive(re)
	{
		return new RegExp(re.source, 'gi');
	}
	function regex(raw)
	{
		return new RegExp(raw, 'g');
	}

	function contains(re, string)
	{
		return string.match(re) !== null;
	}

	function find(n, re, str)
	{
		n = n.ctor === 'All' ? Infinity : n._0;
		var out = [];
		var number = 0;
		var string = str;
		var lastIndex = re.lastIndex;
		var prevLastIndex = -1;
		var result;
		while (number++ < n && (result = re.exec(string)))
		{
			if (prevLastIndex === re.lastIndex) break;
			var i = result.length - 1;
			var subs = new Array(i);
			while (i > 0)
			{
				var submatch = result[i];
				subs[--i] = submatch === undefined
					? Maybe.Nothing
					: Maybe.Just(submatch);
			}
			out.push({
				match: result[0],
				submatches: List.fromArray(subs),
				index: result.index,
				number: number
			});
			prevLastIndex = re.lastIndex;
		}
		re.lastIndex = lastIndex;
		return List.fromArray(out);
	}

	function replace(n, re, replacer, string)
	{
		n = n.ctor === 'All' ? Infinity : n._0;
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
				submatches[--i] = submatch === undefined
					? Maybe.Nothing
					: Maybe.Just(submatch);
			}
			return replacer({
				match: match,
				submatches: List.fromArray(submatches),
				index: arguments[i - 1],
				number: count
			});
		}
		return string.replace(re, jsReplacer);
	}

	function split(n, re, str)
	{
		n = n.ctor === 'All' ? Infinity : n._0;
		if (n === Infinity)
		{
			return List.fromArray(str.split(re));
		}
		var string = str;
		var result;
		var out = [];
		var start = re.lastIndex;
		while (n--)
		{
			if (!(result = re.exec(string))) break;
			out.push(string.slice(start, result.index));
			start = re.lastIndex;
		}
		out.push(string.slice(start));
		return List.fromArray(out);
	}

	return Elm.Native.Regex.values = {
		regex: regex,
		caseInsensitive: caseInsensitive,
		escape: escape,

		contains: F2(contains),
		find: F3(find),
		replace: F4(replace),
		split: F3(split)
	};
};

Elm.Random = Elm.Random || {};
Elm.Random.make = function (_elm) {
   "use strict";
   _elm.Random = _elm.Random || {};
   if (_elm.Random.values) return _elm.Random.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $List = Elm.List.make(_elm);
   var _op = {};
   var magicNum8 = 2147483562;
   var range = function (_p0) {
      return {ctor: "_Tuple2",_0: 0,_1: magicNum8};
   };
   var magicNum7 = 2137383399;
   var magicNum6 = 2147483563;
   var magicNum5 = 3791;
   var magicNum4 = 40692;
   var magicNum3 = 52774;
   var magicNum2 = 12211;
   var magicNum1 = 53668;
   var magicNum0 = 40014;
   var generate = F2(function (_p1,seed) {
      var _p2 = _p1;
      return _p2._0(seed);
   });
   var Seed = function (a) {    return {ctor: "Seed",_0: a};};
   var State = F2(function (a,b) {
      return {ctor: "State",_0: a,_1: b};
   });
   var initState = function (s$) {
      var s = A2($Basics.max,s$,0 - s$);
      var q = s / (magicNum6 - 1) | 0;
      var s2 = A2($Basics._op["%"],q,magicNum7 - 1);
      var s1 = A2($Basics._op["%"],s,magicNum6 - 1);
      return A2(State,s1 + 1,s2 + 1);
   };
   var next = function (_p3) {
      var _p4 = _p3;
      var _p6 = _p4._1;
      var _p5 = _p4._0;
      var k$ = _p6 / magicNum3 | 0;
      var s2$ = magicNum4 * (_p6 - k$ * magicNum3) - k$ * magicNum5;
      var s2$$ = _U.cmp(s2$,0) < 0 ? s2$ + magicNum7 : s2$;
      var k = _p5 / magicNum1 | 0;
      var s1$ = magicNum0 * (_p5 - k * magicNum1) - k * magicNum2;
      var s1$$ = _U.cmp(s1$,0) < 0 ? s1$ + magicNum6 : s1$;
      var z = s1$$ - s2$$;
      var z$ = _U.cmp(z,1) < 0 ? z + magicNum8 : z;
      return {ctor: "_Tuple2",_0: z$,_1: A2(State,s1$$,s2$$)};
   };
   var split = function (_p7) {
      var _p8 = _p7;
      var _p11 = _p8._1;
      var _p10 = _p8._0;
      var _p9 = $Basics.snd(next(_p8));
      var t1 = _p9._0;
      var t2 = _p9._1;
      var new_s2 = _U.eq(_p11,1) ? magicNum7 - 1 : _p11 - 1;
      var new_s1 = _U.eq(_p10,magicNum6 - 1) ? 1 : _p10 + 1;
      return {ctor: "_Tuple2"
             ,_0: A2(State,new_s1,t2)
             ,_1: A2(State,t1,new_s2)};
   };
   var initialSeed = function (n) {
      return Seed({state: initState(n)
                  ,next: next
                  ,split: split
                  ,range: range});
   };
   var Generator = function (a) {
      return {ctor: "Generator",_0: a};
   };
   var andThen = F2(function (_p12,callback) {
      var _p13 = _p12;
      return Generator(function (seed) {
         var _p14 = _p13._0(seed);
         var result = _p14._0;
         var newSeed = _p14._1;
         var _p15 = callback(result);
         var genB = _p15._0;
         return genB(newSeed);
      });
   });
   var map5 = F6(function (func,_p20,_p19,_p18,_p17,_p16) {
      var _p21 = _p20;
      var _p22 = _p19;
      var _p23 = _p18;
      var _p24 = _p17;
      var _p25 = _p16;
      return Generator(function (seed0) {
         var _p26 = _p21._0(seed0);
         var a = _p26._0;
         var seed1 = _p26._1;
         var _p27 = _p22._0(seed1);
         var b = _p27._0;
         var seed2 = _p27._1;
         var _p28 = _p23._0(seed2);
         var c = _p28._0;
         var seed3 = _p28._1;
         var _p29 = _p24._0(seed3);
         var d = _p29._0;
         var seed4 = _p29._1;
         var _p30 = _p25._0(seed4);
         var e = _p30._0;
         var seed5 = _p30._1;
         return {ctor: "_Tuple2",_0: A5(func,a,b,c,d,e),_1: seed5};
      });
   });
   var map4 = F5(function (func,_p34,_p33,_p32,_p31) {
      var _p35 = _p34;
      var _p36 = _p33;
      var _p37 = _p32;
      var _p38 = _p31;
      return Generator(function (seed0) {
         var _p39 = _p35._0(seed0);
         var a = _p39._0;
         var seed1 = _p39._1;
         var _p40 = _p36._0(seed1);
         var b = _p40._0;
         var seed2 = _p40._1;
         var _p41 = _p37._0(seed2);
         var c = _p41._0;
         var seed3 = _p41._1;
         var _p42 = _p38._0(seed3);
         var d = _p42._0;
         var seed4 = _p42._1;
         return {ctor: "_Tuple2",_0: A4(func,a,b,c,d),_1: seed4};
      });
   });
   var map3 = F4(function (func,_p45,_p44,_p43) {
      var _p46 = _p45;
      var _p47 = _p44;
      var _p48 = _p43;
      return Generator(function (seed0) {
         var _p49 = _p46._0(seed0);
         var a = _p49._0;
         var seed1 = _p49._1;
         var _p50 = _p47._0(seed1);
         var b = _p50._0;
         var seed2 = _p50._1;
         var _p51 = _p48._0(seed2);
         var c = _p51._0;
         var seed3 = _p51._1;
         return {ctor: "_Tuple2",_0: A3(func,a,b,c),_1: seed3};
      });
   });
   var map2 = F3(function (func,_p53,_p52) {
      var _p54 = _p53;
      var _p55 = _p52;
      return Generator(function (seed0) {
         var _p56 = _p54._0(seed0);
         var a = _p56._0;
         var seed1 = _p56._1;
         var _p57 = _p55._0(seed1);
         var b = _p57._0;
         var seed2 = _p57._1;
         return {ctor: "_Tuple2",_0: A2(func,a,b),_1: seed2};
      });
   });
   var map = F2(function (func,_p58) {
      var _p59 = _p58;
      return Generator(function (seed0) {
         var _p60 = _p59._0(seed0);
         var a = _p60._0;
         var seed1 = _p60._1;
         return {ctor: "_Tuple2",_0: func(a),_1: seed1};
      });
   });
   var listHelp = F4(function (list,n,generate,seed) {
      listHelp: while (true) if (_U.cmp(n,1) < 0)
      return {ctor: "_Tuple2",_0: $List.reverse(list),_1: seed};
      else {
            var _p61 = generate(seed);
            var value = _p61._0;
            var newSeed = _p61._1;
            var _v19 = A2($List._op["::"],value,list),
            _v20 = n - 1,
            _v21 = generate,
            _v22 = newSeed;
            list = _v19;
            n = _v20;
            generate = _v21;
            seed = _v22;
            continue listHelp;
         }
   });
   var list = F2(function (n,_p62) {
      var _p63 = _p62;
      return Generator(function (seed) {
         return A4(listHelp,_U.list([]),n,_p63._0,seed);
      });
   });
   var pair = F2(function (genA,genB) {
      return A3(map2,
      F2(function (v0,v1) {
         return {ctor: "_Tuple2",_0: v0,_1: v1};
      }),
      genA,
      genB);
   });
   var minInt = -2147483648;
   var maxInt = 2147483647;
   var iLogBase = F2(function (b,i) {
      return _U.cmp(i,b) < 0 ? 1 : 1 + A2(iLogBase,b,i / b | 0);
   });
   var $int = F2(function (a,b) {
      return Generator(function (_p64) {
         var _p65 = _p64;
         var _p70 = _p65._0;
         var base = 2147483561;
         var f = F3(function (n,acc,state) {
            f: while (true) {
               var _p66 = n;
               if (_p66 === 0) {
                     return {ctor: "_Tuple2",_0: acc,_1: state};
                  } else {
                     var _p67 = _p70.next(state);
                     var x = _p67._0;
                     var state$ = _p67._1;
                     var _v26 = n - 1,_v27 = x + acc * base,_v28 = state$;
                     n = _v26;
                     acc = _v27;
                     state = _v28;
                     continue f;
                  }
            }
         });
         var _p68 = _U.cmp(a,b) < 0 ? {ctor: "_Tuple2"
                                      ,_0: a
                                      ,_1: b} : {ctor: "_Tuple2",_0: b,_1: a};
         var lo = _p68._0;
         var hi = _p68._1;
         var k = hi - lo + 1;
         var n = A2(iLogBase,base,k);
         var _p69 = A3(f,n,1,_p70.state);
         var v = _p69._0;
         var state$ = _p69._1;
         return {ctor: "_Tuple2"
                ,_0: lo + A2($Basics._op["%"],v,k)
                ,_1: Seed(_U.update(_p70,{state: state$}))};
      });
   });
   var $float = F2(function (a,b) {
      return Generator(function (seed) {
         var _p71 = A2(generate,A2($int,minInt,maxInt),seed);
         var number = _p71._0;
         var newSeed = _p71._1;
         var negativeOneToOne = $Basics.toFloat(number) / $Basics.toFloat(maxInt - minInt);
         var _p72 = _U.cmp(a,b) < 0 ? {ctor: "_Tuple2"
                                      ,_0: a
                                      ,_1: b} : {ctor: "_Tuple2",_0: b,_1: a};
         var lo = _p72._0;
         var hi = _p72._1;
         var scaled = (lo + hi) / 2 + (hi - lo) * negativeOneToOne;
         return {ctor: "_Tuple2",_0: scaled,_1: newSeed};
      });
   });
   var bool = A2(map,
   F2(function (x,y) {    return _U.eq(x,y);})(1),
   A2($int,0,1));
   return _elm.Random.values = {_op: _op
                               ,bool: bool
                               ,$int: $int
                               ,$float: $float
                               ,list: list
                               ,pair: pair
                               ,map: map
                               ,map2: map2
                               ,map3: map3
                               ,map4: map4
                               ,map5: map5
                               ,andThen: andThen
                               ,minInt: minInt
                               ,maxInt: maxInt
                               ,generate: generate
                               ,initialSeed: initialSeed};
};
Elm.Regex = Elm.Regex || {};
Elm.Regex.make = function (_elm) {
   "use strict";
   _elm.Regex = _elm.Regex || {};
   if (_elm.Regex.values) return _elm.Regex.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Regex = Elm.Native.Regex.make(_elm);
   var _op = {};
   var split = $Native$Regex.split;
   var replace = $Native$Regex.replace;
   var find = $Native$Regex.find;
   var AtMost = function (a) {    return {ctor: "AtMost",_0: a};};
   var All = {ctor: "All"};
   var Match = F4(function (a,b,c,d) {
      return {match: a,submatches: b,index: c,number: d};
   });
   var contains = $Native$Regex.contains;
   var caseInsensitive = $Native$Regex.caseInsensitive;
   var regex = $Native$Regex.regex;
   var escape = $Native$Regex.escape;
   var Regex = {ctor: "Regex"};
   return _elm.Regex.values = {_op: _op
                              ,regex: regex
                              ,escape: escape
                              ,caseInsensitive: caseInsensitive
                              ,contains: contains
                              ,find: find
                              ,replace: replace
                              ,split: split
                              ,Match: Match
                              ,All: All
                              ,AtMost: AtMost};
};
Elm.Trampoline = Elm.Trampoline || {};
Elm.Trampoline.make = function (_elm) {
   "use strict";
   _elm.Trampoline = _elm.Trampoline || {};
   if (_elm.Trampoline.values) return _elm.Trampoline.values;
   var _U = Elm.Native.Utils.make(_elm);
   var _op = {};
   var trampoline = function (tramp) {
      trampoline: while (true) {
         var _p0 = tramp;
         if (_p0.ctor === "Done") {
               return _p0._0;
            } else {
               var _v1 = _p0._0({ctor: "_Tuple0"});
               tramp = _v1;
               continue trampoline;
            }
      }
   };
   var Continue = function (a) {
      return {ctor: "Continue",_0: a};
   };
   var Done = function (a) {    return {ctor: "Done",_0: a};};
   return _elm.Trampoline.values = {_op: _op
                                   ,trampoline: trampoline
                                   ,Done: Done
                                   ,Continue: Continue};
};
Elm.ParseInt = Elm.ParseInt || {};
Elm.ParseInt.make = function (_elm) {
   "use strict";
   _elm.ParseInt = _elm.ParseInt || {};
   if (_elm.ParseInt.values) return _elm.ParseInt.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Char = Elm.Char.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var charFromInt = function (i) {
      return _U.cmp(i,
      10) < 0 ? $Char.fromCode(i + $Char.toCode(_U.chr("0"))) : _U.cmp(i,
      36) < 0 ? $Char.fromCode(i - 10 + $Char.toCode(_U.chr("A"))) : _U.crash("ParseInt",
      {start: {line: 141,column: 5}
      ,end: {line: 141,column: 16}})($Basics.toString(i));
   };
   var toRadix$ = F2(function (radix,i) {
      return _U.cmp(i,
      radix) < 0 ? $String.fromChar(charFromInt(i)) : A2($Basics._op["++"],
      A2(toRadix$,radix,i / radix | 0),
      $String.fromChar(charFromInt(A2($Basics._op["%"],i,radix))));
   });
   var isBetween = F3(function (lower,upper,c) {
      var ci = $Char.toCode(c);
      return _U.cmp($Char.toCode(lower),ci) < 1 && _U.cmp(ci,
      $Char.toCode(upper)) < 1;
   });
   var charOffset = F2(function (basis,c) {
      return $Char.toCode(c) - $Char.toCode(basis);
   });
   var InvalidRadix = function (a) {
      return {ctor: "InvalidRadix",_0: a};
   };
   var toRadix = F2(function (radix,i) {
      return _U.cmp(2,radix) < 1 && _U.cmp(radix,
      36) < 1 ? _U.cmp(i,0) < 0 ? $Result.Ok(A2($Basics._op["++"],
      "-",
      A2(toRadix$,radix,0 - i))) : $Result.Ok(A2(toRadix$,
      radix,
      i)) : $Result.Err(InvalidRadix(radix));
   });
   var OutOfRange = function (a) {
      return {ctor: "OutOfRange",_0: a};
   };
   var InvalidChar = function (a) {
      return {ctor: "InvalidChar",_0: a};
   };
   var intFromChar = F2(function (radix,c) {
      var validInt = function (i) {
         return _U.cmp(i,
         radix) < 0 ? $Result.Ok(i) : $Result.Err(OutOfRange(c));
      };
      var toInt = A3(isBetween,
      _U.chr("0"),
      _U.chr("9"),
      c) ? $Result.Ok(A2(charOffset,_U.chr("0"),c)) : A3(isBetween,
      _U.chr("a"),
      _U.chr("z"),
      c) ? $Result.Ok(10 + A2(charOffset,
      _U.chr("a"),
      c)) : A3(isBetween,
      _U.chr("A"),
      _U.chr("Z"),
      c) ? $Result.Ok(10 + A2(charOffset,
      _U.chr("A"),
      c)) : $Result.Err(InvalidChar(c));
      return A2($Result.andThen,toInt,validInt);
   });
   var parseIntR = F2(function (radix,rstring) {
      var _p0 = $String.uncons(rstring);
      if (_p0.ctor === "Nothing") {
            return $Result.Ok(0);
         } else {
            return A2($Result.andThen,
            A2(intFromChar,radix,_p0._0._0),
            function (ci) {
               return A2($Result.andThen,
               A2(parseIntR,radix,_p0._0._1),
               function (ri) {
                  return $Result.Ok(ci + ri * radix);
               });
            });
         }
   });
   var parseIntRadix = F2(function (radix,string) {
      return _U.cmp(2,radix) < 1 && _U.cmp(radix,
      36) < 1 ? A2(parseIntR,
      radix,
      $String.reverse(string)) : $Result.Err(InvalidRadix(radix));
   });
   var parseInt = parseIntRadix(10);
   var parseIntOct = parseIntRadix(8);
   var parseIntHex = parseIntRadix(16);
   return _elm.ParseInt.values = {_op: _op
                                 ,parseInt: parseInt
                                 ,parseIntOct: parseIntOct
                                 ,parseIntHex: parseIntHex
                                 ,parseIntRadix: parseIntRadix
                                 ,toRadix: toRadix
                                 ,toRadix$: toRadix$
                                 ,InvalidChar: InvalidChar
                                 ,OutOfRange: OutOfRange
                                 ,InvalidRadix: InvalidRadix};
};
Elm.Color = Elm.Color || {};
Elm.Color.Convert = Elm.Color.Convert || {};
Elm.Color.Convert.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   _elm.Color.Convert = _elm.Color.Convert || {};
   if (_elm.Color.Convert.values) return _elm.Color.Convert.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Char = Elm.Char.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $ParseInt = Elm.ParseInt.make(_elm),
   $Regex = Elm.Regex.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var xyzToColor = function (_p0) {
      var _p1 = _p0;
      var c = function (ch) {
         return _U.cmp(ch,3.1308e-3) > 0 ? 1.055 * Math.pow(ch,
         1 / 2.4) - 5.5e-2 : 12.92 * ch;
      };
      var z$ = _p1.z / 100;
      var y$ = _p1.y / 100;
      var x$ = _p1.x / 100;
      return A3($Color.rgb,
      $Basics.round(c(x$ * 3.2404542 + y$ * -1.5371385 + z$ * -0.4986) * 255),
      $Basics.round(c(x$ * -0.969266 + y$ * 1.8760108 + z$ * 4.1556e-2) * 255),
      $Basics.round(c(x$ * 5.56434e-2 + y$ * 0.2040259 + z$ * 1.0572252) * 255));
   };
   var labToXyz = function (_p2) {
      var _p3 = _p2;
      var y = (_p3.l + 16) / 116;
      var c = function (ch) {
         var ch$ = ch * ch * ch;
         return _U.cmp(ch$,8.856e-3) > 0 ? ch$ : (ch - 16 / 116) / 7.787;
      };
      return {y: c(y) * 100
             ,x: c(y + _p3.a / 500) * 95.047
             ,z: c(y - _p3.b / 200) * 108.883};
   };
   var labToColor = function (lab) {
      return xyzToColor(labToXyz(lab));
   };
   var xyzToLab = function (_p4) {
      var _p5 = _p4;
      var c = function (ch) {
         return _U.cmp(ch,8.856e-3) > 0 ? Math.pow(ch,
         1 / 3) : 7.787 * ch + 16 / 116;
      };
      var x$ = c(_p5.x / 95.047);
      var y$ = c(_p5.y / 100);
      var z$ = c(_p5.z / 108.883);
      return {l: 116 * y$ - 16,a: 500 * (x$ - y$),b: 200 * (y$ - z$)};
   };
   var colorToXyz = function (cl) {
      var _p6 = $Color.toRgb(cl);
      var red = _p6.red;
      var green = _p6.green;
      var blue = _p6.blue;
      var c = function (ch) {
         var ch$ = $Basics.toFloat(ch) / 255;
         var ch$$ = _U.cmp(ch$,
         4.045e-2) > 0 ? Math.pow((ch$ + 5.5e-2) / 1.055,
         2.4) : ch$ / 12.92;
         return ch$$ * 100;
      };
      var r = c(red);
      var g = c(green);
      var b = c(blue);
      return {x: r * 0.4124 + g * 0.3576 + b * 0.1805
             ,y: r * 0.2126 + g * 0.7152 + b * 7.22e-2
             ,z: r * 1.93e-2 + g * 0.1192 + b * 0.9505};
   };
   var colorToLab = function (cl) {
      return xyzToLab(colorToXyz(cl));
   };
   var toRadix = function (n) {
      var getChr = function (c) {
         return _U.cmp(c,
         10) < 0 ? $Basics.toString(c) : $String.fromChar($Char.fromCode(87 + c));
      };
      return _U.cmp(n,16) < 0 ? getChr(n) : A2($Basics._op["++"],
      toRadix(n / 16 | 0),
      getChr(A2($Basics._op["%"],n,16)));
   };
   var toHex = function (n) {
      var hex = toRadix(n);
      return _U.eq($String.length(hex),1) ? A2($Basics._op["++"],
      "0",
      hex) : hex;
   };
   var colorToHex = function (cl) {
      var _p7 = $Color.toRgb(cl);
      var red = _p7.red;
      var green = _p7.green;
      var blue = _p7.blue;
      var alpha = _p7.alpha;
      return A2($Basics._op["++"],
      "#",
      A2($Basics._op["++"],
      toHex(red),
      A2($Basics._op["++"],toHex(green),toHex(blue))));
   };
   var hexToColor = function (c) {
      var r = $List.head(A2($List.map,
      function (_) {
         return _.submatches;
      },
      A3($Regex.find,
      $Regex.All,
      $Regex.regex("^#?([a-f\\d]{2})([a-f\\d]{2})([a-f\\d]{2})$"),
      $String.toLower(c))));
      var _p8 = r;
      if (_p8.ctor === "Just") {
            var v = $Array.fromList(A2($List.filterMap,
            $Basics.identity,
            A2($List.map,
            $Result.toMaybe,
            A2($List.map,
            $ParseInt.parseIntHex,
            A2($List.filterMap,$Basics.identity,_p8._0)))));
            var r = A2($Array.get,0,v);
            var g = A2($Array.get,1,v);
            var b = A2($Array.get,2,v);
            var _p9 = r;
            if (_p9.ctor === "Just") {
                  var _p10 = g;
                  if (_p10.ctor === "Just") {
                        var _p11 = b;
                        if (_p11.ctor === "Just") {
                              return $Maybe.Just(A3($Color.rgb,_p9._0,_p10._0,_p11._0));
                           } else {
                              return $Maybe.Nothing;
                           }
                     } else {
                        return $Maybe.Nothing;
                     }
               } else {
                  return $Maybe.Nothing;
               }
         } else {
            return $Maybe.Nothing;
         }
   };
   var cssColorString = F2(function (kind,values) {
      return A2($Basics._op["++"],
      kind,
      A2($Basics._op["++"],
      "(",
      A2($Basics._op["++"],A2($String.join,", ",values),")")));
   });
   var toPercentString = function (h) {
      return A2($Basics._op["++"],
      $Basics.toString($Basics.round(h * 100)),
      "%");
   };
   var hueToString = function (h) {
      return $Basics.toString($Basics.round(h * 180 / $Basics.pi));
   };
   var colorToCssHsla = function (cl) {
      var _p12 = $Color.toHsl(cl);
      var hue = _p12.hue;
      var saturation = _p12.saturation;
      var lightness = _p12.lightness;
      var alpha = _p12.alpha;
      return A2(cssColorString,
      "hsla",
      _U.list([hueToString(hue)
              ,toPercentString(saturation)
              ,toPercentString(lightness)
              ,$Basics.toString(alpha)]));
   };
   var colorToCssHsl = function (cl) {
      var _p13 = $Color.toHsl(cl);
      var hue = _p13.hue;
      var saturation = _p13.saturation;
      var lightness = _p13.lightness;
      var alpha = _p13.alpha;
      return A2(cssColorString,
      "hsl",
      _U.list([hueToString(hue)
              ,toPercentString(saturation)
              ,toPercentString(lightness)]));
   };
   var colorToCssRgba = function (cl) {
      var _p14 = $Color.toRgb(cl);
      var red = _p14.red;
      var green = _p14.green;
      var blue = _p14.blue;
      var alpha = _p14.alpha;
      return A2(cssColorString,
      "rgba",
      _U.list([$Basics.toString(red)
              ,$Basics.toString(green)
              ,$Basics.toString(blue)
              ,$Basics.toString(alpha)]));
   };
   var colorToCssRgb = function (cl) {
      var _p15 = $Color.toRgb(cl);
      var red = _p15.red;
      var green = _p15.green;
      var blue = _p15.blue;
      var alpha = _p15.alpha;
      return A2(cssColorString,
      "rgb",
      _U.list([$Basics.toString(red)
              ,$Basics.toString(green)
              ,$Basics.toString(blue)]));
   };
   var Lab = F3(function (a,b,c) {    return {l: a,a: b,b: c};});
   var XYZ = F3(function (a,b,c) {    return {x: a,y: b,z: c};});
   return _elm.Color.Convert.values = {_op: _op
                                      ,colorToCssRgb: colorToCssRgb
                                      ,colorToCssRgba: colorToCssRgba
                                      ,colorToCssHsl: colorToCssHsl
                                      ,colorToCssHsla: colorToCssHsla
                                      ,colorToHex: colorToHex
                                      ,hexToColor: hexToColor
                                      ,colorToLab: colorToLab
                                      ,labToColor: labToColor};
};
Elm.Color = Elm.Color || {};
Elm.Color.Interpolate = Elm.Color.Interpolate || {};
Elm.Color.Interpolate.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   _elm.Color.Interpolate = _elm.Color.Interpolate || {};
   if (_elm.Color.Interpolate.values)
   return _elm.Color.Interpolate.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Color$Convert = Elm.Color.Convert.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var linear = F3(function (t,i1,i2) {
      return i1 + (i2 - i1) * t;
   });
   var degree360 = $Basics.degrees(360);
   var degree180 = $Basics.degrees(180);
   var interpolate = F4(function (space,cl1,cl2,t) {
      var i = linear(t);
      var _p0 = space;
      switch (_p0.ctor)
      {case "RGB": var cl2$ = $Color.toRgb(cl2);
           var cl1$ = $Color.toRgb(cl1);
           return A4($Color.rgba,
           $Basics.round(A2(i,
           $Basics.toFloat(cl1$.red),
           $Basics.toFloat(cl2$.red))),
           $Basics.round(A2(i,
           $Basics.toFloat(cl1$.green),
           $Basics.toFloat(cl2$.green))),
           $Basics.round(A2(i,
           $Basics.toFloat(cl1$.blue),
           $Basics.toFloat(cl2$.blue))),
           A2(i,cl1$.alpha,cl2$.alpha));
         case "HSL": var cl2$ = $Color.toHsl(cl2);
           var h2 = cl2$.hue;
           var cl1$ = $Color.toHsl(cl1);
           var h1 = cl1$.hue;
           var dH = _U.cmp(h2,h1) > 0 && _U.cmp(h2 - h1,
           degree180) > 0 ? h2 - h1 + degree360 : _U.cmp(h2,
           h1) < 0 && _U.cmp(h1 - h2,
           degree180) > 0 ? h2 + degree360 - h1 : h2 - h1;
           return A4($Color.hsla,
           h1 + t * dH,
           A2(i,cl1$.saturation,cl2$.saturation),
           A2(i,cl1$.lightness,cl2$.lightness),
           A2(i,cl1$.alpha,cl2$.alpha));
         default: var lab2 = $Color$Convert.colorToLab(cl2);
           var lab1 = $Color$Convert.colorToLab(cl1);
           return $Color$Convert.labToColor({l: A2(i,lab1.l,lab2.l)
                                            ,a: A2(i,lab1.a,lab2.a)
                                            ,b: A2(i,lab1.b,lab2.b)});}
   });
   var LAB = {ctor: "LAB"};
   var HSL = {ctor: "HSL"};
   var RGB = {ctor: "RGB"};
   return _elm.Color.Interpolate.values = {_op: _op
                                          ,interpolate: interpolate
                                          ,RGB: RGB
                                          ,HSL: HSL
                                          ,LAB: LAB};
};
Elm.Color = Elm.Color || {};
Elm.Color.Gradient = Elm.Color.Gradient || {};
Elm.Color.Gradient.make = function (_elm) {
   "use strict";
   _elm.Color = _elm.Color || {};
   _elm.Color.Gradient = _elm.Color.Gradient || {};
   if (_elm.Color.Gradient.values)
   return _elm.Color.Gradient.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color = Elm.Color.make(_elm),
   $Color$Interpolate = Elm.Color.Interpolate.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var getNextGradientStop = F2(function (currentStop,gradient) {
      var nextStop = $List.head(gradient);
      var _p0 = nextStop;
      if (_p0.ctor === "Just") {
            return {ctor: "_Tuple2"
                   ,_0: _p0._0
                   ,_1: A2($Maybe.withDefault,_U.list([]),$List.tail(gradient))};
         } else {
            return {ctor: "_Tuple2",_0: currentStop,_1: gradient};
         }
   });
   var calculateColor = F4(function (space,_p2,_p1,t) {
      var _p3 = _p2;
      var _p7 = _p3._0;
      var _p6 = _p3._1;
      var _p4 = _p1;
      var _p5 = _p4._1;
      return _U.eq(t,0) ? _p6 : _U.eq(t,
      1) ? _p5 : A4($Color$Interpolate.interpolate,
      space,
      _p6,
      _p5,
      (t - _p7) / (_p4._0 - _p7));
   });
   var calculateGradient = F5(function (space,
   stop1,
   stop2,
   gradient,
   t) {
      if (_U.cmp($Basics.fst(stop2),t) < 0) {
            var _p8 = A2(getNextGradientStop,stop2,gradient);
            var stop2$ = _p8._0;
            var gradient$ = _p8._1;
            var stop1$ = stop2;
            return {ctor: "_Tuple4"
                   ,_0: stop1$
                   ,_1: stop2$
                   ,_2: gradient$
                   ,_3: A4(calculateColor,space,stop1$,stop2$,t)};
         } else return {ctor: "_Tuple4"
                       ,_0: stop1
                       ,_1: stop2
                       ,_2: gradient
                       ,_3: A4(calculateColor,space,stop1,stop2,t)};
   });
   var c = F3(function (space,t,_p9) {
      var _p10 = _p9;
      var _p11 = A5(calculateGradient,
      space,
      _p10._0,
      _p10._1,
      _p10._2,
      t);
      var stop1$ = _p11._0;
      var stop2$ = _p11._1;
      var gradient$ = _p11._2;
      var color = _p11._3;
      return {ctor: "_Tuple4"
             ,_0: stop1$
             ,_1: stop2$
             ,_2: gradient$
             ,_3: A2($List._op["::"],color,_p10._3)};
   });
   var gradientFromStops = F3(function (space,stops,size) {
      var purifiedStops = A2($List.sortBy,
      function (_p12) {
         var _p13 = _p12;
         return _p13._0;
      },
      A2($List.filter,
      function (_p14) {
         var _p15 = _p14;
         var _p16 = _p15._0;
         return _U.cmp(_p16,0) > -1 && _U.cmp(_p16,1) < 1;
      },
      stops));
      var stop1 = $List.head(purifiedStops);
      var _p17 = stop1;
      if (_p17.ctor === "Just") {
            var _p21 = _p17._0;
            var currentStops = A2($Maybe.withDefault,
            _U.list([]),
            $List.tail(purifiedStops));
            var _p18 = A2(getNextGradientStop,_p21,currentStops);
            var s2 = _p18._0;
            var g = _p18._1;
            var l = size - 1;
            var stops = A2($List.map,
            function (i) {
               return $Basics.toFloat(i) / l;
            },
            _U.range(0,l));
            return $List.reverse(function (_p19) {
               var _p20 = _p19;
               return _p20._3;
            }(A3($List.foldl,
            c(space),
            {ctor: "_Tuple4",_0: _p21,_1: s2,_2: g,_3: _U.list([])},
            stops)));
         } else {
            return _U.list([]);
         }
   });
   var gradient = F3(function (space,palette,size) {
      var l = $List.length(palette) - 1;
      var gr = A3($List.map2,
      F2(function (i,cl) {
         return {ctor: "_Tuple2"
                ,_0: $Basics.toFloat(i) / $Basics.toFloat(l)
                ,_1: cl};
      }),
      _U.range(0,l),
      palette);
      return A3(gradientFromStops,space,gr,size);
   });
   return _elm.Color.Gradient.values = {_op: _op
                                       ,gradient: gradient
                                       ,gradientFromStops: gradientFromStops};
};
Elm.Random = Elm.Random || {};
Elm.Random.Array = Elm.Random.Array || {};
Elm.Random.Array.make = function (_elm) {
   "use strict";
   _elm.Random = _elm.Random || {};
   _elm.Random.Array = _elm.Random.Array || {};
   if (_elm.Random.Array.values) return _elm.Random.Array.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Random = Elm.Random.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Trampoline = Elm.Trampoline.make(_elm);
   var _op = {};
   var choose = F2(function (seed,arr) {
      if ($Array.isEmpty(arr)) return {ctor: "_Tuple3"
                                      ,_0: $Maybe.Nothing
                                      ,_1: seed
                                      ,_2: arr}; else {
            var lastIndex = $Array.length(arr) - 1;
            var intGen = A2($Random.$int,0,lastIndex);
            var _p0 = A2($Random.generate,intGen,seed);
            var index = _p0._0;
            var seed$ = _p0._1;
            var front = A3($Array.slice,0,index,arr);
            var back = _U.eq(index,
            lastIndex) ? $Array.empty : A3($Array.slice,
            index + 1,
            $Array.length(arr),
            arr);
            return {ctor: "_Tuple3"
                   ,_0: A2($Array.get,index,arr)
                   ,_1: seed$
                   ,_2: A2($Array.append,front,back)};
         }
   });
   var shuffle = F2(function (seed,arr) {
      if ($Array.isEmpty(arr)) return {ctor: "_Tuple2"
                                      ,_0: arr
                                      ,_1: seed}; else {
            var helper = function (_p1) {
               var _p2 = _p1;
               var _p9 = _p2._1;
               var _p8 = _p2._0;
               var _p7 = _p2._2;
               var _p3 = A2(choose,_p8,_p7);
               var m_val = _p3._0;
               var s$ = _p3._1;
               var a$ = _p3._2;
               var _p4 = m_val;
               if (_p4.ctor === "Nothing") {
                     return $Trampoline.Done({ctor: "_Tuple3"
                                             ,_0: _p8
                                             ,_1: _p9
                                             ,_2: _p7});
                  } else {
                     return $Trampoline.Continue(function (_p5) {
                        var _p6 = _p5;
                        return helper({ctor: "_Tuple3"
                                      ,_0: s$
                                      ,_1: A2($List._op["::"],_p4._0,_p9)
                                      ,_2: a$});
                     });
                  }
            };
            var _p10 = $Trampoline.trampoline(helper({ctor: "_Tuple3"
                                                     ,_0: seed
                                                     ,_1: _U.list([])
                                                     ,_2: arr}));
            var seed$ = _p10._0;
            var shuffled = _p10._1;
            return {ctor: "_Tuple2"
                   ,_0: $Array.fromList(shuffled)
                   ,_1: seed$};
         }
   });
   var sample = F2(function (seed,arr) {
      var intGen = A2($Random.$int,0,$Array.length(arr) - 1);
      var _p11 = A2($Random.generate,intGen,seed);
      var index = _p11._0;
      var seed$ = _p11._1;
      return {ctor: "_Tuple2",_0: A2($Array.get,index,arr),_1: seed$};
   });
   return _elm.Random.Array.values = {_op: _op
                                     ,sample: sample
                                     ,choose: choose
                                     ,shuffle: shuffle};
};
Elm.Noise = Elm.Noise || {};
Elm.Noise.make = function (_elm) {
   "use strict";
   _elm.Noise = _elm.Noise || {};
   if (_elm.Noise.values) return _elm.Noise.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Bitwise = Elm.Bitwise.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Random = Elm.Random.make(_elm),
   $Random$Array = Elm.Random.Array.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var PermutationTable = F2(function (a,b) {
      return {perm: a,permMod12: b};
   });
   var getCornerOffset3d = F3(function (x,y,z) {
      return _U.cmp(x,y) > -1 ? _U.cmp(y,z) > -1 ? {ctor: "_Tuple6"
                                                   ,_0: 1
                                                   ,_1: 0
                                                   ,_2: 0
                                                   ,_3: 1
                                                   ,_4: 1
                                                   ,_5: 0} : _U.cmp(x,z) > -1 ? {ctor: "_Tuple6"
                                                                                ,_0: 1
                                                                                ,_1: 0
                                                                                ,_2: 0
                                                                                ,_3: 1
                                                                                ,_4: 0
                                                                                ,_5: 1} : {ctor: "_Tuple6"
                                                                                          ,_0: 0
                                                                                          ,_1: 0
                                                                                          ,_2: 1
                                                                                          ,_3: 1
                                                                                          ,_4: 0
                                                                                          ,_5: 1} : _U.cmp(y,z) < 0 ? {ctor: "_Tuple6"
                                                                                                                      ,_0: 0
                                                                                                                      ,_1: 0
                                                                                                                      ,_2: 1
                                                                                                                      ,_3: 0
                                                                                                                      ,_4: 1
                                                                                                                      ,_5: 1} : _U.cmp(x,
      z) < 0 ? {ctor: "_Tuple6"
               ,_0: 0
               ,_1: 1
               ,_2: 0
               ,_3: 0
               ,_4: 1
               ,_5: 1} : {ctor: "_Tuple6",_0: 0,_1: 1,_2: 0,_3: 1,_4: 1,_5: 0};
   });
   var getCornerOffset2d = F2(function (x,y) {
      return _U.cmp(x,y) > 0 ? {ctor: "_Tuple2"
                               ,_0: 1
                               ,_1: 0} : {ctor: "_Tuple2",_0: 0,_1: 1};
   });
   var addUp = function (bs) {
      return A3($List.foldl,
      F2(function (b,i) {    return b ? i + 1 : i;}),
      0,
      bs);
   };
   var grad4 = $Array.fromList(_U.list([0
                                       ,1
                                       ,1
                                       ,1
                                       ,0
                                       ,1
                                       ,1
                                       ,-1
                                       ,0
                                       ,1
                                       ,-1
                                       ,1
                                       ,0
                                       ,1
                                       ,-1
                                       ,-1
                                       ,0
                                       ,-1
                                       ,1
                                       ,1
                                       ,0
                                       ,-1
                                       ,1
                                       ,-1
                                       ,0
                                       ,-1
                                       ,-1
                                       ,1
                                       ,0
                                       ,-1
                                       ,-1
                                       ,-1
                                       ,1
                                       ,0
                                       ,1
                                       ,1
                                       ,1
                                       ,0
                                       ,1
                                       ,-1
                                       ,1
                                       ,0
                                       ,-1
                                       ,1
                                       ,1
                                       ,0
                                       ,-1
                                       ,-1
                                       ,-1
                                       ,0
                                       ,1
                                       ,1
                                       ,-1
                                       ,0
                                       ,1
                                       ,-1
                                       ,-1
                                       ,0
                                       ,-1
                                       ,1
                                       ,-1
                                       ,0
                                       ,-1
                                       ,-1
                                       ,1
                                       ,1
                                       ,0
                                       ,1
                                       ,1
                                       ,1
                                       ,0
                                       ,-1
                                       ,1
                                       ,-1
                                       ,0
                                       ,1
                                       ,1
                                       ,-1
                                       ,0
                                       ,-1
                                       ,-1
                                       ,1
                                       ,0
                                       ,1
                                       ,-1
                                       ,1
                                       ,0
                                       ,-1
                                       ,-1
                                       ,-1
                                       ,0
                                       ,1
                                       ,-1
                                       ,-1
                                       ,0
                                       ,-1
                                       ,1
                                       ,1
                                       ,1
                                       ,0
                                       ,1
                                       ,1
                                       ,-1
                                       ,0
                                       ,1
                                       ,-1
                                       ,1
                                       ,0
                                       ,1
                                       ,-1
                                       ,-1
                                       ,0
                                       ,-1
                                       ,1
                                       ,1
                                       ,0
                                       ,-1
                                       ,1
                                       ,-1
                                       ,0
                                       ,-1
                                       ,-1
                                       ,1
                                       ,0
                                       ,-1
                                       ,-1
                                       ,-1
                                       ,0]));
   var grad3 = $Array.fromList(_U.list([1
                                       ,1
                                       ,0
                                       ,-1
                                       ,1
                                       ,0
                                       ,1
                                       ,-1
                                       ,0
                                       ,-1
                                       ,-1
                                       ,0
                                       ,1
                                       ,0
                                       ,1
                                       ,-1
                                       ,0
                                       ,1
                                       ,1
                                       ,0
                                       ,-1
                                       ,-1
                                       ,0
                                       ,-1
                                       ,0
                                       ,1
                                       ,1
                                       ,0
                                       ,-1
                                       ,1
                                       ,0
                                       ,1
                                       ,-1
                                       ,0
                                       ,-1
                                       ,-1]));
   var generatePermMod12 = function (perm) {
      return A2($Array.map,
      function (i) {
         return A2($Basics._op["%"],i,12);
      },
      perm);
   };
   var reverseArray = function (array) {
      return $Array.fromList($List.reverse($Array.toList(array)));
   };
   var permutationTable = function (seed) {
      var _p0 = function (_p1) {
         var _p2 = _p1;
         var _p3 = _p2._0;
         return {ctor: "_Tuple2"
                ,_0: A2($Array.append,_p3,reverseArray(_p3))
                ,_1: _p2._1};
      }(A2($Random$Array.shuffle,
      seed,
      $Array.fromList(_U.range(0,255))));
      var perm = _p0._0;
      var seed$ = _p0._1;
      return {ctor: "_Tuple2"
             ,_0: {perm: perm,permMod12: generatePermMod12(perm)}
             ,_1: seed$};
   };
   var get = F2(function (arr,i) {
      var _p4 = A2($Array.get,i,arr);
      if (_p4.ctor === "Just") {
            return _p4._0;
         } else {
            return _U.crashCase("Noise",
            {start: {line: 51,column: 3},end: {line: 53,column: 48}},
            _p4)("Error getting item");
         }
   });
   var getN2d = F6(function (x,y,i,j,perm,permMod12) {
      var t = 0.5 - x * x - y * y;
      if (_U.cmp(t,0) < 0) return 0; else {
            var t$ = t * t;
            var gi = A2(get,permMod12,i + A2(get,perm,j)) * 3;
            return t$ * t$ * (A2(get,grad3,gi) * x + A2(get,
            grad3,
            gi + 1) * y);
         }
   });
   var getN3d = F8(function (x,y,z,i,j,k,perm,permMod12) {
      var t = 0.6 - x * x - y * y - z * z;
      if (_U.cmp(t,0) < 0) return 0; else {
            var t$ = t * t;
            var gi = A2(get,
            permMod12,
            i + A2(get,perm,j + A2(get,perm,k))) * 3;
            return t$ * t$ * (A2(get,grad3,gi) * x + A2(get,
            grad3,
            gi + 1) * y + A2(get,grad3,gi + 2) * z);
         }
   });
   var getN4d = function (x) {
      return function (y) {
         return function (z) {
            return function (w) {
               return function (i) {
                  return function (j) {
                     return function (k) {
                        return function (l) {
                           return function (perm) {
                              return function (permMod12) {
                                 var t = 0.6 - x * x - y * y - z * z - w * w;
                                 if (_U.cmp(t,0) < 0) return 0; else {
                                       var t$ = t * t;
                                       var gi = A2($Basics._op["%"],
                                       A2(get,perm,i) + (A2(get,perm,j) + (A2(get,perm,k) + A2(get,
                                       perm,
                                       l))),
                                       32) * 4;
                                       return t$ * t$ * (A2(get,grad4,gi) * x + A2(get,
                                       grad4,
                                       gi + 1) * y + A2(get,grad4,gi + 2) * z + A2(get,
                                       grad4,
                                       gi + 3) * w);
                                    }
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   var g4 = (5 - $Basics.sqrt(5)) / 20;
   var f4 = ($Basics.sqrt(5) - 1) / 4;
   var noise4d = F5(function (_p6,x,y,z,w) {
      var _p7 = _p6;
      var _p9 = _p7.permMod12;
      var _p8 = _p7.perm;
      var s = (x + y + z + w) * f4;
      var i = $Basics.floor(x + s);
      var ii = A2($Bitwise.and,i,255);
      var j = $Basics.floor(y + s);
      var jj = A2($Bitwise.and,j,255);
      var k = $Basics.floor(z + s);
      var kk = A2($Bitwise.and,k,255);
      var l = $Basics.floor(w + s);
      var t = $Basics.toFloat(i + j + k + l) * g4;
      var x0$ = $Basics.toFloat(i) - t;
      var x0 = x - x0$;
      var x4 = x0 - 1 + 4 * g4;
      var y0$ = $Basics.toFloat(j) - t;
      var y0 = y - y0$;
      var y4 = y0 - 1 + 4 * g4;
      var z0$ = $Basics.toFloat(k) - t;
      var z0 = z - z0$;
      var ranky = addUp(_U.list([_U.cmp(x0,y0) < 1
                                ,_U.cmp(y0,z0) > 0
                                ,_U.cmp(y0,z0) > 0]));
      var j1 = _U.cmp(ranky,3) > -1 ? 1 : 0;
      var y1 = y0 - j1 + g4;
      var j2 = _U.cmp(ranky,2) > -1 ? 1 : 0;
      var y2 = y0 - j2 + 2 * g4;
      var j3 = _U.cmp(ranky,1) > -1 ? 1 : 0;
      var y3 = y0 - j3 + 3 * g4;
      var z4 = z0 - 1 + 4 * g4;
      var w0$ = $Basics.toFloat(l) - t;
      var w0 = w - w0$;
      var rankx = addUp(_U.list([_U.cmp(x0,y0) > 0
                                ,_U.cmp(x0,z0) > 0
                                ,_U.cmp(x0,w0) > 0]));
      var i1 = _U.cmp(rankx,3) > -1 ? 1 : 0;
      var x1 = x0 - i1 + g4;
      var i2 = _U.cmp(rankx,2) > -1 ? 1 : 0;
      var x2 = x0 - i2 + 2 * g4;
      var i3 = _U.cmp(rankx,1) > -1 ? 1 : 0;
      var x3 = x0 - i3 + 3 * g4;
      var rankz = addUp(_U.list([_U.cmp(x0,z0) < 1
                                ,_U.cmp(y0,z0) < 1
                                ,_U.cmp(z0,w0) > 0]));
      var k1 = _U.cmp(rankz,3) > -1 ? 1 : 0;
      var z1 = z0 - k1 + g4;
      var k2 = _U.cmp(rankz,2) > -1 ? 1 : 0;
      var z2 = z0 - k2 + 2 * g4;
      var k3 = _U.cmp(rankz,1) > -1 ? 1 : 0;
      var z3 = z0 - k3 + 3 * g4;
      var rankw = addUp(_U.list([_U.cmp(x0,w0) < 1
                                ,_U.cmp(y0,w0) < 1
                                ,_U.cmp(z0,w0) < 1]));
      var l1 = _U.cmp(rankw,3) > -1 ? 1 : 0;
      var l2 = _U.cmp(rankw,2) > -1 ? 1 : 0;
      var l3 = _U.cmp(rankw,1) > -1 ? 1 : 0;
      var w1 = w0 - l1 + g4;
      var w2 = w0 - l2 + 2 * g4;
      var w3 = w0 - l3 + 3 * g4;
      var w4 = w0 - 1 + 4 * g4;
      var ll = A2($Bitwise.and,l,255);
      var n0 = getN4d(x0)(y0)(z0)(w0)(ii)(jj)(kk)(ll)(_p8)(_p9);
      var n1 = getN4d(x1)(y1)(z1)(w1)(ii + i1)(jj + j1)(kk + k1)(ll + l1)(_p8)(_p9);
      var n2 = getN4d(x2)(y2)(z2)(w2)(ii + i2)(jj + j2)(kk + k2)(ll + l2)(_p8)(_p9);
      var n3 = getN4d(x3)(y3)(z3)(w3)(ii + i3)(jj + j3)(kk + k3)(ll + l3)(_p8)(_p9);
      var n4 = getN4d(x4)(y4)(z4)(w4)(ii + 1)(jj + 1)(kk + 1)(ll + 1)(_p8)(_p9);
      return 27 * (n0 + n1 + n2 + n3 + n4);
   });
   var g3 = 1 / 6;
   var f3 = 1 / 3;
   var noise3d = F4(function (_p10,xin,yin,zin) {
      var _p11 = _p10;
      var _p14 = _p11.permMod12;
      var _p13 = _p11.perm;
      var s = (xin + yin + zin) * f3;
      var i = $Basics.floor(xin + s);
      var ii = A2($Bitwise.and,i,255);
      var j = $Basics.floor(yin + s);
      var jj = A2($Bitwise.and,j,255);
      var k = $Basics.floor(zin + s);
      var t = $Basics.toFloat(i + j + k) * g3;
      var x0$ = $Basics.toFloat(i) - t;
      var x0 = xin - x0$;
      var x3 = x0 - 1 + 3 * g3;
      var y0$ = $Basics.toFloat(j) - t;
      var y0 = yin - y0$;
      var y3 = y0 - 1 + 3 * g3;
      var z0$ = $Basics.toFloat(k) - t;
      var z0 = zin - z0$;
      var _p12 = A3(getCornerOffset3d,x0,y0,z0);
      var i1 = _p12._0;
      var j1 = _p12._1;
      var k1 = _p12._2;
      var i2 = _p12._3;
      var j2 = _p12._4;
      var k2 = _p12._5;
      var x1 = x0 - $Basics.toFloat(i1) + g3;
      var y1 = y0 - $Basics.toFloat(j1) + g3;
      var x2 = x0 - $Basics.toFloat(i2) + 2 * g3;
      var y2 = y0 - $Basics.toFloat(j2) + 2 * g3;
      var z1 = z0 - $Basics.toFloat(k1) + g3;
      var z2 = z0 - $Basics.toFloat(k2) + 2 * g3;
      var z3 = z0 - 1 + 3 * g3;
      var kk = A2($Bitwise.and,k,255);
      var n0 = A8(getN3d,x0,y0,z0,ii,jj,kk,_p13,_p14);
      var n1 = A8(getN3d,x1,y1,z1,ii + i1,jj + j1,kk + k1,_p13,_p14);
      var n2 = A8(getN3d,x2,y2,z2,ii + i2,jj + j2,kk + k2,_p13,_p14);
      var n3 = A8(getN3d,x3,y3,z3,ii + 1,jj + 1,kk + 1,_p13,_p14);
      return 32 * (n0 + n1 + n2 + n3);
   });
   var g2 = (3 - $Basics.sqrt(3)) / 6;
   var f2 = 0.5 * ($Basics.sqrt(3) - 1);
   var noise2d = F3(function (_p15,xin,yin) {
      var _p16 = _p15;
      var _p19 = _p16.permMod12;
      var _p18 = _p16.perm;
      var s = (xin + yin) * f2;
      var i = $Basics.floor(xin + s);
      var ii = A2($Bitwise.and,i,255);
      var j = $Basics.floor(yin + s);
      var t = $Basics.toFloat(i + j) * g2;
      var x0$ = $Basics.toFloat(i) - t;
      var x0 = xin - x0$;
      var x2 = x0 - 1 + 2 * g2;
      var y0$ = $Basics.toFloat(j) - t;
      var y0 = yin - y0$;
      var _p17 = A2(getCornerOffset2d,x0,y0);
      var i1 = _p17._0;
      var j1 = _p17._1;
      var x1 = x0 - $Basics.toFloat(i1) + g2;
      var y1 = y0 - $Basics.toFloat(j1) + g2;
      var y2 = y0 - 1 + 2 * g2;
      var jj = A2($Bitwise.and,j,255);
      var n0 = A6(getN2d,x0,y0,ii,jj,_p18,_p19);
      var n1 = A6(getN2d,x1,y1,ii + i1,jj + j1,_p18,_p19);
      var n2 = A6(getN2d,x2,y2,ii + 1,jj + 1,_p18,_p19);
      return 70 * (n0 + n1 + n2);
   });
   return _elm.Noise.values = {_op: _op
                              ,permutationTable: permutationTable
                              ,noise4d: noise4d
                              ,noise3d: noise3d
                              ,noise2d: noise2d
                              ,PermutationTable: PermutationTable};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Group = Elm.WallpaperGroup.Group || {};
Elm.WallpaperGroup.Group.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Group = _elm.WallpaperGroup.Group || {};
   if (_elm.WallpaperGroup.Group.values)
   return _elm.WallpaperGroup.Group.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var P6 = function (a) {    return {ctor: "P6",_0: a};};
   var P31m = function (a) {    return {ctor: "P31m",_0: a};};
   var P3m1 = function (a) {    return {ctor: "P3m1",_0: a};};
   var P3 = function (a) {    return {ctor: "P3",_0: a};};
   var P4mg = F2(function (a,b) {
      return {ctor: "P4mg",_0: a,_1: b};
   });
   var P4mm = F2(function (a,b) {
      return {ctor: "P4mm",_0: a,_1: b};
   });
   var P4 = F2(function (a,b) {
      return {ctor: "P4",_0: a,_1: b};
   });
   var C2mm = F2(function (a,b) {
      return {ctor: "C2mm",_0: a,_1: b};
   });
   var P2gg = F2(function (a,b) {
      return {ctor: "P2gg",_0: a,_1: b};
   });
   var P2mg = F2(function (a,b) {
      return {ctor: "P2mg",_0: a,_1: b};
   });
   var P2mm = F2(function (a,b) {
      return {ctor: "P2mm",_0: a,_1: b};
   });
   var Cm = F2(function (a,b) {
      return {ctor: "Cm",_0: a,_1: b};
   });
   var Pg = F2(function (a,b) {
      return {ctor: "Pg",_0: a,_1: b};
   });
   var Pm = F2(function (a,b) {
      return {ctor: "Pm",_0: a,_1: b};
   });
   var P2 = F2(function (a,b) {
      return {ctor: "P2",_0: a,_1: b};
   });
   var P1 = F2(function (a,b) {
      return {ctor: "P1",_0: a,_1: b};
   });
   return _elm.WallpaperGroup.Group.values = {_op: _op
                                             ,P1: P1
                                             ,P2: P2
                                             ,Pm: Pm
                                             ,Pg: Pg
                                             ,Cm: Cm
                                             ,P2mm: P2mm
                                             ,P2mg: P2mg
                                             ,P2gg: P2gg
                                             ,C2mm: C2mm
                                             ,P4: P4
                                             ,P4mm: P4mm
                                             ,P4mg: P4mg
                                             ,P3: P3
                                             ,P3m1: P3m1
                                             ,P31m: P31m
                                             ,P6: P6};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Geom = Elm.WallpaperGroup.Geom || {};
Elm.WallpaperGroup.Geom.BoundingBox = Elm.WallpaperGroup.Geom.BoundingBox || {};
Elm.WallpaperGroup.Geom.BoundingBox.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Geom = _elm.WallpaperGroup.Geom || {};
   _elm.WallpaperGroup.Geom.BoundingBox = _elm.WallpaperGroup.Geom.BoundingBox || {};
   if (_elm.WallpaperGroup.Geom.BoundingBox.values)
   return _elm.WallpaperGroup.Geom.BoundingBox.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var Rect = F4(function (a,b,c,d) {
      return {ctor: "Rect",_0: a,_1: b,_2: c,_3: d};
   });
   var Triangle = F3(function (a,b,c) {
      return {ctor: "Triangle",_0: a,_1: b,_2: c};
   });
   return _elm.WallpaperGroup.Geom.BoundingBox.values = {_op: _op
                                                        ,Triangle: Triangle
                                                        ,Rect: Rect};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Types = Elm.Editor.Types || {};
Elm.Editor.Types.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Types = _elm.Editor.Types || {};
   if (_elm.Editor.Types.values) return _elm.Editor.Types.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var Bezier = F7(function (a,b,c,d,e,f,g) {
      return {p1: a
             ,c1: b
             ,c2: c
             ,p2: d
             ,color: e
             ,opacity: f
             ,strokeWidth: g};
   });
   var Line = F2(function (a,b) {    return {start: a,end: b};});
   var Point = F2(function (a,b) {    return {x: a,y: b};});
   return _elm.Editor.Types.values = {_op: _op
                                     ,Point: Point
                                     ,Line: Line
                                     ,Bezier: Bezier};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Util = Elm.Editor.Util || {};
Elm.Editor.Util.Raster = Elm.Editor.Util.Raster || {};
Elm.Editor.Util.Raster.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Util = _elm.Editor.Util || {};
   _elm.Editor.Util.Raster = _elm.Editor.Util.Raster || {};
   if (_elm.Editor.Util.Raster.values)
   return _elm.Editor.Util.Raster.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Types = Elm.Editor.Types.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Geom$BoundingBox = Elm.WallpaperGroup.Geom.BoundingBox.make(_elm);
   var _op = {};
   var calcStep = F3(function (start,step,i) {
      return {x: start.x + step.x * i,y: start.y + step.y * i};
   });
   var splitLine = F2(function (steps,_p0) {
      var _p1 = _p0;
      var _p3 = _p1.start;
      var _p2 = _p1.end;
      if (_U.eq(steps,0)) return _U.list([_p3]); else {
            var step = {x: (_p2.x - _p3.x) / steps
                       ,y: (_p2.y - _p3.y) / steps};
            return A2($List.map,A2(calcStep,_p3,step),_U.range(0,steps));
         }
   });
   var line = F2(function (p1,p2) {
      return {start: p1,end: p2};
   });
   var toLines = F2(function (lines,steps) {
      var l2 = A2(splitLine,steps,$Basics.snd(lines));
      var l1 = A2(splitLine,steps,$Basics.fst(lines));
      return A3($List.map2,line,l1,l2);
   });
   var rectRaster = F5(function (p1,p2,p3,p4,steps) {
      return $List.concat(A2($List.map,
      splitLine(steps),
      A2(toLines,
      {ctor: "_Tuple2",_0: A2(line,p1,p2),_1: A2(line,p4,p3)},
      steps)));
   });
   var triangleRaster = F4(function (p1,p2,p3,steps) {
      return $List.concat(A3($List.map2,
      splitLine,
      _U.range(0,steps),
      A2(toLines,
      {ctor: "_Tuple2",_0: A2(line,p1,p2),_1: A2(line,p1,p3)},
      steps)));
   });
   var rasterCoords = F2(function (steps,bounding) {
      var _p4 = bounding;
      if (_p4.ctor === "Triangle") {
            return A4(triangleRaster,_p4._0,_p4._1,_p4._2,steps);
         } else {
            return A5(rectRaster,_p4._0,_p4._1,_p4._2,_p4._3,steps);
         }
   });
   return _elm.Editor.Util.Raster.values = {_op: _op
                                           ,line: line
                                           ,toLines: toLines
                                           ,calcStep: calcStep
                                           ,splitLine: splitLine
                                           ,rectRaster: rectRaster
                                           ,triangleRaster: triangleRaster
                                           ,rasterCoords: rasterCoords};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Geom = Elm.WallpaperGroup.Geom || {};
Elm.WallpaperGroup.Geom.Point = Elm.WallpaperGroup.Geom.Point || {};
Elm.WallpaperGroup.Geom.Point.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Geom = _elm.WallpaperGroup.Geom || {};
   _elm.WallpaperGroup.Geom.Point = _elm.WallpaperGroup.Geom.Point || {};
   if (_elm.WallpaperGroup.Geom.Point.values)
   return _elm.WallpaperGroup.Geom.Point.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var add = F2(function (p1,p2) {
      return {x: p1.x + p2.x,y: p1.y + p2.y};
   });
   var subtract = F2(function (p1,p2) {
      return {x: p1.x - p2.x,y: p1.y - p2.y};
   });
   var Point = F2(function (a,b) {    return {x: a,y: b};});
   return _elm.WallpaperGroup.Geom.Point.values = {_op: _op
                                                  ,Point: Point
                                                  ,subtract: subtract
                                                  ,add: add};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Geom = Elm.WallpaperGroup.Geom || {};
Elm.WallpaperGroup.Geom.Line = Elm.WallpaperGroup.Geom.Line || {};
Elm.WallpaperGroup.Geom.Line.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Geom = _elm.WallpaperGroup.Geom || {};
   _elm.WallpaperGroup.Geom.Line = _elm.WallpaperGroup.Geom.Line || {};
   if (_elm.WallpaperGroup.Geom.Line.values)
   return _elm.WallpaperGroup.Geom.Line.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Geom$Point = Elm.WallpaperGroup.Geom.Point.make(_elm);
   var _op = {};
   return _elm.WallpaperGroup.Geom.Line.values = {_op: _op};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Geom = Elm.WallpaperGroup.Geom || {};
Elm.WallpaperGroup.Geom.Tile = Elm.WallpaperGroup.Geom.Tile || {};
Elm.WallpaperGroup.Geom.Tile.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Geom = _elm.WallpaperGroup.Geom || {};
   _elm.WallpaperGroup.Geom.Tile = _elm.WallpaperGroup.Geom.Tile || {};
   if (_elm.WallpaperGroup.Geom.Tile.values)
   return _elm.WallpaperGroup.Geom.Tile.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Geom$Line = Elm.WallpaperGroup.Geom.Line.make(_elm);
   var _op = {};
   return _elm.WallpaperGroup.Geom.Tile.values = {_op: _op};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Geom = Elm.WallpaperGroup.Geom || {};
Elm.WallpaperGroup.Geom.Util = Elm.WallpaperGroup.Geom.Util || {};
Elm.WallpaperGroup.Geom.Util.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Geom = _elm.WallpaperGroup.Geom || {};
   _elm.WallpaperGroup.Geom.Util = _elm.WallpaperGroup.Geom.Util || {};
   if (_elm.WallpaperGroup.Geom.Util.values)
   return _elm.WallpaperGroup.Geom.Util.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Geom$BoundingBox = Elm.WallpaperGroup.Geom.BoundingBox.make(_elm),
   $WallpaperGroup$Geom$Line = Elm.WallpaperGroup.Geom.Line.make(_elm),
   $WallpaperGroup$Geom$Point = Elm.WallpaperGroup.Geom.Point.make(_elm),
   $WallpaperGroup$Geom$Tile = Elm.WallpaperGroup.Geom.Tile.make(_elm);
   var _op = {};
   var rightTriangleCoords = F2(function (w,h) {
      return A3($WallpaperGroup$Geom$BoundingBox.Triangle,
      {x: w,y: 0},
      {x: w,y: h},
      {x: 0,y: h});
   });
   var triangleCoords = F2(function (w,h) {
      return A3($WallpaperGroup$Geom$BoundingBox.Triangle,
      {x: w / 2,y: 0},
      {x: w,y: h},
      {x: 0,y: h});
   });
   var rectCoords = F2(function (w,h) {
      return A4($WallpaperGroup$Geom$BoundingBox.Rect,
      {x: 0,y: 0},
      {x: w,y: 0},
      {x: w,y: h},
      {x: 0,y: h});
   });
   var split = F3(function (start,end,percentage) {
      return {x: start.x + (end.x - start.x) * percentage
             ,y: start.y + (end.y - start.y) * percentage};
   });
   var mapOverPoints = F2(function (transformFunc,line) {
      return A2($List.map,transformFunc,line);
   });
   var mapTransform = F2(function (transformFunc,tiles) {
      var tile = $List.head(tiles);
      var _p0 = tile;
      if (_p0.ctor === "Just") {
            return A2($List._op["::"],
            A2($List.map,mapOverPoints(transformFunc),_p0._0),
            tiles);
         } else {
            return tiles;
         }
   });
   return _elm.WallpaperGroup.Geom.Util.values = {_op: _op
                                                 ,mapOverPoints: mapOverPoints
                                                 ,mapTransform: mapTransform
                                                 ,split: split
                                                 ,rectCoords: rectCoords
                                                 ,triangleCoords: triangleCoords
                                                 ,rightTriangleCoords: rightTriangleCoords};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Geom = Elm.WallpaperGroup.Geom || {};
Elm.WallpaperGroup.Geom.GlideTranslate = Elm.WallpaperGroup.Geom.GlideTranslate || {};
Elm.WallpaperGroup.Geom.GlideTranslate.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Geom = _elm.WallpaperGroup.Geom || {};
   _elm.WallpaperGroup.Geom.GlideTranslate = _elm.WallpaperGroup.Geom.GlideTranslate || {};
   if (_elm.WallpaperGroup.Geom.GlideTranslate.values)
   return _elm.WallpaperGroup.Geom.GlideTranslate.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Geom$Point = Elm.WallpaperGroup.Geom.Point.make(_elm),
   $WallpaperGroup$Geom$Tile = Elm.WallpaperGroup.Geom.Tile.make(_elm),
   $WallpaperGroup$Geom$Util = Elm.WallpaperGroup.Geom.Util.make(_elm);
   var _op = {};
   var glideTranslate$ = F4(function (mirrorFn,
   offsetX,
   offsetY,
   point) {
      var p = mirrorFn(point);
      return {x: p.x + offsetX,y: p.y + offsetY};
   });
   var glideTranslate = F3(function (mirrorFn,offsetX,offsetY) {
      return $WallpaperGroup$Geom$Util.mapTransform(A3(glideTranslate$,
      mirrorFn,
      offsetX,
      offsetY));
   });
   return _elm.WallpaperGroup.Geom.GlideTranslate.values = {_op: _op
                                                           ,glideTranslate$: glideTranslate$
                                                           ,glideTranslate: glideTranslate};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Geom = Elm.WallpaperGroup.Geom || {};
Elm.WallpaperGroup.Geom.Mirror = Elm.WallpaperGroup.Geom.Mirror || {};
Elm.WallpaperGroup.Geom.Mirror.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Geom = _elm.WallpaperGroup.Geom || {};
   _elm.WallpaperGroup.Geom.Mirror = _elm.WallpaperGroup.Geom.Mirror || {};
   if (_elm.WallpaperGroup.Geom.Mirror.values)
   return _elm.WallpaperGroup.Geom.Mirror.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Geom$Point = Elm.WallpaperGroup.Geom.Point.make(_elm),
   $WallpaperGroup$Geom$Tile = Elm.WallpaperGroup.Geom.Tile.make(_elm),
   $WallpaperGroup$Geom$Util = Elm.WallpaperGroup.Geom.Util.make(_elm);
   var _op = {};
   var mirror = F2(function (_p0,point) {
      var _p1 = _p0;
      var _p2 = _p1.p1;
      var e = A2($WallpaperGroup$Geom$Point.subtract,point,_p2);
      var d = A2($WallpaperGroup$Geom$Point.subtract,_p1.p2,_p2);
      var a = (d.x * d.x - d.y * d.y) / (d.x * d.x + d.y * d.y);
      var b = 2 * d.x * d.y / (d.x * d.x + d.y * d.y);
      return {x: a * e.x + b * e.y + _p2.x
             ,y: b * e.x - a * e.y + _p2.y};
   });
   var mirrorHorizontal = F2(function (w,h) {
      return $WallpaperGroup$Geom$Util.mapTransform(mirror({p1: {x: 0
                                                                ,y: w}
                                                           ,p2: {x: 2 * w,y: h}}));
   });
   var mirrorVertical = F2(function (w,h) {
      return $WallpaperGroup$Geom$Util.mapTransform(mirror({p1: {x: w
                                                                ,y: 0}
                                                           ,p2: {x: w,y: 2 * h}}));
   });
   var mirrorDiagonalRL = F2(function (w,h) {
      return $WallpaperGroup$Geom$Util.mapTransform(mirror({p1: {x: 0
                                                                ,y: 0}
                                                           ,p2: {x: w,y: h}}));
   });
   var mirrorDiagonalLR = F2(function (w,h) {
      return $WallpaperGroup$Geom$Util.mapTransform(mirror({p1: {x: w
                                                                ,y: 0}
                                                           ,p2: {x: 0,y: h}}));
   });
   var mirrorHex = function (w) {
      return $WallpaperGroup$Geom$Util.mapTransform(mirror({p1: {x: $Basics.sqrt(3) / 2 * w
                                                                ,y: w}
                                                           ,p2: {x: 0,y: w * 0.5}}));
   };
   var mirrorTriangle = F2(function (w,h) {
      return $WallpaperGroup$Geom$Util.mapTransform(mirror({p1: {x: w / 2
                                                                ,y: 0}
                                                           ,p2: {x: w,y: h}}));
   });
   var Axis = F2(function (a,b) {    return {p1: a,p2: b};});
   return _elm.WallpaperGroup.Geom.Mirror.values = {_op: _op
                                                   ,Axis: Axis
                                                   ,mirror: mirror
                                                   ,mirrorHorizontal: mirrorHorizontal
                                                   ,mirrorVertical: mirrorVertical
                                                   ,mirrorDiagonalRL: mirrorDiagonalRL
                                                   ,mirrorDiagonalLR: mirrorDiagonalLR
                                                   ,mirrorHex: mirrorHex
                                                   ,mirrorTriangle: mirrorTriangle};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Geom = Elm.WallpaperGroup.Geom || {};
Elm.WallpaperGroup.Geom.Rotate = Elm.WallpaperGroup.Geom.Rotate || {};
Elm.WallpaperGroup.Geom.Rotate.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Geom = _elm.WallpaperGroup.Geom || {};
   _elm.WallpaperGroup.Geom.Rotate = _elm.WallpaperGroup.Geom.Rotate || {};
   if (_elm.WallpaperGroup.Geom.Rotate.values)
   return _elm.WallpaperGroup.Geom.Rotate.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Geom$Point = Elm.WallpaperGroup.Geom.Point.make(_elm),
   $WallpaperGroup$Geom$Tile = Elm.WallpaperGroup.Geom.Tile.make(_elm),
   $WallpaperGroup$Geom$Util = Elm.WallpaperGroup.Geom.Util.make(_elm);
   var _op = {};
   var rotate = F3(function (theta,center,p) {
      var oY = p.y - center.y;
      var oX = p.x - center.x;
      var angleInRadians = theta * $Basics.pi / 180;
      var sinTheta = $Basics.sin(angleInRadians);
      var cosTheta = $Basics.cos(angleInRadians);
      return {x: cosTheta * oX - sinTheta * oY + center.x
             ,y: sinTheta * oX + cosTheta * oY + center.y};
   });
   var rotate90 = function (center) {
      return $WallpaperGroup$Geom$Util.mapTransform(A2(rotate,
      90,
      center));
   };
   var rotate120 = function (center) {
      return $WallpaperGroup$Geom$Util.mapTransform(A2(rotate,
      120,
      center));
   };
   var rotate180 = function (center) {
      return $WallpaperGroup$Geom$Util.mapTransform(A2(rotate,
      180,
      center));
   };
   return _elm.WallpaperGroup.Geom.Rotate.values = {_op: _op
                                                   ,rotate: rotate
                                                   ,rotate90: rotate90
                                                   ,rotate120: rotate120
                                                   ,rotate180: rotate180};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Geom = Elm.WallpaperGroup.Geom || {};
Elm.WallpaperGroup.Geom.Translate = Elm.WallpaperGroup.Geom.Translate || {};
Elm.WallpaperGroup.Geom.Translate.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Geom = _elm.WallpaperGroup.Geom || {};
   _elm.WallpaperGroup.Geom.Translate = _elm.WallpaperGroup.Geom.Translate || {};
   if (_elm.WallpaperGroup.Geom.Translate.values)
   return _elm.WallpaperGroup.Geom.Translate.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Geom$Point = Elm.WallpaperGroup.Geom.Point.make(_elm);
   var _op = {};
   var h = F3(function (height,i,columns) {
      return height * $Basics.toFloat(i / columns | 0);
   });
   var w = F3(function (width,i,columns) {
      return width * $Basics.toFloat(A2($Basics._op["%"],
      i,
      columns));
   });
   var translate = F6(function (fW,fH,width,height,columns,i) {
      return {x: fW * A3(w,width,i,columns)
             ,y: fH * A3(h,height,i,columns)};
   });
   var w1h1 = A2(translate,1,1);
   var w1h2 = A2(translate,1,2);
   var w2h1 = A2(translate,2,1);
   var w2h2 = A2(translate,2,2);
   var translateShifted = F5(function (fH,width,height,columns,i) {
      var offsetX = _U.eq(A2($Basics._op["%"],i / columns | 0,2),
      0) ? 0 : width / 2;
      return {x: offsetX + A3(w,width,i,columns)
             ,y: fH * A3(h,height,i,columns)};
   });
   var shifted = translateShifted(1);
   var hex = translateShifted(0.75);
   return _elm.WallpaperGroup.Geom.Translate.values = {_op: _op
                                                      ,w: w
                                                      ,h: h
                                                      ,translate: translate
                                                      ,translateShifted: translateShifted
                                                      ,w1h1: w1h1
                                                      ,w1h2: w1h2
                                                      ,w2h1: w2h1
                                                      ,w2h2: w2h2
                                                      ,shifted: shifted
                                                      ,hex: hex};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Settings = Elm.WallpaperGroup.Settings || {};
Elm.WallpaperGroup.Settings.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Settings = _elm.WallpaperGroup.Settings || {};
   if (_elm.WallpaperGroup.Settings.values)
   return _elm.WallpaperGroup.Settings.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Geom$BoundingBox = Elm.WallpaperGroup.Geom.BoundingBox.make(_elm),
   $WallpaperGroup$Geom$GlideTranslate = Elm.WallpaperGroup.Geom.GlideTranslate.make(_elm),
   $WallpaperGroup$Geom$Mirror = Elm.WallpaperGroup.Geom.Mirror.make(_elm),
   $WallpaperGroup$Geom$Point = Elm.WallpaperGroup.Geom.Point.make(_elm),
   $WallpaperGroup$Geom$Rotate = Elm.WallpaperGroup.Geom.Rotate.make(_elm),
   $WallpaperGroup$Geom$Tile = Elm.WallpaperGroup.Geom.Tile.make(_elm),
   $WallpaperGroup$Geom$Translate = Elm.WallpaperGroup.Geom.Translate.make(_elm),
   $WallpaperGroup$Geom$Util = Elm.WallpaperGroup.Geom.Util.make(_elm),
   $WallpaperGroup$Group = Elm.WallpaperGroup.Group.make(_elm);
   var _op = {};
   var Setting = F3(function (a,b,c) {
      return {steps: a,translate: b,tileCoordinates: c};
   });
   var linesToTile = function (tiles) {
      return _U.list([$List.concat(tiles)]);
   };
   var getGroupSettings = function (gr) {
      var _p0 = gr;
      switch (_p0.ctor)
      {case "P1": var _p2 = _p0._0;
           var _p1 = _p0._1;
           return {steps: _U.list([])
                  ,translate: A2($WallpaperGroup$Geom$Translate.w1h1,_p2,_p1)
                  ,tileCoordinates: A2($WallpaperGroup$Geom$Util.rectCoords,
                  _p2,
                  _p1)};
         case "P2": var _p4 = _p0._0;
           var _p3 = _p0._1;
           return {steps: _U.list([$WallpaperGroup$Geom$Rotate.rotate180({x: _p4 / 2
                                                                         ,y: _p3})])
                  ,translate: A2($WallpaperGroup$Geom$Translate.w1h2,_p4,_p3)
                  ,tileCoordinates: A2($WallpaperGroup$Geom$Util.rectCoords,
                  _p4,
                  _p3)};
         case "Pm": var _p6 = _p0._0;
           var _p5 = _p0._1;
           return {steps: _U.list([A2($WallpaperGroup$Geom$Mirror.mirrorVertical,
                  _p6,
                  _p5)])
                  ,translate: A2($WallpaperGroup$Geom$Translate.w2h1,_p6,_p5)
                  ,tileCoordinates: A2($WallpaperGroup$Geom$Util.rectCoords,
                  _p6,
                  _p5)};
         case "Pg": var _p8 = _p0._0;
           var _p7 = _p0._1;
           return {steps: _U.list([A3($WallpaperGroup$Geom$GlideTranslate.glideTranslate,
                  $WallpaperGroup$Geom$Mirror.mirror({p1: {x: 0,y: _p7 / 2}
                                                     ,p2: {x: _p8,y: _p7 / 2}}),
                  _p8,
                  0)])
                  ,translate: A2($WallpaperGroup$Geom$Translate.w2h1,_p8,_p7)
                  ,tileCoordinates: A2($WallpaperGroup$Geom$Util.rectCoords,
                  _p8,
                  _p7)};
         case "Cm": var _p10 = _p0._0;
           var _p9 = _p0._1;
           return {steps: _U.list([A2($WallpaperGroup$Geom$Mirror.mirrorHorizontal,
                  _p10,
                  _p9)])
                  ,translate: A2($WallpaperGroup$Geom$Translate.shifted,_p10,_p9)
                  ,tileCoordinates: A2($WallpaperGroup$Geom$Util.triangleCoords,
                  _p10,
                  _p9)};
         case "P2mm": var _p12 = _p0._0;
           var _p11 = _p0._1;
           return {steps: _U.list([A2($WallpaperGroup$Geom$Mirror.mirrorHorizontal,
                                  _p12,
                                  _p11)
                                  ,linesToTile
                                  ,A2($WallpaperGroup$Geom$Mirror.mirrorVertical,_p12,_p11)])
                  ,translate: A2($WallpaperGroup$Geom$Translate.w2h2,_p12,_p11)
                  ,tileCoordinates: A2($WallpaperGroup$Geom$Util.rectCoords,
                  _p12,
                  _p11)};
         case "P2mg": var _p14 = _p0._0;
           var _p13 = _p0._1;
           return {steps: _U.list([$WallpaperGroup$Geom$Rotate.rotate180({x: _p14
                                                                         ,y: _p13 / 2})
                                  ,A2($WallpaperGroup$Geom$Mirror.mirrorHorizontal,_p14,_p13)])
                  ,translate: A2($WallpaperGroup$Geom$Translate.w2h2,_p14,_p13)
                  ,tileCoordinates: A2($WallpaperGroup$Geom$Util.rectCoords,
                  _p14,
                  _p13)};
         case "P2gg": var _p16 = _p0._0;
           var _p15 = _p0._1;
           return {steps: _U.list([$WallpaperGroup$Geom$Rotate.rotate180({x: _p16 / 2
                                                                         ,y: _p15})])
                  ,translate: A2($WallpaperGroup$Geom$Translate.shifted,_p16,_p15)
                  ,tileCoordinates: A2($WallpaperGroup$Geom$Util.rectCoords,
                  _p16,
                  _p15)};
         case "C2mm": var _p18 = _p0._0;
           var _p17 = _p0._1;
           return {steps: _U.list([A2($WallpaperGroup$Geom$Mirror.mirrorHorizontal,
                                  _p18,
                                  _p17)
                                  ,A2($WallpaperGroup$Geom$Mirror.mirrorVertical,_p18,_p17)
                                  ,A2($WallpaperGroup$Geom$Mirror.mirrorHorizontal,_p18,_p17)])
                  ,translate: A2($WallpaperGroup$Geom$Translate.shifted,
                  _p18 * 2,
                  _p17)
                  ,tileCoordinates: A2($WallpaperGroup$Geom$Util.rectCoords,
                  _p18,
                  _p17)};
         case "P4": var _p20 = _p0._0;
           var _p19 = _p0._1;
           return {steps: _U.list([$WallpaperGroup$Geom$Rotate.rotate90({x: _p20
                                                                        ,y: _p19})
                                  ,$WallpaperGroup$Geom$Rotate.rotate90({x: _p20,y: _p19})
                                  ,$WallpaperGroup$Geom$Rotate.rotate90({x: _p20,y: _p19})])
                  ,translate: A2($WallpaperGroup$Geom$Translate.w2h2,_p20,_p19)
                  ,tileCoordinates: A2($WallpaperGroup$Geom$Util.rectCoords,
                  _p20,
                  _p19)};
         case "P4mm": var _p22 = _p0._0;
           var _p21 = _p0._1;
           return {steps: _U.list([A2($WallpaperGroup$Geom$Mirror.mirrorDiagonalRL,
                                  _p22,
                                  _p21)
                                  ,linesToTile
                                  ,$WallpaperGroup$Geom$Rotate.rotate90({x: _p22,y: _p21})
                                  ,$WallpaperGroup$Geom$Rotate.rotate90({x: _p22,y: _p21})
                                  ,$WallpaperGroup$Geom$Rotate.rotate90({x: _p22,y: _p21})])
                  ,translate: A2($WallpaperGroup$Geom$Translate.w2h2,_p22,_p21)
                  ,tileCoordinates: A3($WallpaperGroup$Geom$BoundingBox.Triangle,
                  {x: 0,y: 0},
                  {x: _p22,y: _p21},
                  {x: 0,y: _p21})};
         case "P4mg": var _p24 = _p0._0;
           var _p23 = _p0._1;
           return {steps: _U.list([A2($WallpaperGroup$Geom$Mirror.mirrorDiagonalLR,
                                  _p24,
                                  _p23)
                                  ,linesToTile
                                  ,$WallpaperGroup$Geom$Rotate.rotate90({x: _p24,y: _p23})
                                  ,$WallpaperGroup$Geom$Rotate.rotate90({x: _p24,y: _p23})
                                  ,$WallpaperGroup$Geom$Rotate.rotate90({x: _p24,y: _p23})])
                  ,translate: A2($WallpaperGroup$Geom$Translate.w2h2,_p24,_p23)
                  ,tileCoordinates: A2($WallpaperGroup$Geom$Util.rightTriangleCoords,
                  _p24,
                  _p23)};
         case "P3": var _p25 = _p0._0;
           var centerX = $Basics.sqrt(3) / 2 * _p25;
           return {steps: _U.list([$WallpaperGroup$Geom$Rotate.rotate120({x: centerX
                                                                         ,y: _p25})
                                  ,$WallpaperGroup$Geom$Rotate.rotate120({x: centerX,y: _p25})])
                  ,translate: A2($WallpaperGroup$Geom$Translate.hex,
                  centerX * 2,
                  _p25 * 2)
                  ,tileCoordinates: A4($WallpaperGroup$Geom$BoundingBox.Rect,
                  {x: 0,y: _p25 / 2},
                  {x: centerX,y: _p25},
                  {x: centerX,y: _p25 * 2},
                  {x: 0,y: _p25 * 1.5})};
         case "P3m1": var _p26 = _p0._0;
           var centerX = $Basics.sqrt(3) / 2 * _p26;
           return {steps: _U.list([$WallpaperGroup$Geom$Mirror.mirrorHex(_p26)
                                  ,linesToTile
                                  ,$WallpaperGroup$Geom$Rotate.rotate120({x: centerX,y: _p26})
                                  ,$WallpaperGroup$Geom$Rotate.rotate120({x: centerX,y: _p26})])
                  ,translate: A2($WallpaperGroup$Geom$Translate.hex,
                  centerX * 2,
                  _p26 * 2)
                  ,tileCoordinates: A3($WallpaperGroup$Geom$BoundingBox.Triangle,
                  {x: centerX,y: _p26},
                  {x: 0,y: _p26 * 0.5},
                  {x: 0,y: _p26 * 1.5})};
         case "P31m": var _p27 = _p0._0;
           var h = $Basics.sqrt(3) / 2 * _p27;
           return {steps: _U.list([$WallpaperGroup$Geom$Rotate.rotate120({x: _p27 / 2
                                                                         ,y: 2 * h / 3})
                                  ,$WallpaperGroup$Geom$Rotate.rotate120({x: _p27 / 2
                                                                         ,y: 2 * h / 3})
                                  ,linesToTile
                                  ,A2($WallpaperGroup$Geom$Mirror.mirrorTriangle,_p27,h)])
                  ,translate: A2($WallpaperGroup$Geom$Translate.shifted,_p27,h)
                  ,tileCoordinates: A3($WallpaperGroup$Geom$BoundingBox.Triangle,
                  {x: 0,y: h},
                  {x: _p27 / 2,y: 2 * h / 3},
                  {x: _p27,y: h})};
         default: var _p28 = _p0._0;
           var h = $Basics.sqrt(3) / 2 * _p28;
           return {steps: _U.list([$WallpaperGroup$Geom$Rotate.rotate120({x: _p28 / 2
                                                                         ,y: 2 * h / 3})
                                  ,$WallpaperGroup$Geom$Rotate.rotate120({x: _p28 / 2
                                                                         ,y: 2 * h / 3})
                                  ,linesToTile
                                  ,$WallpaperGroup$Geom$Rotate.rotate180(A3($WallpaperGroup$Geom$Util.split,
                                  {x: _p28 / 2,y: 0},
                                  {x: _p28,y: h},
                                  0.5))])
                  ,translate: A2($WallpaperGroup$Geom$Translate.shifted,_p28,h)
                  ,tileCoordinates: A3($WallpaperGroup$Geom$BoundingBox.Triangle,
                  {x: 0,y: h},
                  {x: _p28 / 2,y: 2 * h / 3},
                  {x: _p28,y: h})};}
   };
   return _elm.WallpaperGroup.Settings.values = {_op: _op
                                                ,linesToTile: linesToTile
                                                ,Setting: Setting
                                                ,getGroupSettings: getGroupSettings};
};
Elm.WallpaperGroup = Elm.WallpaperGroup || {};
Elm.WallpaperGroup.Pattern = Elm.WallpaperGroup.Pattern || {};
Elm.WallpaperGroup.Pattern.make = function (_elm) {
   "use strict";
   _elm.WallpaperGroup = _elm.WallpaperGroup || {};
   _elm.WallpaperGroup.Pattern = _elm.WallpaperGroup.Pattern || {};
   if (_elm.WallpaperGroup.Pattern.values)
   return _elm.WallpaperGroup.Pattern.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Geom$BoundingBox = Elm.WallpaperGroup.Geom.BoundingBox.make(_elm),
   $WallpaperGroup$Geom$Line = Elm.WallpaperGroup.Geom.Line.make(_elm),
   $WallpaperGroup$Geom$Point = Elm.WallpaperGroup.Geom.Point.make(_elm),
   $WallpaperGroup$Geom$Tile = Elm.WallpaperGroup.Geom.Tile.make(_elm),
   $WallpaperGroup$Group = Elm.WallpaperGroup.Group.make(_elm),
   $WallpaperGroup$Settings = Elm.WallpaperGroup.Settings.make(_elm);
   var _op = {};
   var bounding = function (group) {
      return function (_) {
         return _.tileCoordinates;
      }($WallpaperGroup$Settings.getGroupSettings(group));
   };
   var translateTile = F2(function (transition,line) {
      return A2($List.map,
      $WallpaperGroup$Geom$Point.add(transition),
      line);
   });
   var translate = F2(function (tile,transition) {
      return A2($List.map,translateTile(transition),tile);
   });
   var calcStep = F2(function (func,lines) {
      return func(lines);
   });
   var calculateTile = F2(function (lines,steps) {
      return $List.concat(A3($List.foldl,
      calcStep,
      _U.list([lines]),
      steps));
   });
   var pattern = F4(function (group,columns,rows,lines) {
      var numberOfTiles = columns * rows - 1;
      var settings = $WallpaperGroup$Settings.getGroupSettings(group);
      var tile = A2(calculateTile,lines,settings.steps);
      return A2($List.map,
      translate(tile),
      A2($List.map,
      settings.translate(columns),
      _U.range(0,numberOfTiles)));
   });
   return _elm.WallpaperGroup.Pattern.values = {_op: _op
                                               ,pattern: pattern
                                               ,bounding: bounding};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Model = Elm.Editor.Model || {};
Elm.Editor.Model.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Model = _elm.Editor.Model || {};
   if (_elm.Editor.Model.values) return _elm.Editor.Model.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Types = Elm.Editor.Types.make(_elm),
   $Editor$Util$Raster = Elm.Editor.Util.Raster.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Random = Elm.Random.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Geom$BoundingBox = Elm.WallpaperGroup.Geom.BoundingBox.make(_elm),
   $WallpaperGroup$Group = Elm.WallpaperGroup.Group.make(_elm),
   $WallpaperGroup$Pattern = Elm.WallpaperGroup.Pattern.make(_elm);
   var _op = {};
   var initialColorState = {colorSearch: ""
                           ,palettes: _U.list([])
                           ,selectedPalette: _U.list([])
                           ,selectedGradient: _U.list([])
                           ,loading: false
                           ,paletteOpen: false};
   var initialDrawingState = {lineStart: {x: 0,y: 0}
                             ,lineEnd: {x: 0,y: 0}
                             ,isDrawing: false
                             ,rasterCoords: A2($Editor$Util$Raster.rasterCoords,
                             4,
                             $WallpaperGroup$Pattern.bounding(A2($WallpaperGroup$Group.P1,
                             150,
                             150)))};
   var initialPatternState = {columns: 10
                             ,rows: 10
                             ,noiseX: 10
                             ,noiseY: 10
                             ,noiseZ: 10
                             ,noiseDesctruction: 5
                             ,groupType: "P4"
                             ,rasterSize: 4
                             ,boundingBox: $WallpaperGroup$Pattern.bounding(A2($WallpaperGroup$Group.P4,
                             150,
                             150))
                             ,tile: _U.list([])
                             ,group: A2($WallpaperGroup$Group.P4,40,40)
                             ,previewGroup: A2($WallpaperGroup$Group.P4,150,150)
                             ,noise: _U.list([])
                             ,pattern: _U.list([])};
   var initialModel = {patternState: initialPatternState
                      ,drawingState: initialDrawingState
                      ,colorState: initialColorState
                      ,seed: $Random.initialSeed(31415)
                      ,undoStack: _U.list([])
                      ,redoStack: _U.list([])};
   var Model = F6(function (a,b,c,d,e,f) {
      return {patternState: a
             ,drawingState: b
             ,colorState: c
             ,undoStack: d
             ,redoStack: e
             ,seed: f};
   });
   var ColorState = F6(function (a,b,c,d,e,f) {
      return {colorSearch: a
             ,palettes: b
             ,selectedPalette: c
             ,selectedGradient: d
             ,loading: e
             ,paletteOpen: f};
   });
   var DrawingState = F4(function (a,b,c,d) {
      return {lineStart: a
             ,lineEnd: b
             ,isDrawing: c
             ,rasterCoords: d};
   });
   var PatternState = function (a) {
      return function (b) {
         return function (c) {
            return function (d) {
               return function (e) {
                  return function (f) {
                     return function (g) {
                        return function (h) {
                           return function (i) {
                              return function (j) {
                                 return function (k) {
                                    return function (l) {
                                       return function (m) {
                                          return function (n) {
                                             return {columns: a
                                                    ,rows: b
                                                    ,noiseX: c
                                                    ,noiseY: d
                                                    ,noiseZ: e
                                                    ,noiseDesctruction: f
                                                    ,group: g
                                                    ,previewGroup: h
                                                    ,groupType: i
                                                    ,boundingBox: j
                                                    ,rasterSize: k
                                                    ,tile: l
                                                    ,noise: m
                                                    ,pattern: n};
                                          };
                                       };
                                    };
                                 };
                              };
                           };
                        };
                     };
                  };
               };
            };
         };
      };
   };
   return _elm.Editor.Model.values = {_op: _op
                                     ,PatternState: PatternState
                                     ,DrawingState: DrawingState
                                     ,ColorState: ColorState
                                     ,Model: Model
                                     ,initialPatternState: initialPatternState
                                     ,initialDrawingState: initialDrawingState
                                     ,initialColorState: initialColorState
                                     ,initialModel: initialModel};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Util = Elm.Editor.Util || {};
Elm.Editor.Util.Geom = Elm.Editor.Util.Geom || {};
Elm.Editor.Util.Geom.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Util = _elm.Editor.Util || {};
   _elm.Editor.Util.Geom = _elm.Editor.Util.Geom || {};
   if (_elm.Editor.Util.Geom.values)
   return _elm.Editor.Util.Geom.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Types = Elm.Editor.Types.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Random = Elm.Random.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var coords = F7(function (param,x1,x2,y1,y2,c,d) {
      return _U.cmp(param,0) < 0 || _U.eq(x1,x2) && _U.eq(y1,
      y2) ? {ctor: "_Tuple2",_0: x1,_1: y1} : _U.cmp(param,
      1) > 0 ? {ctor: "_Tuple2",_0: x2,_1: y2} : {ctor: "_Tuple2"
                                                 ,_0: x1 + param * c
                                                 ,_1: y1 + param * d};
   });
   var multiLineToTuple = function (line) {
      var p1 = A2($Maybe.withDefault,{x: 0,y: 0},$List.head(line));
      var p2 = A2($Maybe.withDefault,
      p1,
      $List.head(A2($List.drop,1,line)));
      return {ctor: "_Tuple2",_0: p1,_1: p2};
   };
   var lineIsNearPoint = F3(function (p,distance,line$) {
      var line = multiLineToTuple(line$);
      var p1 = $Basics.fst(line);
      var x1 = p1.x;
      var a = p.x - x1;
      var y1 = p1.y;
      var b = p.y - y1;
      var p2 = $Basics.snd(line);
      var x2 = p2.x;
      var c = x2 - x1;
      var y2 = p2.y;
      var d = y2 - y1;
      var dot = a * c + b * d;
      var len_sq = c * c + d * d;
      var param = dot / len_sq;
      var cords = A7(coords,param,x1,x2,y1,y2,c,d);
      var dx = p.x - $Basics.fst(cords);
      var dy = p.y - $Basics.snd(cords);
      var actualDistance = $Basics.sqrt(dx) * dx + dy * dy;
      return _U.cmp(actualDistance,distance) > 0;
   });
   var distance = F2(function (p1,p2) {
      return $Basics.sqrt(Math.pow(p1.x - p2.x,
      2) + Math.pow(p1.y - p2.y,2));
   });
   var findShortest = F3(function (p1,p2,r) {
      var dist = A2(distance,p1,p2);
      return _U.cmp(dist,r.d) < 0 ? {p: p2,d: dist} : r;
   });
   var snap = F2(function (points,p) {
      return function (_) {
         return _.p;
      }(A3($List.foldl,
      findShortest(p),
      {d: $Basics.toFloat($Random.maxInt),p: p},
      points));
   });
   return _elm.Editor.Util.Geom.values = {_op: _op
                                         ,distance: distance
                                         ,findShortest: findShortest
                                         ,snap: snap
                                         ,multiLineToTuple: multiLineToTuple
                                         ,coords: coords
                                         ,lineIsNearPoint: lineIsNearPoint};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Util = Elm.Editor.Util || {};
Elm.Editor.Util.TileSize = Elm.Editor.Util.TileSize || {};
Elm.Editor.Util.TileSize.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Util = _elm.Editor.Util || {};
   _elm.Editor.Util.TileSize = _elm.Editor.Util.TileSize || {};
   if (_elm.Editor.Util.TileSize.values)
   return _elm.Editor.Util.TileSize.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Regex = Elm.Regex.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var getTileSize = function (groupType) {
      return A2($Regex.contains,
      $Regex.regex("(P6)|(P31m)|(Cm)"),
      groupType) ? 70 : A2($Regex.contains,
      $Regex.regex("P3"),
      groupType) ? 50 : 40;
   };
   var getPreviewTileSize = function (groupType) {
      return A2($Regex.contains,
      $Regex.regex("(P6)|(P31m)"),
      groupType) ? 200 : A2($Regex.contains,
      $Regex.regex("P3"),
      groupType) ? 172 : 150;
   };
   return _elm.Editor.Util.TileSize.values = {_op: _op
                                             ,getPreviewTileSize: getPreviewTileSize
                                             ,getTileSize: getTileSize};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Util = Elm.Editor.Util || {};
Elm.Editor.Util.Noise = Elm.Editor.Util.Noise || {};
Elm.Editor.Util.Noise.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Util = _elm.Editor.Util || {};
   _elm.Editor.Util.Noise = _elm.Editor.Util.Noise || {};
   if (_elm.Editor.Util.Noise.values)
   return _elm.Editor.Util.Noise.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Model = Elm.Editor.Model.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Noise = Elm.Noise.make(_elm),
   $Random = Elm.Random.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var noise = F2(function (model,z) {
      var seed = model.seed;
      var noiseZ = $Basics.toFloat(model.patternState.noiseZ);
      var noiseY = $Basics.toFloat(model.patternState.noiseY);
      var noiseX = $Basics.toFloat(model.patternState.noiseX);
      var maxZ = $Basics.toFloat(z);
      var maxY = $Basics.toFloat(model.patternState.rows);
      var maxX = $Basics.toFloat(model.patternState.columns);
      if (_U.eq(maxX,0) || (_U.eq(maxY,0) || _U.eq(maxZ,0)))
      return {ctor: "_Tuple2",_0: _U.list([]),_1: seed}; else {
            var _p0 = $Noise.permutationTable(model.seed);
            var perm = _p0._0;
            var newSeed = _p0._1;
            var list = A3($List.foldr,
            F2(function (x,r) {
               return A3($List.foldr,
               F2(function (y,r) {
                  return A2($List._op["::"],
                  A3($List.foldr,
                  F2(function (z,r) {
                     return A2($List._op["::"],
                     A4($Noise.noise3d,perm,x / noiseX,y / noiseY,z / noiseZ),
                     r);
                  }),
                  _U.list([]),
                  _U.range(1,maxZ)),
                  r);
               }),
               r,
               _U.range(1,maxY));
            }),
            _U.list([]),
            _U.range(1,maxX));
            return {ctor: "_Tuple2",_0: list,_1: newSeed};
         }
   });
   return _elm.Editor.Util.Noise.values = {_op: _op,noise: noise};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Util = Elm.Editor.Util || {};
Elm.Editor.Util.Pattern = Elm.Editor.Util.Pattern || {};
Elm.Editor.Util.Pattern.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Util = _elm.Editor.Util || {};
   _elm.Editor.Util.Pattern = _elm.Editor.Util.Pattern || {};
   if (_elm.Editor.Util.Pattern.values)
   return _elm.Editor.Util.Pattern.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Model = Elm.Editor.Model.make(_elm),
   $Editor$Types = Elm.Editor.Types.make(_elm),
   $Editor$Util$Noise = Elm.Editor.Util.Noise.make(_elm),
   $Editor$Util$TileSize = Elm.Editor.Util.TileSize.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $WallpaperGroup$Pattern = Elm.WallpaperGroup.Pattern.make(_elm);
   var _op = {};
   var randomPoint = F3(function (_p0,noise,noiseDesctruction) {
      var _p1 = _p0;
      var _p3 = _p1._1;
      var _p2 = _p1._0;
      var rad = noise * $Basics.toFloat(noiseDesctruction);
      var angle = noise * $Basics.pi * 4;
      return {ctor: "_Tuple2"
             ,_0: {x: _p2.x + $Basics.cos(angle) * rad
                  ,y: _p2.y - $Basics.sin(angle) * rad}
             ,_1: {x: _p3.x - $Basics.cos(angle) * rad
                  ,y: _p3.y + $Basics.sin(angle) * rad}};
   });
   var getTileLength = function (tiles) {
      return $List.length(A2($Maybe.withDefault,
      _U.list([]),
      $List.head(tiles)));
   };
   var scalePoint = F2(function (_p4,p) {
      var _p5 = _p4;
      var _p6 = _p5.groupType;
      return {x: p.x / $Editor$Util$TileSize.getPreviewTileSize(_p6) * $Editor$Util$TileSize.getTileSize(_p6)
             ,y: p.y / $Editor$Util$TileSize.getPreviewTileSize(_p6) * $Editor$Util$TileSize.getTileSize(_p6)};
   });
   var getColor = F2(function (colors,noise) {
      var i = $Basics.floor($Basics.toFloat($List.length(colors)) * ((1 + noise) / 2));
      var color = A2($Array.get,i,$Array.fromList(colors));
      var _p7 = color;
      if (_p7.ctor === "Just") {
            return _p7._0;
         } else {
            return "grey";
         }
   });
   var calcPath = F3(function (noiseDesctruction,colors,_p8) {
      var _p9 = _p8;
      var _p12 = _p9._0;
      var _p11 = _p9._1;
      var color = A2(getColor,colors,_p12);
      var p2 = A2($Maybe.withDefault,
      {x: 0,y: 0},
      $List.head($List.reverse(_p11)));
      var p1 = A2($Maybe.withDefault,{x: 0,y: 0},$List.head(_p11));
      var _p10 = A3(randomPoint,
      {ctor: "_Tuple2",_0: p1,_1: p2},
      0 - _p12,
      noiseDesctruction);
      var c1 = _p10._0;
      var c2 = _p10._1;
      return {p1: p1
             ,p2: p2
             ,c1: c1
             ,c2: c2
             ,color: color
             ,opacity: 1
             ,strokeWidth: $Basics.abs($Basics.sin(_p12)) * 2};
   });
   var calcTile = F3(function (noiseDesctruction,colors,tile) {
      var lines = A2($List.filter,
      function (_p13) {
         var _p14 = _p13;
         return _U.cmp($Basics.abs(_p14._0),0.1) > -1;
      },
      tile);
      return A2($List.map,
      A2(calcPath,noiseDesctruction,colors),
      lines);
   });
   var updatePatternInModel = function (model) {
      var colors = model.colorState.selectedGradient;
      var patternState = model.patternState;
      var group = patternState.group;
      var columns = patternState.columns;
      var rows = patternState.rows;
      var tile = A2($List.map,
      $List.map(scalePoint(patternState)),
      patternState.tile);
      var groups = A4($WallpaperGroup$Pattern.pattern,
      group,
      rows,
      columns,
      tile);
      var maxZ = getTileLength(groups);
      var _p15 = A2($Editor$Util$Noise.noise,model,maxZ);
      var noise = _p15._0;
      var seed = _p15._1;
      var noisyGroups = A3($List.map2,
      $List.map2(F2(function (v0,v1) {
         return {ctor: "_Tuple2",_0: v0,_1: v1};
      })),
      noise,
      groups);
      var noiseDesctruction = patternState.noiseDesctruction;
      var pattern = A2($List.map,
      A2(calcTile,noiseDesctruction,colors),
      noisyGroups);
      return _U.update(model,
      {patternState: _U.update(patternState,{pattern: pattern})
      ,seed: seed});
   };
   return _elm.Editor.Util.Pattern.values = {_op: _op
                                            ,getColor: getColor
                                            ,scalePoint: scalePoint
                                            ,getTileLength: getTileLength
                                            ,randomPoint: randomPoint
                                            ,calcPath: calcPath
                                            ,calcTile: calcTile
                                            ,updatePatternInModel: updatePatternInModel};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Util = Elm.Editor.Util || {};
Elm.Editor.Util.History = Elm.Editor.Util.History || {};
Elm.Editor.Util.History.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Util = _elm.Editor.Util || {};
   _elm.Editor.Util.History = _elm.Editor.Util.History || {};
   if (_elm.Editor.Util.History.values)
   return _elm.Editor.Util.History.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Model = Elm.Editor.Model.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var redo = function (model) {
      var newHistory = A2($Maybe.withDefault,
      _U.list([]),
      $List.tail(model.redoStack));
      var lastState = $List.head(model.redoStack);
      var actualState = model.patternState;
      var undoStack = model.undoStack;
      var _p0 = lastState;
      if (_p0.ctor === "Just") {
            return _U.update(model,
            {redoStack: newHistory
            ,patternState: _p0._0
            ,undoStack: A2($List._op["::"],actualState,undoStack)});
         } else {
            return model;
         }
   };
   var undo = function (model) {
      var redoStack = model.redoStack;
      var undoStack = model.undoStack;
      var actualState = model.patternState;
      var lastState = $List.head(model.undoStack);
      var _p1 = lastState;
      if (_p1.ctor === "Just") {
            return _U.update(model,
            {undoStack: A2($Maybe.withDefault,
            _U.list([]),
            $List.tail(undoStack))
            ,patternState: _p1._0
            ,redoStack: A2($List._op["::"],actualState,redoStack)});
         } else {
            return model;
         }
   };
   var addHistory = function (model) {
      var undoStack = model.undoStack;
      var actualState = model.patternState;
      return _U.update(model,
      {undoStack: A2($List._op["::"],actualState,undoStack)
      ,redoStack: _U.list([])});
   };
   return _elm.Editor.Util.History.values = {_op: _op
                                            ,addHistory: addHistory
                                            ,undo: undo
                                            ,redo: redo};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Util = Elm.Editor.Util || {};
Elm.Editor.Util.Color = Elm.Editor.Util.Color || {};
Elm.Editor.Util.Color.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Util = _elm.Editor.Util || {};
   _elm.Editor.Util.Color = _elm.Editor.Util.Color || {};
   if (_elm.Editor.Util.Color.values)
   return _elm.Editor.Util.Color.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Color$Convert = Elm.Color.Convert.make(_elm),
   $Color$Gradient = Elm.Color.Gradient.make(_elm),
   $Color$Interpolate = Elm.Color.Interpolate.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var toGradient = function (l) {
      return A2($List.map,
      $Color$Convert.colorToCssRgb,
      A3($Basics.flip,
      $Color$Gradient.gradient($Color$Interpolate.LAB),
      20,
      A2($Debug.log,
      "colors",
      A2($List.filterMap,
      $Basics.identity,
      A2($List.map,$Color$Convert.hexToColor,l)))));
   };
   return _elm.Editor.Util.Color.values = {_op: _op
                                          ,toGradient: toGradient};
};
Elm.Native.Effects = {};
Elm.Native.Effects.make = function(localRuntime) {

	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Effects = localRuntime.Native.Effects || {};
	if (localRuntime.Native.Effects.values)
	{
		return localRuntime.Native.Effects.values;
	}

	var Task = Elm.Native.Task.make(localRuntime);
	var Utils = Elm.Native.Utils.make(localRuntime);
	var Signal = Elm.Signal.make(localRuntime);
	var List = Elm.Native.List.make(localRuntime);


	// polyfill so things will work even if rAF is not available for some reason
	var _requestAnimationFrame =
		typeof requestAnimationFrame !== 'undefined'
			? requestAnimationFrame
			: function(cb) { setTimeout(cb, 1000 / 60); }
			;


	// batchedSending and sendCallback implement a small state machine in order
	// to schedule only one send(time) call per animation frame.
	//
	// Invariants:
	// 1. In the NO_REQUEST state, there is never a scheduled sendCallback.
	// 2. In the PENDING_REQUEST and EXTRA_REQUEST states, there is always exactly
	//    one scheduled sendCallback.
	var NO_REQUEST = 0;
	var PENDING_REQUEST = 1;
	var EXTRA_REQUEST = 2;
	var state = NO_REQUEST;
	var messageArray = [];


	function batchedSending(address, tickMessages)
	{
		// insert ticks into the messageArray
		var foundAddress = false;

		for (var i = messageArray.length; i--; )
		{
			if (messageArray[i].address === address)
			{
				foundAddress = true;
				messageArray[i].tickMessages = A3(List.foldl, List.cons, messageArray[i].tickMessages, tickMessages);
				break;
			}
		}

		if (!foundAddress)
		{
			messageArray.push({ address: address, tickMessages: tickMessages });
		}

		// do the appropriate state transition
		switch (state)
		{
			case NO_REQUEST:
				_requestAnimationFrame(sendCallback);
				state = PENDING_REQUEST;
				break;
			case PENDING_REQUEST:
				state = PENDING_REQUEST;
				break;
			case EXTRA_REQUEST:
				state = PENDING_REQUEST;
				break;
		}
	}


	function sendCallback(time)
	{
		switch (state)
		{
			case NO_REQUEST:
				// This state should not be possible. How can there be no
				// request, yet somehow we are actively fulfilling a
				// request?
				throw new Error(
					'Unexpected send callback.\n' +
					'Please report this to <https://github.com/evancz/elm-effects/issues>.'
				);

			case PENDING_REQUEST:
				// At this point, we do not *know* that another frame is
				// needed, but we make an extra request to rAF just in
				// case. It's possible to drop a frame if rAF is called
				// too late, so we just do it preemptively.
				_requestAnimationFrame(sendCallback);
				state = EXTRA_REQUEST;

				// There's also stuff we definitely need to send.
				send(time);
				return;

			case EXTRA_REQUEST:
				// Turns out the extra request was not needed, so we will
				// stop calling rAF. No reason to call it all the time if
				// no one needs it.
				state = NO_REQUEST;
				return;
		}
	}


	function send(time)
	{
		for (var i = messageArray.length; i--; )
		{
			var messages = A3(
				List.foldl,
				F2( function(toAction, list) { return List.Cons(toAction(time), list); } ),
				List.Nil,
				messageArray[i].tickMessages
			);
			Task.perform( A2(Signal.send, messageArray[i].address, messages) );
		}
		messageArray = [];
	}


	function requestTickSending(address, tickMessages)
	{
		return Task.asyncFunction(function(callback) {
			batchedSending(address, tickMessages);
			callback(Task.succeed(Utils.Tuple0));
		});
	}


	return localRuntime.Native.Effects.values = {
		requestTickSending: F2(requestTickSending)
	};

};

Elm.Effects = Elm.Effects || {};
Elm.Effects.make = function (_elm) {
   "use strict";
   _elm.Effects = _elm.Effects || {};
   if (_elm.Effects.values) return _elm.Effects.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$Effects = Elm.Native.Effects.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm),
   $Time = Elm.Time.make(_elm);
   var _op = {};
   var ignore = function (task) {
      return A2($Task.map,$Basics.always({ctor: "_Tuple0"}),task);
   };
   var requestTickSending = $Native$Effects.requestTickSending;
   var toTaskHelp = F3(function (address,effect,_p0) {
      var _p1 = _p0;
      var _p5 = _p1._1;
      var _p4 = _p1;
      var _p3 = _p1._0;
      var _p2 = effect;
      switch (_p2.ctor)
      {case "Task": var reporter = A2($Task.andThen,
           _p2._0,
           function (answer) {
              return A2($Signal.send,address,_U.list([answer]));
           });
           return {ctor: "_Tuple2"
                  ,_0: A2($Task.andThen,
                  _p3,
                  $Basics.always(ignore($Task.spawn(reporter))))
                  ,_1: _p5};
         case "Tick": return {ctor: "_Tuple2"
                             ,_0: _p3
                             ,_1: A2($List._op["::"],_p2._0,_p5)};
         case "None": return _p4;
         default: return A3($List.foldl,toTaskHelp(address),_p4,_p2._0);}
   });
   var toTask = F2(function (address,effect) {
      var _p6 = A3(toTaskHelp,
      address,
      effect,
      {ctor: "_Tuple2"
      ,_0: $Task.succeed({ctor: "_Tuple0"})
      ,_1: _U.list([])});
      var combinedTask = _p6._0;
      var tickMessages = _p6._1;
      return $List.isEmpty(tickMessages) ? combinedTask : A2($Task.andThen,
      combinedTask,
      $Basics.always(A2(requestTickSending,address,tickMessages)));
   });
   var Never = function (a) {    return {ctor: "Never",_0: a};};
   var Batch = function (a) {    return {ctor: "Batch",_0: a};};
   var batch = Batch;
   var None = {ctor: "None"};
   var none = None;
   var Tick = function (a) {    return {ctor: "Tick",_0: a};};
   var tick = Tick;
   var Task = function (a) {    return {ctor: "Task",_0: a};};
   var task = Task;
   var map = F2(function (func,effect) {
      var _p7 = effect;
      switch (_p7.ctor)
      {case "Task": return Task(A2($Task.map,func,_p7._0));
         case "Tick": return Tick(function (_p8) {
              return func(_p7._0(_p8));
           });
         case "None": return None;
         default: return Batch(A2($List.map,map(func),_p7._0));}
   });
   return _elm.Effects.values = {_op: _op
                                ,none: none
                                ,task: task
                                ,tick: tick
                                ,map: map
                                ,batch: batch
                                ,toTask: toTask};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Signals = Elm.Editor.Signals || {};
Elm.Editor.Signals.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Signals = _elm.Editor.Signals || {};
   if (_elm.Editor.Signals.values)
   return _elm.Editor.Signals.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Signal$Time = Elm.Signal.Time.make(_elm),
   $String = Elm.String.make(_elm),
   $Time = Elm.Time.make(_elm);
   var _op = {};
   var requestPalette = $Signal.mailbox("");
   var requestPaletteFilter = A2($Signal$Time.settledAfter,
   300 * $Time.millisecond,
   A3($Signal.filter,
   function (_p0) {
      return $Basics.not($String.isEmpty(_p0));
   },
   "",
   requestPalette.signal));
   return _elm.Editor.Signals.values = {_op: _op
                                       ,requestPalette: requestPalette
                                       ,requestPaletteFilter: requestPaletteFilter};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Action = Elm.Editor.Action || {};
Elm.Editor.Action.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Action = _elm.Editor.Action || {};
   if (_elm.Editor.Action.values) return _elm.Editor.Action.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Array = Elm.Array.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Model = Elm.Editor.Model.make(_elm),
   $Editor$Signals = Elm.Editor.Signals.make(_elm),
   $Editor$Types = Elm.Editor.Types.make(_elm),
   $Editor$Util$Color = Elm.Editor.Util.Color.make(_elm),
   $Editor$Util$Geom = Elm.Editor.Util.Geom.make(_elm),
   $Editor$Util$History = Elm.Editor.Util.History.make(_elm),
   $Editor$Util$Pattern = Elm.Editor.Util.Pattern.make(_elm),
   $Editor$Util$Raster = Elm.Editor.Util.Raster.make(_elm),
   $Editor$Util$TileSize = Elm.Editor.Util.TileSize.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Random = Elm.Random.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm),
   $WallpaperGroup$Group = Elm.WallpaperGroup.Group.make(_elm),
   $WallpaperGroup$Pattern = Elm.WallpaperGroup.Pattern.make(_elm);
   var _op = {};
   var getRandom = F3(function (seed,min,max) {
      var generator = A2($Random.$int,min,max);
      return A2($Random.generate,generator,seed);
   });
   var getRandomCoord = F3(function (points,seed,i) {
      var getValue = function (item) {
         var _p0 = item;
         if (_p0.ctor === "Just") {
               return _p0._0;
            } else {
               return {x: 0,y: 0};
            }
      };
      var seed$ = $Random.initialSeed(i + $Basics.fst(A3(getRandom,
      seed,
      $Random.minInt,
      $Random.maxInt)));
      var _p1 = A3(getRandom,seed$,0,$List.length(points) - 1);
      var i1 = _p1._0;
      var seed$$ = _p1._1;
      var _p2 = A3(getRandom,seed$$,0,$List.length(points) - 1);
      var i2 = _p2._0;
      return _U.list([getValue(A2($Array.get,
                     i1,
                     $Array.fromList(points)))
                     ,getValue(A2($Array.get,i2,$Array.fromList(points)))]);
   });
   var getGroup = F3(function (groupType,height,width) {
      return _U.eq(groupType,"P1") ? A2($WallpaperGroup$Group.P1,
      width,
      height) : _U.eq(groupType,"P2") ? A2($WallpaperGroup$Group.P2,
      width,
      height) : _U.eq(groupType,"Pm") ? A2($WallpaperGroup$Group.Pm,
      width,
      height) : _U.eq(groupType,"Pg") ? A2($WallpaperGroup$Group.Pg,
      width,
      height) : _U.eq(groupType,"Cm") ? A2($WallpaperGroup$Group.Cm,
      width,
      height) : _U.eq(groupType,
      "P2mm") ? A2($WallpaperGroup$Group.P2mm,
      width,
      height) : _U.eq(groupType,
      "P2mg") ? A2($WallpaperGroup$Group.P2mg,
      width,
      height) : _U.eq(groupType,
      "P2gg") ? A2($WallpaperGroup$Group.P2gg,
      width,
      height) : _U.eq(groupType,
      "C2mm") ? A2($WallpaperGroup$Group.C2mm,
      width,
      height) : _U.eq(groupType,"P4") ? A2($WallpaperGroup$Group.P4,
      width,
      height) : _U.eq(groupType,
      "P4mm") ? A2($WallpaperGroup$Group.P4mm,
      width,
      height) : _U.eq(groupType,
      "P4mg") ? A2($WallpaperGroup$Group.P4mg,
      width,
      height) : _U.eq(groupType,
      "P3") ? $WallpaperGroup$Group.P3(width) : _U.eq(groupType,
      "P3m1") ? $WallpaperGroup$Group.P3m1(width) : _U.eq(groupType,
      "P31m") ? $WallpaperGroup$Group.P31m(width) : _U.eq(groupType,
      "P6") ? $WallpaperGroup$Group.P6(width) : A2($WallpaperGroup$Group.P1,
      width,
      height);
   });
   var UpadtePattern = {ctor: "UpadtePattern"};
   var ClosePallete = {ctor: "ClosePallete"};
   var TogglePallete = function (a) {
      return {ctor: "TogglePallete",_0: a};
   };
   var SelectPalette = function (a) {
      return {ctor: "SelectPalette",_0: a};
   };
   var NewColors = function (a) {
      return {ctor: "NewColors",_0: a};
   };
   var StartColorSearch = function (a) {
      return {ctor: "StartColorSearch",_0: a};
   };
   var Redo = {ctor: "Redo"};
   var Undo = {ctor: "Undo"};
   var Random = {ctor: "Random"};
   var ClearTiles = {ctor: "ClearTiles"};
   var DeleteLine = function (a) {
      return {ctor: "DeleteLine",_0: a};
   };
   var LineEnd = function (a) {
      return {ctor: "LineEnd",_0: a};
   };
   var LineMove = function (a) {
      return {ctor: "LineMove",_0: a};
   };
   var LineStart = function (a) {
      return {ctor: "LineStart",_0: a};
   };
   var RasterSize = function (a) {
      return {ctor: "RasterSize",_0: a};
   };
   var Group = function (a) {    return {ctor: "Group",_0: a};};
   var NoiseDesctruction = function (a) {
      return {ctor: "NoiseDesctruction",_0: a};
   };
   var NoiseZ = function (a) {    return {ctor: "NoiseZ",_0: a};};
   var NoiseY = function (a) {    return {ctor: "NoiseY",_0: a};};
   var NoiseX = function (a) {    return {ctor: "NoiseX",_0: a};};
   var Rows = function (a) {    return {ctor: "Rows",_0: a};};
   var Columns = function (a) {
      return {ctor: "Columns",_0: a};
   };
   var NoOp = {ctor: "NoOp"};
   var update = F2(function (action,model) {
      var colorState = model.colorState;
      var drawingState = model.drawingState;
      var patternState = model.patternState;
      var _p3 = action;
      switch (_p3.ctor)
      {case "NoOp": return {ctor: "_Tuple2"
                           ,_0: model
                           ,_1: $Effects.none};
         case "Rows": var model = $Editor$Util$History.addHistory(model);
           return {ctor: "_Tuple2"
                  ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                  {patternState: _U.update(patternState,{rows: _p3._0})}))
                  ,_1: $Effects.none};
         case "Columns":
         var model = $Editor$Util$History.addHistory(model);
           return {ctor: "_Tuple2"
                  ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                  {patternState: _U.update(patternState,{columns: _p3._0})}))
                  ,_1: $Effects.none};
         case "NoiseX":
         var model = $Editor$Util$History.addHistory(model);
           return {ctor: "_Tuple2"
                  ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                  {patternState: _U.update(patternState,{noiseX: _p3._0})}))
                  ,_1: $Effects.none};
         case "NoiseY":
         var model = $Editor$Util$History.addHistory(model);
           return {ctor: "_Tuple2"
                  ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                  {patternState: _U.update(patternState,{noiseY: _p3._0})}))
                  ,_1: $Effects.none};
         case "NoiseZ":
         var model = $Editor$Util$History.addHistory(model);
           return {ctor: "_Tuple2"
                  ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                  {patternState: _U.update(patternState,{noiseZ: _p3._0})}))
                  ,_1: $Effects.none};
         case "NoiseDesctruction":
         var model = $Editor$Util$History.addHistory(model);
           return {ctor: "_Tuple2"
                  ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                  {patternState: _U.update(patternState,
                  {noiseDesctruction: _p3._0})}))
                  ,_1: $Effects.none};
         case "Group": var _p4 = _p3._0;
           var groupSize = $Editor$Util$TileSize.getTileSize(_p4);
           var previewGroupSize = $Editor$Util$TileSize.getPreviewTileSize(_p4);
           var previewGroup = A3(getGroup,
           _p4,
           previewGroupSize,
           previewGroupSize);
           var boundingBox = $WallpaperGroup$Pattern.bounding(previewGroup);
           var model = $Editor$Util$History.addHistory(model);
           return {ctor: "_Tuple2"
                  ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                  {patternState: _U.update(patternState,
                  {group: A3(getGroup,_p4,groupSize,groupSize)
                  ,previewGroup: previewGroup
                  ,groupType: _p4
                  ,boundingBox: boundingBox})
                  ,drawingState: _U.update(drawingState,
                  {rasterCoords: A2($Editor$Util$Raster.rasterCoords,
                  patternState.rasterSize,
                  $WallpaperGroup$Pattern.bounding(A3(getGroup,
                  _p4,
                  previewGroupSize,
                  previewGroupSize)))})}))
                  ,_1: $Effects.none};
         case "RasterSize": var _p5 = _p3._0;
           var model = $Editor$Util$History.addHistory(model);
           var previewGroupSize = $Editor$Util$TileSize.getPreviewTileSize(model.patternState.groupType);
           return {ctor: "_Tuple2"
                  ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                  {patternState: _U.update(patternState,
                  {rasterSize: _p5,tile: _U.list([])})
                  ,drawingState: _U.update(drawingState,
                  {rasterCoords: A2($Editor$Util$Raster.rasterCoords,
                  _p5,
                  $WallpaperGroup$Pattern.bounding(A3(getGroup,
                  patternState.groupType,
                  previewGroupSize,
                  previewGroupSize)))})}))
                  ,_1: $Effects.none};
         case "LineStart": var _p6 = _p3._0;
           return {ctor: "_Tuple2"
                  ,_0: _U.update(model,
                  {drawingState: _U.update(drawingState,
                  {lineStart: A2($Editor$Util$Geom.snap,
                  drawingState.rasterCoords,
                  _p6)
                  ,lineEnd: A2($Editor$Util$Geom.snap,
                  drawingState.rasterCoords,
                  _p6)
                  ,isDrawing: true})})
                  ,_1: $Effects.none};
         case "LineMove":
         return drawingState.isDrawing ? {ctor: "_Tuple2"
                                         ,_0: _U.update(model,
                                         {drawingState: _U.update(drawingState,
                                         {lineEnd: A2($Editor$Util$Geom.snap,
                                         drawingState.rasterCoords,
                                         _p3._0)})})
                                         ,_1: $Effects.none} : {ctor: "_Tuple2"
                                                               ,_0: model
                                                               ,_1: $Effects.none};
         case "LineEnd": if (drawingState.isDrawing) {
                 var tile = A2($List._op["::"],
                 _U.list([drawingState.lineStart,drawingState.lineEnd]),
                 patternState.tile);
                 var model = $Editor$Util$History.addHistory(model);
                 return {ctor: "_Tuple2"
                        ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                        {drawingState: _U.update(drawingState,{isDrawing: false})
                        ,patternState: _U.update(patternState,{tile: tile})}))
                        ,_1: $Effects.none};
              } else return {ctor: "_Tuple2",_0: model,_1: $Effects.none};
         case "DeleteLine": var tile = A2($List.filter,
           A2($Editor$Util$Geom.lineIsNearPoint,_p3._0,5),
           patternState.tile);
           var model = $Editor$Util$History.addHistory(model);
           return {ctor: "_Tuple2"
                  ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                  {patternState: _U.update(patternState,{tile: tile})}))
                  ,_1: $Effects.none};
         case "ClearTiles":
         var model = $Editor$Util$History.addHistory(model);
           return {ctor: "_Tuple2"
                  ,_0: _U.update(model,
                  {patternState: _U.update(patternState,
                  {tile: _U.list([]),noise: _U.list([]),pattern: _U.list([])})})
                  ,_1: $Effects.none};
         case "Random":
         var model = $Editor$Util$History.addHistory(model);
           var _p7 = A3(getRandom,model.seed,3,10);
           var i = _p7._0;
           var seed$ = _p7._1;
           var tile = A2($List.map,
           A2(getRandomCoord,drawingState.rasterCoords,seed$),
           _U.range(1,i));
           return {ctor: "_Tuple2"
                  ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                  {patternState: _U.update(patternState,{tile: tile})
                  ,seed: seed$}))
                  ,_1: $Effects.none};
         case "Undo": return {ctor: "_Tuple2"
                             ,_0: $Editor$Util$History.undo(model)
                             ,_1: $Effects.none};
         case "Redo": return {ctor: "_Tuple2"
                             ,_0: $Editor$Util$History.redo(model)
                             ,_1: $Effects.none};
         case "StartColorSearch": var _p9 = _p3._0;
           var sendTask = $Effects.task(A2($Task.andThen,
           A2($Signal.send,$Editor$Signals.requestPalette.address,_p9),
           function (_p8) {
              return $Task.succeed(NoOp);
           }));
           var model = $Editor$Util$History.addHistory(model);
           return {ctor: "_Tuple2"
                  ,_0: _U.update(model,
                  {colorState: _U.update(colorState,
                  {colorSearch: _p9,loading: true})})
                  ,_1: sendTask};
         case "NewColors": var _p10 = _p3._0;
           if (_p10.ctor === "Ok") {
                 return {ctor: "_Tuple2"
                        ,_0: _U.update(model,
                        {colorState: _U.update(colorState,
                        {palettes: _p10._0,loading: false})})
                        ,_1: $Effects.none};
              } else {
                 return {ctor: "_Tuple2"
                        ,_0: _U.update(model,
                        {colorState: _U.update(colorState,
                        {palettes: _U.list([]),loading: false})})
                        ,_1: $Effects.none};
              }
         case "SelectPalette": var _p11 = _p3._0;
           var p = $Editor$Util$Color.toGradient(_p11);
           var model = $Editor$Util$History.addHistory(model);
           return {ctor: "_Tuple2"
                  ,_0: $Editor$Util$Pattern.updatePatternInModel(_U.update(model,
                  {colorState: _U.update(colorState,
                  {selectedGradient: p,selectedPalette: _p11})}))
                  ,_1: $Effects.none};
         case "TogglePallete": var _p13 = _p3._0;
           return _p13 ? {ctor: "_Tuple2"
                         ,_0: _U.update(model,
                         {colorState: _U.update(colorState,{paletteOpen: _p13})})
                         ,_1: $Effects.none} : {ctor: "_Tuple2"
                                               ,_0: model
                                               ,_1: $Effects.task(A2($Task.andThen,
                                               $Task.sleep(300),
                                               function (_p12) {
                                                  return $Task.succeed(ClosePallete);
                                               }))};
         case "ClosePallete": return {ctor: "_Tuple2"
                                     ,_0: _U.update(model,
                                     {colorState: _U.update(colorState,{paletteOpen: false})})
                                     ,_1: $Effects.none};
         default: return {ctor: "_Tuple2"
                         ,_0: $Editor$Util$Pattern.updatePatternInModel(model)
                         ,_1: $Effects.none};}
   });
   return _elm.Editor.Action.values = {_op: _op
                                      ,NoOp: NoOp
                                      ,Columns: Columns
                                      ,Rows: Rows
                                      ,NoiseX: NoiseX
                                      ,NoiseY: NoiseY
                                      ,NoiseZ: NoiseZ
                                      ,NoiseDesctruction: NoiseDesctruction
                                      ,Group: Group
                                      ,RasterSize: RasterSize
                                      ,LineStart: LineStart
                                      ,LineMove: LineMove
                                      ,LineEnd: LineEnd
                                      ,DeleteLine: DeleteLine
                                      ,ClearTiles: ClearTiles
                                      ,Random: Random
                                      ,Undo: Undo
                                      ,Redo: Redo
                                      ,StartColorSearch: StartColorSearch
                                      ,NewColors: NewColors
                                      ,SelectPalette: SelectPalette
                                      ,TogglePallete: TogglePallete
                                      ,ClosePallete: ClosePallete
                                      ,UpadtePattern: UpadtePattern
                                      ,getGroup: getGroup
                                      ,getRandom: getRandom
                                      ,getRandomCoord: getRandomCoord
                                      ,update: update};
};
(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);var f=new Error("Cannot find module '"+o+"'");throw f.code="MODULE_NOT_FOUND",f}var l=n[o]={exports:{}};t[o][0].call(l.exports,function(e){var n=t[o][1][e];return s(n?n:e)},l,l.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){

},{}],2:[function(require,module,exports){
(function (global){
var topLevel = typeof global !== 'undefined' ? global :
    typeof window !== 'undefined' ? window : {}
var minDoc = require('min-document');

if (typeof document !== 'undefined') {
    module.exports = document;
} else {
    var doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'];

    if (!doccy) {
        doccy = topLevel['__GLOBAL_DOCUMENT_CACHE@4'] = minDoc;
    }

    module.exports = doccy;
}

}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{"min-document":1}],3:[function(require,module,exports){
"use strict";

module.exports = function isObject(x) {
	return typeof x === "object" && x !== null;
};

},{}],4:[function(require,module,exports){
var nativeIsArray = Array.isArray
var toString = Object.prototype.toString

module.exports = nativeIsArray || isArray

function isArray(obj) {
    return toString.call(obj) === "[object Array]"
}

},{}],5:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook.js")

module.exports = applyProperties

function applyProperties(node, props, previous) {
    for (var propName in props) {
        var propValue = props[propName]

        if (propValue === undefined) {
            removeProperty(node, propName, propValue, previous);
        } else if (isHook(propValue)) {
            removeProperty(node, propName, propValue, previous)
            if (propValue.hook) {
                propValue.hook(node,
                    propName,
                    previous ? previous[propName] : undefined)
            }
        } else {
            if (isObject(propValue)) {
                patchObject(node, props, previous, propName, propValue);
            } else {
                node[propName] = propValue
            }
        }
    }
}

function removeProperty(node, propName, propValue, previous) {
    if (previous) {
        var previousValue = previous[propName]

        if (!isHook(previousValue)) {
            if (propName === "attributes") {
                for (var attrName in previousValue) {
                    node.removeAttribute(attrName)
                }
            } else if (propName === "style") {
                for (var i in previousValue) {
                    node.style[i] = ""
                }
            } else if (typeof previousValue === "string") {
                node[propName] = ""
            } else {
                node[propName] = null
            }
        } else if (previousValue.unhook) {
            previousValue.unhook(node, propName, propValue)
        }
    }
}

function patchObject(node, props, previous, propName, propValue) {
    var previousValue = previous ? previous[propName] : undefined

    // Set attributes
    if (propName === "attributes") {
        for (var attrName in propValue) {
            var attrValue = propValue[attrName]

            if (attrValue === undefined) {
                node.removeAttribute(attrName)
            } else {
                node.setAttribute(attrName, attrValue)
            }
        }

        return
    }

    if(previousValue && isObject(previousValue) &&
        getPrototype(previousValue) !== getPrototype(propValue)) {
        node[propName] = propValue
        return
    }

    if (!isObject(node[propName])) {
        node[propName] = {}
    }

    var replacer = propName === "style" ? "" : undefined

    for (var k in propValue) {
        var value = propValue[k]
        node[propName][k] = (value === undefined) ? replacer : value
    }
}

function getPrototype(value) {
    if (Object.getPrototypeOf) {
        return Object.getPrototypeOf(value)
    } else if (value.__proto__) {
        return value.__proto__
    } else if (value.constructor) {
        return value.constructor.prototype
    }
}

},{"../vnode/is-vhook.js":13,"is-object":3}],6:[function(require,module,exports){
var document = require("global/document")

var applyProperties = require("./apply-properties")

var isVNode = require("../vnode/is-vnode.js")
var isVText = require("../vnode/is-vtext.js")
var isWidget = require("../vnode/is-widget.js")
var handleThunk = require("../vnode/handle-thunk.js")

module.exports = createElement

function createElement(vnode, opts) {
    var doc = opts ? opts.document || document : document
    var warn = opts ? opts.warn : null

    vnode = handleThunk(vnode).a

    if (isWidget(vnode)) {
        return vnode.init()
    } else if (isVText(vnode)) {
        return doc.createTextNode(vnode.text)
    } else if (!isVNode(vnode)) {
        if (warn) {
            warn("Item is not a valid virtual dom node", vnode)
        }
        return null
    }

    var node = (vnode.namespace === null) ?
        doc.createElement(vnode.tagName) :
        doc.createElementNS(vnode.namespace, vnode.tagName)

    var props = vnode.properties
    applyProperties(node, props)

    var children = vnode.children

    for (var i = 0; i < children.length; i++) {
        var childNode = createElement(children[i], opts)
        if (childNode) {
            node.appendChild(childNode)
        }
    }

    return node
}

},{"../vnode/handle-thunk.js":11,"../vnode/is-vnode.js":14,"../vnode/is-vtext.js":15,"../vnode/is-widget.js":16,"./apply-properties":5,"global/document":2}],7:[function(require,module,exports){
// Maps a virtual DOM tree onto a real DOM tree in an efficient manner.
// We don't want to read all of the DOM nodes in the tree so we use
// the in-order tree indexing to eliminate recursion down certain branches.
// We only recurse into a DOM node if we know that it contains a child of
// interest.

var noChild = {}

module.exports = domIndex

function domIndex(rootNode, tree, indices, nodes) {
    if (!indices || indices.length === 0) {
        return {}
    } else {
        indices.sort(ascending)
        return recurse(rootNode, tree, indices, nodes, 0)
    }
}

function recurse(rootNode, tree, indices, nodes, rootIndex) {
    nodes = nodes || {}


    if (rootNode) {
        if (indexInRange(indices, rootIndex, rootIndex)) {
            nodes[rootIndex] = rootNode
        }

        var vChildren = tree.children

        if (vChildren) {

            var childNodes = rootNode.childNodes

            for (var i = 0; i < tree.children.length; i++) {
                rootIndex += 1

                var vChild = vChildren[i] || noChild
                var nextIndex = rootIndex + (vChild.count || 0)

                // skip recursion down the tree if there are no nodes down here
                if (indexInRange(indices, rootIndex, nextIndex)) {
                    recurse(childNodes[i], vChild, indices, nodes, rootIndex)
                }

                rootIndex = nextIndex
            }
        }
    }

    return nodes
}

// Binary search for an index in the interval [left, right]
function indexInRange(indices, left, right) {
    if (indices.length === 0) {
        return false
    }

    var minIndex = 0
    var maxIndex = indices.length - 1
    var currentIndex
    var currentItem

    while (minIndex <= maxIndex) {
        currentIndex = ((maxIndex + minIndex) / 2) >> 0
        currentItem = indices[currentIndex]

        if (minIndex === maxIndex) {
            return currentItem >= left && currentItem <= right
        } else if (currentItem < left) {
            minIndex = currentIndex + 1
        } else  if (currentItem > right) {
            maxIndex = currentIndex - 1
        } else {
            return true
        }
    }

    return false;
}

function ascending(a, b) {
    return a > b ? 1 : -1
}

},{}],8:[function(require,module,exports){
var applyProperties = require("./apply-properties")

var isWidget = require("../vnode/is-widget.js")
var VPatch = require("../vnode/vpatch.js")

var render = require("./create-element")
var updateWidget = require("./update-widget")

module.exports = applyPatch

function applyPatch(vpatch, domNode, renderOptions) {
    var type = vpatch.type
    var vNode = vpatch.vNode
    var patch = vpatch.patch

    switch (type) {
        case VPatch.REMOVE:
            return removeNode(domNode, vNode)
        case VPatch.INSERT:
            return insertNode(domNode, patch, renderOptions)
        case VPatch.VTEXT:
            return stringPatch(domNode, vNode, patch, renderOptions)
        case VPatch.WIDGET:
            return widgetPatch(domNode, vNode, patch, renderOptions)
        case VPatch.VNODE:
            return vNodePatch(domNode, vNode, patch, renderOptions)
        case VPatch.ORDER:
            reorderChildren(domNode, patch)
            return domNode
        case VPatch.PROPS:
            applyProperties(domNode, patch, vNode.properties)
            return domNode
        case VPatch.THUNK:
            return replaceRoot(domNode,
                renderOptions.patch(domNode, patch, renderOptions))
        default:
            return domNode
    }
}

function removeNode(domNode, vNode) {
    var parentNode = domNode.parentNode

    if (parentNode) {
        parentNode.removeChild(domNode)
    }

    destroyWidget(domNode, vNode);

    return null
}

function insertNode(parentNode, vNode, renderOptions) {
    var newNode = render(vNode, renderOptions)

    if (parentNode) {
        parentNode.appendChild(newNode)
    }

    return parentNode
}

function stringPatch(domNode, leftVNode, vText, renderOptions) {
    var newNode

    if (domNode.nodeType === 3) {
        domNode.replaceData(0, domNode.length, vText.text)
        newNode = domNode
    } else {
        var parentNode = domNode.parentNode
        newNode = render(vText, renderOptions)

        if (parentNode && newNode !== domNode) {
            parentNode.replaceChild(newNode, domNode)
        }
    }

    return newNode
}

function widgetPatch(domNode, leftVNode, widget, renderOptions) {
    var updating = updateWidget(leftVNode, widget)
    var newNode

    if (updating) {
        newNode = widget.update(leftVNode, domNode) || domNode
    } else {
        newNode = render(widget, renderOptions)
    }

    var parentNode = domNode.parentNode

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    if (!updating) {
        destroyWidget(domNode, leftVNode)
    }

    return newNode
}

function vNodePatch(domNode, leftVNode, vNode, renderOptions) {
    var parentNode = domNode.parentNode
    var newNode = render(vNode, renderOptions)

    if (parentNode && newNode !== domNode) {
        parentNode.replaceChild(newNode, domNode)
    }

    return newNode
}

function destroyWidget(domNode, w) {
    if (typeof w.destroy === "function" && isWidget(w)) {
        w.destroy(domNode)
    }
}

function reorderChildren(domNode, moves) {
    var childNodes = domNode.childNodes
    var keyMap = {}
    var node
    var remove
    var insert

    for (var i = 0; i < moves.removes.length; i++) {
        remove = moves.removes[i]
        node = childNodes[remove.from]
        if (remove.key) {
            keyMap[remove.key] = node
        }
        domNode.removeChild(node)
    }

    var length = childNodes.length
    for (var j = 0; j < moves.inserts.length; j++) {
        insert = moves.inserts[j]
        node = keyMap[insert.key]
        // this is the weirdest bug i've ever seen in webkit
        domNode.insertBefore(node, insert.to >= length++ ? null : childNodes[insert.to])
    }
}

function replaceRoot(oldRoot, newRoot) {
    if (oldRoot && newRoot && oldRoot !== newRoot && oldRoot.parentNode) {
        oldRoot.parentNode.replaceChild(newRoot, oldRoot)
    }

    return newRoot;
}

},{"../vnode/is-widget.js":16,"../vnode/vpatch.js":19,"./apply-properties":5,"./create-element":6,"./update-widget":10}],9:[function(require,module,exports){
var document = require("global/document")
var isArray = require("x-is-array")

var domIndex = require("./dom-index")
var patchOp = require("./patch-op")
module.exports = patch

function patch(rootNode, patches) {
    return patchRecursive(rootNode, patches)
}

function patchRecursive(rootNode, patches, renderOptions) {
    var indices = patchIndices(patches)

    if (indices.length === 0) {
        return rootNode
    }

    var index = domIndex(rootNode, patches.a, indices)
    var ownerDocument = rootNode.ownerDocument

    if (!renderOptions) {
        renderOptions = { patch: patchRecursive }
        if (ownerDocument !== document) {
            renderOptions.document = ownerDocument
        }
    }

    for (var i = 0; i < indices.length; i++) {
        var nodeIndex = indices[i]
        rootNode = applyPatch(rootNode,
            index[nodeIndex],
            patches[nodeIndex],
            renderOptions)
    }

    return rootNode
}

function applyPatch(rootNode, domNode, patchList, renderOptions) {
    if (!domNode) {
        return rootNode
    }

    var newNode

    if (isArray(patchList)) {
        for (var i = 0; i < patchList.length; i++) {
            newNode = patchOp(patchList[i], domNode, renderOptions)

            if (domNode === rootNode) {
                rootNode = newNode
            }
        }
    } else {
        newNode = patchOp(patchList, domNode, renderOptions)

        if (domNode === rootNode) {
            rootNode = newNode
        }
    }

    return rootNode
}

function patchIndices(patches) {
    var indices = []

    for (var key in patches) {
        if (key !== "a") {
            indices.push(Number(key))
        }
    }

    return indices
}

},{"./dom-index":7,"./patch-op":8,"global/document":2,"x-is-array":4}],10:[function(require,module,exports){
var isWidget = require("../vnode/is-widget.js")

module.exports = updateWidget

function updateWidget(a, b) {
    if (isWidget(a) && isWidget(b)) {
        if ("name" in a && "name" in b) {
            return a.id === b.id
        } else {
            return a.init === b.init
        }
    }

    return false
}

},{"../vnode/is-widget.js":16}],11:[function(require,module,exports){
var isVNode = require("./is-vnode")
var isVText = require("./is-vtext")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")

module.exports = handleThunk

function handleThunk(a, b) {
    var renderedA = a
    var renderedB = b

    if (isThunk(b)) {
        renderedB = renderThunk(b, a)
    }

    if (isThunk(a)) {
        renderedA = renderThunk(a, null)
    }

    return {
        a: renderedA,
        b: renderedB
    }
}

function renderThunk(thunk, previous) {
    var renderedThunk = thunk.vnode

    if (!renderedThunk) {
        renderedThunk = thunk.vnode = thunk.render(previous)
    }

    if (!(isVNode(renderedThunk) ||
            isVText(renderedThunk) ||
            isWidget(renderedThunk))) {
        throw new Error("thunk did not return a valid node");
    }

    return renderedThunk
}

},{"./is-thunk":12,"./is-vnode":14,"./is-vtext":15,"./is-widget":16}],12:[function(require,module,exports){
module.exports = isThunk

function isThunk(t) {
    return t && t.type === "Thunk"
}

},{}],13:[function(require,module,exports){
module.exports = isHook

function isHook(hook) {
    return hook &&
      (typeof hook.hook === "function" && !hook.hasOwnProperty("hook") ||
       typeof hook.unhook === "function" && !hook.hasOwnProperty("unhook"))
}

},{}],14:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualNode

function isVirtualNode(x) {
    return x && x.type === "VirtualNode" && x.version === version
}

},{"./version":17}],15:[function(require,module,exports){
var version = require("./version")

module.exports = isVirtualText

function isVirtualText(x) {
    return x && x.type === "VirtualText" && x.version === version
}

},{"./version":17}],16:[function(require,module,exports){
module.exports = isWidget

function isWidget(w) {
    return w && w.type === "Widget"
}

},{}],17:[function(require,module,exports){
module.exports = "2"

},{}],18:[function(require,module,exports){
var version = require("./version")
var isVNode = require("./is-vnode")
var isWidget = require("./is-widget")
var isThunk = require("./is-thunk")
var isVHook = require("./is-vhook")

module.exports = VirtualNode

var noProperties = {}
var noChildren = []

function VirtualNode(tagName, properties, children, key, namespace) {
    this.tagName = tagName
    this.properties = properties || noProperties
    this.children = children || noChildren
    this.key = key != null ? String(key) : undefined
    this.namespace = (typeof namespace === "string") ? namespace : null

    var count = (children && children.length) || 0
    var descendants = 0
    var hasWidgets = false
    var hasThunks = false
    var descendantHooks = false
    var hooks

    for (var propName in properties) {
        if (properties.hasOwnProperty(propName)) {
            var property = properties[propName]
            if (isVHook(property) && property.unhook) {
                if (!hooks) {
                    hooks = {}
                }

                hooks[propName] = property
            }
        }
    }

    for (var i = 0; i < count; i++) {
        var child = children[i]
        if (isVNode(child)) {
            descendants += child.count || 0

            if (!hasWidgets && child.hasWidgets) {
                hasWidgets = true
            }

            if (!hasThunks && child.hasThunks) {
                hasThunks = true
            }

            if (!descendantHooks && (child.hooks || child.descendantHooks)) {
                descendantHooks = true
            }
        } else if (!hasWidgets && isWidget(child)) {
            if (typeof child.destroy === "function") {
                hasWidgets = true
            }
        } else if (!hasThunks && isThunk(child)) {
            hasThunks = true;
        }
    }

    this.count = count + descendants
    this.hasWidgets = hasWidgets
    this.hasThunks = hasThunks
    this.hooks = hooks
    this.descendantHooks = descendantHooks
}

VirtualNode.prototype.version = version
VirtualNode.prototype.type = "VirtualNode"

},{"./is-thunk":12,"./is-vhook":13,"./is-vnode":14,"./is-widget":16,"./version":17}],19:[function(require,module,exports){
var version = require("./version")

VirtualPatch.NONE = 0
VirtualPatch.VTEXT = 1
VirtualPatch.VNODE = 2
VirtualPatch.WIDGET = 3
VirtualPatch.PROPS = 4
VirtualPatch.ORDER = 5
VirtualPatch.INSERT = 6
VirtualPatch.REMOVE = 7
VirtualPatch.THUNK = 8

module.exports = VirtualPatch

function VirtualPatch(type, vNode, patch) {
    this.type = Number(type)
    this.vNode = vNode
    this.patch = patch
}

VirtualPatch.prototype.version = version
VirtualPatch.prototype.type = "VirtualPatch"

},{"./version":17}],20:[function(require,module,exports){
var version = require("./version")

module.exports = VirtualText

function VirtualText(text) {
    this.text = String(text)
}

VirtualText.prototype.version = version
VirtualText.prototype.type = "VirtualText"

},{"./version":17}],21:[function(require,module,exports){
var isObject = require("is-object")
var isHook = require("../vnode/is-vhook")

module.exports = diffProps

function diffProps(a, b) {
    var diff

    for (var aKey in a) {
        if (!(aKey in b)) {
            diff = diff || {}
            diff[aKey] = undefined
        }

        var aValue = a[aKey]
        var bValue = b[aKey]

        if (aValue === bValue) {
            continue
        } else if (isObject(aValue) && isObject(bValue)) {
            if (getPrototype(bValue) !== getPrototype(aValue)) {
                diff = diff || {}
                diff[aKey] = bValue
            } else if (isHook(bValue)) {
                 diff = diff || {}
                 diff[aKey] = bValue
            } else {
                var objectDiff = diffProps(aValue, bValue)
                if (objectDiff) {
                    diff = diff || {}
                    diff[aKey] = objectDiff
                }
            }
        } else {
            diff = diff || {}
            diff[aKey] = bValue
        }
    }

    for (var bKey in b) {
        if (!(bKey in a)) {
            diff = diff || {}
            diff[bKey] = b[bKey]
        }
    }

    return diff
}

function getPrototype(value) {
  if (Object.getPrototypeOf) {
    return Object.getPrototypeOf(value)
  } else if (value.__proto__) {
    return value.__proto__
  } else if (value.constructor) {
    return value.constructor.prototype
  }
}

},{"../vnode/is-vhook":13,"is-object":3}],22:[function(require,module,exports){
var isArray = require("x-is-array")

var VPatch = require("../vnode/vpatch")
var isVNode = require("../vnode/is-vnode")
var isVText = require("../vnode/is-vtext")
var isWidget = require("../vnode/is-widget")
var isThunk = require("../vnode/is-thunk")
var handleThunk = require("../vnode/handle-thunk")

var diffProps = require("./diff-props")

module.exports = diff

function diff(a, b) {
    var patch = { a: a }
    walk(a, b, patch, 0)
    return patch
}

function walk(a, b, patch, index) {
    if (a === b) {
        return
    }

    var apply = patch[index]
    var applyClear = false

    if (isThunk(a) || isThunk(b)) {
        thunks(a, b, patch, index)
    } else if (b == null) {

        // If a is a widget we will add a remove patch for it
        // Otherwise any child widgets/hooks must be destroyed.
        // This prevents adding two remove patches for a widget.
        if (!isWidget(a)) {
            clearState(a, patch, index)
            apply = patch[index]
        }

        apply = appendPatch(apply, new VPatch(VPatch.REMOVE, a, b))
    } else if (isVNode(b)) {
        if (isVNode(a)) {
            if (a.tagName === b.tagName &&
                a.namespace === b.namespace &&
                a.key === b.key) {
                var propsPatch = diffProps(a.properties, b.properties)
                if (propsPatch) {
                    apply = appendPatch(apply,
                        new VPatch(VPatch.PROPS, a, propsPatch))
                }
                apply = diffChildren(a, b, patch, apply, index)
            } else {
                apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
                applyClear = true
            }
        } else {
            apply = appendPatch(apply, new VPatch(VPatch.VNODE, a, b))
            applyClear = true
        }
    } else if (isVText(b)) {
        if (!isVText(a)) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
            applyClear = true
        } else if (a.text !== b.text) {
            apply = appendPatch(apply, new VPatch(VPatch.VTEXT, a, b))
        }
    } else if (isWidget(b)) {
        if (!isWidget(a)) {
            applyClear = true
        }

        apply = appendPatch(apply, new VPatch(VPatch.WIDGET, a, b))
    }

    if (apply) {
        patch[index] = apply
    }

    if (applyClear) {
        clearState(a, patch, index)
    }
}

function diffChildren(a, b, patch, apply, index) {
    var aChildren = a.children
    var orderedSet = reorder(aChildren, b.children)
    var bChildren = orderedSet.children

    var aLen = aChildren.length
    var bLen = bChildren.length
    var len = aLen > bLen ? aLen : bLen

    for (var i = 0; i < len; i++) {
        var leftNode = aChildren[i]
        var rightNode = bChildren[i]
        index += 1

        if (!leftNode) {
            if (rightNode) {
                // Excess nodes in b need to be added
                apply = appendPatch(apply,
                    new VPatch(VPatch.INSERT, null, rightNode))
            }
        } else {
            walk(leftNode, rightNode, patch, index)
        }

        if (isVNode(leftNode) && leftNode.count) {
            index += leftNode.count
        }
    }

    if (orderedSet.moves) {
        // Reorder nodes last
        apply = appendPatch(apply, new VPatch(
            VPatch.ORDER,
            a,
            orderedSet.moves
        ))
    }

    return apply
}

function clearState(vNode, patch, index) {
    // TODO: Make this a single walk, not two
    unhook(vNode, patch, index)
    destroyWidgets(vNode, patch, index)
}

// Patch records for all destroyed widgets must be added because we need
// a DOM node reference for the destroy function
function destroyWidgets(vNode, patch, index) {
    if (isWidget(vNode)) {
        if (typeof vNode.destroy === "function") {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(VPatch.REMOVE, vNode, null)
            )
        }
    } else if (isVNode(vNode) && (vNode.hasWidgets || vNode.hasThunks)) {
        var children = vNode.children
        var len = children.length
        for (var i = 0; i < len; i++) {
            var child = children[i]
            index += 1

            destroyWidgets(child, patch, index)

            if (isVNode(child) && child.count) {
                index += child.count
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

// Create a sub-patch for thunks
function thunks(a, b, patch, index) {
    var nodes = handleThunk(a, b)
    var thunkPatch = diff(nodes.a, nodes.b)
    if (hasPatches(thunkPatch)) {
        patch[index] = new VPatch(VPatch.THUNK, null, thunkPatch)
    }
}

function hasPatches(patch) {
    for (var index in patch) {
        if (index !== "a") {
            return true
        }
    }

    return false
}

// Execute hooks when two nodes are identical
function unhook(vNode, patch, index) {
    if (isVNode(vNode)) {
        if (vNode.hooks) {
            patch[index] = appendPatch(
                patch[index],
                new VPatch(
                    VPatch.PROPS,
                    vNode,
                    undefinedKeys(vNode.hooks)
                )
            )
        }

        if (vNode.descendantHooks || vNode.hasThunks) {
            var children = vNode.children
            var len = children.length
            for (var i = 0; i < len; i++) {
                var child = children[i]
                index += 1

                unhook(child, patch, index)

                if (isVNode(child) && child.count) {
                    index += child.count
                }
            }
        }
    } else if (isThunk(vNode)) {
        thunks(vNode, null, patch, index)
    }
}

function undefinedKeys(obj) {
    var result = {}

    for (var key in obj) {
        result[key] = undefined
    }

    return result
}

// List diff, naive left to right reordering
function reorder(aChildren, bChildren) {
    // O(M) time, O(M) memory
    var bChildIndex = keyIndex(bChildren)
    var bKeys = bChildIndex.keys
    var bFree = bChildIndex.free

    if (bFree.length === bChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(N) time, O(N) memory
    var aChildIndex = keyIndex(aChildren)
    var aKeys = aChildIndex.keys
    var aFree = aChildIndex.free

    if (aFree.length === aChildren.length) {
        return {
            children: bChildren,
            moves: null
        }
    }

    // O(MAX(N, M)) memory
    var newChildren = []

    var freeIndex = 0
    var freeCount = bFree.length
    var deletedItems = 0

    // Iterate through a and match a node in b
    // O(N) time,
    for (var i = 0 ; i < aChildren.length; i++) {
        var aItem = aChildren[i]
        var itemIndex

        if (aItem.key) {
            if (bKeys.hasOwnProperty(aItem.key)) {
                // Match up the old keys
                itemIndex = bKeys[aItem.key]
                newChildren.push(bChildren[itemIndex])

            } else {
                // Remove old keyed items
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        } else {
            // Match the item in a with the next free item in b
            if (freeIndex < freeCount) {
                itemIndex = bFree[freeIndex++]
                newChildren.push(bChildren[itemIndex])
            } else {
                // There are no free items in b to match with
                // the free items in a, so the extra free nodes
                // are deleted.
                itemIndex = i - deletedItems++
                newChildren.push(null)
            }
        }
    }

    var lastFreeIndex = freeIndex >= bFree.length ?
        bChildren.length :
        bFree[freeIndex]

    // Iterate through b and append any new keys
    // O(M) time
    for (var j = 0; j < bChildren.length; j++) {
        var newItem = bChildren[j]

        if (newItem.key) {
            if (!aKeys.hasOwnProperty(newItem.key)) {
                // Add any new keyed items
                // We are adding new items to the end and then sorting them
                // in place. In future we should insert new items in place.
                newChildren.push(newItem)
            }
        } else if (j >= lastFreeIndex) {
            // Add any leftover non-keyed items
            newChildren.push(newItem)
        }
    }

    var simulate = newChildren.slice()
    var simulateIndex = 0
    var removes = []
    var inserts = []
    var simulateItem

    for (var k = 0; k < bChildren.length;) {
        var wantedItem = bChildren[k]
        simulateItem = simulate[simulateIndex]

        // remove items
        while (simulateItem === null && simulate.length) {
            removes.push(remove(simulate, simulateIndex, null))
            simulateItem = simulate[simulateIndex]
        }

        if (!simulateItem || simulateItem.key !== wantedItem.key) {
            // if we need a key in this position...
            if (wantedItem.key) {
                if (simulateItem && simulateItem.key) {
                    // if an insert doesn't put this key in place, it needs to move
                    if (bKeys[simulateItem.key] !== k + 1) {
                        removes.push(remove(simulate, simulateIndex, simulateItem.key))
                        simulateItem = simulate[simulateIndex]
                        // if the remove didn't put the wanted item in place, we need to insert it
                        if (!simulateItem || simulateItem.key !== wantedItem.key) {
                            inserts.push({key: wantedItem.key, to: k})
                        }
                        // items are matching, so skip ahead
                        else {
                            simulateIndex++
                        }
                    }
                    else {
                        inserts.push({key: wantedItem.key, to: k})
                    }
                }
                else {
                    inserts.push({key: wantedItem.key, to: k})
                }
                k++
            }
            // a key in simulate has no matching wanted key, remove it
            else if (simulateItem && simulateItem.key) {
                removes.push(remove(simulate, simulateIndex, simulateItem.key))
            }
        }
        else {
            simulateIndex++
            k++
        }
    }

    // remove all the remaining nodes from simulate
    while(simulateIndex < simulate.length) {
        simulateItem = simulate[simulateIndex]
        removes.push(remove(simulate, simulateIndex, simulateItem && simulateItem.key))
    }

    // If the only moves we have are deletes then we can just
    // let the delete patch remove these items.
    if (removes.length === deletedItems && !inserts.length) {
        return {
            children: newChildren,
            moves: null
        }
    }

    return {
        children: newChildren,
        moves: {
            removes: removes,
            inserts: inserts
        }
    }
}

function remove(arr, index, key) {
    arr.splice(index, 1)

    return {
        from: index,
        key: key
    }
}

function keyIndex(children) {
    var keys = {}
    var free = []
    var length = children.length

    for (var i = 0; i < length; i++) {
        var child = children[i]

        if (child.key) {
            keys[child.key] = i
        } else {
            free.push(i)
        }
    }

    return {
        keys: keys,     // A hash of key name to index
        free: free,     // An array of unkeyed item indices
    }
}

function appendPatch(apply, patch) {
    if (apply) {
        if (isArray(apply)) {
            apply.push(patch)
        } else {
            apply = [apply, patch]
        }

        return apply
    } else {
        return patch
    }
}

},{"../vnode/handle-thunk":11,"../vnode/is-thunk":12,"../vnode/is-vnode":14,"../vnode/is-vtext":15,"../vnode/is-widget":16,"../vnode/vpatch":19,"./diff-props":21,"x-is-array":4}],23:[function(require,module,exports){
var VNode = require('virtual-dom/vnode/vnode');
var VText = require('virtual-dom/vnode/vtext');
var diff = require('virtual-dom/vtree/diff');
var patch = require('virtual-dom/vdom/patch');
var createElement = require('virtual-dom/vdom/create-element');
var isHook = require("virtual-dom/vnode/is-vhook");


Elm.Native.VirtualDom = {};
Elm.Native.VirtualDom.make = function(elm)
{
	elm.Native = elm.Native || {};
	elm.Native.VirtualDom = elm.Native.VirtualDom || {};
	if (elm.Native.VirtualDom.values)
	{
		return elm.Native.VirtualDom.values;
	}

	var Element = Elm.Native.Graphics.Element.make(elm);
	var Json = Elm.Native.Json.make(elm);
	var List = Elm.Native.List.make(elm);
	var Signal = Elm.Native.Signal.make(elm);
	var Utils = Elm.Native.Utils.make(elm);

	var ATTRIBUTE_KEY = 'UniqueNameThatOthersAreVeryUnlikelyToUse';



	// VIRTUAL DOM NODES


	function text(string)
	{
		return new VText(string);
	}

	function node(name)
	{
		return F2(function(propertyList, contents) {
			return makeNode(name, propertyList, contents);
		});
	}


	// BUILD VIRTUAL DOME NODES


	function makeNode(name, propertyList, contents)
	{
		var props = listToProperties(propertyList);

		var key, namespace;
		// support keys
		if (props.key !== undefined)
		{
			key = props.key;
			props.key = undefined;
		}

		// support namespace
		if (props.namespace !== undefined)
		{
			namespace = props.namespace;
			props.namespace = undefined;
		}

		// ensure that setting text of an input does not move the cursor
		var useSoftSet =
			(name === 'input' || name === 'textarea')
			&& props.value !== undefined
			&& !isHook(props.value);

		if (useSoftSet)
		{
			props.value = SoftSetHook(props.value);
		}

		return new VNode(name, props, List.toArray(contents), key, namespace);
	}

	function listToProperties(list)
	{
		var object = {};
		while (list.ctor !== '[]')
		{
			var entry = list._0;
			if (entry.key === ATTRIBUTE_KEY)
			{
				object.attributes = object.attributes || {};
				object.attributes[entry.value.attrKey] = entry.value.attrValue;
			}
			else
			{
				object[entry.key] = entry.value;
			}
			list = list._1;
		}
		return object;
	}



	// PROPERTIES AND ATTRIBUTES


	function property(key, value)
	{
		return {
			key: key,
			value: value
		};
	}

	function attribute(key, value)
	{
		return {
			key: ATTRIBUTE_KEY,
			value: {
				attrKey: key,
				attrValue: value
			}
		};
	}



	// NAMESPACED ATTRIBUTES


	function attributeNS(namespace, key, value)
	{
		return {
			key: key,
			value: new AttributeHook(namespace, key, value)
		};
	}

	function AttributeHook(namespace, key, value)
	{
		if (!(this instanceof AttributeHook))
		{
			return new AttributeHook(namespace, key, value);
		}

		this.namespace = namespace;
		this.key = key;
		this.value = value;
	}

	AttributeHook.prototype.hook = function (node, prop, prev)
	{
		if (prev
			&& prev.type === 'AttributeHook'
			&& prev.value === this.value
			&& prev.namespace === this.namespace)
		{
			return;
		}

		node.setAttributeNS(this.namespace, prop, this.value);
	};

	AttributeHook.prototype.unhook = function (node, prop, next)
	{
		if (next
			&& next.type === 'AttributeHook'
			&& next.namespace === this.namespace)
		{
			return;
		}

		node.removeAttributeNS(this.namespace, this.key);
	};

	AttributeHook.prototype.type = 'AttributeHook';



	// EVENTS


	function on(name, options, decoder, createMessage)
	{
		function eventHandler(event)
		{
			var value = A2(Json.runDecoderValue, decoder, event);
			if (value.ctor === 'Ok')
			{
				if (options.stopPropagation)
				{
					event.stopPropagation();
				}
				if (options.preventDefault)
				{
					event.preventDefault();
				}
				Signal.sendMessage(createMessage(value._0));
			}
		}
		return property('on' + name, eventHandler);
	}

	function SoftSetHook(value)
	{
		if (!(this instanceof SoftSetHook))
		{
			return new SoftSetHook(value);
		}

		this.value = value;
	}

	SoftSetHook.prototype.hook = function (node, propertyName)
	{
		if (node[propertyName] !== this.value)
		{
			node[propertyName] = this.value;
		}
	};



	// INTEGRATION WITH ELEMENTS


	function ElementWidget(element)
	{
		this.element = element;
	}

	ElementWidget.prototype.type = "Widget";

	ElementWidget.prototype.init = function init()
	{
		return Element.render(this.element);
	};

	ElementWidget.prototype.update = function update(previous, node)
	{
		return Element.update(node, previous.element, this.element);
	};

	function fromElement(element)
	{
		return new ElementWidget(element);
	}

	function toElement(width, height, html)
	{
		return A3(Element.newElement, width, height, {
			ctor: 'Custom',
			type: 'evancz/elm-html',
			render: render,
			update: update,
			model: html
		});
	}



	// RENDER AND UPDATE


	function render(model)
	{
		var element = Element.createNode('div');
		element.appendChild(createElement(model));
		return element;
	}

	function update(node, oldModel, newModel)
	{
		updateAndReplace(node.firstChild, oldModel, newModel);
		return node;
	}

	function updateAndReplace(node, oldModel, newModel)
	{
		var patches = diff(oldModel, newModel);
		var newNode = patch(node, patches);
		return newNode;
	}



	// LAZINESS


	function lazyRef(fn, a)
	{
		function thunk()
		{
			return fn(a);
		}
		return new Thunk(fn, [a], thunk);
	}

	function lazyRef2(fn, a, b)
	{
		function thunk()
		{
			return A2(fn, a, b);
		}
		return new Thunk(fn, [a,b], thunk);
	}

	function lazyRef3(fn, a, b, c)
	{
		function thunk()
		{
			return A3(fn, a, b, c);
		}
		return new Thunk(fn, [a,b,c], thunk);
	}

	function Thunk(fn, args, thunk)
	{
		/* public (used by VirtualDom.js) */
		this.vnode = null;
		this.key = undefined;

		/* private */
		this.fn = fn;
		this.args = args;
		this.thunk = thunk;
	}

	Thunk.prototype.type = "Thunk";
	Thunk.prototype.render = renderThunk;

	function shouldUpdate(current, previous)
	{
		if (current.fn !== previous.fn)
		{
			return true;
		}

		// if it's the same function, we know the number of args must match
		var cargs = current.args;
		var pargs = previous.args;

		for (var i = cargs.length; i--; )
		{
			if (cargs[i] !== pargs[i])
			{
				return true;
			}
		}

		return false;
	}

	function renderThunk(previous)
	{
		if (previous == null || shouldUpdate(this, previous))
		{
			return this.thunk();
		}
		else
		{
			return previous.vnode;
		}
	}


	return elm.Native.VirtualDom.values = Elm.Native.VirtualDom.values = {
		node: node,
		text: text,
		on: F4(on),

		property: F2(property),
		attribute: F2(attribute),
		attributeNS: F3(attributeNS),

		lazy: F2(lazyRef),
		lazy2: F3(lazyRef2),
		lazy3: F4(lazyRef3),

		toElement: F3(toElement),
		fromElement: fromElement,

		render: createElement,
		updateAndReplace: updateAndReplace
	};
};

},{"virtual-dom/vdom/create-element":6,"virtual-dom/vdom/patch":9,"virtual-dom/vnode/is-vhook":13,"virtual-dom/vnode/vnode":18,"virtual-dom/vnode/vtext":20,"virtual-dom/vtree/diff":22}]},{},[23]);

Elm.VirtualDom = Elm.VirtualDom || {};
Elm.VirtualDom.make = function (_elm) {
   "use strict";
   _elm.VirtualDom = _elm.VirtualDom || {};
   if (_elm.VirtualDom.values) return _elm.VirtualDom.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Native$VirtualDom = Elm.Native.VirtualDom.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var lazy3 = $Native$VirtualDom.lazy3;
   var lazy2 = $Native$VirtualDom.lazy2;
   var lazy = $Native$VirtualDom.lazy;
   var defaultOptions = {stopPropagation: false
                        ,preventDefault: false};
   var Options = F2(function (a,b) {
      return {stopPropagation: a,preventDefault: b};
   });
   var onWithOptions = $Native$VirtualDom.on;
   var on = F3(function (eventName,decoder,toMessage) {
      return A4($Native$VirtualDom.on,
      eventName,
      defaultOptions,
      decoder,
      toMessage);
   });
   var attributeNS = $Native$VirtualDom.attributeNS;
   var attribute = $Native$VirtualDom.attribute;
   var property = $Native$VirtualDom.property;
   var Property = {ctor: "Property"};
   var fromElement = $Native$VirtualDom.fromElement;
   var toElement = $Native$VirtualDom.toElement;
   var text = $Native$VirtualDom.text;
   var node = $Native$VirtualDom.node;
   var Node = {ctor: "Node"};
   return _elm.VirtualDom.values = {_op: _op
                                   ,text: text
                                   ,node: node
                                   ,toElement: toElement
                                   ,fromElement: fromElement
                                   ,property: property
                                   ,attribute: attribute
                                   ,attributeNS: attributeNS
                                   ,on: on
                                   ,onWithOptions: onWithOptions
                                   ,defaultOptions: defaultOptions
                                   ,lazy: lazy
                                   ,lazy2: lazy2
                                   ,lazy3: lazy3
                                   ,Options: Options};
};
Elm.Html = Elm.Html || {};
Elm.Html.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   if (_elm.Html.values) return _elm.Html.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Graphics$Element = Elm.Graphics.Element.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var fromElement = $VirtualDom.fromElement;
   var toElement = $VirtualDom.toElement;
   var text = $VirtualDom.text;
   var node = $VirtualDom.node;
   var body = node("body");
   var section = node("section");
   var nav = node("nav");
   var article = node("article");
   var aside = node("aside");
   var h1 = node("h1");
   var h2 = node("h2");
   var h3 = node("h3");
   var h4 = node("h4");
   var h5 = node("h5");
   var h6 = node("h6");
   var header = node("header");
   var footer = node("footer");
   var address = node("address");
   var main$ = node("main");
   var p = node("p");
   var hr = node("hr");
   var pre = node("pre");
   var blockquote = node("blockquote");
   var ol = node("ol");
   var ul = node("ul");
   var li = node("li");
   var dl = node("dl");
   var dt = node("dt");
   var dd = node("dd");
   var figure = node("figure");
   var figcaption = node("figcaption");
   var div = node("div");
   var a = node("a");
   var em = node("em");
   var strong = node("strong");
   var small = node("small");
   var s = node("s");
   var cite = node("cite");
   var q = node("q");
   var dfn = node("dfn");
   var abbr = node("abbr");
   var time = node("time");
   var code = node("code");
   var $var = node("var");
   var samp = node("samp");
   var kbd = node("kbd");
   var sub = node("sub");
   var sup = node("sup");
   var i = node("i");
   var b = node("b");
   var u = node("u");
   var mark = node("mark");
   var ruby = node("ruby");
   var rt = node("rt");
   var rp = node("rp");
   var bdi = node("bdi");
   var bdo = node("bdo");
   var span = node("span");
   var br = node("br");
   var wbr = node("wbr");
   var ins = node("ins");
   var del = node("del");
   var img = node("img");
   var iframe = node("iframe");
   var embed = node("embed");
   var object = node("object");
   var param = node("param");
   var video = node("video");
   var audio = node("audio");
   var source = node("source");
   var track = node("track");
   var canvas = node("canvas");
   var svg = node("svg");
   var math = node("math");
   var table = node("table");
   var caption = node("caption");
   var colgroup = node("colgroup");
   var col = node("col");
   var tbody = node("tbody");
   var thead = node("thead");
   var tfoot = node("tfoot");
   var tr = node("tr");
   var td = node("td");
   var th = node("th");
   var form = node("form");
   var fieldset = node("fieldset");
   var legend = node("legend");
   var label = node("label");
   var input = node("input");
   var button = node("button");
   var select = node("select");
   var datalist = node("datalist");
   var optgroup = node("optgroup");
   var option = node("option");
   var textarea = node("textarea");
   var keygen = node("keygen");
   var output = node("output");
   var progress = node("progress");
   var meter = node("meter");
   var details = node("details");
   var summary = node("summary");
   var menuitem = node("menuitem");
   var menu = node("menu");
   return _elm.Html.values = {_op: _op
                             ,node: node
                             ,text: text
                             ,toElement: toElement
                             ,fromElement: fromElement
                             ,body: body
                             ,section: section
                             ,nav: nav
                             ,article: article
                             ,aside: aside
                             ,h1: h1
                             ,h2: h2
                             ,h3: h3
                             ,h4: h4
                             ,h5: h5
                             ,h6: h6
                             ,header: header
                             ,footer: footer
                             ,address: address
                             ,main$: main$
                             ,p: p
                             ,hr: hr
                             ,pre: pre
                             ,blockquote: blockquote
                             ,ol: ol
                             ,ul: ul
                             ,li: li
                             ,dl: dl
                             ,dt: dt
                             ,dd: dd
                             ,figure: figure
                             ,figcaption: figcaption
                             ,div: div
                             ,a: a
                             ,em: em
                             ,strong: strong
                             ,small: small
                             ,s: s
                             ,cite: cite
                             ,q: q
                             ,dfn: dfn
                             ,abbr: abbr
                             ,time: time
                             ,code: code
                             ,$var: $var
                             ,samp: samp
                             ,kbd: kbd
                             ,sub: sub
                             ,sup: sup
                             ,i: i
                             ,b: b
                             ,u: u
                             ,mark: mark
                             ,ruby: ruby
                             ,rt: rt
                             ,rp: rp
                             ,bdi: bdi
                             ,bdo: bdo
                             ,span: span
                             ,br: br
                             ,wbr: wbr
                             ,ins: ins
                             ,del: del
                             ,img: img
                             ,iframe: iframe
                             ,embed: embed
                             ,object: object
                             ,param: param
                             ,video: video
                             ,audio: audio
                             ,source: source
                             ,track: track
                             ,canvas: canvas
                             ,svg: svg
                             ,math: math
                             ,table: table
                             ,caption: caption
                             ,colgroup: colgroup
                             ,col: col
                             ,tbody: tbody
                             ,thead: thead
                             ,tfoot: tfoot
                             ,tr: tr
                             ,td: td
                             ,th: th
                             ,form: form
                             ,fieldset: fieldset
                             ,legend: legend
                             ,label: label
                             ,input: input
                             ,button: button
                             ,select: select
                             ,datalist: datalist
                             ,optgroup: optgroup
                             ,option: option
                             ,textarea: textarea
                             ,keygen: keygen
                             ,output: output
                             ,progress: progress
                             ,meter: meter
                             ,details: details
                             ,summary: summary
                             ,menuitem: menuitem
                             ,menu: menu};
};
Elm.StartApp = Elm.StartApp || {};
Elm.StartApp.make = function (_elm) {
   "use strict";
   _elm.StartApp = _elm.StartApp || {};
   if (_elm.StartApp.values) return _elm.StartApp.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $Html = Elm.Html.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var start = function (config) {
      var updateStep = F2(function (action,_p0) {
         var _p1 = _p0;
         var _p2 = A2(config.update,action,_p1._0);
         var newModel = _p2._0;
         var additionalEffects = _p2._1;
         return {ctor: "_Tuple2"
                ,_0: newModel
                ,_1: $Effects.batch(_U.list([_p1._1,additionalEffects]))};
      });
      var update = F2(function (actions,_p3) {
         var _p4 = _p3;
         return A3($List.foldl,
         updateStep,
         {ctor: "_Tuple2",_0: _p4._0,_1: $Effects.none},
         actions);
      });
      var messages = $Signal.mailbox(_U.list([]));
      var singleton = function (action) {
         return _U.list([action]);
      };
      var address = A2($Signal.forwardTo,messages.address,singleton);
      var inputs = $Signal.mergeMany(A2($List._op["::"],
      messages.signal,
      A2($List.map,$Signal.map(singleton),config.inputs)));
      var effectsAndModel = A3($Signal.foldp,
      update,
      config.init,
      inputs);
      var model = A2($Signal.map,$Basics.fst,effectsAndModel);
      return {html: A2($Signal.map,config.view(address),model)
             ,model: model
             ,tasks: A2($Signal.map,
             function (_p5) {
                return A2($Effects.toTask,messages.address,$Basics.snd(_p5));
             },
             effectsAndModel)};
   };
   var App = F3(function (a,b,c) {
      return {html: a,model: b,tasks: c};
   });
   var Config = F4(function (a,b,c,d) {
      return {init: a,update: b,view: c,inputs: d};
   });
   return _elm.StartApp.values = {_op: _op
                                 ,start: start
                                 ,Config: Config
                                 ,App: App};
};
Elm.Html = Elm.Html || {};
Elm.Html.Attributes = Elm.Html.Attributes || {};
Elm.Html.Attributes.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Attributes = _elm.Html.Attributes || {};
   if (_elm.Html.Attributes.values)
   return _elm.Html.Attributes.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var attribute = $VirtualDom.attribute;
   var contextmenu = function (value) {
      return A2(attribute,"contextmenu",value);
   };
   var property = $VirtualDom.property;
   var stringProperty = F2(function (name,string) {
      return A2(property,name,$Json$Encode.string(string));
   });
   var $class = function (name) {
      return A2(stringProperty,"className",name);
   };
   var id = function (name) {
      return A2(stringProperty,"id",name);
   };
   var title = function (name) {
      return A2(stringProperty,"title",name);
   };
   var accesskey = function ($char) {
      return A2(stringProperty,
      "accessKey",
      $String.fromChar($char));
   };
   var dir = function (value) {
      return A2(stringProperty,"dir",value);
   };
   var draggable = function (value) {
      return A2(stringProperty,"draggable",value);
   };
   var dropzone = function (value) {
      return A2(stringProperty,"dropzone",value);
   };
   var itemprop = function (value) {
      return A2(stringProperty,"itemprop",value);
   };
   var lang = function (value) {
      return A2(stringProperty,"lang",value);
   };
   var tabindex = function (n) {
      return A2(stringProperty,"tabIndex",$Basics.toString(n));
   };
   var charset = function (value) {
      return A2(stringProperty,"charset",value);
   };
   var content = function (value) {
      return A2(stringProperty,"content",value);
   };
   var httpEquiv = function (value) {
      return A2(stringProperty,"httpEquiv",value);
   };
   var language = function (value) {
      return A2(stringProperty,"language",value);
   };
   var src = function (value) {
      return A2(stringProperty,"src",value);
   };
   var height = function (value) {
      return A2(stringProperty,"height",$Basics.toString(value));
   };
   var width = function (value) {
      return A2(stringProperty,"width",$Basics.toString(value));
   };
   var alt = function (value) {
      return A2(stringProperty,"alt",value);
   };
   var preload = function (value) {
      return A2(stringProperty,"preload",value);
   };
   var poster = function (value) {
      return A2(stringProperty,"poster",value);
   };
   var kind = function (value) {
      return A2(stringProperty,"kind",value);
   };
   var srclang = function (value) {
      return A2(stringProperty,"srclang",value);
   };
   var sandbox = function (value) {
      return A2(stringProperty,"sandbox",value);
   };
   var srcdoc = function (value) {
      return A2(stringProperty,"srcdoc",value);
   };
   var type$ = function (value) {
      return A2(stringProperty,"type",value);
   };
   var value = function (value) {
      return A2(stringProperty,"value",value);
   };
   var placeholder = function (value) {
      return A2(stringProperty,"placeholder",value);
   };
   var accept = function (value) {
      return A2(stringProperty,"accept",value);
   };
   var acceptCharset = function (value) {
      return A2(stringProperty,"acceptCharset",value);
   };
   var action = function (value) {
      return A2(stringProperty,"action",value);
   };
   var autocomplete = function (bool) {
      return A2(stringProperty,"autocomplete",bool ? "on" : "off");
   };
   var autosave = function (value) {
      return A2(stringProperty,"autosave",value);
   };
   var enctype = function (value) {
      return A2(stringProperty,"enctype",value);
   };
   var formaction = function (value) {
      return A2(stringProperty,"formAction",value);
   };
   var list = function (value) {
      return A2(stringProperty,"list",value);
   };
   var minlength = function (n) {
      return A2(stringProperty,"minLength",$Basics.toString(n));
   };
   var maxlength = function (n) {
      return A2(stringProperty,"maxLength",$Basics.toString(n));
   };
   var method = function (value) {
      return A2(stringProperty,"method",value);
   };
   var name = function (value) {
      return A2(stringProperty,"name",value);
   };
   var pattern = function (value) {
      return A2(stringProperty,"pattern",value);
   };
   var size = function (n) {
      return A2(stringProperty,"size",$Basics.toString(n));
   };
   var $for = function (value) {
      return A2(stringProperty,"htmlFor",value);
   };
   var form = function (value) {
      return A2(stringProperty,"form",value);
   };
   var max = function (value) {
      return A2(stringProperty,"max",value);
   };
   var min = function (value) {
      return A2(stringProperty,"min",value);
   };
   var step = function (n) {
      return A2(stringProperty,"step",n);
   };
   var cols = function (n) {
      return A2(stringProperty,"cols",$Basics.toString(n));
   };
   var rows = function (n) {
      return A2(stringProperty,"rows",$Basics.toString(n));
   };
   var wrap = function (value) {
      return A2(stringProperty,"wrap",value);
   };
   var usemap = function (value) {
      return A2(stringProperty,"useMap",value);
   };
   var shape = function (value) {
      return A2(stringProperty,"shape",value);
   };
   var coords = function (value) {
      return A2(stringProperty,"coords",value);
   };
   var challenge = function (value) {
      return A2(stringProperty,"challenge",value);
   };
   var keytype = function (value) {
      return A2(stringProperty,"keytype",value);
   };
   var align = function (value) {
      return A2(stringProperty,"align",value);
   };
   var cite = function (value) {
      return A2(stringProperty,"cite",value);
   };
   var href = function (value) {
      return A2(stringProperty,"href",value);
   };
   var target = function (value) {
      return A2(stringProperty,"target",value);
   };
   var downloadAs = function (value) {
      return A2(stringProperty,"download",value);
   };
   var hreflang = function (value) {
      return A2(stringProperty,"hreflang",value);
   };
   var media = function (value) {
      return A2(stringProperty,"media",value);
   };
   var ping = function (value) {
      return A2(stringProperty,"ping",value);
   };
   var rel = function (value) {
      return A2(stringProperty,"rel",value);
   };
   var datetime = function (value) {
      return A2(stringProperty,"datetime",value);
   };
   var pubdate = function (value) {
      return A2(stringProperty,"pubdate",value);
   };
   var start = function (n) {
      return A2(stringProperty,"start",$Basics.toString(n));
   };
   var colspan = function (n) {
      return A2(stringProperty,"colSpan",$Basics.toString(n));
   };
   var headers = function (value) {
      return A2(stringProperty,"headers",value);
   };
   var rowspan = function (n) {
      return A2(stringProperty,"rowSpan",$Basics.toString(n));
   };
   var scope = function (value) {
      return A2(stringProperty,"scope",value);
   };
   var manifest = function (value) {
      return A2(stringProperty,"manifest",value);
   };
   var boolProperty = F2(function (name,bool) {
      return A2(property,name,$Json$Encode.bool(bool));
   });
   var hidden = function (bool) {
      return A2(boolProperty,"hidden",bool);
   };
   var contenteditable = function (bool) {
      return A2(boolProperty,"contentEditable",bool);
   };
   var spellcheck = function (bool) {
      return A2(boolProperty,"spellcheck",bool);
   };
   var async = function (bool) {
      return A2(boolProperty,"async",bool);
   };
   var defer = function (bool) {
      return A2(boolProperty,"defer",bool);
   };
   var scoped = function (bool) {
      return A2(boolProperty,"scoped",bool);
   };
   var autoplay = function (bool) {
      return A2(boolProperty,"autoplay",bool);
   };
   var controls = function (bool) {
      return A2(boolProperty,"controls",bool);
   };
   var loop = function (bool) {
      return A2(boolProperty,"loop",bool);
   };
   var $default = function (bool) {
      return A2(boolProperty,"default",bool);
   };
   var seamless = function (bool) {
      return A2(boolProperty,"seamless",bool);
   };
   var checked = function (bool) {
      return A2(boolProperty,"checked",bool);
   };
   var selected = function (bool) {
      return A2(boolProperty,"selected",bool);
   };
   var autofocus = function (bool) {
      return A2(boolProperty,"autofocus",bool);
   };
   var disabled = function (bool) {
      return A2(boolProperty,"disabled",bool);
   };
   var multiple = function (bool) {
      return A2(boolProperty,"multiple",bool);
   };
   var novalidate = function (bool) {
      return A2(boolProperty,"noValidate",bool);
   };
   var readonly = function (bool) {
      return A2(boolProperty,"readOnly",bool);
   };
   var required = function (bool) {
      return A2(boolProperty,"required",bool);
   };
   var ismap = function (value) {
      return A2(boolProperty,"isMap",value);
   };
   var download = function (bool) {
      return A2(boolProperty,"download",bool);
   };
   var reversed = function (bool) {
      return A2(boolProperty,"reversed",bool);
   };
   var classList = function (list) {
      return $class(A2($String.join,
      " ",
      A2($List.map,$Basics.fst,A2($List.filter,$Basics.snd,list))));
   };
   var style = function (props) {
      return A2(property,
      "style",
      $Json$Encode.object(A2($List.map,
      function (_p0) {
         var _p1 = _p0;
         return {ctor: "_Tuple2"
                ,_0: _p1._0
                ,_1: $Json$Encode.string(_p1._1)};
      },
      props)));
   };
   var key = function (k) {    return A2(stringProperty,"key",k);};
   return _elm.Html.Attributes.values = {_op: _op
                                        ,key: key
                                        ,style: style
                                        ,$class: $class
                                        ,classList: classList
                                        ,id: id
                                        ,title: title
                                        ,hidden: hidden
                                        ,type$: type$
                                        ,value: value
                                        ,checked: checked
                                        ,placeholder: placeholder
                                        ,selected: selected
                                        ,accept: accept
                                        ,acceptCharset: acceptCharset
                                        ,action: action
                                        ,autocomplete: autocomplete
                                        ,autofocus: autofocus
                                        ,autosave: autosave
                                        ,disabled: disabled
                                        ,enctype: enctype
                                        ,formaction: formaction
                                        ,list: list
                                        ,maxlength: maxlength
                                        ,minlength: minlength
                                        ,method: method
                                        ,multiple: multiple
                                        ,name: name
                                        ,novalidate: novalidate
                                        ,pattern: pattern
                                        ,readonly: readonly
                                        ,required: required
                                        ,size: size
                                        ,$for: $for
                                        ,form: form
                                        ,max: max
                                        ,min: min
                                        ,step: step
                                        ,cols: cols
                                        ,rows: rows
                                        ,wrap: wrap
                                        ,href: href
                                        ,target: target
                                        ,download: download
                                        ,downloadAs: downloadAs
                                        ,hreflang: hreflang
                                        ,media: media
                                        ,ping: ping
                                        ,rel: rel
                                        ,ismap: ismap
                                        ,usemap: usemap
                                        ,shape: shape
                                        ,coords: coords
                                        ,src: src
                                        ,height: height
                                        ,width: width
                                        ,alt: alt
                                        ,autoplay: autoplay
                                        ,controls: controls
                                        ,loop: loop
                                        ,preload: preload
                                        ,poster: poster
                                        ,$default: $default
                                        ,kind: kind
                                        ,srclang: srclang
                                        ,sandbox: sandbox
                                        ,seamless: seamless
                                        ,srcdoc: srcdoc
                                        ,reversed: reversed
                                        ,start: start
                                        ,align: align
                                        ,colspan: colspan
                                        ,rowspan: rowspan
                                        ,headers: headers
                                        ,scope: scope
                                        ,async: async
                                        ,charset: charset
                                        ,content: content
                                        ,defer: defer
                                        ,httpEquiv: httpEquiv
                                        ,language: language
                                        ,scoped: scoped
                                        ,accesskey: accesskey
                                        ,contenteditable: contenteditable
                                        ,contextmenu: contextmenu
                                        ,dir: dir
                                        ,draggable: draggable
                                        ,dropzone: dropzone
                                        ,itemprop: itemprop
                                        ,lang: lang
                                        ,spellcheck: spellcheck
                                        ,tabindex: tabindex
                                        ,challenge: challenge
                                        ,keytype: keytype
                                        ,cite: cite
                                        ,datetime: datetime
                                        ,pubdate: pubdate
                                        ,manifest: manifest
                                        ,property: property
                                        ,attribute: attribute};
};
Elm.Html = Elm.Html || {};
Elm.Html.Events = Elm.Html.Events || {};
Elm.Html.Events.make = function (_elm) {
   "use strict";
   _elm.Html = _elm.Html || {};
   _elm.Html.Events = _elm.Html.Events || {};
   if (_elm.Html.Events.values) return _elm.Html.Events.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var keyCode = A2($Json$Decode._op[":="],
   "keyCode",
   $Json$Decode.$int);
   var targetChecked = A2($Json$Decode.at,
   _U.list(["target","checked"]),
   $Json$Decode.bool);
   var targetValue = A2($Json$Decode.at,
   _U.list(["target","value"]),
   $Json$Decode.string);
   var defaultOptions = $VirtualDom.defaultOptions;
   var Options = F2(function (a,b) {
      return {stopPropagation: a,preventDefault: b};
   });
   var onWithOptions = $VirtualDom.onWithOptions;
   var on = $VirtualDom.on;
   var messageOn = F3(function (name,addr,msg) {
      return A3(on,
      name,
      $Json$Decode.value,
      function (_p0) {
         return A2($Signal.message,addr,msg);
      });
   });
   var onClick = messageOn("click");
   var onDoubleClick = messageOn("dblclick");
   var onMouseMove = messageOn("mousemove");
   var onMouseDown = messageOn("mousedown");
   var onMouseUp = messageOn("mouseup");
   var onMouseEnter = messageOn("mouseenter");
   var onMouseLeave = messageOn("mouseleave");
   var onMouseOver = messageOn("mouseover");
   var onMouseOut = messageOn("mouseout");
   var onBlur = messageOn("blur");
   var onFocus = messageOn("focus");
   var onSubmit = messageOn("submit");
   var onKey = F3(function (name,addr,handler) {
      return A3(on,
      name,
      keyCode,
      function (code) {
         return A2($Signal.message,addr,handler(code));
      });
   });
   var onKeyUp = onKey("keyup");
   var onKeyDown = onKey("keydown");
   var onKeyPress = onKey("keypress");
   return _elm.Html.Events.values = {_op: _op
                                    ,onBlur: onBlur
                                    ,onFocus: onFocus
                                    ,onSubmit: onSubmit
                                    ,onKeyUp: onKeyUp
                                    ,onKeyDown: onKeyDown
                                    ,onKeyPress: onKeyPress
                                    ,onClick: onClick
                                    ,onDoubleClick: onDoubleClick
                                    ,onMouseMove: onMouseMove
                                    ,onMouseDown: onMouseDown
                                    ,onMouseUp: onMouseUp
                                    ,onMouseEnter: onMouseEnter
                                    ,onMouseLeave: onMouseLeave
                                    ,onMouseOver: onMouseOver
                                    ,onMouseOut: onMouseOut
                                    ,on: on
                                    ,onWithOptions: onWithOptions
                                    ,defaultOptions: defaultOptions
                                    ,targetValue: targetValue
                                    ,targetChecked: targetChecked
                                    ,keyCode: keyCode
                                    ,Options: Options};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Ui = Elm.Editor.Ui || {};
Elm.Editor.Ui.Slider = Elm.Editor.Ui.Slider || {};
Elm.Editor.Ui.Slider.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Ui = _elm.Editor.Ui || {};
   _elm.Editor.Ui.Slider = _elm.Editor.Ui.Slider || {};
   if (_elm.Editor.Ui.Slider.values)
   return _elm.Editor.Ui.Slider.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Action = Elm.Editor.Action.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var slider = function (_p0) {
      var _p1 = _p0;
      var _p2 = _p1.value;
      return A2($Html.div,
      _U.list([]),
      _U.list([A2($Html.input,
              _U.list([A3($Html$Events.on,
                      "input",
                      $Html$Events.targetValue,
                      function (str) {
                         return A2($Signal.message,_p1.address,_p1.createAction(str));
                      })
                      ,$Html$Attributes.type$("range")
                      ,$Html$Attributes.value(_p2)
                      ,$Html$Attributes.max(_p1.max)
                      ,$Html$Attributes.min(_p1.min)]),
              _U.list([]))
              ,A2($Html.span,_U.list([]),_U.list([$Html.text(_p2)]))]));
   };
   var SliderSettings = F5(function (a,b,c,d,e) {
      return {min: a,max: b,value: c,address: d,createAction: e};
   });
   return _elm.Editor.Ui.Slider.values = {_op: _op
                                         ,SliderSettings: SliderSettings
                                         ,slider: slider};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Ui = Elm.Editor.Ui || {};
Elm.Editor.Ui.GroupSelect = Elm.Editor.Ui.GroupSelect || {};
Elm.Editor.Ui.GroupSelect.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Ui = _elm.Editor.Ui || {};
   _elm.Editor.Ui.GroupSelect = _elm.Editor.Ui.GroupSelect || {};
   if (_elm.Editor.Ui.GroupSelect.values)
   return _elm.Editor.Ui.GroupSelect.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Action = Elm.Editor.Action.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var createOption = F2(function (selected,value) {
      return A2($Html.option,
      _U.list([$Html$Attributes.value(value)
              ,$Html$Attributes.selected(_U.eq(value,selected))]),
      _U.list([$Html.text(value)]));
   });
   var groupTypes = _U.list(["P1"
                            ,"P2"
                            ,"Pm"
                            ,"Pg"
                            ,"Cm"
                            ,"P2mm"
                            ,"P2mg"
                            ,"P2gg"
                            ,"C2mm"
                            ,"P4"
                            ,"P4mm"
                            ,"P4mg"
                            ,"P3"
                            ,"P3m1"
                            ,"P31m"
                            ,"P6"]);
   var groupSelect = F2(function (selected,address) {
      return A2($Html.select,
      _U.list([A3($Html$Events.on,
      "change",
      $Html$Events.targetValue,
      function (str) {
         return A2($Signal.message,address,$Editor$Action.Group(str));
      })]),
      A2($List.map,createOption(selected),groupTypes));
   });
   return _elm.Editor.Ui.GroupSelect.values = {_op: _op
                                              ,groupTypes: groupTypes
                                              ,createOption: createOption
                                              ,groupSelect: groupSelect};
};
Elm.Svg = Elm.Svg || {};
Elm.Svg.make = function (_elm) {
   "use strict";
   _elm.Svg = _elm.Svg || {};
   if (_elm.Svg.values) return _elm.Svg.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Encode = Elm.Json.Encode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var text = $VirtualDom.text;
   var svgNamespace = A2($VirtualDom.property,
   "namespace",
   $Json$Encode.string("http://www.w3.org/2000/svg"));
   var node = F3(function (name,attributes,children) {
      return A3($VirtualDom.node,
      name,
      A2($List._op["::"],svgNamespace,attributes),
      children);
   });
   var svg = node("svg");
   var foreignObject = node("foreignObject");
   var animate = node("animate");
   var animateColor = node("animateColor");
   var animateMotion = node("animateMotion");
   var animateTransform = node("animateTransform");
   var mpath = node("mpath");
   var set = node("set");
   var a = node("a");
   var defs = node("defs");
   var g = node("g");
   var marker = node("marker");
   var mask = node("mask");
   var missingGlyph = node("missingGlyph");
   var pattern = node("pattern");
   var $switch = node("switch");
   var symbol = node("symbol");
   var desc = node("desc");
   var metadata = node("metadata");
   var title = node("title");
   var feBlend = node("feBlend");
   var feColorMatrix = node("feColorMatrix");
   var feComponentTransfer = node("feComponentTransfer");
   var feComposite = node("feComposite");
   var feConvolveMatrix = node("feConvolveMatrix");
   var feDiffuseLighting = node("feDiffuseLighting");
   var feDisplacementMap = node("feDisplacementMap");
   var feFlood = node("feFlood");
   var feFuncA = node("feFuncA");
   var feFuncB = node("feFuncB");
   var feFuncG = node("feFuncG");
   var feFuncR = node("feFuncR");
   var feGaussianBlur = node("feGaussianBlur");
   var feImage = node("feImage");
   var feMerge = node("feMerge");
   var feMergeNode = node("feMergeNode");
   var feMorphology = node("feMorphology");
   var feOffset = node("feOffset");
   var feSpecularLighting = node("feSpecularLighting");
   var feTile = node("feTile");
   var feTurbulence = node("feTurbulence");
   var font = node("font");
   var fontFace = node("fontFace");
   var fontFaceFormat = node("fontFaceFormat");
   var fontFaceName = node("fontFaceName");
   var fontFaceSrc = node("fontFaceSrc");
   var fontFaceUri = node("fontFaceUri");
   var hkern = node("hkern");
   var vkern = node("vkern");
   var linearGradient = node("linearGradient");
   var radialGradient = node("radialGradient");
   var stop = node("stop");
   var circle = node("circle");
   var ellipse = node("ellipse");
   var image = node("image");
   var line = node("line");
   var path = node("path");
   var polygon = node("polygon");
   var polyline = node("polyline");
   var rect = node("rect");
   var use = node("use");
   var feDistantLight = node("feDistantLight");
   var fePointLight = node("fePointLight");
   var feSpotLight = node("feSpotLight");
   var altGlyph = node("altGlyph");
   var altGlyphDef = node("altGlyphDef");
   var altGlyphItem = node("altGlyphItem");
   var glyph = node("glyph");
   var glyphRef = node("glyphRef");
   var textPath = node("textPath");
   var text$ = node("text");
   var tref = node("tref");
   var tspan = node("tspan");
   var clipPath = node("clipPath");
   var colorProfile = node("colorProfile");
   var cursor = node("cursor");
   var filter = node("filter");
   var script = node("script");
   var style = node("style");
   var view = node("view");
   return _elm.Svg.values = {_op: _op
                            ,text: text
                            ,node: node
                            ,svg: svg
                            ,foreignObject: foreignObject
                            ,circle: circle
                            ,ellipse: ellipse
                            ,image: image
                            ,line: line
                            ,path: path
                            ,polygon: polygon
                            ,polyline: polyline
                            ,rect: rect
                            ,use: use
                            ,animate: animate
                            ,animateColor: animateColor
                            ,animateMotion: animateMotion
                            ,animateTransform: animateTransform
                            ,mpath: mpath
                            ,set: set
                            ,desc: desc
                            ,metadata: metadata
                            ,title: title
                            ,a: a
                            ,defs: defs
                            ,g: g
                            ,marker: marker
                            ,mask: mask
                            ,missingGlyph: missingGlyph
                            ,pattern: pattern
                            ,$switch: $switch
                            ,symbol: symbol
                            ,altGlyph: altGlyph
                            ,altGlyphDef: altGlyphDef
                            ,altGlyphItem: altGlyphItem
                            ,glyph: glyph
                            ,glyphRef: glyphRef
                            ,textPath: textPath
                            ,text$: text$
                            ,tref: tref
                            ,tspan: tspan
                            ,font: font
                            ,fontFace: fontFace
                            ,fontFaceFormat: fontFaceFormat
                            ,fontFaceName: fontFaceName
                            ,fontFaceSrc: fontFaceSrc
                            ,fontFaceUri: fontFaceUri
                            ,hkern: hkern
                            ,vkern: vkern
                            ,linearGradient: linearGradient
                            ,radialGradient: radialGradient
                            ,stop: stop
                            ,feBlend: feBlend
                            ,feColorMatrix: feColorMatrix
                            ,feComponentTransfer: feComponentTransfer
                            ,feComposite: feComposite
                            ,feConvolveMatrix: feConvolveMatrix
                            ,feDiffuseLighting: feDiffuseLighting
                            ,feDisplacementMap: feDisplacementMap
                            ,feFlood: feFlood
                            ,feFuncA: feFuncA
                            ,feFuncB: feFuncB
                            ,feFuncG: feFuncG
                            ,feFuncR: feFuncR
                            ,feGaussianBlur: feGaussianBlur
                            ,feImage: feImage
                            ,feMerge: feMerge
                            ,feMergeNode: feMergeNode
                            ,feMorphology: feMorphology
                            ,feOffset: feOffset
                            ,feSpecularLighting: feSpecularLighting
                            ,feTile: feTile
                            ,feTurbulence: feTurbulence
                            ,feDistantLight: feDistantLight
                            ,fePointLight: fePointLight
                            ,feSpotLight: feSpotLight
                            ,clipPath: clipPath
                            ,colorProfile: colorProfile
                            ,cursor: cursor
                            ,filter: filter
                            ,script: script
                            ,style: style
                            ,view: view};
};
Elm.Svg = Elm.Svg || {};
Elm.Svg.Attributes = Elm.Svg.Attributes || {};
Elm.Svg.Attributes.make = function (_elm) {
   "use strict";
   _elm.Svg = _elm.Svg || {};
   _elm.Svg.Attributes = _elm.Svg.Attributes || {};
   if (_elm.Svg.Attributes.values)
   return _elm.Svg.Attributes.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $Svg = Elm.Svg.make(_elm),
   $VirtualDom = Elm.VirtualDom.make(_elm);
   var _op = {};
   var writingMode = $VirtualDom.attribute("writing-mode");
   var wordSpacing = $VirtualDom.attribute("word-spacing");
   var visibility = $VirtualDom.attribute("visibility");
   var unicodeBidi = $VirtualDom.attribute("unicode-bidi");
   var textRendering = $VirtualDom.attribute("text-rendering");
   var textDecoration = $VirtualDom.attribute("text-decoration");
   var textAnchor = $VirtualDom.attribute("text-anchor");
   var stroke = $VirtualDom.attribute("stroke");
   var strokeWidth = $VirtualDom.attribute("stroke-width");
   var strokeOpacity = $VirtualDom.attribute("stroke-opacity");
   var strokeMiterlimit = $VirtualDom.attribute("stroke-miterlimit");
   var strokeLinejoin = $VirtualDom.attribute("stroke-linejoin");
   var strokeLinecap = $VirtualDom.attribute("stroke-linecap");
   var strokeDashoffset = $VirtualDom.attribute("stroke-dashoffset");
   var strokeDasharray = $VirtualDom.attribute("stroke-dasharray");
   var stopOpacity = $VirtualDom.attribute("stop-opacity");
   var stopColor = $VirtualDom.attribute("stop-color");
   var shapeRendering = $VirtualDom.attribute("shape-rendering");
   var pointerEvents = $VirtualDom.attribute("pointer-events");
   var overflow = $VirtualDom.attribute("overflow");
   var opacity = $VirtualDom.attribute("opacity");
   var mask = $VirtualDom.attribute("mask");
   var markerStart = $VirtualDom.attribute("marker-start");
   var markerMid = $VirtualDom.attribute("marker-mid");
   var markerEnd = $VirtualDom.attribute("marker-end");
   var lightingColor = $VirtualDom.attribute("lighting-color");
   var letterSpacing = $VirtualDom.attribute("letter-spacing");
   var kerning = $VirtualDom.attribute("kerning");
   var imageRendering = $VirtualDom.attribute("image-rendering");
   var glyphOrientationVertical = $VirtualDom.attribute("glyph-orientation-vertical");
   var glyphOrientationHorizontal = $VirtualDom.attribute("glyph-orientation-horizontal");
   var fontWeight = $VirtualDom.attribute("font-weight");
   var fontVariant = $VirtualDom.attribute("font-variant");
   var fontStyle = $VirtualDom.attribute("font-style");
   var fontStretch = $VirtualDom.attribute("font-stretch");
   var fontSize = $VirtualDom.attribute("font-size");
   var fontSizeAdjust = $VirtualDom.attribute("font-size-adjust");
   var fontFamily = $VirtualDom.attribute("font-family");
   var floodOpacity = $VirtualDom.attribute("flood-opacity");
   var floodColor = $VirtualDom.attribute("flood-color");
   var filter = $VirtualDom.attribute("filter");
   var fill = $VirtualDom.attribute("fill");
   var fillRule = $VirtualDom.attribute("fill-rule");
   var fillOpacity = $VirtualDom.attribute("fill-opacity");
   var enableBackground = $VirtualDom.attribute("enable-background");
   var dominantBaseline = $VirtualDom.attribute("dominant-baseline");
   var display = $VirtualDom.attribute("display");
   var direction = $VirtualDom.attribute("direction");
   var cursor = $VirtualDom.attribute("cursor");
   var color = $VirtualDom.attribute("color");
   var colorRendering = $VirtualDom.attribute("color-rendering");
   var colorProfile = $VirtualDom.attribute("color-profile");
   var colorInterpolation = $VirtualDom.attribute("color-interpolation");
   var colorInterpolationFilters = $VirtualDom.attribute("color-interpolation-filters");
   var clip = $VirtualDom.attribute("clip");
   var clipRule = $VirtualDom.attribute("clip-rule");
   var clipPath = $VirtualDom.attribute("clip-path");
   var baselineShift = $VirtualDom.attribute("baseline-shift");
   var alignmentBaseline = $VirtualDom.attribute("alignment-baseline");
   var zoomAndPan = $VirtualDom.attribute("zoomAndPan");
   var z = $VirtualDom.attribute("z");
   var yChannelSelector = $VirtualDom.attribute("yChannelSelector");
   var y2 = $VirtualDom.attribute("y2");
   var y1 = $VirtualDom.attribute("y1");
   var y = $VirtualDom.attribute("y");
   var xmlSpace = A2($VirtualDom.attributeNS,
   "http://www.w3.org/XML/1998/namespace",
   "xml:space");
   var xmlLang = A2($VirtualDom.attributeNS,
   "http://www.w3.org/XML/1998/namespace",
   "xml:lang");
   var xmlBase = A2($VirtualDom.attributeNS,
   "http://www.w3.org/XML/1998/namespace",
   "xml:base");
   var xlinkType = A2($VirtualDom.attributeNS,
   "http://www.w3.org/1999/xlink",
   "xlink:type");
   var xlinkTitle = A2($VirtualDom.attributeNS,
   "http://www.w3.org/1999/xlink",
   "xlink:title");
   var xlinkShow = A2($VirtualDom.attributeNS,
   "http://www.w3.org/1999/xlink",
   "xlink:show");
   var xlinkRole = A2($VirtualDom.attributeNS,
   "http://www.w3.org/1999/xlink",
   "xlink:role");
   var xlinkHref = A2($VirtualDom.attributeNS,
   "http://www.w3.org/1999/xlink",
   "xlink:href");
   var xlinkArcrole = A2($VirtualDom.attributeNS,
   "http://www.w3.org/1999/xlink",
   "xlink:arcrole");
   var xlinkActuate = A2($VirtualDom.attributeNS,
   "http://www.w3.org/1999/xlink",
   "xlink:actuate");
   var xChannelSelector = $VirtualDom.attribute("xChannelSelector");
   var x2 = $VirtualDom.attribute("x2");
   var x1 = $VirtualDom.attribute("x1");
   var xHeight = $VirtualDom.attribute("x-height");
   var x = $VirtualDom.attribute("x");
   var widths = $VirtualDom.attribute("widths");
   var width = $VirtualDom.attribute("width");
   var viewTarget = $VirtualDom.attribute("viewTarget");
   var viewBox = $VirtualDom.attribute("viewBox");
   var vertOriginY = $VirtualDom.attribute("vert-origin-y");
   var vertOriginX = $VirtualDom.attribute("vert-origin-x");
   var vertAdvY = $VirtualDom.attribute("vert-adv-y");
   var version = $VirtualDom.attribute("version");
   var values = $VirtualDom.attribute("values");
   var vMathematical = $VirtualDom.attribute("v-mathematical");
   var vIdeographic = $VirtualDom.attribute("v-ideographic");
   var vHanging = $VirtualDom.attribute("v-hanging");
   var vAlphabetic = $VirtualDom.attribute("v-alphabetic");
   var unitsPerEm = $VirtualDom.attribute("units-per-em");
   var unicodeRange = $VirtualDom.attribute("unicode-range");
   var unicode = $VirtualDom.attribute("unicode");
   var underlineThickness = $VirtualDom.attribute("underline-thickness");
   var underlinePosition = $VirtualDom.attribute("underline-position");
   var u2 = $VirtualDom.attribute("u2");
   var u1 = $VirtualDom.attribute("u1");
   var type$ = $VirtualDom.attribute("type");
   var transform = $VirtualDom.attribute("transform");
   var to = $VirtualDom.attribute("to");
   var title = $VirtualDom.attribute("title");
   var textLength = $VirtualDom.attribute("textLength");
   var targetY = $VirtualDom.attribute("targetY");
   var targetX = $VirtualDom.attribute("targetX");
   var target = $VirtualDom.attribute("target");
   var tableValues = $VirtualDom.attribute("tableValues");
   var systemLanguage = $VirtualDom.attribute("systemLanguage");
   var surfaceScale = $VirtualDom.attribute("surfaceScale");
   var style = $VirtualDom.attribute("style");
   var string = $VirtualDom.attribute("string");
   var strikethroughThickness = $VirtualDom.attribute("strikethrough-thickness");
   var strikethroughPosition = $VirtualDom.attribute("strikethrough-position");
   var stitchTiles = $VirtualDom.attribute("stitchTiles");
   var stemv = $VirtualDom.attribute("stemv");
   var stemh = $VirtualDom.attribute("stemh");
   var stdDeviation = $VirtualDom.attribute("stdDeviation");
   var startOffset = $VirtualDom.attribute("startOffset");
   var spreadMethod = $VirtualDom.attribute("spreadMethod");
   var speed = $VirtualDom.attribute("speed");
   var specularExponent = $VirtualDom.attribute("specularExponent");
   var specularConstant = $VirtualDom.attribute("specularConstant");
   var spacing = $VirtualDom.attribute("spacing");
   var slope = $VirtualDom.attribute("slope");
   var seed = $VirtualDom.attribute("seed");
   var scale = $VirtualDom.attribute("scale");
   var ry = $VirtualDom.attribute("ry");
   var rx = $VirtualDom.attribute("rx");
   var rotate = $VirtualDom.attribute("rotate");
   var result = $VirtualDom.attribute("result");
   var restart = $VirtualDom.attribute("restart");
   var requiredFeatures = $VirtualDom.attribute("requiredFeatures");
   var requiredExtensions = $VirtualDom.attribute("requiredExtensions");
   var repeatDur = $VirtualDom.attribute("repeatDur");
   var repeatCount = $VirtualDom.attribute("repeatCount");
   var renderingIntent = $VirtualDom.attribute("rendering-intent");
   var refY = $VirtualDom.attribute("refY");
   var refX = $VirtualDom.attribute("refX");
   var radius = $VirtualDom.attribute("radius");
   var r = $VirtualDom.attribute("r");
   var primitiveUnits = $VirtualDom.attribute("primitiveUnits");
   var preserveAspectRatio = $VirtualDom.attribute("preserveAspectRatio");
   var preserveAlpha = $VirtualDom.attribute("preserveAlpha");
   var pointsAtZ = $VirtualDom.attribute("pointsAtZ");
   var pointsAtY = $VirtualDom.attribute("pointsAtY");
   var pointsAtX = $VirtualDom.attribute("pointsAtX");
   var points = $VirtualDom.attribute("points");
   var pointOrder = $VirtualDom.attribute("point-order");
   var patternUnits = $VirtualDom.attribute("patternUnits");
   var patternTransform = $VirtualDom.attribute("patternTransform");
   var patternContentUnits = $VirtualDom.attribute("patternContentUnits");
   var pathLength = $VirtualDom.attribute("pathLength");
   var path = $VirtualDom.attribute("path");
   var panose1 = $VirtualDom.attribute("panose-1");
   var overlineThickness = $VirtualDom.attribute("overline-thickness");
   var overlinePosition = $VirtualDom.attribute("overline-position");
   var origin = $VirtualDom.attribute("origin");
   var orientation = $VirtualDom.attribute("orientation");
   var orient = $VirtualDom.attribute("orient");
   var order = $VirtualDom.attribute("order");
   var operator = $VirtualDom.attribute("operator");
   var offset = $VirtualDom.attribute("offset");
   var numOctaves = $VirtualDom.attribute("numOctaves");
   var name = $VirtualDom.attribute("name");
   var mode = $VirtualDom.attribute("mode");
   var min = $VirtualDom.attribute("min");
   var method = $VirtualDom.attribute("method");
   var media = $VirtualDom.attribute("media");
   var max = $VirtualDom.attribute("max");
   var mathematical = $VirtualDom.attribute("mathematical");
   var maskUnits = $VirtualDom.attribute("maskUnits");
   var maskContentUnits = $VirtualDom.attribute("maskContentUnits");
   var markerWidth = $VirtualDom.attribute("markerWidth");
   var markerUnits = $VirtualDom.attribute("markerUnits");
   var markerHeight = $VirtualDom.attribute("markerHeight");
   var local = $VirtualDom.attribute("local");
   var limitingConeAngle = $VirtualDom.attribute("limitingConeAngle");
   var lengthAdjust = $VirtualDom.attribute("lengthAdjust");
   var lang = $VirtualDom.attribute("lang");
   var keyTimes = $VirtualDom.attribute("keyTimes");
   var keySplines = $VirtualDom.attribute("keySplines");
   var keyPoints = $VirtualDom.attribute("keyPoints");
   var kernelUnitLength = $VirtualDom.attribute("kernelUnitLength");
   var kernelMatrix = $VirtualDom.attribute("kernelMatrix");
   var k4 = $VirtualDom.attribute("k4");
   var k3 = $VirtualDom.attribute("k3");
   var k2 = $VirtualDom.attribute("k2");
   var k1 = $VirtualDom.attribute("k1");
   var k = $VirtualDom.attribute("k");
   var intercept = $VirtualDom.attribute("intercept");
   var in2 = $VirtualDom.attribute("in2");
   var in$ = $VirtualDom.attribute("in");
   var ideographic = $VirtualDom.attribute("ideographic");
   var id = $VirtualDom.attribute("id");
   var horizOriginY = $VirtualDom.attribute("horiz-origin-y");
   var horizOriginX = $VirtualDom.attribute("horiz-origin-x");
   var horizAdvX = $VirtualDom.attribute("horiz-adv-x");
   var height = $VirtualDom.attribute("height");
   var hanging = $VirtualDom.attribute("hanging");
   var gradientUnits = $VirtualDom.attribute("gradientUnits");
   var gradientTransform = $VirtualDom.attribute("gradientTransform");
   var glyphRef = $VirtualDom.attribute("glyphRef");
   var glyphName = $VirtualDom.attribute("glyph-name");
   var g2 = $VirtualDom.attribute("g2");
   var g1 = $VirtualDom.attribute("g1");
   var fy = $VirtualDom.attribute("fy");
   var fx = $VirtualDom.attribute("fx");
   var from = $VirtualDom.attribute("from");
   var format = $VirtualDom.attribute("format");
   var filterUnits = $VirtualDom.attribute("filterUnits");
   var filterRes = $VirtualDom.attribute("filterRes");
   var externalResourcesRequired = $VirtualDom.attribute("externalResourcesRequired");
   var exponent = $VirtualDom.attribute("exponent");
   var end = $VirtualDom.attribute("end");
   var elevation = $VirtualDom.attribute("elevation");
   var edgeMode = $VirtualDom.attribute("edgeMode");
   var dy = $VirtualDom.attribute("dy");
   var dx = $VirtualDom.attribute("dx");
   var dur = $VirtualDom.attribute("dur");
   var divisor = $VirtualDom.attribute("divisor");
   var diffuseConstant = $VirtualDom.attribute("diffuseConstant");
   var descent = $VirtualDom.attribute("descent");
   var decelerate = $VirtualDom.attribute("decelerate");
   var d = $VirtualDom.attribute("d");
   var cy = $VirtualDom.attribute("cy");
   var cx = $VirtualDom.attribute("cx");
   var contentStyleType = $VirtualDom.attribute("contentStyleType");
   var contentScriptType = $VirtualDom.attribute("contentScriptType");
   var clipPathUnits = $VirtualDom.attribute("clipPathUnits");
   var $class = $VirtualDom.attribute("class");
   var capHeight = $VirtualDom.attribute("cap-height");
   var calcMode = $VirtualDom.attribute("calcMode");
   var by = $VirtualDom.attribute("by");
   var bias = $VirtualDom.attribute("bias");
   var begin = $VirtualDom.attribute("begin");
   var bbox = $VirtualDom.attribute("bbox");
   var baseProfile = $VirtualDom.attribute("baseProfile");
   var baseFrequency = $VirtualDom.attribute("baseFrequency");
   var azimuth = $VirtualDom.attribute("azimuth");
   var autoReverse = $VirtualDom.attribute("autoReverse");
   var attributeType = $VirtualDom.attribute("attributeType");
   var attributeName = $VirtualDom.attribute("attributeName");
   var ascent = $VirtualDom.attribute("ascent");
   var arabicForm = $VirtualDom.attribute("arabic-form");
   var amplitude = $VirtualDom.attribute("amplitude");
   var allowReorder = $VirtualDom.attribute("allowReorder");
   var alphabetic = $VirtualDom.attribute("alphabetic");
   var additive = $VirtualDom.attribute("additive");
   var accumulate = $VirtualDom.attribute("accumulate");
   var accelerate = $VirtualDom.attribute("accelerate");
   var accentHeight = $VirtualDom.attribute("accent-height");
   return _elm.Svg.Attributes.values = {_op: _op
                                       ,accentHeight: accentHeight
                                       ,accelerate: accelerate
                                       ,accumulate: accumulate
                                       ,additive: additive
                                       ,alphabetic: alphabetic
                                       ,allowReorder: allowReorder
                                       ,amplitude: amplitude
                                       ,arabicForm: arabicForm
                                       ,ascent: ascent
                                       ,attributeName: attributeName
                                       ,attributeType: attributeType
                                       ,autoReverse: autoReverse
                                       ,azimuth: azimuth
                                       ,baseFrequency: baseFrequency
                                       ,baseProfile: baseProfile
                                       ,bbox: bbox
                                       ,begin: begin
                                       ,bias: bias
                                       ,by: by
                                       ,calcMode: calcMode
                                       ,capHeight: capHeight
                                       ,$class: $class
                                       ,clipPathUnits: clipPathUnits
                                       ,contentScriptType: contentScriptType
                                       ,contentStyleType: contentStyleType
                                       ,cx: cx
                                       ,cy: cy
                                       ,d: d
                                       ,decelerate: decelerate
                                       ,descent: descent
                                       ,diffuseConstant: diffuseConstant
                                       ,divisor: divisor
                                       ,dur: dur
                                       ,dx: dx
                                       ,dy: dy
                                       ,edgeMode: edgeMode
                                       ,elevation: elevation
                                       ,end: end
                                       ,exponent: exponent
                                       ,externalResourcesRequired: externalResourcesRequired
                                       ,filterRes: filterRes
                                       ,filterUnits: filterUnits
                                       ,format: format
                                       ,from: from
                                       ,fx: fx
                                       ,fy: fy
                                       ,g1: g1
                                       ,g2: g2
                                       ,glyphName: glyphName
                                       ,glyphRef: glyphRef
                                       ,gradientTransform: gradientTransform
                                       ,gradientUnits: gradientUnits
                                       ,hanging: hanging
                                       ,height: height
                                       ,horizAdvX: horizAdvX
                                       ,horizOriginX: horizOriginX
                                       ,horizOriginY: horizOriginY
                                       ,id: id
                                       ,ideographic: ideographic
                                       ,in$: in$
                                       ,in2: in2
                                       ,intercept: intercept
                                       ,k: k
                                       ,k1: k1
                                       ,k2: k2
                                       ,k3: k3
                                       ,k4: k4
                                       ,kernelMatrix: kernelMatrix
                                       ,kernelUnitLength: kernelUnitLength
                                       ,keyPoints: keyPoints
                                       ,keySplines: keySplines
                                       ,keyTimes: keyTimes
                                       ,lang: lang
                                       ,lengthAdjust: lengthAdjust
                                       ,limitingConeAngle: limitingConeAngle
                                       ,local: local
                                       ,markerHeight: markerHeight
                                       ,markerUnits: markerUnits
                                       ,markerWidth: markerWidth
                                       ,maskContentUnits: maskContentUnits
                                       ,maskUnits: maskUnits
                                       ,mathematical: mathematical
                                       ,max: max
                                       ,media: media
                                       ,method: method
                                       ,min: min
                                       ,mode: mode
                                       ,name: name
                                       ,numOctaves: numOctaves
                                       ,offset: offset
                                       ,operator: operator
                                       ,order: order
                                       ,orient: orient
                                       ,orientation: orientation
                                       ,origin: origin
                                       ,overlinePosition: overlinePosition
                                       ,overlineThickness: overlineThickness
                                       ,panose1: panose1
                                       ,path: path
                                       ,pathLength: pathLength
                                       ,patternContentUnits: patternContentUnits
                                       ,patternTransform: patternTransform
                                       ,patternUnits: patternUnits
                                       ,pointOrder: pointOrder
                                       ,points: points
                                       ,pointsAtX: pointsAtX
                                       ,pointsAtY: pointsAtY
                                       ,pointsAtZ: pointsAtZ
                                       ,preserveAlpha: preserveAlpha
                                       ,preserveAspectRatio: preserveAspectRatio
                                       ,primitiveUnits: primitiveUnits
                                       ,r: r
                                       ,radius: radius
                                       ,refX: refX
                                       ,refY: refY
                                       ,renderingIntent: renderingIntent
                                       ,repeatCount: repeatCount
                                       ,repeatDur: repeatDur
                                       ,requiredExtensions: requiredExtensions
                                       ,requiredFeatures: requiredFeatures
                                       ,restart: restart
                                       ,result: result
                                       ,rotate: rotate
                                       ,rx: rx
                                       ,ry: ry
                                       ,scale: scale
                                       ,seed: seed
                                       ,slope: slope
                                       ,spacing: spacing
                                       ,specularConstant: specularConstant
                                       ,specularExponent: specularExponent
                                       ,speed: speed
                                       ,spreadMethod: spreadMethod
                                       ,startOffset: startOffset
                                       ,stdDeviation: stdDeviation
                                       ,stemh: stemh
                                       ,stemv: stemv
                                       ,stitchTiles: stitchTiles
                                       ,strikethroughPosition: strikethroughPosition
                                       ,strikethroughThickness: strikethroughThickness
                                       ,string: string
                                       ,style: style
                                       ,surfaceScale: surfaceScale
                                       ,systemLanguage: systemLanguage
                                       ,tableValues: tableValues
                                       ,target: target
                                       ,targetX: targetX
                                       ,targetY: targetY
                                       ,textLength: textLength
                                       ,title: title
                                       ,to: to
                                       ,transform: transform
                                       ,type$: type$
                                       ,u1: u1
                                       ,u2: u2
                                       ,underlinePosition: underlinePosition
                                       ,underlineThickness: underlineThickness
                                       ,unicode: unicode
                                       ,unicodeRange: unicodeRange
                                       ,unitsPerEm: unitsPerEm
                                       ,vAlphabetic: vAlphabetic
                                       ,vHanging: vHanging
                                       ,vIdeographic: vIdeographic
                                       ,vMathematical: vMathematical
                                       ,values: values
                                       ,version: version
                                       ,vertAdvY: vertAdvY
                                       ,vertOriginX: vertOriginX
                                       ,vertOriginY: vertOriginY
                                       ,viewBox: viewBox
                                       ,viewTarget: viewTarget
                                       ,width: width
                                       ,widths: widths
                                       ,x: x
                                       ,xHeight: xHeight
                                       ,x1: x1
                                       ,x2: x2
                                       ,xChannelSelector: xChannelSelector
                                       ,xlinkActuate: xlinkActuate
                                       ,xlinkArcrole: xlinkArcrole
                                       ,xlinkHref: xlinkHref
                                       ,xlinkRole: xlinkRole
                                       ,xlinkShow: xlinkShow
                                       ,xlinkTitle: xlinkTitle
                                       ,xlinkType: xlinkType
                                       ,xmlBase: xmlBase
                                       ,xmlLang: xmlLang
                                       ,xmlSpace: xmlSpace
                                       ,y: y
                                       ,y1: y1
                                       ,y2: y2
                                       ,yChannelSelector: yChannelSelector
                                       ,z: z
                                       ,zoomAndPan: zoomAndPan
                                       ,alignmentBaseline: alignmentBaseline
                                       ,baselineShift: baselineShift
                                       ,clipPath: clipPath
                                       ,clipRule: clipRule
                                       ,clip: clip
                                       ,colorInterpolationFilters: colorInterpolationFilters
                                       ,colorInterpolation: colorInterpolation
                                       ,colorProfile: colorProfile
                                       ,colorRendering: colorRendering
                                       ,color: color
                                       ,cursor: cursor
                                       ,direction: direction
                                       ,display: display
                                       ,dominantBaseline: dominantBaseline
                                       ,enableBackground: enableBackground
                                       ,fillOpacity: fillOpacity
                                       ,fillRule: fillRule
                                       ,fill: fill
                                       ,filter: filter
                                       ,floodColor: floodColor
                                       ,floodOpacity: floodOpacity
                                       ,fontFamily: fontFamily
                                       ,fontSizeAdjust: fontSizeAdjust
                                       ,fontSize: fontSize
                                       ,fontStretch: fontStretch
                                       ,fontStyle: fontStyle
                                       ,fontVariant: fontVariant
                                       ,fontWeight: fontWeight
                                       ,glyphOrientationHorizontal: glyphOrientationHorizontal
                                       ,glyphOrientationVertical: glyphOrientationVertical
                                       ,imageRendering: imageRendering
                                       ,kerning: kerning
                                       ,letterSpacing: letterSpacing
                                       ,lightingColor: lightingColor
                                       ,markerEnd: markerEnd
                                       ,markerMid: markerMid
                                       ,markerStart: markerStart
                                       ,mask: mask
                                       ,opacity: opacity
                                       ,overflow: overflow
                                       ,pointerEvents: pointerEvents
                                       ,shapeRendering: shapeRendering
                                       ,stopColor: stopColor
                                       ,stopOpacity: stopOpacity
                                       ,strokeDasharray: strokeDasharray
                                       ,strokeDashoffset: strokeDashoffset
                                       ,strokeLinecap: strokeLinecap
                                       ,strokeLinejoin: strokeLinejoin
                                       ,strokeMiterlimit: strokeMiterlimit
                                       ,strokeOpacity: strokeOpacity
                                       ,strokeWidth: strokeWidth
                                       ,stroke: stroke
                                       ,textAnchor: textAnchor
                                       ,textDecoration: textDecoration
                                       ,textRendering: textRendering
                                       ,unicodeBidi: unicodeBidi
                                       ,visibility: visibility
                                       ,wordSpacing: wordSpacing
                                       ,writingMode: writingMode};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Util = Elm.Editor.Util || {};
Elm.Editor.Util.Svg = Elm.Editor.Util.Svg || {};
Elm.Editor.Util.Svg.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Util = _elm.Editor.Util || {};
   _elm.Editor.Util.Svg = _elm.Editor.Util.Svg || {};
   if (_elm.Editor.Util.Svg.values)
   return _elm.Editor.Util.Svg.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Types = Elm.Editor.Types.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Svg = Elm.Svg.make(_elm),
   $Svg$Attributes = Elm.Svg.Attributes.make(_elm),
   $WallpaperGroup$Group = Elm.WallpaperGroup.Group.make(_elm),
   $WallpaperGroup$Pattern = Elm.WallpaperGroup.Pattern.make(_elm);
   var _op = {};
   var pointToString = function (p) {
      return A2($Basics._op["++"],
      $Basics.toString(p.x),
      A2($Basics._op["++"]," ",$Basics.toString(p.y)));
   };
   var renderPath = function (multiLine) {
      var path = A2($Basics._op["++"],
      "M ",
      A2($Basics._op["++"],
      A2($String.join," L ",A2($List.map,pointToString,multiLine)),
      "z"));
      return A2($Svg.path,
      _U.list([$Svg$Attributes.d(path)
              ,$Svg$Attributes.$class("tile")]),
      _U.list([]));
   };
   var renderPaths = F4(function (group,columns,rows,tile) {
      return A2($Svg.g,
      _U.list([]),
      $List.concat(A2($List.map,
      function (t) {
         return A2($List.map,renderPath,t);
      },
      A4($WallpaperGroup$Pattern.pattern,group,columns,rows,tile))));
   });
   var lineToAttribute = F2(function (_p0,attributes) {
      var _p1 = _p0;
      var _p3 = _p1.y;
      var _p2 = _p1.x;
      return _U.eq($List.length(attributes),0) ? A2($List.append,
      _U.list([$Svg$Attributes.x1($Basics.toString(_p2))
              ,$Svg$Attributes.y1($Basics.toString(_p3))]),
      attributes) : A2($List.append,
      _U.list([$Svg$Attributes.x2($Basics.toString(_p2))
              ,$Svg$Attributes.y2($Basics.toString(_p3))]),
      attributes);
   });
   var renderLine = function (line) {
      return A2($Svg.line,
      A3($List.foldl,lineToAttribute,_U.list([]),line),
      _U.list([]));
   };
   var renderTile = function (tile) {
      return A2($Svg.g,
      _U.list([$Svg$Attributes.stroke("grey")]),
      A2($List.map,renderLine,tile));
   };
   var renderTiles = F4(function (group,columns,rows,tile) {
      return A2($Svg.g,
      _U.list([]),
      A2($List.map,
      renderTile,
      A4($WallpaperGroup$Pattern.pattern,group,columns,rows,tile)));
   });
   return _elm.Editor.Util.Svg.values = {_op: _op
                                        ,lineToAttribute: lineToAttribute
                                        ,pointToString: pointToString
                                        ,renderPath: renderPath
                                        ,renderPaths: renderPaths
                                        ,renderLine: renderLine
                                        ,renderTile: renderTile
                                        ,renderTiles: renderTiles};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Ui = Elm.Editor.Ui || {};
Elm.Editor.Ui.Raster = Elm.Editor.Ui.Raster || {};
Elm.Editor.Ui.Raster.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Ui = _elm.Editor.Ui || {};
   _elm.Editor.Ui.Raster = _elm.Editor.Ui.Raster || {};
   if (_elm.Editor.Ui.Raster.values)
   return _elm.Editor.Ui.Raster.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Action = Elm.Editor.Action.make(_elm),
   $Editor$Model = Elm.Editor.Model.make(_elm),
   $Editor$Types = Elm.Editor.Types.make(_elm),
   $Editor$Util$Svg = Elm.Editor.Util.Svg.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Svg = Elm.Svg.make(_elm),
   $Svg$Attributes = Elm.Svg.Attributes.make(_elm),
   $WallpaperGroup$Geom$BoundingBox = Elm.WallpaperGroup.Geom.BoundingBox.make(_elm),
   $WallpaperGroup$Group = Elm.WallpaperGroup.Group.make(_elm);
   var _op = {};
   var sendMousePositionOrDelete = F2(function (sendAction,
   mouseData) {
      return $Basics.snd(mouseData) ? A2(sendAction,
      $Editor$Action.DeleteLine,
      $Basics.fst(mouseData)) : A2(sendAction,
      $Editor$Action.LineStart,
      $Basics.fst(mouseData));
   });
   var sendMousePosition = F3(function (sendAction,
   action,
   mouseData) {
      return A2(sendAction,action,$Basics.fst(mouseData));
   });
   var preview = function (model) {
      var childs = model.isDrawing ? _U.list([A2($Svg.line,
      _U.list([$Svg$Attributes.x1($Basics.toString(model.lineStart.x))
              ,$Svg$Attributes.y1($Basics.toString(model.lineStart.y))
              ,$Svg$Attributes.x2($Basics.toString(model.lineEnd.x))
              ,$Svg$Attributes.y2($Basics.toString(model.lineEnd.y))
              ,$Svg$Attributes.stroke("grey")]),
      _U.list([]))]) : _U.list([]);
      return A2($Svg.g,_U.list([]),childs);
   };
   var sendTo = F3(function (address,action,point) {
      return A2($Signal.message,address,action(point));
   });
   var mousePosition = A4($Json$Decode.object3,
   F3(function (x,y,altKeyPressed) {
      return {ctor: "_Tuple2",_0: {x: x,y: y},_1: altKeyPressed};
   }),
   A2($Json$Decode._op[":="],"offsetX",$Json$Decode.$float),
   A2($Json$Decode._op[":="],"offsetY",$Json$Decode.$float),
   A2($Json$Decode._op[":="],"altKey",$Json$Decode.bool));
   var boundingBoxAsPath = function (boundingBox) {
      var _p0 = boundingBox;
      if (_p0.ctor === "Rect") {
            return _U.list([_U.list([_p0._0,_p0._1,_p0._2,_p0._3])]);
         } else {
            return _U.list([_U.list([_p0._0,_p0._1,_p0._2])]);
         }
   };
   var bounding = function (boundingBox) {
      var _p1 = boundingBox;
      if (_p1.ctor === "Rect") {
            var _p5 = _p1._3;
            var _p4 = _p1._2;
            var _p3 = _p1._1;
            var _p2 = _p1._0;
            return A2($Svg.g,
            _U.list([$Svg$Attributes.$class("single-tile")]),
            _U.list([$Editor$Util$Svg.renderLine(_U.list([_p2,_p3]))
                    ,$Editor$Util$Svg.renderLine(_U.list([_p3,_p4]))
                    ,$Editor$Util$Svg.renderLine(_U.list([_p4,_p5]))
                    ,$Editor$Util$Svg.renderLine(_U.list([_p5,_p2]))]));
         } else {
            var _p8 = _p1._2;
            var _p7 = _p1._1;
            var _p6 = _p1._0;
            return A2($Svg.g,
            _U.list([$Svg$Attributes.$class("single-tile")]),
            _U.list([$Editor$Util$Svg.renderLine(_U.list([_p6,_p7]))
                    ,$Editor$Util$Svg.renderLine(_U.list([_p7,_p8]))
                    ,$Editor$Util$Svg.renderLine(_U.list([_p8,_p6]))]));
         }
   };
   var renderPoint = function (p) {
      return A2($Svg.circle,
      _U.list([$Svg$Attributes.cx($Basics.toString(p.x))
              ,$Svg$Attributes.cy($Basics.toString(p.y))
              ,$Svg$Attributes.r(".7")]),
      _U.list([]));
   };
   var raster = F5(function (model,
   tile,
   group,
   boundingBox,
   address) {
      var sendAction = sendTo(address);
      return A2($Html.div,
      _U.list([A3($Html$Events.on,
              "mousedown",
              mousePosition,
              sendMousePositionOrDelete(sendAction))
              ,A3($Html$Events.on,
              "mousemove",
              mousePosition,
              A2(sendMousePosition,sendAction,$Editor$Action.LineMove))
              ,A3($Html$Events.on,
              "mouseup",
              mousePosition,
              A2(sendMousePosition,sendAction,$Editor$Action.LineEnd))
              ,$Html$Attributes.$class("drawingArea")]),
      _U.list([A2($Svg.svg,
      _U.list([$Svg$Attributes.version("1.1")
              ,$Svg$Attributes.x("0")
              ,$Svg$Attributes.y("0")
              ,$Svg$Attributes.$class($String.toLower(A2($Basics._op["++"],
              "preview ",
              $Basics.toString(group))))]),
      _U.list([A4($Editor$Util$Svg.renderPaths,
              group,
              1,
              1,
              boundingBoxAsPath(boundingBox))
              ,A4($Editor$Util$Svg.renderTiles,group,1,1,tile)
              ,A2($Svg.g,
              _U.list([]),
              A2($List.map,renderPoint,model.rasterCoords))
              ,preview(model)
              ,$Editor$Util$Svg.renderTile(tile)
              ,bounding(boundingBox)]))]));
   });
   return _elm.Editor.Ui.Raster.values = {_op: _op
                                         ,renderPoint: renderPoint
                                         ,bounding: bounding
                                         ,boundingBoxAsPath: boundingBoxAsPath
                                         ,mousePosition: mousePosition
                                         ,sendTo: sendTo
                                         ,preview: preview
                                         ,sendMousePosition: sendMousePosition
                                         ,sendMousePositionOrDelete: sendMousePositionOrDelete
                                         ,raster: raster};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Ui = Elm.Editor.Ui || {};
Elm.Editor.Ui.ColorFinder = Elm.Editor.Ui.ColorFinder || {};
Elm.Editor.Ui.ColorFinder.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Ui = _elm.Editor.Ui || {};
   _elm.Editor.Ui.ColorFinder = _elm.Editor.Ui.ColorFinder || {};
   if (_elm.Editor.Ui.ColorFinder.values)
   return _elm.Editor.Ui.ColorFinder.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Action = Elm.Editor.Action.make(_elm),
   $Editor$Model = Elm.Editor.Model.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var colorItem = function (color) {
      return A2($Html.span,
      _U.list([$Html$Attributes.style(_U.list([{ctor: "_Tuple2"
                                               ,_0: "backgroundColor"
                                               ,_1: A2($Basics._op["++"],"#",color)}]))]),
      _U.list([]));
   };
   var palette = F2(function (address,colors) {
      return A2($Html.li,
      _U.list([$Html$Attributes.$class("preview")
              ,A2($Html$Events.onClick,
              address,
              $Editor$Action.SelectPalette(colors))]),
      A2($List.map,colorItem,colors));
   });
   var colorList = F4(function (isShown,
   isLoading,
   palettes,
   address) {
      return isLoading ? A2($Html.div,
      _U.list([$Html$Attributes.$class("palettes loader")]),
      _U.list([A2($Html.span,
      _U.list([]),
      _U.list([]))])) : isShown && _U.cmp($List.length(palettes),
      0) > 0 ? A2($Html.div,
      _U.list([$Html$Attributes.$class("palettes")]),
      _U.list([A2($Html.ul,
      _U.list([]),
      A2($List.map,palette(address),palettes))])) : A2($Html.span,
      _U.list([]),
      _U.list([]));
   });
   var selectedColor = F2(function (address,colors) {
      return A2($Html.div,
      _U.list([$Html$Attributes.$class("preview")
              ,A2($Html$Events.onClick,
              address,
              $Editor$Action.UpadtePattern)]),
      A2($List.map,colorItem,colors));
   });
   var colorFinder = F2(function (address,model) {
      return A2($Html.div,
      _U.list([$Html$Attributes.$class("palette")]),
      _U.list([A2($Html.div,
      _U.list([$Html$Attributes.$class("column")]),
      _U.list([A2($Html.input,
              _U.list([A3($Html$Events.on,
                      "input",
                      $Html$Events.targetValue,
                      function (str) {
                         return A2($Signal.message,
                         address,
                         $Editor$Action.StartColorSearch(str));
                      })
                      ,A2($Html$Events.onBlur,
                      address,
                      $Editor$Action.TogglePallete(false))
                      ,A2($Html$Events.onFocus,
                      address,
                      $Editor$Action.TogglePallete(true))
                      ,$Html$Attributes.type$("text")
                      ,$Html$Attributes.value(model.colorSearch)
                      ,$Html$Attributes.placeholder("Search palettes")]),
              _U.list([]))
              ,A2(selectedColor,address,model.selectedPalette)
              ,A4(colorList,
              model.paletteOpen,
              model.loading,
              model.palettes,
              address)]))]));
   });
   return _elm.Editor.Ui.ColorFinder.values = {_op: _op
                                              ,colorItem: colorItem
                                              ,palette: palette
                                              ,selectedColor: selectedColor
                                              ,colorList: colorList
                                              ,colorFinder: colorFinder};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Util = Elm.Editor.Util || {};
Elm.Editor.Util.Convert = Elm.Editor.Util.Convert || {};
Elm.Editor.Util.Convert.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Util = _elm.Editor.Util || {};
   _elm.Editor.Util.Convert = _elm.Editor.Util.Convert || {};
   if (_elm.Editor.Util.Convert.values)
   return _elm.Editor.Util.Convert.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm);
   var _op = {};
   var toFloat = function (str) {
      var result = $String.toFloat(str);
      var _p0 = result;
      if (_p0.ctor === "Ok") {
            return _p0._0;
         } else {
            return 0;
         }
   };
   var toInt = function (str) {
      var result = $String.toInt(str);
      var _p1 = result;
      if (_p1.ctor === "Ok") {
            return _p1._0;
         } else {
            return 0;
         }
   };
   return _elm.Editor.Util.Convert.values = {_op: _op
                                            ,toInt: toInt
                                            ,toFloat: toFloat};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.Ui = Elm.Editor.Ui || {};
Elm.Editor.Ui.PatternStage = Elm.Editor.Ui.PatternStage || {};
Elm.Editor.Ui.PatternStage.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.Ui = _elm.Editor.Ui || {};
   _elm.Editor.Ui.PatternStage = _elm.Editor.Ui.PatternStage || {};
   if (_elm.Editor.Ui.PatternStage.values)
   return _elm.Editor.Ui.PatternStage.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Types = Elm.Editor.Types.make(_elm),
   $Editor$Util$Svg = Elm.Editor.Util.Svg.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $String = Elm.String.make(_elm),
   $Svg = Elm.Svg.make(_elm),
   $Svg$Attributes = Elm.Svg.Attributes.make(_elm);
   var _op = {};
   var renderPath = function (_p0) {
      var _p1 = _p0;
      var _p5 = _p1.p1;
      var _p4 = _p1.color;
      var _p3 = _p1.c2;
      var _p2 = _p1.c1;
      var path = A2($String.join,
      " ",
      _U.list(["M"
              ,$Editor$Util$Svg.pointToString(_p5)
              ,"C"
              ,$Editor$Util$Svg.pointToString(_p3)
              ,$Editor$Util$Svg.pointToString(_p2)
              ,$Editor$Util$Svg.pointToString(_p1.p2)
              ,"C"
              ,$Editor$Util$Svg.pointToString(_p2)
              ,$Editor$Util$Svg.pointToString(_p3)
              ,$Editor$Util$Svg.pointToString(_p5)
              ,"Z"]));
      return A2($Svg.path,
      _U.list([$Svg$Attributes.d(path)
              ,$Svg$Attributes.strokeWidth($Basics.toString(_p1.strokeWidth))
              ,$Svg$Attributes.stroke(_p4)
              ,$Svg$Attributes.opacity("1")
              ,$Svg$Attributes.fill(_p4)
              ,$Svg$Attributes.$class("tile")]),
      _U.list([]));
   };
   var renderTile = function (tile) {
      return A2($Svg.g,_U.list([]),A2($List.map,renderPath,tile));
   };
   var renderColorizedNoisyTiles = function (tiles) {
      return A2($Svg.g,_U.list([]),A2($List.map,renderTile,tiles));
   };
   var stage = function (model) {
      return A2($Svg.svg,
      _U.list([$Svg$Attributes.version("1.1")
              ,$Svg$Attributes.x("0")
              ,$Svg$Attributes.y("0")]),
      _U.list([renderColorizedNoisyTiles(model)]));
   };
   var renderLine = function (_p6) {
      var _p7 = _p6;
      return A2($Svg.line,
      A3($List.foldl,
      $Editor$Util$Svg.lineToAttribute,
      _U.list([]),
      _p7._1),
      _U.list([]));
   };
   return _elm.Editor.Ui.PatternStage.values = {_op: _op
                                               ,renderLine: renderLine
                                               ,renderPath: renderPath
                                               ,renderTile: renderTile
                                               ,renderColorizedNoisyTiles: renderColorizedNoisyTiles
                                               ,stage: stage};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.View = Elm.Editor.View || {};
Elm.Editor.View.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.View = _elm.Editor.View || {};
   if (_elm.Editor.View.values) return _elm.Editor.View.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Action = Elm.Editor.Action.make(_elm),
   $Editor$Model = Elm.Editor.Model.make(_elm),
   $Editor$Ui$ColorFinder = Elm.Editor.Ui.ColorFinder.make(_elm),
   $Editor$Ui$GroupSelect = Elm.Editor.Ui.GroupSelect.make(_elm),
   $Editor$Ui$PatternStage = Elm.Editor.Ui.PatternStage.make(_elm),
   $Editor$Ui$Raster = Elm.Editor.Ui.Raster.make(_elm),
   $Editor$Ui$Slider = Elm.Editor.Ui.Slider.make(_elm),
   $Editor$Util$Convert = Elm.Editor.Util.Convert.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Html$Attributes = Elm.Html.Attributes.make(_elm),
   $Html$Events = Elm.Html.Events.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm);
   var _op = {};
   var view = F2(function (address,model) {
      var redoDisabled = _U.eq($List.length(model.redoStack),0);
      var undoDisabled = _U.eq($List.length(model.undoStack),0);
      var drawingState = model.drawingState;
      var patternState = model.patternState;
      return A2($Html.div,
      _U.list([$Html$Attributes.$class("row")
              ,$Html$Attributes.id("String")]),
      _U.list([A2($Html.div,
              _U.list([$Html$Attributes.$class("sidebar")]),
              _U.list([A2($Editor$Ui$GroupSelect.groupSelect,
                      patternState.groupType,
                      address)
                      ,A2($Editor$Ui$ColorFinder.colorFinder,address,model.colorState)
                      ,A5($Editor$Ui$Raster.raster,
                      drawingState,
                      patternState.tile,
                      patternState.previewGroup,
                      patternState.boundingBox,
                      address)
                      ,$Editor$Ui$Slider.slider({value: $Basics.toString(patternState.rasterSize)
                                                ,min: "1"
                                                ,max: "20"
                                                ,address: address
                                                ,createAction: function (str) {
                                                   return $Editor$Action.RasterSize($Editor$Util$Convert.toFloat(str));
                                                }})
                      ,$Editor$Ui$Slider.slider({value: $Basics.toString(patternState.columns)
                                                ,min: "1"
                                                ,max: "20"
                                                ,address: address
                                                ,createAction: function (str) {
                                                   return $Editor$Action.Columns($Editor$Util$Convert.toInt(str));
                                                }})
                      ,$Editor$Ui$Slider.slider({value: $Basics.toString(patternState.rows)
                                                ,min: "1"
                                                ,max: "20"
                                                ,address: address
                                                ,createAction: function (str) {
                                                   return $Editor$Action.Rows($Editor$Util$Convert.toInt(str));
                                                }})
                      ,$Editor$Ui$Slider.slider({value: $Basics.toString(patternState.noiseX)
                                                ,min: "1"
                                                ,max: "100"
                                                ,address: address
                                                ,createAction: function (str) {
                                                   return $Editor$Action.NoiseX($Editor$Util$Convert.toInt(str));
                                                }})
                      ,$Editor$Ui$Slider.slider({value: $Basics.toString(patternState.noiseY)
                                                ,min: "1"
                                                ,max: "100"
                                                ,address: address
                                                ,createAction: function (str) {
                                                   return $Editor$Action.NoiseY($Editor$Util$Convert.toInt(str));
                                                }})
                      ,$Editor$Ui$Slider.slider({value: $Basics.toString(patternState.noiseZ)
                                                ,min: "1"
                                                ,max: "100"
                                                ,address: address
                                                ,createAction: function (str) {
                                                   return $Editor$Action.NoiseZ($Editor$Util$Convert.toInt(str));
                                                }})
                      ,$Editor$Ui$Slider.slider({value: $Basics.toString(patternState.noiseDesctruction)
                                                ,min: "0"
                                                ,max: "100"
                                                ,address: address
                                                ,createAction: function (str) {
                                                   return $Editor$Action.NoiseDesctruction($Editor$Util$Convert.toInt(str));
                                                }})
                      ,A2($Html.button,
                      _U.list([A3($Html$Events.on,
                      "click",
                      $Html$Events.targetValue,
                      function (_p0) {
                         return A2($Signal.message,address,$Editor$Action.ClearTiles);
                      })]),
                      _U.list([$Html.text("Clear")]))
                      ,A2($Html.button,
                      _U.list([A3($Html$Events.on,
                      "click",
                      $Html$Events.targetValue,
                      function (_p1) {
                         return A2($Signal.message,address,$Editor$Action.Random);
                      })]),
                      _U.list([$Html.text("Random")]))
                      ,A2($Html.button,
                      _U.list([A3($Html$Events.on,
                              "click",
                              $Html$Events.targetValue,
                              function (_p2) {
                                 return A2($Signal.message,address,$Editor$Action.Undo);
                              })
                              ,$Html$Attributes.disabled(undoDisabled)]),
                      _U.list([$Html.text("Undo")]))
                      ,A2($Html.button,
                      _U.list([A3($Html$Events.on,
                              "click",
                              $Html$Events.targetValue,
                              function (_p3) {
                                 return A2($Signal.message,address,$Editor$Action.Redo);
                              })
                              ,$Html$Attributes.disabled(redoDisabled)]),
                      _U.list([$Html.text("Redo")]))]))
              ,A2($Html.div,
              _U.list([$Html$Attributes.$class("main")]),
              _U.list([$Editor$Ui$PatternStage.stage(model.patternState.pattern)]))]));
   });
   return _elm.Editor.View.values = {_op: _op,view: view};
};
Elm.Editor = Elm.Editor || {};
Elm.Editor.App = Elm.Editor.App || {};
Elm.Editor.App.make = function (_elm) {
   "use strict";
   _elm.Editor = _elm.Editor || {};
   _elm.Editor.App = _elm.Editor.App || {};
   if (_elm.Editor.App.values) return _elm.Editor.App.values;
   var _U = Elm.Native.Utils.make(_elm),
   $Basics = Elm.Basics.make(_elm),
   $Debug = Elm.Debug.make(_elm),
   $Editor$Action = Elm.Editor.Action.make(_elm),
   $Editor$Model = Elm.Editor.Model.make(_elm),
   $Editor$Signals = Elm.Editor.Signals.make(_elm),
   $Editor$View = Elm.Editor.View.make(_elm),
   $Effects = Elm.Effects.make(_elm),
   $Html = Elm.Html.make(_elm),
   $Json$Decode = Elm.Json.Decode.make(_elm),
   $List = Elm.List.make(_elm),
   $Maybe = Elm.Maybe.make(_elm),
   $Result = Elm.Result.make(_elm),
   $Signal = Elm.Signal.make(_elm),
   $StartApp = Elm.StartApp.make(_elm),
   $Task = Elm.Task.make(_elm);
   var _op = {};
   var palettes = $Json$Decode.list(A2($Json$Decode._op[":="],
   "colors",
   $Json$Decode.list($Json$Decode.string)));
   var responseColors = Elm.Native.Port.make(_elm).inboundSignal("responseColors",
   "Json.Decode.Value",
   function (v) {
      return v;
   });
   var responseColor = A2($Signal.map,
   function (_p0) {
      return $Editor$Action.NewColors(A2($Json$Decode.decodeValue,
      palettes,
      _p0));
   },
   responseColors);
   var request = Elm.Native.Port.make(_elm).outboundSignal("request",
   function (v) {
      return v;
   },
   $Editor$Signals.requestPaletteFilter);
   var app = $StartApp.start({init: {ctor: "_Tuple2"
                                    ,_0: $Editor$Model.initialModel
                                    ,_1: $Effects.none}
                             ,update: $Editor$Action.update
                             ,view: $Editor$View.view
                             ,inputs: _U.list([responseColor])});
   var main = app.html;
   var tasks = Elm.Native.Task.make(_elm).performSignal("tasks",
   app.tasks);
   return _elm.Editor.App.values = {_op: _op,main: main,app: app};
};
