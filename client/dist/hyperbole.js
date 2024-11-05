/******/ (() => { // webpackBootstrap
/******/ 	var __webpack_modules__ = ({

/***/ "./node_modules/debounce/index.js":
/*!****************************************!*\
  !*** ./node_modules/debounce/index.js ***!
  \****************************************/
/***/ ((module) => {

function debounce(function_, wait = 100, options = {}) {
	if (typeof function_ !== 'function') {
		throw new TypeError(`Expected the first parameter to be a function, got \`${typeof function_}\`.`);
	}

	if (wait < 0) {
		throw new RangeError('`wait` must not be negative.');
	}

	// TODO: Deprecate the boolean parameter at some point.
	const {immediate} = typeof options === 'boolean' ? {immediate: options} : options;

	let storedContext;
	let storedArguments;
	let timeoutId;
	let timestamp;
	let result;

	function run() {
		const callContext = storedContext;
		const callArguments = storedArguments;
		storedContext = undefined;
		storedArguments = undefined;
		result = function_.apply(callContext, callArguments);
		return result;
	}

	function later() {
		const last = Date.now() - timestamp;

		if (last < wait && last >= 0) {
			timeoutId = setTimeout(later, wait - last);
		} else {
			timeoutId = undefined;

			if (!immediate) {
				result = run();
			}
		}
	}

	const debounced = function (...arguments_) {
		if (
			storedContext
			&& this !== storedContext
			&& Object.getPrototypeOf(this) === Object.getPrototypeOf(storedContext)
		) {
			throw new Error('Debounced method called with different contexts of the same prototype.');
		}

		storedContext = this; // eslint-disable-line unicorn/no-this-assignment
		storedArguments = arguments_;
		timestamp = Date.now();

		const callNow = immediate && !timeoutId;

		if (!timeoutId) {
			timeoutId = setTimeout(later, wait);
		}

		if (callNow) {
			result = run();
		}

		return result;
	};

	Object.defineProperty(debounced, 'isPending', {
		get() {
			return timeoutId !== undefined;
		},
	});

	debounced.clear = () => {
		if (!timeoutId) {
			return;
		}

		clearTimeout(timeoutId);
		timeoutId = undefined;
	};

	debounced.flush = () => {
		if (!timeoutId) {
			return;
		}

		debounced.trigger();
	};

	debounced.trigger = () => {
		result = run();

		debounced.clear();
	};

	return debounced;
}

// Adds compatibility for ES modules
module.exports.debounce = debounce;

module.exports = debounce;


/***/ }),

/***/ "./src/action.ts":
/*!***********************!*\
  !*** ./src/action.ts ***!
  \***********************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   actionMessage: () => (/* binding */ actionMessage),
/* harmony export */   actionUrl: () => (/* binding */ actionUrl),
/* harmony export */   toSearch: () => (/* binding */ toSearch)
/* harmony export */ });
function actionUrl(id, action) {
    var url = new URL(window.location.href);
    url.searchParams.append("id", id);
    url.searchParams.append("action", action);
    return url;
}
function toSearch(form) {
    if (!form)
        return undefined;
    var params = new URLSearchParams();
    form.forEach(function (value, key) {
        params.append(key, value);
    });
    return params;
}
function actionMessage(id, action, form) {
    var url = actionUrl(id, action);
    return { id: id, url: url, form: toSearch(form) };
}


/***/ }),

/***/ "./src/events.ts":
/*!***********************!*\
  !*** ./src/events.ts ***!
  \***********************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   listenChange: () => (/* binding */ listenChange),
/* harmony export */   listenClick: () => (/* binding */ listenClick),
/* harmony export */   listenFormSubmit: () => (/* binding */ listenFormSubmit),
/* harmony export */   listenInput: () => (/* binding */ listenInput),
/* harmony export */   listenLoad: () => (/* binding */ listenLoad),
/* harmony export */   listenLoadDocument: () => (/* binding */ listenLoadDocument)
/* harmony export */ });
/* harmony import */ var debounce__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! debounce */ "./node_modules/debounce/index.js");
/* harmony import */ var debounce__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(debounce__WEBPACK_IMPORTED_MODULE_0__);

function listenClick(cb) {
    document.addEventListener("click", function (e) {
        var el = e.target;
        // clicks can fire on internal elements. Find the parent with a click handler
        var source = el.closest("[data-on-click]");
        // console.log("CLICK", source?.dataset.onClick)
        // they should all have an action and target
        if ((source === null || source === void 0 ? void 0 : source.dataset.onClick) && (source === null || source === void 0 ? void 0 : source.dataset.target)) {
            e.preventDefault();
            var target = document.getElementById(source.dataset.target);
            if (!target) {
                console.error("Missing target: ", source.dataset.target);
                return;
            }
            cb(target, source.dataset.onClick);
        }
    });
}
function listenLoadDocument(cb) {
    document.addEventListener("hyp-load", function (e) {
        var load = e.target;
        var action = load.dataset.onLoad;
        var target = document.getElementById(load.dataset.target);
        if (!target) {
            console.error("Missing load target: ", target);
            return;
        }
        cb(target, action);
    });
}
function listenLoad(node) {
    // it doesn't really matter WHO runs this except that it should have target
    node.querySelectorAll("[data-on-load]").forEach(function (load) {
        var delay = parseInt(load.dataset.delay) || 0;
        setTimeout(function () {
            var event = new Event("hyp-load", { bubbles: true });
            load.dispatchEvent(event);
        }, delay);
    });
}
function listenChange(cb) {
    document.addEventListener("change", function (e) {
        var el = e.target;
        // clicks can fire on internal elements. Find the parent with a click handler
        var source = el.closest("[data-on-change]");
        // they should all have an action and target
        if ((source === null || source === void 0 ? void 0 : source.dataset.target) && source.value) {
            e.preventDefault();
            var target = document.getElementById(source.dataset.target);
            if (!target) {
                console.error("Missing target: ", source.dataset.target);
                return;
            }
            cb(target, source.value);
        }
    });
}
function listenInput(cb) {
    document.addEventListener("input", function (e) {
        var el = e.target;
        var source = el.closest("[data-on-input]");
        var delay = parseInt(source.dataset.delay) || 100;
        if (delay < 100) {
            console.warn("Input delay < 100 can result in poor performance");
        }
        if ((source === null || source === void 0 ? void 0 : source.dataset.onInput) && (source === null || source === void 0 ? void 0 : source.dataset.target)) {
            e.preventDefault();
            var target_1 = document.getElementById(source.dataset.target);
            if (!target_1) {
                console.error("Missing target: ", source.dataset.target);
                return;
            }
            if (!source.debouncedCallback) {
                source.debouncedCallback = debounce__WEBPACK_IMPORTED_MODULE_0__(function () { return cb(target_1, source.dataset.onInput, source.value); }, delay);
            }
            source.debouncedCallback();
        }
    });
}
function listenFormSubmit(cb) {
    document.addEventListener("submit", function (e) {
        var form = e.target;
        // they should all have an action and target
        if ((form === null || form === void 0 ? void 0 : form.dataset.onSubmit) && (form === null || form === void 0 ? void 0 : form.dataset.target)) {
            e.preventDefault();
            var target = document.getElementById(form.dataset.target);
            if (!target) {
                console.error("Missing target: ", form.dataset.target);
                return;
            }
            var formData = new FormData(form);
            cb(target, form.dataset.onSubmit, formData);
        }
    });
}


/***/ }),

/***/ "./src/lib.ts":
/*!********************!*\
  !*** ./src/lib.ts ***!
  \********************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   dropWhile: () => (/* binding */ dropWhile),
/* harmony export */   takeWhileMap: () => (/* binding */ takeWhileMap)
/* harmony export */ });
function takeWhileMap(pred, lines) {
    var output = [];
    for (var _i = 0, lines_1 = lines; _i < lines_1.length; _i++) {
        var line = lines_1[_i];
        var a = pred(line);
        if (a)
            output.push(a);
        else
            break;
    }
    return output;
}
function dropWhile(pred, lines) {
    var index = 0;
    while (index < lines.length && pred(lines[index])) {
        index++;
    }
    return lines.slice(index);
}


/***/ }),

/***/ "./src/sockets.ts":
/*!************************!*\
  !*** ./src/sockets.ts ***!
  \************************/
/***/ ((__unused_webpack_module, __webpack_exports__, __webpack_require__) => {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   SocketConnection: () => (/* binding */ SocketConnection)
/* harmony export */ });
/* harmony import */ var _lib__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! ./lib */ "./src/lib.ts");
var __awaiter = (undefined && undefined.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (undefined && undefined.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};

var protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
var defaultAddress = "".concat(protocol, "//").concat(window.location.host).concat(window.location.pathname);
var SocketConnection = /** @class */ (function () {
    function SocketConnection() {
        this.reconnectDelay = 0;
    }
    // we need to faithfully transmit the 
    SocketConnection.prototype.connect = function (addr) {
        var _this = this;
        if (addr === void 0) { addr = defaultAddress; }
        var sock = new WebSocket(addr);
        this.socket = sock;
        function onConnectError(ev) {
            console.log("Connection Error", ev);
        }
        sock.addEventListener('error', onConnectError);
        sock.addEventListener('open', function (event) {
            console.log("Opened", event);
            _this.isConnected = true;
            _this.hasEverConnected = true;
            _this.reconnectDelay = 0;
            _this.socket.removeEventListener('error', onConnectError);
        });
        // TODO: Don't reconnet if the socket server is OFF, only if we've successfully connected once
        sock.addEventListener('close', function (_) {
            _this.isConnected = false;
            console.log("Closed");
            // attempt to reconnect in 1s
            if (_this.hasEverConnected) {
                _this.reconnectDelay += 1000;
                console.log("Reconnecting in " + (_this.reconnectDelay / 1000) + "s");
                setTimeout(function () { return _this.connect(addr); }, _this.reconnectDelay);
            }
        });
    };
    SocketConnection.prototype.sendAction = function (action) {
        return __awaiter(this, void 0, void 0, function () {
            var msg, _a, metadata, rest;
            return __generator(this, function (_b) {
                switch (_b.label) {
                    case 0:
                        msg = [action.url.pathname + action.url.search,
                            "Host: " + window.location.host,
                            "Cookie: " + document.cookie,
                            action.form
                        ].join("\n");
                        return [4 /*yield*/, this.fetch(action.id, msg)];
                    case 1:
                        _a = _b.sent(), metadata = _a.metadata, rest = _a.rest;
                        return [2 /*return*/, { rest: rest, metadata: metadata }];
                }
            });
        });
    };
    SocketConnection.prototype.fetch = function (id, msg) {
        return __awaiter(this, void 0, void 0, function () {
            var res;
            return __generator(this, function (_a) {
                switch (_a.label) {
                    case 0:
                        this.sendMessage(msg);
                        return [4 /*yield*/, this.waitMessage(id)];
                    case 1:
                        res = _a.sent();
                        return [2 /*return*/, res];
                }
            });
        });
    };
    SocketConnection.prototype.sendMessage = function (msg) {
        this.socket.send(msg);
    };
    SocketConnection.prototype.waitMessage = function (id) {
        return __awaiter(this, void 0, void 0, function () {
            var _this = this;
            return __generator(this, function (_a) {
                return [2 /*return*/, new Promise(function (resolve, reject) {
                        var onMessage = function (event) {
                            var data = event.data;
                            var _a = parseMetadataResponse(data), metadata = _a.metadata, rest = _a.rest;
                            if (metadata.error) {
                                throw socketError(metadata.error);
                            }
                            if (metadata.redirect) {
                                window.location.href = metadata.redirect;
                                return;
                            }
                            if (metadata.session) {
                                // console.log("setting cookie", metadata.session)
                                document.cookie = metadata.session;
                            }
                            if (metadata.viewId != id) {
                                // console.warn("Mismatched ids, ignoring", metadata.viewId, id)
                                return;
                            }
                            resolve({ metadata: metadata, rest: rest });
                            // we should only resolve if we have found our message
                            _this.socket.removeEventListener('message', onMessage);
                        };
                        _this.socket.addEventListener('message', onMessage);
                        _this.socket.addEventListener('error', reject);
                    })];
            });
        });
    };
    SocketConnection.prototype.disconnect = function () {
        this.socket.close();
    };
    return SocketConnection;
}());

function socketError(inp) {
    var error = new Error();
    error.name = inp.substring(0, inp.indexOf(' '));
    error.message = inp.substring(inp.indexOf(' ') + 1);
    return error;
}
console.log("CONNECTING", window.location);
function parseMetadataResponse(ret) {
    var lines = ret.split("\n");
    var metas = (0,_lib__WEBPACK_IMPORTED_MODULE_0__.takeWhileMap)(parseMeta, lines);
    var rest = (0,_lib__WEBPACK_IMPORTED_MODULE_0__.dropWhile)(parseMeta, lines).join("\n");
    return {
        metadata: parseMetas(metas),
        rest: rest
    };
    function parseMeta(line) {
        var match = line.match(/^\|([A-Z\-]+)\|(.*)$/);
        if (match) {
            return {
                key: match[1],
                value: match[2]
            };
        }
    }
}
function parseMetas(meta) {
    var _a, _b, _c, _d, _e;
    return {
        session: (_a = meta.find(function (m) { return m.key == "SESSION"; })) === null || _a === void 0 ? void 0 : _a.value,
        redirect: (_b = meta.find(function (m) { return m.key == "REDIRECT"; })) === null || _b === void 0 ? void 0 : _b.value,
        error: (_c = meta.find(function (m) { return m.key == "ERROR"; })) === null || _c === void 0 ? void 0 : _c.value,
        viewId: (_d = meta.find(function (m) { return m.key == "VIEW-ID"; })) === null || _d === void 0 ? void 0 : _d.value,
        triggers: (_e = meta.filter(function (m) { return m.key == "TRIGGER"; })) === null || _e === void 0 ? void 0 : _e.map(parseTrigger)
    };
}
function parseTrigger(meta) {
    var _a = meta.value.split("|"), view = _a[0], action = _a[1];
    return { view: view, action: action };
}


/***/ }),

/***/ "./node_modules/omdomdom/lib/omdomdom.es.js":
/*!**************************************************!*\
  !*** ./node_modules/omdomdom/lib/omdomdom.es.js ***!
  \**************************************************/
/***/ ((__unused_webpack___webpack_module__, __webpack_exports__, __webpack_require__) => {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export */ __webpack_require__.d(__webpack_exports__, {
/* harmony export */   create: () => (/* binding */ create),
/* harmony export */   patch: () => (/* binding */ patch),
/* harmony export */   render: () => (/* binding */ render)
/* harmony export */ });
/*!
  * @license MIT (https://github.com/geotrev/omdomdom/blob/master/LICENSE)
  * Omdomdom v0.3.2 (https://github.com/geotrev/omdomdom)
  * Copyright 2023 George Treviranus <geowtrev@gmail.com>
  */
var DATA_KEY_ATTRIBUTE$1 = "data-key";
var hasProperty = function hasProperty(obj, prop) {
  return Object.prototype.hasOwnProperty.call(obj, prop);
};
var keyIsValid = function keyIsValid(map, key) {
  if (!key) return false;
  if (hasProperty(map, key)) {
    console.warn("[OmDomDom]: Children with duplicate keys detected. Children with duplicate keys will be skipped, resulting in dropped node references. Keys must be unique and non-indexed.");
    return false;
  }
  return true;
};
var forEach = function forEach(items, fn) {
  var length = items.length;
  var idx = -1;
  if (!length) return;
  while (++idx < length) {
    if (fn(items[idx], idx) === false) break;
  }
};
var createKeyMap = function createKeyMap(children) {
  var map = {};
  forEach(children, function (child) {
    var key = child.attributes[DATA_KEY_ATTRIBUTE$1];
    if (keyIsValid(map, key)) {
      map[key] = child;
    }
  });
  for (var _ in map) {
    return map;
  }
};
var insertBefore = function insertBefore(parent, child, refNode) {
  return parent.node.insertBefore(child.node, refNode);
};
var assignVNode = function assignVNode(template, vNode) {
  for (var property in template) {
    vNode[property] = template[property];
  }
};

var toHTML = function toHTML(htmlString) {
  var processedDOMString = htmlString.trim().replace(/\s+</g, "<").replace(/>\s+/g, ">");
  var parser = new DOMParser();
  var context = parser.parseFromString(processedDOMString, "text/html");
  return context.body;
};

function _iterableToArrayLimit(arr, i) {
  var _i = null == arr ? null : "undefined" != typeof Symbol && arr[Symbol.iterator] || arr["@@iterator"];
  if (null != _i) {
    var _s,
      _e,
      _x,
      _r,
      _arr = [],
      _n = !0,
      _d = !1;
    try {
      if (_x = (_i = _i.call(arr)).next, 0 === i) {
        if (Object(_i) !== _i) return;
        _n = !1;
      } else for (; !(_n = (_s = _x.call(_i)).done) && (_arr.push(_s.value), _arr.length !== i); _n = !0);
    } catch (err) {
      _d = !0, _e = err;
    } finally {
      try {
        if (!_n && null != _i.return && (_r = _i.return(), Object(_r) !== _r)) return;
      } finally {
        if (_d) throw _e;
      }
    }
    return _arr;
  }
}
function _slicedToArray(arr, i) {
  return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest();
}
function _arrayWithHoles(arr) {
  if (Array.isArray(arr)) return arr;
}
function _unsupportedIterableToArray(o, minLen) {
  if (!o) return;
  if (typeof o === "string") return _arrayLikeToArray(o, minLen);
  var n = Object.prototype.toString.call(o).slice(8, -1);
  if (n === "Object" && o.constructor) n = o.constructor.name;
  if (n === "Map" || n === "Set") return Array.from(o);
  if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen);
}
function _arrayLikeToArray(arr, len) {
  if (len == null || len > arr.length) len = arr.length;
  for (var i = 0, arr2 = new Array(len); i < len; i++) arr2[i] = arr[i];
  return arr2;
}
function _nonIterableRest() {
  throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}

var Types = {
  STRING: "string",
  INT: "number",
  BOOL: "boolean"
};
var DomProperties = {};
var createRecord = function createRecord(attrName, propName, type) {
  return {
    attrName: attrName,
    propName: propName,
    type: type
  };
};
var stringProps = [["style", "cssText"], ["class", "className"]];
forEach(stringProps, function (_ref) {
  var _ref2 = _slicedToArray(_ref, 2),
    attr = _ref2[0],
    property = _ref2[1];
  DomProperties[attr] = createRecord(attr, property || attr, Types.STRING);
});
var booleanProps = ["autofocus", "draggable", "hidden", "checked", "multiple", "muted", "selected"];
forEach(booleanProps, function (attr) {
  DomProperties[attr] = createRecord(attr, attr, Types.BOOL);
});
var integerProps = [["tabindex", "tabIndex"]];
forEach(integerProps, function (_ref3) {
  var _ref4 = _slicedToArray(_ref3, 2),
    attr = _ref4[0],
    property = _ref4[1];
  DomProperties[attr] = createRecord(attr, property, Types.INT);
});
var Namespace = {
  xlink: {
    prefix: "xlink:",
    resource: "http://www.w3.org/1999/xlink"
  },
  xml: {
    prefix: "xml:",
    resource: "http://www.w3.org/XML/1998/namespace"
  }
};

var setProperty = function setProperty(element, type, prop, value) {
  switch (type) {
    case Types.STRING:
      {
        if (prop === DomProperties.style.propName) {
          if (value === null) {
            element.style[prop] = "";
          } else {
            element.style[prop] = value;
          }
        } else if (value === null) {
          element[prop] = "";
        } else {
          element[prop] = value;
        }
        break;
      }
    case Types.INT:
      {
        if (value === null) {
          var attr = prop.toLowerCase();
          element.removeAttribute(attr);
        } else if (value === "0") {
          element[prop] = 0;
        } else if (value === "-1") {
          element[prop] = -1;
        } else {
          var parsed = parseInt(value, 10);
          if (!isNaN(parsed)) {
            element[prop] = parsed;
          }
        }
        break;
      }
    case Types.BOOL:
      {
        if (["", "true"].indexOf(value) < 0) {
          element[prop] = false;
        } else {
          element[prop] = true;
        }
        break;
      }
  }
};

var removeAttributes = function removeAttributes(vNode, attributes) {
  forEach(attributes, function (attrName) {
    if (hasProperty(DomProperties, attrName)) {
      var propertyRecord = DomProperties[attrName];
      setProperty(vNode.node, propertyRecord.type, propertyRecord.propName, null);
    } else {
      if (attrName in vNode.node) {
        setProperty(vNode.node, Types.STRING, attrName, null);
      }
      vNode.node.removeAttribute(attrName);
    }
    delete vNode.attributes[attrName];
  });
};
var setAttributes = function setAttributes(vNode, attributes) {
  for (var attrName in attributes) {
    var value = attributes[attrName];
    vNode.attributes[attrName] = value;
    if (hasProperty(DomProperties, attrName)) {
      var propertyRecord = DomProperties[attrName];
      setProperty(vNode.node, propertyRecord.type, propertyRecord.propName, value);
      continue;
    }
    if (attrName.startsWith(Namespace.xlink.prefix)) {
      vNode.node.setAttributeNS(Namespace.xlink.resource, attrName, value);
      continue;
    }
    if (attrName.startsWith(Namespace.xml.prefix)) {
      vNode.node.setAttributeNS(Namespace.xml.resource, attrName, value);
      continue;
    }
    if (attrName in vNode.node) {
      setProperty(vNode.node, Types.STRING, attrName, value);
    }
    vNode.node.setAttribute(attrName, value || "");
  }
};
var getPropertyValues = function getPropertyValues(element, attributes) {
  for (var attrName in DomProperties) {
    var propertyRecord = DomProperties[attrName];
    var propName = propertyRecord.propName;
    var attrValue = element.getAttribute(attrName);
    if (attrName === DomProperties.style.attrName) {
      attributes[attrName] = element.style[propName];
    } else if (typeof attrValue === "string") {
      attributes[attrName] = attrValue;
    }
  }
};
var getBaseAttributes = function getBaseAttributes(element) {
  return Array.prototype.reduce.call(element.attributes, function (attributes, attrName) {
    if (!hasProperty(DomProperties, attrName.name)) {
      attributes[attrName.name] = attrName.value;
    }
    return attributes;
  }, {});
};
var getAttributes = function getAttributes(element) {
  var attributes = getBaseAttributes(element);
  getPropertyValues(element, attributes);
  return attributes;
};
var updateAttributes = function updateAttributes(template, vNode) {
  var removedAttributes = [];
  var changedAttributes = {};
  for (var attrName in vNode.attributes) {
    var oldValue = vNode.attributes[attrName];
    var nextValue = template.attributes[attrName];
    if (oldValue === nextValue) continue;
    if (typeof nextValue === "undefined") {
      removedAttributes.push(attrName);
    }
  }
  for (var _attrName in template.attributes) {
    var _oldValue = vNode.attributes[_attrName];
    var _nextValue = template.attributes[_attrName];
    if (_oldValue === _nextValue) continue;
    if (typeof _nextValue !== "undefined") {
      changedAttributes[_attrName] = _nextValue;
    }
  }
  removeAttributes(vNode, removedAttributes);
  setAttributes(vNode, changedAttributes);
};

var DATA_KEY_ATTRIBUTE = "data-key";
function patchChildren(template, vNode, patch) {
  var templateChildrenLength = template.children.length;
  var vNodeChildrenLength = vNode.children.length;
  if (!templateChildrenLength && !vNodeChildrenLength) return;
  var vNodeKeyMap = createKeyMap(vNode.children);
  var nextChildren = Array(templateChildrenLength);
  if (vNodeKeyMap !== undefined) {
    forEach(template.children, function (templateChild, idx) {
      var childNodes = vNode.node.childNodes;
      var key = templateChild.attributes[DATA_KEY_ATTRIBUTE];
      if (Object.prototype.hasOwnProperty.call(vNodeKeyMap, key)) {
        var keyedChild = vNodeKeyMap[key];
        if (Array.prototype.indexOf.call(childNodes, keyedChild.node) !== idx) {
          insertBefore(vNode, keyedChild, childNodes[idx]);
        }
        nextChildren[idx] = keyedChild;
        delete vNodeKeyMap[key];
        patch(templateChild, nextChildren[idx]);
      } else {
        insertBefore(vNode, templateChild, childNodes[idx]);
        nextChildren[idx] = templateChild;
      }
    });
  } else {
    forEach(template.children, function (templateChild, idx) {
      var vNodeChild = vNode.children[idx];
      if (typeof vNodeChild !== "undefined") {
        patch(templateChild, vNodeChild);
        nextChildren[idx] = vNodeChild;
      } else {
        vNode.node.appendChild(templateChild.node);
        nextChildren[idx] = templateChild;
      }
    });
  }
  vNode.children = nextChildren;
  var childNodesLength = vNode.node.childNodes.length;
  var delta = childNodesLength - templateChildrenLength;
  if (delta > 0) {
    while (delta > 0) {
      vNode.node.removeChild(vNode.node.childNodes[childNodesLength - 1]);
      childNodesLength--;
      delta--;
    }
  }
}

var patch = function patch(template, vNode, rootNode) {
  if (!template || !vNode) return;
  rootNode = rootNode || vNode.node.parentNode;
  var contentChanged = template.content && template.content !== vNode.content;
  if (template.type !== vNode.type || contentChanged) {
    rootNode.replaceChild(template.node, vNode.node);
    return assignVNode(template, vNode);
  }
  updateAttributes(template, vNode);
  patchChildren(template, vNode, patch);
};
var render = function render(vNode, root) {
  root.appendChild(vNode.node);
};
var create = function create(node) {
  var isSVGContext = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;
  if (typeof node === "string") {
    node = toHTML(node);
  }
  var isRoot = node.tagName === "BODY";
  var childNodes = node.childNodes;
  var numChildNodes = childNodes ? childNodes.length : 0;
  if (isRoot) {
    if (numChildNodes > 1) {
      throw new Error("[OmDomDom]: Your element should not have more than one root node.");
    } else if (numChildNodes === 0) {
      throw new Error("[OmDomDom]: Your element should have at least one root node.");
    } else {
      return create(childNodes[0]);
    }
  }
  var type = node.nodeType === 3 ? "text" : node.nodeType === 8 ? "comment" : node.tagName.toLowerCase();
  var isSVG = isSVGContext || type === "svg";
  var attributes = node.nodeType === 1 ? getAttributes(node) : {};
  var content = numChildNodes > 0 ? null : node.textContent;
  var children = Array(numChildNodes);
  forEach(childNodes, function (child, idx) {
    children[idx] = create(child, isSVG);
  });
  return {
    type: type,
    attributes: attributes,
    children: children,
    content: content,
    node: node,
    isSVGContext: isSVG
  };
};


//# sourceMappingURL=omdomdom.es.js.map


/***/ })

/******/ 	});
/************************************************************************/
/******/ 	// The module cache
/******/ 	var __webpack_module_cache__ = {};
/******/ 	
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/ 		// Check if module is in cache
/******/ 		var cachedModule = __webpack_module_cache__[moduleId];
/******/ 		if (cachedModule !== undefined) {
/******/ 			return cachedModule.exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = __webpack_module_cache__[moduleId] = {
/******/ 			// no module.id needed
/******/ 			// no module.loaded needed
/******/ 			exports: {}
/******/ 		};
/******/ 	
/******/ 		// Execute the module function
/******/ 		__webpack_modules__[moduleId](module, module.exports, __webpack_require__);
/******/ 	
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/ 	
/************************************************************************/
/******/ 	/* webpack/runtime/compat get default export */
/******/ 	(() => {
/******/ 		// getDefaultExport function for compatibility with non-harmony modules
/******/ 		__webpack_require__.n = (module) => {
/******/ 			var getter = module && module.__esModule ?
/******/ 				() => (module['default']) :
/******/ 				() => (module);
/******/ 			__webpack_require__.d(getter, { a: getter });
/******/ 			return getter;
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/define property getters */
/******/ 	(() => {
/******/ 		// define getter functions for harmony exports
/******/ 		__webpack_require__.d = (exports, definition) => {
/******/ 			for(var key in definition) {
/******/ 				if(__webpack_require__.o(definition, key) && !__webpack_require__.o(exports, key)) {
/******/ 					Object.defineProperty(exports, key, { enumerable: true, get: definition[key] });
/******/ 				}
/******/ 			}
/******/ 		};
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/hasOwnProperty shorthand */
/******/ 	(() => {
/******/ 		__webpack_require__.o = (obj, prop) => (Object.prototype.hasOwnProperty.call(obj, prop))
/******/ 	})();
/******/ 	
/******/ 	/* webpack/runtime/make namespace object */
/******/ 	(() => {
/******/ 		// define __esModule on exports
/******/ 		__webpack_require__.r = (exports) => {
/******/ 			if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 				Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 			}
/******/ 			Object.defineProperty(exports, '__esModule', { value: true });
/******/ 		};
/******/ 	})();
/******/ 	
/************************************************************************/
var __webpack_exports__ = {};
// This entry need to be wrapped in an IIFE because it need to be in strict mode.
(() => {
"use strict";
/*!**********************!*\
  !*** ./src/index.ts ***!
  \**********************/
__webpack_require__.r(__webpack_exports__);
/* harmony import */ var omdomdom_lib_omdomdom_es_js__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! omdomdom/lib/omdomdom.es.js */ "./node_modules/omdomdom/lib/omdomdom.es.js");
/* harmony import */ var _sockets__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! ./sockets */ "./src/sockets.ts");
/* harmony import */ var _events__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! ./events */ "./src/events.ts");
/* harmony import */ var _action__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ./action */ "./src/action.ts");
var __awaiter = (undefined && undefined.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
var __generator = (undefined && undefined.__generator) || function (thisArg, body) {
    var _ = { label: 0, sent: function() { if (t[0] & 1) throw t[1]; return t[1]; }, trys: [], ops: [] }, f, y, t, g;
    return g = { next: verb(0), "throw": verb(1), "return": verb(2) }, typeof Symbol === "function" && (g[Symbol.iterator] = function() { return this; }), g;
    function verb(n) { return function (v) { return step([n, v]); }; }
    function step(op) {
        if (f) throw new TypeError("Generator is already executing.");
        while (g && (g = 0, op[0] && (_ = 0)), _) try {
            if (f = 1, y && (t = op[0] & 2 ? y["return"] : op[0] ? y["throw"] || ((t = y["return"]) && t.call(y), 0) : y.next) && !(t = t.call(y, op[1])).done) return t;
            if (y = 0, t) op = [op[0] & 2, t.value];
            switch (op[0]) {
                case 0: case 1: t = op; break;
                case 4: _.label++; return { value: op[1], done: false };
                case 5: _.label++; y = op[1]; op = [0]; continue;
                case 7: op = _.ops.pop(); _.trys.pop(); continue;
                default:
                    if (!(t = _.trys, t = t.length > 0 && t[t.length - 1]) && (op[0] === 6 || op[0] === 2)) { _ = 0; continue; }
                    if (op[0] === 3 && (!t || (op[1] > t[0] && op[1] < t[3]))) { _.label = op[1]; break; }
                    if (op[0] === 6 && _.label < t[1]) { _.label = t[1]; t = op; break; }
                    if (t && _.label < t[2]) { _.label = t[2]; _.ops.push(op); break; }
                    if (t[2]) _.ops.pop();
                    _.trys.pop(); continue;
            }
            op = body.call(thisArg, _);
        } catch (e) { op = [6, e]; y = 0; } finally { f = t = 0; }
        if (op[0] & 5) throw op[1]; return { value: op[0] ? op[1] : void 0, done: true };
    }
};




// import { listenEvents } from './events';
// import { WEBSOCKET_ADDRESS, Messages } from './Messages'
// import { INIT_PAGE, INIT_STATE, State, Class } from './types';
// import { fromVDOM, VDOM } from './vdom'
// const CONTENT_ID = "yeti-root-content"
// console.log("VERSION 2", INIT_PAGE, INIT_STATE)
console.log("Hyperbole 0.4.1a");
var rootStyles;
var addedRulesIndex = new Set();
function sendAction(msg) {
    return __awaiter(this, void 0, void 0, function () {
        function sendActionHttp(msg) {
            return __awaiter(this, void 0, void 0, function () {
                var res, error, body;
                return __generator(this, function (_a) {
                    switch (_a.label) {
                        case 0:
                            console.log("HTTP sendAction", msg.url.toString());
                            return [4 /*yield*/, fetch(msg.url, {
                                    method: "POST",
                                    headers: { 'Accept': 'text/html', 'Content-Type': 'application/x-www-form-urlencoded' },
                                    body: msg.form,
                                    // we never want this to be redirected
                                    redirect: "manual"
                                })];
                        case 1:
                            res = _a.sent();
                            if (res.headers.get('location')) {
                                // manual redirect with status 200
                                console.log("Found Redirect", res.headers.get('location'));
                                window.location.href = res.headers.get('location');
                                return [2 /*return*/];
                            }
                            console.log("RES", res.headers.get("location"));
                            if (res.headers.get("location")) {
                                window.location.href = res.headers.get("location");
                                return [2 /*return*/];
                            }
                            if (!!res.ok) return [3 /*break*/, 3];
                            error = new Error();
                            error.name = "Fetch Error " + res.status;
                            return [4 /*yield*/, res.text()];
                        case 2:
                            body = _a.sent();
                            error.message = body;
                            throw error;
                        case 3: return [2 /*return*/, res.text()];
                    }
                });
            });
        }
        return __generator(this, function (_a) {
            if (sock.isConnected) {
                return [2 /*return*/, sock.sendAction(msg)];
            }
            else {
                return [2 /*return*/, sendActionHttp(msg)];
            }
            return [2 /*return*/];
        });
    });
}
function fetchAction(msg) {
    return __awaiter(this, void 0, void 0, function () {
        var ret, err_1;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    _a.trys.push([0, 2, , 3]);
                    return [4 /*yield*/, sendAction(msg)];
                case 1:
                    ret = _a.sent();
                    return [2 /*return*/, ret];
                case 2:
                    err_1 = _a.sent();
                    // handle error here
                    document.body.innerHTML = errorHTML(err_1);
                    throw err_1;
                case 3: return [2 /*return*/];
            }
        });
    });
}
function runAction(target, action, form) {
    return __awaiter(this, void 0, void 0, function () {
        var timeout, msg, ret, res, next, old, newTarget;
        return __generator(this, function (_a) {
            switch (_a.label) {
                case 0:
                    timeout = setTimeout(function () {
                        // add loading after 200ms, not right away
                        target.classList.add("hyp-loading");
                    }, 200);
                    msg = (0,_action__WEBPACK_IMPORTED_MODULE_3__.actionMessage)(target.id, action, form);
                    return [4 /*yield*/, fetchAction(msg)];
                case 1:
                    ret = _a.sent();
                    res = parseResponse(ret);
                    if (!res.css || !res.content) {
                        console.error("Empty Response", res);
                        return [2 /*return*/];
                    }
                    // First, update the stylesheet
                    addCSS(res.css);
                    next = (0,omdomdom_lib_omdomdom_es_js__WEBPACK_IMPORTED_MODULE_0__.create)(res.content);
                    old = (0,omdomdom_lib_omdomdom_es_js__WEBPACK_IMPORTED_MODULE_0__.create)(target);
                    (0,omdomdom_lib_omdomdom_es_js__WEBPACK_IMPORTED_MODULE_0__.patch)(next, old);
                    newTarget = document.getElementById(target.id);
                    // let event = new Event("content", {bubbles:true})
                    // newTarget.dispatchEvent(event)
                    // load doesn't bubble
                    (0,_events__WEBPACK_IMPORTED_MODULE_2__.listenLoad)(newTarget);
                    // Remove loading and clear add timeout
                    clearTimeout(timeout);
                    target.classList.remove("hyp-loading");
                    return [2 /*return*/];
            }
        });
    });
}
function addCSS(src) {
    var rules = src.sheet.cssRules;
    for (var _i = 0, rules_1 = rules; _i < rules_1.length; _i++) {
        var rule = rules_1[_i];
        if (addedRulesIndex.has(rule.cssText) == false) {
            rootStyles.sheet.insertRule(rule.cssText);
            addedRulesIndex.add(rule.cssText);
        }
    }
}
function parseResponse(vw) {
    var parser = new DOMParser();
    var doc = parser.parseFromString(vw, 'text/html');
    var css = doc.querySelector("style");
    var content = doc.querySelector("div");
    return {
        content: content,
        css: css
    };
}
function init() {
    rootStyles = document.querySelector('style');
    (0,_events__WEBPACK_IMPORTED_MODULE_2__.listenLoadDocument)(function (target, action) {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                // console.log("INIT LOAD", target.id, action)
                runAction(target, action);
                return [2 /*return*/];
            });
        });
    });
    (0,_events__WEBPACK_IMPORTED_MODULE_2__.listenLoad)(document.body);
    (0,_events__WEBPACK_IMPORTED_MODULE_2__.listenClick)(function (target, action) {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                console.log("CLICK", target.id, action);
                runAction(target, action);
                return [2 /*return*/];
            });
        });
    });
    (0,_events__WEBPACK_IMPORTED_MODULE_2__.listenFormSubmit)(function (target, action, form) {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                console.log("FORM", target.id, action, form);
                runAction(target, action, form);
                return [2 /*return*/];
            });
        });
    });
    (0,_events__WEBPACK_IMPORTED_MODULE_2__.listenChange)(function (target, action) {
        return __awaiter(this, void 0, void 0, function () {
            return __generator(this, function (_a) {
                console.log("CHANGE", target.id, action);
                runAction(target, action);
                return [2 /*return*/];
            });
        });
    });
    // WARNING: security flaw, unescaped output. no closing quotes allowed?
    (0,_events__WEBPACK_IMPORTED_MODULE_2__.listenInput)(function (target, actionConstructor, term) {
        return __awaiter(this, void 0, void 0, function () {
            var action;
            return __generator(this, function (_a) {
                console.log("INPUT", target.id, actionConstructor, term);
                action = "".concat(actionConstructor, " \"").concat(sanitizeInput(term), "\"");
                runAction(target, action);
                return [2 /*return*/];
            });
        });
    });
}
function sanitizeInput(input) {
    return input.replace(/\\/g, "\\\\").replace(/"/g, '\\"');
}
document.addEventListener("DOMContentLoaded", init);
// Should we connect to the socket or not?
var sock = new _sockets__WEBPACK_IMPORTED_MODULE_1__.SocketConnection();
sock.connect();
// no it should take over the whole page...
function errorHTML(error) {
    // TODO: match on error.name and handle it differently
    var style = [
        ".hyp-error {background-color:#DB3524; color:white; padding: 10px}",
        ".hyp-details {padding: 10px}"
    ];
    var content = "<div class='hyp-error'>".concat(error.name, "</div>");
    var details = "<div class='hyp-details'>".concat(error.message, "</div>");
    return ["<style>" + style.join("\n") + "</style>", content, details].join("\n");
}

})();

/******/ })()
;
//# sourceMappingURL=hyperbole.js.map