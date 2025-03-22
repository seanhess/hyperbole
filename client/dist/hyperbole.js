/*! For license information please see hyperbole.js.LICENSE.txt */
(()=>{var e={296:e=>{function t(e,t=100,n={}){if("function"!=typeof e)throw new TypeError(`Expected the first parameter to be a function, got \`${typeof e}\`.`);if(t<0)throw new RangeError("`wait` must not be negative.");const{immediate:r}="boolean"==typeof n?{immediate:n}:n;let o,i,a,c,u;function s(){const t=o,n=i;return o=void 0,i=void 0,u=e.apply(t,n),u}function l(){const e=Date.now()-c;e<t&&e>=0?a=setTimeout(l,t-e):(a=void 0,r||(u=s()))}const d=function(...e){if(o&&this!==o&&Object.getPrototypeOf(this)===Object.getPrototypeOf(o))throw new Error("Debounced method called with different contexts of the same prototype.");o=this,i=e,c=Date.now();const n=r&&!a;return a||(a=setTimeout(l,t)),n&&(u=s()),u};return Object.defineProperty(d,"isPending",{get:()=>void 0!==a}),d.clear=()=>{a&&(clearTimeout(a),a=void 0)},d.flush=()=>{a&&d.trigger()},d.trigger=()=>{u=s(),d.clear()},d}e.exports.debounce=t,e.exports=t},147:e=>{"use strict";e.exports=JSON.parse('{"name":"web-ui","version":"0.4.3h","description":"Development -----------","main":"index.js","directories":{"client":"client"},"scripts":{"build":"npx webpack"},"author":"","license":"ISC","devDependencies":{"ts-loader":"^9.4.1","typescript":"^4.8.3","uglify":"^0.1.5","webpack":"^5.88.2","webpack-cli":"^4.10.0"},"dependencies":{"omdomdom":"^0.3.2","debounce":"^2.2.0"}}')}},t={};function n(r){var o=t[r];if(void 0!==o)return o.exports;var i=t[r]={exports:{}};return e[r](i,i.exports,n),i.exports}(()=>{"use strict";var e=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},t=function(e,t){var n=e.length,r=-1;if(n)for(;++r<n&&!1!==t(e[r],r););},r=function(e,t,n){return e.node.insertBefore(t.node,n)};function o(e,t){return function(e){if(Array.isArray(e))return e}(e)||function(e,t){var n=null==e?null:"undefined"!=typeof Symbol&&e[Symbol.iterator]||e["@@iterator"];if(null!=n){var r,o,i,a,c=[],u=!0,s=!1;try{if(i=(n=n.call(e)).next,0===t){if(Object(n)!==n)return;u=!1}else for(;!(u=(r=i.call(n)).done)&&(c.push(r.value),c.length!==t);u=!0);}catch(e){s=!0,o=e}finally{try{if(!u&&null!=n.return&&(a=n.return(),Object(a)!==a))return}finally{if(s)throw o}}return c}}(e,t)||function(e,t){if(e){if("string"==typeof e)return i(e,t);var n=Object.prototype.toString.call(e).slice(8,-1);return"Object"===n&&e.constructor&&(n=e.constructor.name),"Map"===n||"Set"===n?Array.from(e):"Arguments"===n||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)?i(e,t):void 0}}(e,t)||function(){throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}()}function i(e,t){(null==t||t>e.length)&&(t=e.length);for(var n=0,r=new Array(t);n<t;n++)r[n]=e[n];return r}var a="string",c="number",u="boolean",s={},l=function(e,t,n){return{attrName:e,propName:t,type:n}};t([["style","cssText"],["class","className"]],(function(e){var t=o(e,2),n=t[0],r=t[1];s[n]=l(n,r||n,a)})),t(["autofocus","draggable","hidden","checked","multiple","muted","selected"],(function(e){s[e]=l(e,e,u)})),t([["tabindex","tabIndex"]],(function(e){var t=o(e,2),n=t[0],r=t[1];s[n]=l(n,r,c)}));var d="xlink:",f="http://www.w3.org/1999/xlink",v="xml:",p="http://www.w3.org/XML/1998/namespace",h=function(e,t,n,r){switch(t){case a:n===s.style.propName?e.style[n]=null===r?"":r:e[n]=null===r?"":r;break;case c:if(null===r){var o=n.toLowerCase();e.removeAttribute(o)}else if("0"===r)e[n]=0;else if("-1"===r)e[n]=-1;else{var i=parseInt(r,10);isNaN(i)||(e[n]=i)}break;case u:["","true"].indexOf(r)<0?e[n]=!1:e[n]=!0}},y=function n(o,i,c){if(o&&i){c=c||i.node.parentNode;var u=o.content&&o.content!==i.content;if(o.type!==i.type||u)return c.replaceChild(o.node,i.node),function(e,t){for(var n in e)t[n]=e[n]}(o,i);(function(n,r){var o=[],i={};for(var c in r.attributes){var u=r.attributes[c],l=n.attributes[c];u!==l&&void 0===l&&o.push(c)}for(var y in n.attributes){var b=r.attributes[y],m=n.attributes[y];b!==m&&void 0!==m&&(i[y]=m)}!function(n,r){t(r,(function(t){if(e(s,t)){var r=s[t];h(n.node,r.type,r.propName,null)}else t in n.node&&h(n.node,a,t,null),n.node.removeAttribute(t);delete n.attributes[t]}))}(r,o),function(t,n){for(var r in n){var o=n[r];if(t.attributes[r]=o,e(s,r)){var i=s[r];h(t.node,i.type,i.propName,o)}else r.startsWith(d)?t.node.setAttributeNS(f,r,o):r.startsWith(v)?t.node.setAttributeNS(p,r,o):(r in t.node&&h(t.node,a,r,o),t.node.setAttribute(r,o||""))}}(r,i)})(o,i),function(n,o,i){var a=n.children.length,c=o.children.length;if(a||c){var u=function(n){var r={};for(var o in t(n,(function(t){var n=t.attributes["data-key"];(function(t,n){return!(!n||e(t,n)&&(console.warn("[OmDomDom]: Children with duplicate keys detected. Children with duplicate keys will be skipped, resulting in dropped node references. Keys must be unique and non-indexed."),1))})(r,n)&&(r[n]=t)})),r)return r}(o.children),s=Array(a);t(n.children,void 0!==u?function(e,t){var n=o.node.childNodes,a=e.attributes["data-key"];if(Object.prototype.hasOwnProperty.call(u,a)){var c=u[a];Array.prototype.indexOf.call(n,c.node)!==t&&r(o,c,n[t]),s[t]=c,delete u[a],i(e,s[t])}else r(o,e,n[t]),s[t]=e}:function(e,t){var n=o.children[t];void 0!==n?(i(e,n),s[t]=n):(o.node.appendChild(e.node),s[t]=e)}),o.children=s;var l=o.node.childNodes.length,d=l-a;if(d>0)for(;d>0;)o.node.removeChild(o.node.childNodes[l-1]),l--,d--}}(o,i,n)}},b=function n(r){var o,i=arguments.length>1&&void 0!==arguments[1]&&arguments[1];"string"==typeof r&&(o=r.trim().replace(/\s+</g,"<").replace(/>\s+/g,">"),r=(new DOMParser).parseFromString(o,"text/html").body);var a="BODY"===r.tagName,c=r.childNodes,u=c?c.length:0;if(a){if(u>1)throw new Error("[OmDomDom]: Your element should not have more than one root node.");if(0===u)throw new Error("[OmDomDom]: Your element should have at least one root node.");return n(c[0])}var l=3===r.nodeType?"text":8===r.nodeType?"comment":r.tagName.toLowerCase(),d=i||"svg"===l,f=1===r.nodeType?function(t){var n=function(t){return Array.prototype.reduce.call(t.attributes,(function(t,n){return e(s,n.name)||(t[n.name]=n.value),t}),{})}(t);return function(e,t){for(var n in s){var r=s[n].propName,o=e.getAttribute(n);n===s.style.attrName?t[n]=e.style[r]:"string"==typeof o&&(t[n]=o)}}(t,n),n}(r):{},v=u>0?null:r.textContent,p=Array(u);return t(c,(function(e,t){p[t]=n(e,d)})),{type:l,attributes:f,children:p,content:v,node:r,isSVGContext:d}};function m(e){var t=new Error;return t.name="Fetch Error",t.message=e,t}var w=function(e,t,n,r){return new(n||(n=Promise))((function(o,i){function a(e){try{u(r.next(e))}catch(e){i(e)}}function c(e){try{u(r.throw(e))}catch(e){i(e)}}function u(e){var t;e.done?o(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(a,c)}u((r=r.apply(e,t||[])).next())}))},g=function(e,t){var n,r,o,i,a={label:0,sent:function(){if(1&o[0])throw o[1];return o[1]},trys:[],ops:[]};return i={next:c(0),throw:c(1),return:c(2)},"function"==typeof Symbol&&(i[Symbol.iterator]=function(){return this}),i;function c(c){return function(u){return function(c){if(n)throw new TypeError("Generator is already executing.");for(;i&&(i=0,c[0]&&(a=0)),a;)try{if(n=1,r&&(o=2&c[0]?r.return:c[0]?r.throw||((o=r.return)&&o.call(r),0):r.next)&&!(o=o.call(r,c[1])).done)return o;switch(r=0,o&&(c=[2&c[0],o.value]),c[0]){case 0:case 1:o=c;break;case 4:return a.label++,{value:c[1],done:!1};case 5:a.label++,r=c[1],c=[0];continue;case 7:c=a.ops.pop(),a.trys.pop();continue;default:if(!((o=(o=a.trys).length>0&&o[o.length-1])||6!==c[0]&&2!==c[0])){a=0;continue}if(3===c[0]&&(!o||c[1]>o[0]&&c[1]<o[3])){a.label=c[1];break}if(6===c[0]&&a.label<o[1]){a.label=o[1],o=c;break}if(o&&a.label<o[2]){a.label=o[2],a.ops.push(c);break}o[2]&&a.ops.pop(),a.trys.pop();continue}c=t.call(e,a)}catch(e){c=[6,e],r=0}finally{n=o=0}if(5&c[0])throw c[1];return{value:c[0]?c[1]:void 0,done:!0}}([c,u])}}},k="https:"===window.location.protocol?"wss:":"ws:",E="".concat(k,"//").concat(window.location.host).concat(window.location.pathname),x=function(){function e(){this.reconnectDelay=0}return e.prototype.connect=function(e){var t=this;void 0===e&&(e=E);var n=new WebSocket(e);function r(e){console.log("Connection Error",e)}this.socket=n,n.addEventListener("error",r),n.addEventListener("open",(function(e){console.log("Opened",e),t.isConnected=!0,t.hasEverConnected=!0,t.reconnectDelay=0,t.socket.removeEventListener("error",r)})),n.addEventListener("close",(function(n){t.isConnected=!1,console.log("Closed"),t.hasEverConnected&&(t.reconnectDelay+=1e3,console.log("Reconnecting in "+t.reconnectDelay/1e3+"s"),setTimeout((function(){return t.connect(e)}),t.reconnectDelay))}))},e.prototype.sendAction=function(e,t){return w(this,void 0,void 0,(function(){var n,r,o,i;return g(this,(function(a){switch(a.label){case 0:return n=[t.url.pathname+t.url.search,"Host: "+window.location.host,"Cookie: "+document.cookie,"Request-Id: "+e,t.form].join("\n"),[4,this.fetch(e,t.id,n)];case 1:return r=a.sent(),o=r.metadata,i=r.body,[2,{requestId:o.requestId,location:o.redirect,query:o.query,body:i}]}}))}))},e.prototype.fetch=function(e,t,n){return w(this,void 0,void 0,(function(){return g(this,(function(r){switch(r.label){case 0:return this.sendMessage(n),[4,this.waitMessage(e,t)];case 1:return[2,r.sent()]}}))}))},e.prototype.sendMessage=function(e){this.socket.send(e)},e.prototype.waitMessage=function(e,t){return w(this,void 0,void 0,(function(){var t=this;return g(this,(function(n){return[2,new Promise((function(n,r){var o=function(r){var i=function(e){var t=e.split("\n"),n=function(e,t){for(var n=[],r=0,o=t;r<o.length;r++){var i=e(o[r]);if(!i)break;n.push(i)}return n}(o,t),r=function(e,t){for(var n=0;n<t.length&&e(t[n]);)n++;return t.slice(n)}(o,t).join("\n");return{metadata:S(n),body:r};function o(e){var t=e.match(/^\|([A-Z\-]+)\|(.*)$/);if(t)return{key:t[1],value:t[2]}}}(r.data),a=i.metadata,c=i.body;if(a.requestId){if(a.requestId==e){if(a.error)throw m(a.error);t.socket.removeEventListener("message",o),a.cookies.forEach((function(e){document.cookie=e})),n({metadata:a,body:c})}}else console.error("Missing RequestId!",a,r.data)};t.socket.addEventListener("message",o),t.socket.addEventListener("error",r)}))]}))}))},e.prototype.disconnect=function(){this.socket.close()},e}();function S(e){var t,n,r,o,i,a=null===(t=e.find((function(e){return"REQUEST-ID"==e.key})))||void 0===t?void 0:t.value;return{cookies:e.filter((function(e){return"COOKIE"==e.key})).map((function(e){return e.value})),redirect:null===(n=e.find((function(e){return"REDIRECT"==e.key})))||void 0===n?void 0:n.value,error:null===(r=e.find((function(e){return"ERROR"==e.key})))||void 0===r?void 0:r.value,viewId:null===(o=e.find((function(e){return"VIEW-ID"==e.key})))||void 0===o?void 0:o.value,query:null===(i=e.find((function(e){return"QUERY"==e.key})))||void 0===i?void 0:i.value,requestId:a}}var I=n(296);function D(e){if(e){var t=new URLSearchParams;return e.forEach((function(e,n){t.append(n,e)})),t}}function C(e,t){document.addEventListener(e.toLowerCase(),(function(n){var r=n.target,o="on"+e+n.key,i=r.dataset[o];i&&(n.preventDefault(),t(L(r),i))}))}function A(e){e.querySelectorAll("[data-on-load]").forEach((function(e){var t=parseInt(e.dataset.delay)||0,n=e.dataset.onLoad;setTimeout((function(){var t=L(e);if(e.dataset.onLoad==n){var r=new CustomEvent("hyp-load",{bubbles:!0,detail:{target:t,onLoad:n}});e.dispatchEvent(r)}}),t)}))}function L(e){var t=function(e){var t,n=e.closest("[data-target]");return(null==n?void 0:n.dataset.target)||(null===(t=e.closest("[id]"))||void 0===t?void 0:t.id)}(e),n=document.getElementById(t);if(n)return n;console.error("Cannot find target: ",e)}var O=function(e,t,n,r){return new(n||(n=Promise))((function(o,i){function a(e){try{u(r.next(e))}catch(e){i(e)}}function c(e){try{u(r.throw(e))}catch(e){i(e)}}function u(e){var t;e.done?o(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(a,c)}u((r=r.apply(e,t||[])).next())}))},q=function(e,t){var n,r,o,i,a={label:0,sent:function(){if(1&o[0])throw o[1];return o[1]},trys:[],ops:[]};return i={next:c(0),throw:c(1),return:c(2)},"function"==typeof Symbol&&(i[Symbol.iterator]=function(){return this}),i;function c(c){return function(u){return function(c){if(n)throw new TypeError("Generator is already executing.");for(;i&&(i=0,c[0]&&(a=0)),a;)try{if(n=1,r&&(o=2&c[0]?r.return:c[0]?r.throw||((o=r.return)&&o.call(r),0):r.next)&&!(o=o.call(r,c[1])).done)return o;switch(r=0,o&&(c=[2&c[0],o.value]),c[0]){case 0:case 1:o=c;break;case 4:return a.label++,{value:c[1],done:!1};case 5:a.label++,r=c[1],c=[0];continue;case 7:c=a.ops.pop(),a.trys.pop();continue;default:if(!((o=(o=a.trys).length>0&&o[o.length-1])||6!==c[0]&&2!==c[0])){a=0;continue}if(3===c[0]&&(!o||c[1]>o[0]&&c[1]<o[3])){a.label=c[1];break}if(6===c[0]&&a.label<o[1]){a.label=o[1],o=c;break}if(o&&a.label<o[2]){a.label=o[2],a.ops.push(c);break}o[2]&&a.ops.pop(),a.trys.pop();continue}c=t.call(e,a)}catch(e){c=[6,e],r=0}finally{n=o=0}if(5&c[0])throw c[1];return{value:c[0]?c[1]:void 0,done:!0}}([c,u])}}};function T(e,t){return O(this,void 0,void 0,(function(){var n,r;return q(this,(function(o){switch(o.label){case 0:return[4,fetch(t.url,{method:"POST",headers:{Accept:"text/html","Content-Type":"application/x-www-form-urlencoded","Request-Id":e},body:t.form,redirect:"manual"})];case 1:return[4,(n=o.sent()).text()];case 2:if(r=o.sent(),!n.ok)throw m(r);return[2,{requestId:n.headers.get("Request-Id"),location:n.headers.get("location"),query:n.headers.get("set-query"),body:r}]}}))}))}var N,R=function(e,t,n,r){return new(n||(n=Promise))((function(o,i){function a(e){try{u(r.next(e))}catch(e){i(e)}}function c(e){try{u(r.throw(e))}catch(e){i(e)}}function u(e){var t;e.done?o(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(a,c)}u((r=r.apply(e,t||[])).next())}))},P=function(e,t){var n,r,o,i,a={label:0,sent:function(){if(1&o[0])throw o[1];return o[1]},trys:[],ops:[]};return i={next:c(0),throw:c(1),return:c(2)},"function"==typeof Symbol&&(i[Symbol.iterator]=function(){return this}),i;function c(c){return function(u){return function(c){if(n)throw new TypeError("Generator is already executing.");for(;i&&(i=0,c[0]&&(a=0)),a;)try{if(n=1,r&&(o=2&c[0]?r.return:c[0]?r.throw||((o=r.return)&&o.call(r),0):r.next)&&!(o=o.call(r,c[1])).done)return o;switch(r=0,o&&(c=[2&c[0],o.value]),c[0]){case 0:case 1:o=c;break;case 4:return a.label++,{value:c[1],done:!1};case 5:a.label++,r=c[1],c=[0];continue;case 7:c=a.ops.pop(),a.trys.pop();continue;default:if(!((o=(o=a.trys).length>0&&o[o.length-1])||6!==c[0]&&2!==c[0])){a=0;continue}if(3===c[0]&&(!o||c[1]>o[0]&&c[1]<o[3])){a.label=c[1];break}if(6===c[0]&&a.label<o[1]){a.label=o[1],o=c;break}if(o&&a.label<o[2]){a.label=o[2],a.ops.push(c);break}o[2]&&a.ops.pop(),a.trys.pop();continue}c=t.call(e,a)}catch(e){c=[6,e],r=0}finally{n=o=0}if(5&c[0])throw c[1];return{value:c[0]?c[1]:void 0,done:!0}}([c,u])}}},M=n(147);console.log("Hyperbole "+M.version);var j=new Set;function U(e,t){return R(this,void 0,void 0,(function(){return P(this,(function(n){return F.isConnected?[2,F.sendAction(e,t)]:[2,T(e,t)]}))}))}function B(e,t){return R(this,void 0,void 0,(function(){var n,r;return P(this,(function(o){switch(o.label){case 0:return o.trys.push([0,2,,3]),[4,U(e,t)];case 1:return(n=o.sent()).location?(window.location.href=n.location,[2]):(null!=n.query&&function(e){if(e!=function(){var e=window.location.search;return e.startsWith("?")?e.substring(1):e}()){""!=e&&(e="?"+e);var t=location.pathname+e;console.log("history.replaceState(",t,")"),window.history.replaceState({},"",t)}}(n.query),[2,n]);case 2:throw r=o.sent(),document.body.innerHTML=(a=[".hyp-error {background-color:#DB3524; color:white; padding: 10px}",".hyp-details {padding: 10px}"],c="<div class='hyp-error'>".concat((i=r).name,"</div>"),u="<div class='hyp-details'>".concat(i.message,"</div>"),["<style>"+a.join("\n")+"</style>",c,u].join("\n")),r;case 3:return[2]}var i,a,c,u}))}))}function H(e,t,n){return R(this,void 0,void 0,(function(){var r,o,i,a,c,u,s,l;return P(this,(function(d){switch(d.label){case 0:return void 0===t?(console.error("Undefined Action!",e,"this is a bug, please report: https://github.com/seanhess/hyperbole"),[2]):(r=setTimeout((function(){e.classList.add("hyp-loading")}),100),o=function(e,t,n){var r=function(e,t){var n=new URL(window.location.href);return n.searchParams.append("hyp-id",e),n.searchParams.append("hyp-action",t),n}(e,t);return{id:e,url:r,form:D(n)}}(e.id,t,n),i=Math.random().toString(36).substring(2,8),e.dataset.requestId=i,[4,B(i,o)]);case 1:return a=d.sent(),i!=e.dataset.requestId?(console.warn("Ignoring Stale Action ("+i+"):",t),[2]):(c=function(e){var t=(new DOMParser).parseFromString(e,"text/html"),n=t.querySelector("style");return{content:t.querySelector("div"),css:n}}(a.body),c.css&&c.content?(function(e){for(var t=0,n=e.sheet.cssRules;t<n.length;t++){var r=n[t];0==j.has(r.cssText)&&(N.sheet.insertRule(r.cssText),j.add(r.cssText))}}(c.css),u=b(c.content),s=b(e),y(u,s),(l=document.getElementById(e.id))?(A(l),function(e){var t=e.querySelector("[autofocus]");(null==t?void 0:t.focus)&&t.focus(),e.querySelectorAll("[value]").forEach((function(e){var t=e.getAttribute("value");void 0!==t&&(e.value=t)}))}(l)):console.warn("Target Missing: ",e.id),clearTimeout(r),e.classList.remove("hyp-loading"),[2]):(console.error("Empty Response",a),[2]))}}))}))}document.addEventListener("DOMContentLoaded",(function(){var e;N=document.querySelector("style"),e=function(e,t){return R(this,void 0,void 0,(function(){return P(this,(function(n){return H(e,t),[2]}))}))},document.addEventListener("hyp-load",(function(t){var n=t.detail.onLoad,r=t.detail.target;e(r,n)})),A(document.body),document.body.querySelectorAll("[id]").forEach((function(e){console.log("Found HyperView",e.dataset),e.runAction=function(e){H(this,e)}.bind(e)})),document.addEventListener("click",(function(e){var t=e.target.closest("[data-on-click]");t&&(e.preventDefault(),function(e,t){R(this,void 0,void 0,(function(){return P(this,(function(n){return H(e,t),[2]}))}))}(L(t),t.dataset.onClick))})),document.addEventListener("dblclick",(function(e){var t=e.target.closest("[data-on-dblclick]");t&&(e.preventDefault(),function(e,t){R(this,void 0,void 0,(function(){return P(this,(function(n){return H(e,t),[2]}))}))}(L(t),t.dataset.onDblclick))})),C("Keydown",(function(e,t){return R(this,void 0,void 0,(function(){return P(this,(function(n){return H(e,t),[2]}))}))})),C("Keyup",(function(e,t){return R(this,void 0,void 0,(function(){return P(this,(function(n){return H(e,t),[2]}))}))})),document.addEventListener("submit",(function(e){var t=e.target;if(null==t?void 0:t.dataset.onSubmit){e.preventDefault();var n=L(t),r=new FormData(t);!function(e,t,n){R(this,void 0,void 0,(function(){return P(this,(function(r){return H(e,t,n),[2]}))}))}(n,t.dataset.onSubmit,r)}else console.error("Missing onSubmit: ",t)})),document.addEventListener("change",(function(e){var t=e.target.closest("[data-on-change]");t&&(e.preventDefault(),t.value?function(e,t){R(this,void 0,void 0,(function(){return P(this,(function(n){return H(e,t),[2]}))}))}(L(t),t.value):console.error("Missing input value:",t))})),document.addEventListener("input",(function(e){var t=e.target.closest("[data-on-input]");if(t){var n=parseInt(t.dataset.delay)||250;if(n<250&&console.warn("Input delay < 100 can result in poor performance."),null==t?void 0:t.dataset.onInput){e.preventDefault();var r=L(t);t.debouncedCallback||(t.debouncedCallback=I((function(){var e=function(e,t){return e+' "'+t.replace(/\\/g,"\\\\").replace(/"/g,'\\"')+'"'}(t.dataset.onInput,t.value);!function(e,t){R(this,void 0,void 0,(function(){return P(this,(function(n){return console.log("INPUT",e.id,t),H(e,t),[2]}))}))}(r,e)}),n)),t.debouncedCallback()}else console.error("Missing onInput: ",t)}}))}));var F=new x;F.connect(),window.Hyperbole={runAction:H,action:function(e){for(var t=[],n=1;n<arguments.length;n++)t[n-1]=arguments[n];return e+t.reduce((function(e,t){return e+" "+JSON.stringify(t)}),"")},hyperView:function(e){var t=document.getElementById(e);if(null==t?void 0:t.runAction)return t;console.error("Element id="+e+" was not a HyperView")}}})()})();
//# sourceMappingURL=hyperbole.js.map