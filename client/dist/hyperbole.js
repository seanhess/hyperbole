/*! For license information please see hyperbole.js.LICENSE.txt */
(()=>{var e={296:e=>{function t(e,t=100,n={}){if("function"!=typeof e)throw new TypeError(`Expected the first parameter to be a function, got \`${typeof e}\`.`);if(t<0)throw new RangeError("`wait` must not be negative.");const{immediate:r}="boolean"==typeof n?{immediate:n}:n;let o,i,a,c,u;function s(){const t=o,n=i;return o=void 0,i=void 0,u=e.apply(t,n),u}function l(){const e=Date.now()-c;e<t&&e>=0?a=setTimeout(l,t-e):(a=void 0,r||(u=s()))}const d=function(...e){if(o&&this!==o&&Object.getPrototypeOf(this)===Object.getPrototypeOf(o))throw new Error("Debounced method called with different contexts of the same prototype.");o=this,i=e,c=Date.now();const n=r&&!a;return a||(a=setTimeout(l,t)),n&&(u=s()),u};return Object.defineProperty(d,"isPending",{get:()=>void 0!==a}),d.clear=()=>{a&&(clearTimeout(a),a=void 0)},d.flush=()=>{a&&d.trigger()},d.trigger=()=>{u=s(),d.clear()},d}e.exports.debounce=t,e.exports=t}},t={};function n(r){var o=t[r];if(void 0!==o)return o.exports;var i=t[r]={exports:{}};return e[r](i,i.exports,n),i.exports}(()=>{"use strict";var e=function(e,t){return Object.prototype.hasOwnProperty.call(e,t)},t=function(e,t){var n=e.length,r=-1;if(n)for(;++r<n&&!1!==t(e[r],r););},r=function(e,t,n){return e.node.insertBefore(t.node,n)};function o(e,t){return function(e){if(Array.isArray(e))return e}(e)||function(e,t){var n=null==e?null:"undefined"!=typeof Symbol&&e[Symbol.iterator]||e["@@iterator"];if(null!=n){var r,o,i,a,c=[],u=!0,s=!1;try{if(i=(n=n.call(e)).next,0===t){if(Object(n)!==n)return;u=!1}else for(;!(u=(r=i.call(n)).done)&&(c.push(r.value),c.length!==t);u=!0);}catch(e){s=!0,o=e}finally{try{if(!u&&null!=n.return&&(a=n.return(),Object(a)!==a))return}finally{if(s)throw o}}return c}}(e,t)||function(e,t){if(e){if("string"==typeof e)return i(e,t);var n=Object.prototype.toString.call(e).slice(8,-1);return"Object"===n&&e.constructor&&(n=e.constructor.name),"Map"===n||"Set"===n?Array.from(e):"Arguments"===n||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)?i(e,t):void 0}}(e,t)||function(){throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}()}function i(e,t){(null==t||t>e.length)&&(t=e.length);for(var n=0,r=new Array(t);n<t;n++)r[n]=e[n];return r}var a="string",c="number",u="boolean",s={},l=function(e,t,n){return{attrName:e,propName:t,type:n}};t([["style","cssText"],["class","className"]],(function(e){var t=o(e,2),n=t[0],r=t[1];s[n]=l(n,r||n,a)})),t(["autofocus","draggable","hidden","checked","multiple","muted","selected"],(function(e){s[e]=l(e,e,u)})),t([["tabindex","tabIndex"]],(function(e){var t=o(e,2),n=t[0],r=t[1];s[n]=l(n,r,c)}));var d="xlink:",f="http://www.w3.org/1999/xlink",v="xml:",h="http://www.w3.org/XML/1998/namespace",p=function(e,t,n,r){switch(t){case a:n===s.style.propName?e.style[n]=null===r?"":r:e[n]=null===r?"":r;break;case c:if(null===r){var o=n.toLowerCase();e.removeAttribute(o)}else if("0"===r)e[n]=0;else if("-1"===r)e[n]=-1;else{var i=parseInt(r,10);isNaN(i)||(e[n]=i)}break;case u:["","true"].indexOf(r)<0?e[n]=!1:e[n]=!0}},y=function n(o,i,c){if(o&&i){c=c||i.node.parentNode;var u=o.content&&o.content!==i.content;if(o.type!==i.type||u)return c.replaceChild(o.node,i.node),function(e,t){for(var n in e)t[n]=e[n]}(o,i);(function(n,r){var o=[],i={};for(var c in r.attributes){var u=r.attributes[c],l=n.attributes[c];u!==l&&void 0===l&&o.push(c)}for(var y in n.attributes){var m=r.attributes[y],b=n.attributes[y];m!==b&&void 0!==b&&(i[y]=b)}!function(n,r){t(r,(function(t){if(e(s,t)){var r=s[t];p(n.node,r.type,r.propName,null)}else t in n.node&&p(n.node,a,t,null),n.node.removeAttribute(t);delete n.attributes[t]}))}(r,o),function(t,n){for(var r in n){var o=n[r];if(t.attributes[r]=o,e(s,r)){var i=s[r];p(t.node,i.type,i.propName,o)}else r.startsWith(d)?t.node.setAttributeNS(f,r,o):r.startsWith(v)?t.node.setAttributeNS(h,r,o):(r in t.node&&p(t.node,a,r,o),t.node.setAttribute(r,o||""))}}(r,i)})(o,i),function(n,o,i){var a=n.children.length,c=o.children.length;if(a||c){var u=function(n){var r={};for(var o in t(n,(function(t){var n=t.attributes["data-key"];(function(t,n){return!(!n||e(t,n)&&(console.warn("[OmDomDom]: Children with duplicate keys detected. Children with duplicate keys will be skipped, resulting in dropped node references. Keys must be unique and non-indexed."),1))})(r,n)&&(r[n]=t)})),r)return r}(o.children),s=Array(a);t(n.children,void 0!==u?function(e,t){var n=o.node.childNodes,a=e.attributes["data-key"];if(Object.prototype.hasOwnProperty.call(u,a)){var c=u[a];Array.prototype.indexOf.call(n,c.node)!==t&&r(o,c,n[t]),s[t]=c,delete u[a],i(e,s[t])}else r(o,e,n[t]),s[t]=e}:function(e,t){var n=o.children[t];void 0!==n?(i(e,n),s[t]=n):(o.node.appendChild(e.node),s[t]=e)}),o.children=s;var l=o.node.childNodes.length,d=l-a;if(d>0)for(;d>0;)o.node.removeChild(o.node.childNodes[l-1]),l--,d--}}(o,i,n)}},m=function n(r){var o,i=arguments.length>1&&void 0!==arguments[1]&&arguments[1];"string"==typeof r&&(o=r.trim().replace(/\s+</g,"<").replace(/>\s+/g,">"),r=(new DOMParser).parseFromString(o,"text/html").body);var a="BODY"===r.tagName,c=r.childNodes,u=c?c.length:0;if(a){if(u>1)throw new Error("[OmDomDom]: Your element should not have more than one root node.");if(0===u)throw new Error("[OmDomDom]: Your element should have at least one root node.");return n(c[0])}var l=3===r.nodeType?"text":8===r.nodeType?"comment":r.tagName.toLowerCase(),d=i||"svg"===l,f=1===r.nodeType?function(t){var n=function(t){return Array.prototype.reduce.call(t.attributes,(function(t,n){return e(s,n.name)||(t[n.name]=n.value),t}),{})}(t);return function(e,t){for(var n in s){var r=s[n].propName,o=e.getAttribute(n);n===s.style.attrName?t[n]=e.style[r]:"string"==typeof o&&(t[n]=o)}}(t,n),n}(r):{},v=u>0?null:r.textContent,h=Array(u);return t(c,(function(e,t){h[t]=n(e,d)})),{type:l,attributes:f,children:h,content:v,node:r,isSVGContext:d}},b=function(e,t,n,r){return new(n||(n=Promise))((function(o,i){function a(e){try{u(r.next(e))}catch(e){i(e)}}function c(e){try{u(r.throw(e))}catch(e){i(e)}}function u(e){var t;e.done?o(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(a,c)}u((r=r.apply(e,t||[])).next())}))},w=function(e,t){var n,r,o,i,a={label:0,sent:function(){if(1&o[0])throw o[1];return o[1]},trys:[],ops:[]};return i={next:c(0),throw:c(1),return:c(2)},"function"==typeof Symbol&&(i[Symbol.iterator]=function(){return this}),i;function c(c){return function(u){return function(c){if(n)throw new TypeError("Generator is already executing.");for(;i&&(i=0,c[0]&&(a=0)),a;)try{if(n=1,r&&(o=2&c[0]?r.return:c[0]?r.throw||((o=r.return)&&o.call(r),0):r.next)&&!(o=o.call(r,c[1])).done)return o;switch(r=0,o&&(c=[2&c[0],o.value]),c[0]){case 0:case 1:o=c;break;case 4:return a.label++,{value:c[1],done:!1};case 5:a.label++,r=c[1],c=[0];continue;case 7:c=a.ops.pop(),a.trys.pop();continue;default:if(!((o=(o=a.trys).length>0&&o[o.length-1])||6!==c[0]&&2!==c[0])){a=0;continue}if(3===c[0]&&(!o||c[1]>o[0]&&c[1]<o[3])){a.label=c[1];break}if(6===c[0]&&a.label<o[1]){a.label=o[1],o=c;break}if(o&&a.label<o[2]){a.label=o[2],a.ops.push(c);break}o[2]&&a.ops.pop(),a.trys.pop();continue}c=t.call(e,a)}catch(e){c=[6,e],r=0}finally{n=o=0}if(5&c[0])throw c[1];return{value:c[0]?c[1]:void 0,done:!0}}([c,u])}}},g="https:"===window.location.protocol?"wss:":"ws:",x="".concat(g,"//").concat(window.location.host).concat(window.location.pathname),k=function(){function e(){this.reconnectDelay=0}return e.prototype.connect=function(e){var t=this;void 0===e&&(e=x);var n=new WebSocket(e);function r(e){console.log("Connection Error",e)}this.socket=n,n.addEventListener("error",r),n.addEventListener("open",(function(e){console.log("Opened",e),t.isConnected=!0,t.hasEverConnected=!0,t.reconnectDelay=0,t.socket.removeEventListener("error",r)})),n.addEventListener("close",(function(n){t.isConnected=!1,console.log("Closed"),t.hasEverConnected&&(t.reconnectDelay+=1e3,console.log("Reconnecting in "+t.reconnectDelay/1e3+"s"),setTimeout((function(){return t.connect(e)}),t.reconnectDelay))}))},e.prototype.sendAction=function(e){return b(this,void 0,void 0,(function(){var t,n;return w(this,(function(r){switch(r.label){case 0:return t=[e.url.pathname+e.url.search,"Host: "+window.location.host,"Cookie: "+document.cookie,e.form].join("\n"),[4,this.fetch(e.id,t)];case 1:return(n=r.sent()).metadata,[2,n.rest]}}))}))},e.prototype.fetch=function(e,t){return b(this,void 0,void 0,(function(){return w(this,(function(n){switch(n.label){case 0:return this.sendMessage(t),[4,this.waitMessage(e)];case 1:return[2,n.sent()]}}))}))},e.prototype.sendMessage=function(e){this.socket.send(e)},e.prototype.waitMessage=function(e){return b(this,void 0,void 0,(function(){var t=this;return w(this,(function(n){return[2,new Promise((function(n,r){var o=function(r){var i,a,c=function(e){var t=e.split("\n"),n=function(e,t){for(var n=[],r=0,o=t;r<o.length;r++){var i=e(o[r]);if(!i)break;n.push(i)}return n}(o,t),r=function(e,t){for(var n=0;n<t.length&&e(t[n]);)n++;return t.slice(n)}(o,t).join("\n");return{metadata:E(n),rest:r};function o(e){var t=e.match(/^\|([A-Z\-]+)\|(.*)$/);if(t)return{key:t[1],value:t[2]}}}(r.data),u=c.metadata,s=c.rest;if(u.error)throw i=u.error,(a=new Error).name=i.substring(0,i.indexOf(" ")),a.message=i.substring(i.indexOf(" ")+1),a;u.session&&(document.cookie=u.session),u.redirect?window.location.href=u.redirect:u.viewId==e&&(n({metadata:u,rest:s}),t.socket.removeEventListener("message",o))};t.socket.addEventListener("message",o),t.socket.addEventListener("error",r)}))]}))}))},e.prototype.disconnect=function(){this.socket.close()},e}();function E(e){var t,n,r,o;return{session:null===(t=e.find((function(e){return"SESSION"==e.key})))||void 0===t?void 0:t.value,redirect:null===(n=e.find((function(e){return"REDIRECT"==e.key})))||void 0===n?void 0:n.value,error:null===(r=e.find((function(e){return"ERROR"==e.key})))||void 0===r?void 0:r.value,viewId:null===(o=e.find((function(e){return"VIEW-ID"==e.key})))||void 0===o?void 0:o.value}}console.log("CONNECTING",window.location);var C=n(296);function O(e){e.querySelectorAll("[data-on-load]").forEach((function(e){var t=parseInt(e.dataset.delay)||0;setTimeout((function(){var t=new CustomEvent("hyp-load",{bubbles:!0,detail:{target:S(e)}});e.dispatchEvent(t)}),t)}))}function S(e){var t=function(e){var t,n=e.closest("[data-target]");return(null==n?void 0:n.dataset.target)||(null===(t=e.closest("[id]"))||void 0===t?void 0:t.id)}(e),n=document.getElementById(t);if(n)return n;console.error("Cannot find target: ",e)}function D(e){if(e){var t=new URLSearchParams;return e.forEach((function(e,n){t.append(n,e)})),t}}function T(e,t,n){var r=function(e,t){var n=new URL(window.location.href);return n.searchParams.append("id",e),n.searchParams.append("action",t),n}(e,t);return{id:e,url:r,form:D(n)}}var N,L=function(e,t,n,r){return new(n||(n=Promise))((function(o,i){function a(e){try{u(r.next(e))}catch(e){i(e)}}function c(e){try{u(r.throw(e))}catch(e){i(e)}}function u(e){var t;e.done?o(e.value):(t=e.value,t instanceof n?t:new n((function(e){e(t)}))).then(a,c)}u((r=r.apply(e,t||[])).next())}))},A=function(e,t){var n,r,o,i,a={label:0,sent:function(){if(1&o[0])throw o[1];return o[1]},trys:[],ops:[]};return i={next:c(0),throw:c(1),return:c(2)},"function"==typeof Symbol&&(i[Symbol.iterator]=function(){return this}),i;function c(c){return function(u){return function(c){if(n)throw new TypeError("Generator is already executing.");for(;i&&(i=0,c[0]&&(a=0)),a;)try{if(n=1,r&&(o=2&c[0]?r.return:c[0]?r.throw||((o=r.return)&&o.call(r),0):r.next)&&!(o=o.call(r,c[1])).done)return o;switch(r=0,o&&(c=[2&c[0],o.value]),c[0]){case 0:case 1:o=c;break;case 4:return a.label++,{value:c[1],done:!1};case 5:a.label++,r=c[1],c=[0];continue;case 7:c=a.ops.pop(),a.trys.pop();continue;default:if(!((o=(o=a.trys).length>0&&o[o.length-1])||6!==c[0]&&2!==c[0])){a=0;continue}if(3===c[0]&&(!o||c[1]>o[0]&&c[1]<o[3])){a.label=c[1];break}if(6===c[0]&&a.label<o[1]){a.label=o[1],o=c;break}if(o&&a.label<o[2]){a.label=o[2],a.ops.push(c);break}o[2]&&a.ops.pop(),a.trys.pop();continue}c=t.call(e,a)}catch(e){c=[6,e],r=0}finally{n=o=0}if(5&c[0])throw c[1];return{value:c[0]?c[1]:void 0,done:!0}}([c,u])}}};console.log("Hyperbole 0.4.2");var I=new Set;function P(e){return L(this,void 0,void 0,(function(){function t(e){return L(this,void 0,void 0,(function(){var t,n,r;return A(this,(function(o){switch(o.label){case 0:return console.log("HTTP sendAction",e.url.toString()),[4,fetch(e.url,{method:"POST",headers:{Accept:"text/html","Content-Type":"application/x-www-form-urlencoded"},body:e.form,redirect:"manual"})];case 1:return(t=o.sent()).headers.get("location")?(console.log("Found Redirect",t.headers.get("location")),window.location.href=t.headers.get("location"),[2]):t.headers.get("location")?(window.location.href=t.headers.get("location"),[2]):t.ok?[3,3]:((n=new Error).name="Fetch Error "+t.status,[4,t.text()]);case 2:throw r=o.sent(),n.message=r,n;case 3:return[2,t.text()]}}))}))}return A(this,(function(n){return R.isConnected?[2,R.sendAction(e)]:[2,t(e)]}))}))}function j(e){return L(this,void 0,void 0,(function(){var t;return A(this,(function(n){switch(n.label){case 0:return n.trys.push([0,2,,3]),[4,P(e)];case 1:return[2,n.sent()];case 2:throw t=n.sent(),document.body.innerHTML=(o=[".hyp-error {background-color:#DB3524; color:white; padding: 10px}",".hyp-details {padding: 10px}"],i="<div class='hyp-error'>".concat((r=t).name,"</div>"),a="<div class='hyp-details'>".concat(r.message,"</div>"),["<style>"+o.join("\n")+"</style>",i,a].join("\n")),t;case 3:return[2]}var r,o,i,a}))}))}function M(e,t,n){return L(this,void 0,void 0,(function(){var r,o,i,a;return A(this,(function(c){switch(c.label){case 0:return r=setTimeout((function(){e.classList.add("hyp-loading")}),200),[4,j(T(e.id,t,n))];case 1:return u=c.sent(),l=(s=(new DOMParser).parseFromString(u,"text/html")).querySelector("style"),(o={content:s.querySelector("div"),css:l}).css&&o.content?(function(e){for(var t=0,n=e.sheet.cssRules;t<n.length;t++){var r=n[t];0==I.has(r.cssText)&&(N.sheet.insertRule(r.cssText),I.add(r.cssText))}}(o.css),i=m(o.content),a=m(e),y(i,a),O(document.getElementById(e.id)),clearTimeout(r),e.classList.remove("hyp-loading"),[2]):(console.error("Empty Response",o),[2])}var u,s,l}))}))}document.addEventListener("DOMContentLoaded",(function(){var e;N=document.querySelector("style"),e=function(e,t){return L(this,void 0,void 0,(function(){return A(this,(function(n){return M(e,t),[2]}))}))},document.addEventListener("hyp-load",(function(t){var n=t.target.dataset.onLoad,r=t.detail.target;e(r,n)})),O(document.body),document.addEventListener("click",(function(e){var t=e.target.closest("[data-on-click]");t&&(e.preventDefault(),function(e,t){L(this,void 0,void 0,(function(){return A(this,(function(n){return console.log("CLICK",e.id,t),M(e,t),[2]}))}))}(S(t),t.dataset.onClick))})),document.addEventListener("submit",(function(e){var t=e.target;if(null==t?void 0:t.dataset.onSubmit){e.preventDefault();var n=S(t),r=new FormData(t);!function(e,t,n){L(this,void 0,void 0,(function(){return A(this,(function(r){return console.log("FORM",e.id,t,n),M(e,t,n),[2]}))}))}(n,t.dataset.onSubmit,r)}else console.error("Missing onSubmit: ",t)})),document.addEventListener("change",(function(e){var t=e.target.closest("[data-on-change]");t&&(e.preventDefault(),t.value?function(e,t){L(this,void 0,void 0,(function(){return A(this,(function(n){return console.log("CHANGE",e.id,t),M(e,t),[2]}))}))}(S(t),t.value):console.error("Missing input value:",t))})),document.addEventListener("input",(function(e){var t=e.target.closest("[data-on-input]");if(t){var n=parseInt(t.dataset.delay)||100;if(n<100&&console.warn("Input delay < 100 can result in poor performance"),null==t?void 0:t.dataset.onInput){e.preventDefault();var r=S(t);t.debouncedCallback||(t.debouncedCallback=C((function(){return function(e,t,n){return L(this,void 0,void 0,(function(){var r;return A(this,(function(o){return console.log("INPUT",e.id,t,n),r="".concat(t,' "').concat(n.replace(/\\/g,"\\\\").replace(/"/g,'\\"'),'"'),M(e,r),[2]}))}))}(r,t.dataset.onInput,t.value)}),n)),t.debouncedCallback()}else console.error("Missing onInput: ",t)}}))}));var R=new k;R.connect()})()})();
//# sourceMappingURL=hyperbole.js.map