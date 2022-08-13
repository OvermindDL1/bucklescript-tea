/* TODO:  Polyfill window if it is missing, like on node or in native */

module History = Web_window_history

module LocalStorage = Web_window_localstorage

type timeoutHandlerID = int

type t = {
  @get
  "history": Js.Undefined.t<History.t>,
  @get
  "location": Web_location.t,
  @get
  "localStorage": Js.Undefined.t<LocalStorage.t>,
}


@send external
requestAnimationFrame : (t, float => unit) => int ="requestAnimationFrame"

@send external
cancelAnimationFrame : (t,int) => unit = "cancelAnimationFrame"

@send external
setInterval : (t, unit => unit, float) => timeoutHandlerID = "setInterval"

@send external
clearTimeout : (t,timeoutHandlerID) => unit = "clearTimeout"

@send external
setTimeout : (t,unit => unit, float) => timeoutHandlerID = "setTimeout"

@send external
addEventListener: (t,string, Web_event.cb<Web_node.t>, Web_event.options) => unit="addEventListener"

@send external
removeEventListener: (t,string, Web_event.cb<Web_node.t>, Web_event.options) => unit="removeEventListener"


@val external window: t = "window"

let history = () => window["history"]

let localStorage = () => window["localStorage"]

let location = () => window["location"]

/* requestAnimationFrame callback is a float timestamp in milliseconds */
let requestAnimationFrame = callback => requestAnimationFrame(window, callback)

let cancelAnimationFrame = id => cancelAnimationFrame(window , id)

let clearTimeout = id => clearTimeout(window,id)

let setInterval = (cb, msTime) => setInterval(window,cb, msTime)

let setTimeout = (cb, msTime) => setTimeout(window, cb, msTime)

let addEventListener = (typ, listener, options) =>
  addEventListener(window, typ, listener, options)

let removeEventListener = (typ, listener, options) =>
  removeEventListener(window, typ, listener, options)

/* Polyfills */

let requestAnimationFrame_polyfill: unit => unit = () =>
  %raw(`
  // requestAnimationFrame polyfill
  (function() {
      var lastTime = 0;
      var vendors = ['ms', 'moz', 'webkit', 'o'];
      for(var x = 0; x < vendors.length && !window.requestAnimationFrame; ++x) {
          window.requestAnimationFrame = window[vendors[x]+'RequestAnimationFrame'];
          window.cancelAnimationFrame = window[vendors[x]+'CancelAnimationFrame']
                                     || window[vendors[x]+'CancelRequestAnimationFrame'];
      }

      if (!window.requestAnimationFrame)
          window.requestAnimationFrame = function(callback, element) {
              var currTime = new Date().getTime();
              var timeToCall = Math.max(0, 16 - (currTime - lastTime));
              var id = window.setTimeout(function() { callback(currTime + timeToCall); },
                timeToCall);
              lastTime = currTime + timeToCall;
              return id;
          };

      if (!window.cancelAnimationFrame)
          window.cancelAnimationFrame = function(id) {
              clearTimeout(id);
          };
  }())
  `)
