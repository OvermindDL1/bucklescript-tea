/* TODO:  Polyfill document if it is missing, like on node or in native */
type t = {
  .
  [@bs.get] "body": Web_node.t,
  [@bs.meth] "createElement": string => Web_node.t,
  [@bs.meth] "createElementNS": (string, string) => Web_node.t,
  [@bs.meth] "createComment": string => Web_node.t,
  [@bs.meth] "createTextNode": string => Web_node.t,
  [@bs.meth] "getElementById": string => Js.null_undefined(Web_node.t),
  [@bs.get] "location": Web_location.t,
};

[@bs.val] external document : t = "document";

let body = () => document##body;

let createElement = typ => document##createElement(typ);

let createElementNS = (namespace, key) =>
  document##createElementNS(namespace, key);

let createComment = text => document##createComment(text);

let createTextNode = text => document##createTextNode(text);

let getElementById = id => document##getElementById(id);

let createElementNsOptional = (namespace, tagName) =>
  switch (namespace) {
  | "" => document##createElement(tagName)
  | ns => document##createElementNS(ns, tagName)
  };

let location = () => document##location;
