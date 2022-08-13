/* TODO:  Polyfill document if it is missing, like on node or in native */

type t = {
  @get
  "body": Web_node.t,
  @get
  "location": Web_location.t,
}

@send external
createElement: (t,string) => Web_node.t="createElement"
@send external
createElementNS: (t,string, string) => Web_node.t="createElementNS"
@send external
createComment: (t,string)  => Web_node.t="createComment"
@send external
createTextNode: (t,string)  => Web_node.t="createTextNode"
@send external
getElementById: (t,string)  => Js.null_undefined<Web_node.t>="getElementById"

@val external document: t = "document"

let body = () => document["body"]

let createElement = typ => createElement(document, typ)

let createElementNS = (namespace, key) => createElementNS(document, namespace, key)

let createComment = text => createComment(document, text)

let createTextNode = text => createTextNode(document, text)

let getElementById = id => getElementById(document, id)

let createElementNsOptional = (namespace, tagName) =>
  switch namespace {
  | "" => createElement(tagName)
  | ns => createElementNS(ns, tagName)
  }

let location = () => document["location"]
