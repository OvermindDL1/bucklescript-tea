type eventCallback = Dom.event => unit

type systemMessage<'msg> = Render | AddRenderMsg('msg) | RemoveRenderMsg('msg)

type applicationCallbacks<'msg> = {enqueue: 'msg => unit, on: systemMessage<'msg> => unit}

type eventHandler<'msg> =
  | EventHandlerCallback(string, Dom.event => option<'msg>)
  | EventHandlerMsg('msg)

type eventCache<'msg> = {handler: eventCallback, cb: ref<Dom.event => option<'msg>>}

type property<'msg> =
  | NoProp
  | RawProp(string, string)
  | Attribute(string, string, string)
  | Data(string, string)
  | Event(string, eventHandler<'msg>, ref<option<eventCache<'msg>>>)
  | Style(list<(string, string)>)

type properties<'msg> = list<property<'msg>>

type rec t<'msg> =
  | CommentNode(string)
  | Text(string)
  | Node(string, string, string, string, properties<'msg>, list<t<'msg>>)
  | LazyGen(string, unit => t<'msg>, ref<t<'msg>>)
  | Tagger(ref<applicationCallbacks<'msg>> => ref<applicationCallbacks<'msg>>, t<'msg>)

let noNode: t<'msg>

let comment: string => t<'msg>

let text: string => t<'msg>

let fullnode: (string, string, string, string, properties<'msg>, list<t<'msg>>) => t<'msg>

let node: (
  ~namespace: string=?,
  string,
  ~key: string=?,
  ~unique: string=?,
  properties<'msg>,
  list<t<'msg>>,
) => t<'msg>

let lazyGen: (string, unit => t<'msg>) => t<'msg>

let noProp: property<'msg>

let prop: (string, string) => property<'msg>

let onCB: (string, string, Dom.event => option<'msg>) => property<'msg>

let onMsg: (string, 'msg) => property<'msg>

let attribute: (string, string, string) => property<'msg>

let data: (string, string) => property<'msg>

let style: (string, string) => property<'msg>

let styles: list<(string, string)> => property<'msg>

let renderToHtmlString: t<'msg> => string

let emptyEventHandler: eventCallback

let emptyEventCB: 'msg => option<eventCallback>

let eventHandler: (ref<applicationCallbacks<'msg>>, ref<Dom.event => option<'msg>>) => eventCallback

let eventHandler_GetCB: (eventHandler<'msg>, Dom.event) => option<'msg>

let compareEventHandlerTypes: (eventHandler<'msg>, eventHandler<'msg>) => bool

let eventHandler_Register: (
  ref<applicationCallbacks<'msg>>,
  Dom.eventTarget,
  string,
  eventHandler<'msg>,
) => option<eventCache<'msg>>

let eventHandler_Unregister: (
  Dom.eventTarget,
  string,
  option<eventCache<'msg>>,
) => option<eventCache<'msg>>

let eventHandler_Mutate: (
  ref<applicationCallbacks<'msg>>,
  Dom.eventTarget,
  string,
  string,
  eventHandler<'msg>,
  eventHandler<'msg>,
  ref<option<eventCache<'msg>>>,
  ref<option<eventCache<'msg>>>,
) => unit

let patchVNodesOnElems_PropertiesApply_Add: (
  ref<applicationCallbacks<'msg>>,
  Dom.element,
  int,
  property<'msg>,
) => unit

let patchVNodesOnElems_PropertiesApply_Remove: (
  ref<applicationCallbacks<'msg>>,
  Dom.element,
  int,
  property<'msg>,
) => unit

let patchVNodesOnElems_PropertiesApply_RemoveAdd: (
  ref<applicationCallbacks<'msg>>,
  Dom.element,
  int,
  property<'msg>,
  property<'msg>,
) => unit

let patchVNodesOnElems_PropertiesApply_Mutate: (
  ref<applicationCallbacks<'msg>>,
  Dom.element,
  int,
  property<'msg>,
  property<'msg>,
) => unit

let patchVNodesOnElems_PropertiesApply: (
  ref<applicationCallbacks<'msg>>,
  Dom.element,
  int,
  list<property<'msg>>,
  list<property<'msg>>,
) => bool

let patchVNodesOnElems_Properties: (
  ref<applicationCallbacks<'msg>>,
  Dom.element,
  list<property<'msg>>,
  list<property<'msg>>,
) => bool

let genEmptyProps: int => list<property<'msg>>

let mapEmptyProps: list<property<'msg>> => list<property<'msg>>

let patchVNodesOnElems_ReplaceNode: (
  ref<applicationCallbacks<'msg>>,
  Dom.node,
  Dom.nodeList,
  int,
  t<'msg>,
) => unit

let patchVNodesOnElems_CreateElement: (ref<applicationCallbacks<'msg>>, t<'msg>) => Dom.node

let patchVNodesOnElems_MutateNode: (
  ref<applicationCallbacks<'msg>>,
  Dom.node,
  Dom.nodeList,
  int,
  t<'msg>,
  t<'msg>,
) => unit

let patchVNodesOnElems: (
  ref<applicationCallbacks<'msg>>,
  Dom.node,
  Dom.nodeList,
  int,
  list<t<'msg>>,
  list<t<'msg>>,
) => unit

let patchVNodesIntoElement: (
  ref<applicationCallbacks<'msg>>,
  Dom.node,
  list<t<'msg>>,
  list<t<'msg>>,
) => list<t<'msg>>

let patchVNodeIntoElement: (
  ref<applicationCallbacks<'msg>>,
  Dom.node,
  t<'msg>,
  t<'msg>,
) => list<t<'msg>>

let wrapCallbacks_On: ('msg => 'b, systemMessage<'msg>) => systemMessage<'b>

let wrapCallbacks: ('msg => 'b, ref<applicationCallbacks<'b>>) => ref<applicationCallbacks<'msg>>

let map: ('msg => 'b, t<'msg>) => t<'b>
