## ReasonReact Integration Example

This example describes how to integrate the Bucklescript TEA framework into a ReasonReact component. This is useful if you want to migrate an existing ReasonReact project to use Bucklescript TEA, or simply use it for a single component. You'll soon learn to love "The Elm Architecture" way of designing frontend components and their logic.

See issue https://github.com/OvermindDL1/bucklescript-tea/issues/121 for the beginning of this discussion. The resulting code can be seen below.

The process is as follows:
1. Import the Reason/OCaml Bucklescript-TEA component.
2. Get access to the HTML DOM document object, to later be able to get a reference to the React HTML element.
3. Use a ReasonReact `reducerComponent` so that you can more easily store the TEA component state and manage its lifecycle.
4. When the React component mounts, the `didMount` callback is called and the `RenderMain` action is sent.
5. Because there are side-effects in the `RenderMain` action handler, use the `UpdateWithSideEffects` method to change the current state from `Start` to `Initialized`.
6. The Bucklescript-TEA component is mounted on the DIV HTML element using a predefined HTML `id` - in this case `Tea`.
7. The ReasonReact component can now be used in an existing ReasonReact project, and using React props for example the TEA component can be interacted with via the `pushMsg` method to pass actions through to the TEA component, like `Reset` as seen in the example.

```reason
open Tea; /* Reason/OCaml Bucklescript-TEA component, as seen in existing examples */

/*
 EXAMPLE CONSOLE LOG (using BuckleScripts Js module)
  */
Js.log("Hello, from BuckleScript and Reason!");

type document; /* abstract type for a document object */
[@bs.send] external getElementById: (document, string) => Js.null_undefined(Web.Node.t) = "getElementById";
[@bs.val] external doc: document = "document";

type action = RenderMain;
type state = Start | Initialized;

// Define interface method types and create mutable references
type pushMsg = (msg) => unit;
let pushMsg: pushMsg = (msg) => ();
let pushMsgRef = ref(pushMsg);

type shutdown = (unit) => unit;
let shutdown: shutdown = () => ();

type getHtmlString = (unit) => string;
let getHtmlString: getHtmlString = () => "";

type interface = (unit) => Tea_app.programInterface(msg);

[@bs.obj]
external makeProgramInterface:
  (
    ~pushMsg: 'msg => unit,
    ~shutdown: unit => unit,
    ~getHtmlString: unit => string
  ) =>
  Tea_app.programInterface('msg) =
  "";

let interfaceRef = ref(makeProgramInterface(~pushMsg=(msg)=>(), ~shutdown=()=>(), ~getHtmlString=()=>""));

let id = "Tea";
let component = ReasonReact.reducerComponent(id);
let make = (_children) => {
  ...component,
  initialState: _state => Start,
  reducer: (_action, _state) =>
    switch (_action) {
      | RenderMain => UpdateWithSideEffects(Initialized, (_) => {
        {
          interfaceRef := main(getElementById(doc, id))();
          pushMsgRef := (interfaceRef^)##pushMsg;
          pushMsgRef^(Reset);
        }
      })
    },
  didMount: _self => _self.send(RenderMain),
  render: _self => <div id=id />
}
```