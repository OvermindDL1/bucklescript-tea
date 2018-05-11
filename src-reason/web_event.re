/* type target = <
     value : string Js.undefined [@bs.get];
   > Js.t */
type t('node) = {
  .
  [@bs.get] "target": Js.undefined('node),
  [@bs.get] "keyCode": int,
  [@bs.meth] "preventDefault": unit => unit,
  [@bs.meth] "stopPropagation": unit => unit,
};

type cb('node) = (. t('node)) => unit;

type options = bool; /* false | true (* TODO:  Define a javascript record as another option *) */

type popstateEvent = Js.t({.});

type popstateCb = (. popstateEvent) => unit;
