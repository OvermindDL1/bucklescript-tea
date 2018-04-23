(** This file is organized roughly in order of popularity. The tags which you'd
expect to use frequently will be closer to the top. *)

open Vdom

module Cmds = Tea_html_cmds

(** {1 Primitives} *)

let text str = text str

let node ?(namespace="") tagName ?(key="") ?(unique="") props nodes = fullnode namespace tagName key unique props nodes

let noNode = noNode

let lazy1 key gen = lazyGen key gen

(** {1 Tags} *)

(** {2 Headers} *)

let h1 ?(key="") ?(unique="") props nodes = fullnode "" "h1" key unique props nodes

let h2 ?(key="") ?(unique="") props nodes = fullnode "" "h2" key unique props nodes

let h3 ?(key="") ?(unique="") props nodes = fullnode "" "h3" key unique props nodes

let h4 ?(key="") ?(unique="") props nodes = fullnode "" "h4" key unique props nodes

let h5 ?(key="") ?(unique="") props nodes = fullnode "" "h5" key unique props nodes

let h6 ?(key="") ?(unique="") props nodes = fullnode "" "h6" key unique props nodes

(** {2 Grouping Content} *)

let div ?(key="") ?(unique="") props nodes = fullnode "" "div" key unique props nodes

let p ?(key="") ?(unique="") props nodes = fullnode "" "p" key unique props nodes

let hr ?(key="") ?(unique="") props nodes = fullnode "" "hr" key unique props nodes

let pre ?(key="") ?(unique="") props nodes = fullnode "" "pre" key unique props nodes

let blockquote ?(key="") ?(unique="") props nodes = fullnode "" "blockquote" key unique props nodes

(** {2 Text} *)

let span ?(key="") ?(unique="") props nodes = fullnode "" "span" key unique props nodes

let a ?(key="") ?(unique="") props nodes = fullnode "" "a" key unique props nodes

let code ?(key="") ?(unique="") props nodes = fullnode "" "code" key unique props nodes

let em ?(key="") ?(unique="") props nodes = fullnode "" "em" key unique props nodes

let strong ?(key="") ?(unique="") props nodes = fullnode "" "strong" key unique props nodes

let i ?(key="") ?(unique="") props nodes = fullnode "" "i" key unique props nodes

let b ?(key="") ?(unique="") props nodes = fullnode "" "b" key unique props nodes

let u ?(key="") ?(unique="") props nodes = fullnode "" "u" key unique props nodes

let sub ?(key="") ?(unique="") props nodes = fullnode "" "sub" key unique props nodes

let sup ?(key="") ?(unique="") props nodes = fullnode "" "sup" key unique props nodes

let br props = fullnode "" "br" "br" "br" props []

let br' ?(key="") ?(unique="") props nodes = fullnode "" "br" key unique props nodes

(** {2 Lists} *)

let ol ?(key="") ?(unique="") props nodes = fullnode "" "ol" key unique props nodes

let ul ?(key="") ?(unique="") props nodes = fullnode "" "ul" key unique props nodes

let li ?(key="") ?(unique="") props nodes = fullnode "" "li" key unique props nodes

let dl ?(key="") ?(unique="") props nodes = fullnode "" "dl" key unique props nodes

let dt ?(key="") ?(unique="") props nodes = fullnode "" "dt" key unique props nodes

let dd ?(key="") ?(unique="") props nodes = fullnode "" "dd" key unique props nodes

(** {2 Embedded Content} *)

let img ?(key="") ?(unique="") props nodes = fullnode "" "img" key unique props nodes

let iframe ?(key="") ?(unique="") props nodes = fullnode "" "iframe" key unique props nodes

let canvas ?(key="") ?(unique="") props nodes = fullnode "" "canvas" key unique props nodes

let math ?(key="") ?(unique="") props nodes = fullnode "" "math" key unique props nodes

(** {2 Form and inputs} *)

let form ?(key="") ?(unique="") props nodes = fullnode "" "form" key unique props nodes

let input' ?(key="") ?(unique="") props nodes = fullnode "" "input" key unique props nodes

let textarea ?(key="") ?(unique="") props nodes = fullnode "" "textarea" key unique props nodes

let button ?(key="") ?(unique="") props nodes = fullnode "" "button" key unique props nodes

let select ?(key="") ?(unique="") props nodes = fullnode "" "select" key unique props nodes

let option' ?(key="") ?(unique="") props nodes = fullnode "" "option" key unique props nodes

let optgroup ?(key="") ?(unique="") props nodes = fullnode "" "optgroup" key unique props nodes

let label ?(key="") ?(unique="") props nodes = fullnode "" "label" key unique props nodes

let fieldset ?(key="") ?(unique="") props nodes = fullnode "" "fieldset" key unique props nodes

let legend ?(key="") ?(unique="") props nodes = fullnode "" "legend" key unique props nodes

(** {2 Sections} *)

let section ?(key="") ?(unique="") props nodes = fullnode "" "section" key unique props nodes

let nav ?(key="") ?(unique="") props nodes = fullnode "" "nav" key unique props nodes

let article ?(key="") ?(unique="") props nodes = fullnode "" "article" key unique props nodes

let aside ?(key="") ?(unique="") props nodes = fullnode "" "aside" key unique props nodes

let header ?(key="") ?(unique="") props nodes = fullnode "" "header" key unique props nodes

let footer ?(key="") ?(unique="") props nodes = fullnode "" "footer" key unique props nodes

let address ?(key="") ?(unique="") props nodes = fullnode "" "address" key unique props nodes

let main ?(key="") ?(unique="") props nodes = fullnode "" "main" key unique props nodes

let body ?(key="") ?(unique="") props nodes = fullnode "" "body" key unique props nodes

(** {2 Figures} *)

let figure ?(key="") ?(unique="") props nodes = fullnode "" "figure" key unique props nodes

let figcaption ?(key="") ?(unique="") props nodes = fullnode "" "figcaption" key unique props nodes

(** {2 Tables} *)

let table ?(key="") ?(unique="") props nodes = fullnode "" "table" key unique props nodes

let caption ?(key="") ?(unique="") props nodes = fullnode "" "caption" key unique props nodes

let colgroup ?(key="") ?(unique="") props nodes = fullnode "" "colgroup" key unique props nodes

let col ?(key="") ?(unique="") props nodes = fullnode "" "col" key unique props nodes

let tbody ?(key="") ?(unique="") props nodes = fullnode "" "tbody" key unique props nodes

let thead ?(key="") ?(unique="") props nodes = fullnode "" "thead" key unique props nodes

let tfoot ?(key="") ?(unique="") props nodes = fullnode "" "tfoot" key unique props nodes

let tr ?(key="") ?(unique="") props nodes = fullnode "" "tr" key unique props nodes

let th ?(key="") ?(unique="") props nodes = fullnode "" "th" key unique props nodes

let td ?(key="") ?(unique="") props nodes = fullnode "" "td" key unique props nodes

(** {2 Less common inputs} *)

let datalist ?(key="") ?(unique="") props nodes = fullnode "" "datalist" key unique props nodes

let keygen ?(key="") ?(unique="") props nodes = fullnode "" "keygen" key unique props nodes

let output ?(key="") ?(unique="") props nodes = fullnode "" "output" key unique props nodes

let progress ?(key="") ?(unique="") props nodes = fullnode "" "progress" key unique props nodes

let meter ?(key="") ?(unique="") props nodes = fullnode "" "meter" key unique props nodes

(** {2 Audio and Video} *)

let audio ?(key="") ?(unique="") props nodes = fullnode "" "audio" key unique props nodes

let video ?(key="") ?(unique="") props nodes = fullnode "" "video" key unique props nodes

let source ?(key="") ?(unique="") props nodes = fullnode "" "source" key unique props nodes

let track ?(key="") ?(unique="") props nodes = fullnode "" "track" key unique props nodes

(** {2 Embedded objects} *)

let embed ?(key="") ?(unique="") props nodes = fullnode "" "embed" key unique props nodes

let object' ?(key="") ?(unique="") props nodes = fullnode "" "object" key unique props nodes

let param ?(key="") ?(unique="") props nodes = fullnode "" "param" key unique props nodes

(** {2 Text edits} *)

let ins ?(key="") ?(unique="") props nodes = fullnode "" "ins" key unique props nodes

let del ?(key="") ?(unique="") props nodes = fullnode "" "del" key unique props nodes

(** {2 Semantic text} *)

let small ?(key="") ?(unique="") props nodes = fullnode "" "small" key unique props nodes

let cite ?(key="") ?(unique="") props nodes = fullnode "" "cite" key unique props nodes

let dfn ?(key="") ?(unique="") props nodes = fullnode "" "dfn" key unique props nodes

let abbr ?(key="") ?(unique="") props nodes = fullnode "" "abbr" key unique props nodes

let time ?(key="") ?(unique="") props nodes = fullnode "" "time" key unique props nodes

let var' ?(key="") ?(unique="") props nodes = fullnode "" "var" key unique props nodes

let samp ?(key="") ?(unique="") props nodes = fullnode "" "samp" key unique props nodes

let kbd ?(key="") ?(unique="") props nodes = fullnode "" "kbd" key unique props nodes

let s ?(key="") ?(unique="") props nodes = fullnode "" "s" key unique props nodes

let q ?(key="") ?(unique="") props nodes = fullnode "" "q" key unique props nodes

(** {2 Less common text tags} *)

let mark ?(key="") ?(unique="") props nodes = fullnode "" "mark" key unique props nodes

let ruby ?(key="") ?(unique="") props nodes = fullnode "" "ruby" key unique props nodes

let rt ?(key="") ?(unique="") props nodes = fullnode "" "rt" key unique props nodes

let rp ?(key="") ?(unique="") props nodes = fullnode "" "rp" key unique props nodes

let bdi ?(key="") ?(unique="") props nodes = fullnode "" "bdi" key unique props nodes

let bdo ?(key="") ?(unique="") props nodes = fullnode "" "bdo" key unique props nodes

let wbr ?(key="") ?(unique="") props nodes = fullnode "" "wbr" key unique props nodes

(** {2 Interactive elements} *)

let details ?(key="") ?(unique="") props nodes = fullnode "" "details" key unique props nodes

let summary ?(key="") ?(unique="") props nodes = fullnode "" "summary" key unique props nodes

let menuitem ?(key="") ?(unique="") props nodes = fullnode "" "menuitem" key unique props nodes

let menu ?(key="") ?(unique="") props nodes = fullnode "" "menu" key unique props nodes

(** {2 Header elements} *)

let meta ?(key="") ?(unique="") props = fullnode "" "meta" key unique props []

let style ?(key="") ?(unique="") props content = fullnode "" "style" key unique props [text content]

let title ?(key="") ?(unique="") props content = fullnode "" "title" key unique props [text content]

let link ?(key="") ?(unique="") props = fullnode "" "link" key unique props []


(** Helper functions for HTML attributes. They are organized roughly by category. *)
module Attributes = struct

  (** {1 Primitives} *)

  let noProp = Vdom.noProp

  let style key value = style key value

  let styles s = styles s

  (** {1 Super common attributes} *)

  let class' name = prop "className" name

  let classList classes =
    classes
    |> List.filter (fun (_fst, snd) -> snd)
    |> List.map (fun (fst, _snd) -> fst)
    |> String.concat " "
    |> class'

  let id str = prop "id" str

  let title str = attribute "" "title" str

  let hidden b = if b then prop "hidden" "hidden" else noProp

  (** {1 Inputs} *)

  let type' typ = prop "type" typ

  let value str = prop "value" str

  let defaultValue str = prop "defaultValue" str

  let checked b = if b then prop "checked" "checked" else noProp

  let placeholder str = prop "placeholder" str

  let selected b = if b then attribute "" "selected" "true" else noProp

  (** {1 Input helpers} *)

  let accept c = attribute "" "accept" c

  let acceptCharset c = attribute "" "accept-charset" c

  let action a = prop "action" a

  let autocomplete b = prop "autocomplete" (if b then "on" else "off")

  let autofocus b = if b then prop "autofocus" "autofocus" else noProp

  let disabled b = if b then attribute "" "disabled" "true" else noProp

  let enctype encoding = attribute "" "enctype" encoding

  let formaction url = attribute "" "formaction" url

  let list value = attribute "" "list" value

  let minlength n = attribute "" "minlength" (string_of_int n)

  let maxlength n = attribute "" "maxlength" (string_of_int n)

  let method' m = prop "method" m

  let multiple b = if b then prop "multiple" "multiple" else noProp

  let name str = prop "name" str

  let novalidate b = if b then prop "novalidate" "novalidate" else noProp

  let pattern p = prop "pattern" p

  let readonly b = if b then prop "readonly" "readonly" else noProp

  let required b = if b then prop "required" "required" else noProp

  let size n = attribute "" "size" (string_of_int n)

  let for' str = prop "htmlFor" str

  let form value = attribute "" "form" value

  (** {1 Input ranges} *)

  let max value = attribute "" "max" value

  let min value = attribute "" "min" value

  let step value = attribute "" "step" value

  (** {1 Textarea} *)

  let cols n = attribute "" "cols" (string_of_int n)

  let rows n = attribute "" "rows" (string_of_int n)

  let wrap value = prop "wrap" value

  (** {1 Links and areas} *)

  (* `href` is actually an attribute, not a property, but need it here for Elm compat... *)
  let href str = attribute "" "href" str

  let target t = prop "target" t

  let download b = if b then prop "download" "" else noProp

  let downloadAs name = prop "download" name

  let hreflang code = prop "hreflang" code

  let media value = attribute "" "media" value

  let ping url = prop "ping" url

  let rel value = attribute "" "rel" value

  (** {1 Maps} *)

  let ismap b = if b then prop "ismap" "ismap" else noProp

  let usemap name = prop "usemap" name

  let shape value = prop "shape" value

  let coords value = prop "coords" value

  (** {1 Embedded content} *)

  (* `src` is actually an attribute, not a property, but need it here for Elm compat... *)
  let src str = attribute "" "src" str

  let height n = attribute "" "height" (string_of_int n)

  let width n = attribute "" "width" (string_of_int n)

  let alt value = prop "alt" value

  (** {1 Audio and Video} *)

  let autoplay b = if b then prop "autoplay" "autoplay" else noProp

  let controls b = if b then prop "controls" "controls" else noProp

  let loop b = if b then prop "loop" "loop" else noProp

  let preload value = prop "preload" value

  let poster url = prop "poster" url

  let default b = if b then prop "default" "default" else noProp

  let kind value = prop "kind" value

  let srclang code = prop "srclang" code

  (** {1 IFrames} *)

  let sandbox value = prop "sandbox" value

  let seamless b = if b then prop "seamless" "seamless" else noProp

  let srcdoc value = prop "srcdoc" value

  (** {1 Ordered lists} *)

  let reversed b = if b then prop "reversed" "reversed" else noProp

  let start n = prop "start" (string_of_int n)

  (** {1 Tables} *)

  let colspan n = attribute "" "colspan" (string_of_int n)

  let rowspan n = attribute "" "rowspan" (string_of_int n)

  let headers value = prop "headers" value

  let scope value = prop "scope" value

  let align value = prop "align" value

  (** {1 Header stuff} *)

  let async b = if b then prop "async" "async" else noProp

  let charset value = attribute "" "charset" value

  let content value = attribute "" "content" value

  let defer b = if b then prop "defer" "defer" else noProp

  let httpEquiv value = prop "http-equiv" value

  let language value = prop "language" value

  let scoped value = prop "scoped" value

  (** {1 Less common global attributes} *)

  let accesskey ch = prop "accesskey" (String.make 1 ch)

  let contenteditable b = if b then prop "contenteditable" "contenteditable" else noProp

  let contextmenu id = attribute "" "contextmenu" id

  let dir value = prop "dir" value

  let draggable value = attribute "" "draggable" value

  let dropzone value = prop "dropzone" value

  let itemprop value = attribute "" "itemprop" value

  let lang code = prop "lang" code

  let spellcheck b = if b then prop "spellcheck" "spellcheck" else noProp

  let tabindex n = attribute "" "tabindex" (string_of_int n)

  (** {1 Key generation} *)

  let challenge value = attribute "" "challenge" value

  let keytype value = prop "keytype" value

  (** {1 Miscellaneous} *)

  let cite url = prop "cite" url

  let datetime value = attribute "" "datetime" value

  let pubdate value = attribute "" "pubdate" value

  let manifest value = attribute "" "manifest" value

end


module Events = struct

  (** {1 Primitives} *)

  let onCB eventName key cb = onCB eventName key cb

  let onMsg eventName msg = onMsg eventName msg

  type options = {
    stopPropagation : bool;
    preventDefault : bool;
  }

  let defaultOptions = {
    stopPropagation = false;
    preventDefault = false;
  }

  let onWithOptions eventName options decoder =
    onCB eventName "" (fun event ->
      if options.stopPropagation then event##stopPropagation () |> ignore;
      if options.preventDefault then event##preventDefault () |> ignore;
      event
      |> Tea_json.Decoder.decodeEvent decoder
      |> Tea_result.result_to_option
    )

  let on eventName decoder = onWithOptions eventName defaultOptions decoder

  let targetValue = Tea_json.Decoder.at ["target"; "value"] Tea_json.Decoder.string

  let targetChecked = Tea_json.Decoder.at ["target"; "checked"] Tea_json.Decoder.bool

  let keyCode = Tea_json.Decoder.field "keyCode" Tea_json.Decoder.int

  (** {1 Mouse helpers} *)

  let onClick msg = onMsg "click" msg

  let onDoubleClick msg = onMsg "dblclick" msg

  let onMouseDown msg = onMsg "mousedown" msg

  let onMouseUp msg = onMsg "mouseup" msg

  let onMouseEnter msg = onMsg "mouseenter" msg

  let onMouseLeave msg = onMsg "mouseleave" msg

  let onMouseOver msg = onMsg "mouseover" msg

  let onMouseOut msg = onMsg "mouseout" msg

  (** {1 Form helpers} *)

  let onInputOpt ?(key="") msg =
    onCB "input" key
      (fun ev ->
         match Js.Undefined.toOption ev##target with
         | None -> None
         | Some target -> match Js.Undefined.toOption target##value with
           | None -> None
           | Some value -> msg value
      )

  let onInput ?(key="") msg = onInputOpt ~key:key (fun ev -> Some (msg ev))

  let onCheckOpt ?(key="") msg =
    onCB "change" key
      (fun ev ->
         match Js.Undefined.toOption ev##target with
         | None -> None
         | Some target -> match Js.Undefined.toOption target##checked with
           | None -> None
           | Some value -> msg value
      )

  let onCheck ?(key="") msg = onCheckOpt ~key:key (fun ev -> Some (msg ev))

  let onChangeOpt ?(key="") msg =
    onCB "change" key
    (fun ev ->
         match Js.Undefined.toOption ev##target with
         | None -> None
         | Some target -> match Js.Undefined.toOption target##value with
           | None -> None
           | Some value -> msg value
      )

  let onChange ?(key="") msg = onChangeOpt ~key:key (fun ev -> Some (msg ev))

  (** {1 Focus helpers} *)

  let onBlur msg = onMsg "blur" msg

  let onFocus msg = onMsg "focus" msg

end
