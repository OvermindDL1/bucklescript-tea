module Cmds = Tea_html_cmds
let map: ('a => 'b, Vdom.t<'a>) => Vdom.t<'b>
let text: string => Vdom.t<'a>
let node: (
  ~namespace: string=?,
  string,
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'a>,
  list<Vdom.t<'a>>,
) => Vdom.t<'a>
let noNode: Vdom.t<'a>
let lazy1: (string, unit => Vdom.t<'a>) => Vdom.t<'a>
let h1: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let h2: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let h3: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let h4: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let h5: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let h6: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let div: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let p: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let hr: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let pre: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let blockquote: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'a>,
  list<Vdom.t<'a>>,
) => Vdom.t<'a>
let span: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let a: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let code: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let em: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let strong: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let i: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let b: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let u: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let sub: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let sup: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let br: Vdom.properties<'a> => Vdom.t<'a>
let br': (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let ol: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let ul: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let li: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let dl: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let dt: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let dd: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let img: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let iframe: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let canvas: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let math: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let form: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let input': (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let textarea: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let button: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let select: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let option': (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let optgroup: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let label: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let fieldset: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let legend: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let section: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let nav: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let article: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let aside: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let header: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let footer: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let address: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let main: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let body: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let figure: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let figcaption: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'a>,
  list<Vdom.t<'a>>,
) => Vdom.t<'a>
let table: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let caption: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let colgroup: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let col: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let tbody: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let thead: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let tfoot: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let tr: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let th: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let td: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let datalist: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let keygen: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let output: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let progress: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let meter: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let audio: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let video: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let source: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let track: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let embed: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let object': (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let param: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let ins: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let del: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let small: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let cite: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let dfn: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let abbr: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let time: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let var': (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let samp: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let kbd: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let s: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let q: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let mark: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let ruby: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let rt: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let rp: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let bdi: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let bdo: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let wbr: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let details: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let summary: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let menuitem: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<
  'a,
>
let menu: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, list<Vdom.t<'a>>) => Vdom.t<'a>
let meta: (~key: string=?, ~unique: string=?, Vdom.properties<'a>) => Vdom.t<'a>
let style: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, string) => Vdom.t<'a>
let title: (~key: string=?, ~unique: string=?, Vdom.properties<'a>, string) => Vdom.t<'a>
let link: (~key: string=?, ~unique: string=?, Vdom.properties<'a>) => Vdom.t<'a>
module Attributes: {
  let noProp: Vdom.property<'a>
  let style: (string, string) => Vdom.property<'a>
  let styles: list<(string, string)> => Vdom.property<'a>
  let class': string => Vdom.property<'a>
  let classList: list<(string, bool)> => Vdom.property<'a>
  let id: string => Vdom.property<'a>
  let title: string => Vdom.property<'a>
  let hidden: bool => Vdom.property<'a>
  let type': string => Vdom.property<'a>
  let value: string => Vdom.property<'a>
  let defaultValue: string => Vdom.property<'a>
  let checked: bool => Vdom.property<'a>
  let placeholder: string => Vdom.property<'a>
  let selected: bool => Vdom.property<'a>
  let accept: string => Vdom.property<'a>
  let acceptCharset: string => Vdom.property<'a>
  let action: string => Vdom.property<'a>
  let autocomplete: bool => Vdom.property<'a>
  let autofocus: bool => Vdom.property<'a>
  let disabled: bool => Vdom.property<'a>
  let enctype: string => Vdom.property<'a>
  let formaction: string => Vdom.property<'a>
  let list: string => Vdom.property<'a>
  let minlength: int => Vdom.property<'a>
  let maxlength: int => Vdom.property<'a>
  let method': string => Vdom.property<'a>
  let multiple: bool => Vdom.property<'a>
  let name: string => Vdom.property<'a>
  let novalidate: bool => Vdom.property<'a>
  let pattern: string => Vdom.property<'a>
  let readonly: bool => Vdom.property<'a>
  let required: bool => Vdom.property<'a>
  let size: int => Vdom.property<'a>
  let for': string => Vdom.property<'a>
  let form: string => Vdom.property<'a>
  let max: string => Vdom.property<'a>
  let min: string => Vdom.property<'a>
  let step: string => Vdom.property<'a>
  let cols: int => Vdom.property<'a>
  let rows: int => Vdom.property<'a>
  let wrap: string => Vdom.property<'a>
  let href: string => Vdom.property<'a>
  let target: string => Vdom.property<'a>
  let download: bool => Vdom.property<'a>
  let downloadAs: string => Vdom.property<'a>
  let hreflang: string => Vdom.property<'a>
  let media: string => Vdom.property<'a>
  let ping: string => Vdom.property<'a>
  let rel: string => Vdom.property<'a>
  let ismap: bool => Vdom.property<'a>
  let usemap: string => Vdom.property<'a>
  let shape: string => Vdom.property<'a>
  let coords: string => Vdom.property<'a>
  let src: string => Vdom.property<'a>
  let height: int => Vdom.property<'a>
  let width: int => Vdom.property<'a>
  let alt: string => Vdom.property<'a>
  let autoplay: bool => Vdom.property<'a>
  let controls: bool => Vdom.property<'a>
  let loop: bool => Vdom.property<'a>
  let preload: string => Vdom.property<'a>
  let poster: string => Vdom.property<'a>
  let default: bool => Vdom.property<'a>
  let kind: string => Vdom.property<'a>
  let srclang: string => Vdom.property<'a>
  let sandbox: string => Vdom.property<'a>
  let seamless: bool => Vdom.property<'a>
  let srcdoc: string => Vdom.property<'a>
  let reversed: bool => Vdom.property<'a>
  let start: int => Vdom.property<'a>
  let colspan: int => Vdom.property<'a>
  let rowspan: int => Vdom.property<'a>
  let headers: string => Vdom.property<'a>
  let scope: string => Vdom.property<'a>
  let align: string => Vdom.property<'a>
  let async: bool => Vdom.property<'a>
  let charset: string => Vdom.property<'a>
  let content: string => Vdom.property<'a>
  let defer: bool => Vdom.property<'a>
  let httpEquiv: string => Vdom.property<'a>
  let language: string => Vdom.property<'a>
  let scoped: string => Vdom.property<'a>
  let accesskey: char => Vdom.property<'a>
  let contenteditable: bool => Vdom.property<'a>
  let contextmenu: string => Vdom.property<'a>
  let dir: string => Vdom.property<'a>
  let draggable: string => Vdom.property<'a>
  let dropzone: string => Vdom.property<'a>
  let itemprop: string => Vdom.property<'a>
  let lang: string => Vdom.property<'a>
  let spellcheck: bool => Vdom.property<'a>
  let tabindex: int => Vdom.property<'a>
  let challenge: string => Vdom.property<'a>
  let keytype: string => Vdom.property<'a>
  let cite: string => Vdom.property<'a>
  let datetime: string => Vdom.property<'a>
  let pubdate: string => Vdom.property<'a>
  let manifest: string => Vdom.property<'a>
}
module Events: {
  let onCB: (string, string, Web.Node.event => option<'a>) => Vdom.property<'a>
  let onMsg: (string, 'a) => Vdom.property<'a>
  let on: (~key: string, string, Tea_json.Decoder.t<'a, 'b>) => Vdom.property<'b>
  let onWithOptions: (
  ~key: string,
  string,
  Tea_html.options,
  Tea_json.Decoder.t<'a, 'b>,
) => Vdom.property<'b>
  let defaultOptions: Tea_html.options
  let targetValue: Tea_json.Decoder.t<Web.Json.t, string>
  let targetChecked: Tea_json.Decoder.t<Web.Json.t, bool>
  let keyCode: Tea_json.Decoder.t<Web.Json.t, int>
  let preventDefaultOn: (~key: string=?, string, Tea_json.Decoder.t<'a, 'b>) => Vdom.property<'b>
  let onClick: 'a => Vdom.property<'a>
  let onDoubleClick: 'a => Vdom.property<'a>
  let onMouseDown: 'a => Vdom.property<'a>
  let onMouseUp: 'a => Vdom.property<'a>
  let onMouseEnter: 'a => Vdom.property<'a>
  let onMouseLeave: 'a => Vdom.property<'a>
  let onMouseOver: 'a => Vdom.property<'a>
  let onMouseOut: 'a => Vdom.property<'a>
  let onInputOpt: (~key: string=?, string => option<'a>) => Vdom.property<'a>
  let onInput: (~key: string=?, string => 'a) => Vdom.property<'a>
  let onCheckOpt: (~key: string=?, bool => option<'a>) => Vdom.property<'a>
  let onCheck: (~key: string=?, bool => 'a) => Vdom.property<'a>
  let onChangeOpt: (~key: string=?, string => option<'a>) => Vdom.property<'a>
  let onChange: (~key: string=?, string => 'a) => Vdom.property<'a>
  let onSubmit: 'a => Vdom.property<'a>
  let onBlur: 'a => Vdom.property<'a>
  let onFocus: 'a => Vdom.property<'a>
}
