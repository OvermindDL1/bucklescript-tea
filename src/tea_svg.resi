let svgNamespace: string

let noNode: Vdom.t<'msg>

let text: string => Vdom.t<'msg>

let lazy1: (string, unit => Vdom.t<'msg>) => Vdom.t<'msg>

let node: (
  string,
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let svg: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let foreignObject: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let animate: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let animateColor: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let animateMotion: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let animateTransform: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let mpath: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let set: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let a: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let defs: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let g: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let marker: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let mask: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let missingGlyph: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let pattern: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let \"switch": (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let symbol: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let desc: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let metadata: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let title: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feBlend: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feColorMatrix: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feComponentTransfer: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feComposite: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feConvolveMatrix: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feDiffuseLighting: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feDisplacementMap: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feFlood: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feFuncA: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feFuncB: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feFuncG: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feFuncR: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feGaussianBlur: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feImage: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feMerge: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feMergeNode: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feMorphology: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feOffset: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feSpecularLighting: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feTile: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feTurbulence: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let font: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let fontFace: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let fontFaceFormat: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let fontFaceName: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let fontFaceSrc: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let fontFaceUri: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let hkern: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let vkern: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let linearGradient: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let radialGradient: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let stop: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let circle: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let ellipse: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let svgimage: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let line: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let path: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let polygon: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let polyline: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let rect: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let use: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feDistantLight: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let fePointLight: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let feSpotLight: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let altGlyph: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let altGlyphDef: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let altGlyphItem: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let glyph: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let glyphRef: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let textPath: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let text': (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let tref: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let tspan: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let clipPath: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let svgcolorProfile: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let cursor: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let filter: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let script: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let style: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>

let view: (
  ~key: string=?,
  ~unique: string=?,
  Vdom.properties<'msg>,
  list<Vdom.t<'msg>>,
) => Vdom.t<'msg>
