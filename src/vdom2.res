// Webapi.Dom.Node.insertBefore uses a ~new label which we can't call from
// Bucklescript as it's a keyword. After converting to Rescript, this help can be
// removed and the call can be made directly.
let insertBefore = (
  parent: Webapi.Dom.Node.t,
  ~new_: Dom.node_like<'a>,
  ~before: Dom.node_like<'b>,
): Webapi.Dom.Node.t => Webapi.Dom.Node.insertBefore(parent, ~new=new_, ~before)

// Webapi.Dom.CssStyleDeclaration.setPropertyValue calls the wrong function I think - see https://github.com/tinymce/rescript-webapi/pull/114
@send external setPropertyValue: (Dom.cssStyleDeclaration, string, string) => unit = "setProperty"
