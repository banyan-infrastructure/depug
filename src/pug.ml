open Util

type ast = Js.Json.t

type astAttr =
  { name : string
  ; val_ : string
  ; mustEscape : bool
  }

and astTag =
  { name : string
  ; selfClosing : bool
  ; block : astNode
  ; attrs : astAttr list
  ; attributeBlocks : astAttr list
  }
         
and astNode =
  | Block of astNode list
  | Tag of astTag 

let decodeAttr attr =
  let sel path dec = Xsel.xsel path dec attr in
  let name = sel "name" Js.Json.decodeString in
  let val_ = sel "val" Js.Json.decodeString in
  let mustEscape = sel "mustEscape" Js.Json.decodeBoolean in
  match (name, val_, mustEscape) with
  | (Some name, Some val_, Some mustEscape) ->
     Some { name = name ; val_ = val_ ; mustEscape = mustEscape }
  | _ -> None
         
let rec decodeAstNode ast =
  let sel path dec = Xsel.xsel path dec ast in
  let type_ = sel "type" Js.Json.decodeString in
  let name = sel "name" Js.Json.decodeString in
  let selfClosing = sel "selfClosing" Js.Json.decodeBoolean in
  let nodes =
    sel "nodes" Js.Json.decodeArray
    |> optionDefault [| |]
    |> Array.to_list
    |> List.map decodeAstNode
    |> List.map listSingleton
    |> List.concat
  in
  let attrs =
    sel "attrs" Js.Json.decodeArray
    |> optionDefault [| |]
    |> Array.to_list
    |> List.map decodeAttr
    |> List.map listSingleton
    |> List.concat
  in
  let block = sel "block" decodeAstNode in
  let attrBlocks = sel "attributeBlocks" (fun x -> Some x) in
  match   (type_ , name     , selfClosing     , block     ) with
  | (Some "Block", _        , _               , _         ) ->
     Some (Block nodes)
  | (Some "Tag"  , Some name, Some selfClosing, Some block) ->
     Some
       (Tag
          { name = name
          ; selfClosing = selfClosing
          ; attrs = attrs
          ; block = block
          ; attributeBlocks = []
          }
       )
  | _ -> None
         
let parse : string -> ast =
  [%bs.raw {|
            function(src) {
                var lex = require('pug-lexer');
                var parse = require('pug-parser');
                var filename = 'test.pug';
                var tokens = lex(src, {filename: filename});
                return parse(tokens, {filename: filename, src: src});
            }
  |} ]
