let pugfile =
  let p = Node.Process.process in
  let args = p##argv in
  if Array.length args == 3 then
    Qio.readText (Array.get args 2)
  else
    Js.Promise.reject (Error.error "No filename provided")

let exitProcess err =
  let p = Node.Process.process in
  begin
    Js.log err ;
    p##abort ()
  end

let escaped str = str
  
let rec makeAttrs (t : Pug.astTag) =
  match t.attrs with
  | [] -> ""
  | attr :: attrs ->
     " " ^ attr.name ^ "=" ^ (escaped attr.val_) ^
       (makeAttrs { t with attrs = attrs })

let rec asHtml =
  let continueTag (t : Pug.astTag) =
    match t.block with
    | Block [] ->
       if t.selfClosing then
         "/>"
       else
         "></" ^ t.name ^ ">"
    | Block lst ->
       let inner =
         Util.fold_left
           (fun str ast ->
             str ^ (asHtml ast)
           )
           ">"
           lst
       in
       inner ^ "</" ^ t.name ^ ">"
    | Tag t ->
       ">" ^ (asHtml (Tag t)) ^ "</" ^ t.name ^ ">"
  in
  function
  | Tag t ->
     "<" ^ t.name ^ (makeAttrs t) ^ (continueTag t)
  | Block l ->
     let inner =
       Util.fold_left
         (fun str ast ->
           str ^ (asHtml ast)
         )
         ""
         l
     in
     inner
     
let _ =
  pugfile
  |> Js.Promise.then_ (fun text -> Js.Promise.resolve (Pug.parse text))
  |> Js.Promise.then_
       (fun ast ->
         match Pug.decodeAstNode ast with
         | Some tx ->
            let html = asHtml tx in
            let _ = Js.log (Pretty.pretty html) in
            Js.Promise.resolve ()
         | None ->
            Js.Promise.reject (Error.error "Error decoding pug")
       )
  |> Js.Promise.catch
       (fun err -> exitProcess err ; Js.Promise.resolve ())
