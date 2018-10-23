let xsel_ : string -> Js.Json.t option -> (Js.Json.t -> Js.Json.t option) -> Js.Json.t -> Js.Json.t option =
  [%bs.raw {| function(selection,none,some,obj) {
            var split = selection.split("/");
            for (var i = 0; i < split.length; i++) { 
                var key = split[i];
                if (obj && obj[key] !== undefined) {
                    obj = obj[key];
                } else {
                    return none;
                }
            }
            return some(obj);
  } |} ]

let xsel selection f obj =
  match xsel_ selection None (fun a -> Some a) obj with
  | Some a -> f a
  | None -> None
