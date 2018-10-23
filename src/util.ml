(* Util *)
let optionMap f v =
  match v with
  | Some v -> Some (f v)
  | None -> None

let optionThen f v =
  match v with
  | Some v -> f v
  | None -> None

let optionDefault d v =
  match v with
  | Some v -> v
  | None -> d

let listSingleton v =
  match v with
  | Some v -> [v]
  | None -> []

let rec fold_left f s l =
  match l with
  | hd :: tl -> fold_left f (f s hd) tl
  | _ -> s

