let error : string -> exn =
  [%bs.raw {| function(err) { return new Error(err); } |} ]
