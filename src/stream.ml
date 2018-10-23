type readable = Unused0
type writable = Unused1

let onWClose : (unit -> unit) -> writable -> unit =
  [%bs.raw {| function($0,$1) { return $1.on('close',$0); } |} ]


let onWDrain : (unit -> unit) -> writable -> unit =
  [%bs.raw {| function($0,$1) { return $1.on('drain',$0); } |} ]


let onWError : (string -> unit) -> writable -> unit =
  [%bs.raw {| function($0,$1) { return $1.on('error',function(err) { $0('' + err); }); } |} ]


let endString : string -> writable -> unit =
  [%bs.raw {| function($0,$1) { return $1.end($0); } |} ]

let writeString : string -> writable -> unit =
  [%bs.raw {| function($0,$1) { return $1.write($0); } |} ]


let writeBuffer : Node.Buffer.t -> writable -> unit =
  [%bs.raw {| function($0,$1) { return $1.write($0); } |} ]


let cork : writable -> unit =
  [%bs.raw {| function($0) { return $0.cork(); } |} ]


let uncork : writable -> unit =
  [%bs.raw {| function($0) { return $0.uncork(); } |} ]


let onRDataString : (string -> unit) -> readable -> unit =
  [%bs.raw {| function($0,$1) { return $1.on('data',function(b) { $0(b.toString()); }); } |} ]


let onRDataBuffer : (Node.Buffer.t -> unit) -> readable -> unit =
  [%bs.raw {| function($0,$1) { return $1.on('data',function(b) { $0(b); }); } |} ]


let onRClose : (unit -> unit) -> readable -> unit =
  [%bs.raw {| function($0,$1) { return $1.on('close',$0); } |} ]


let onRError : (string -> unit) -> readable -> unit =
  [%bs.raw {| function($0,$1) { return $1.on('error',function (err) { $0('' + err); }); } |} ]

let onREnd : (unit -> unit) -> readable -> unit =
  [%bs.raw {| function($0) { return $1.on('end',$0); } |} ]

let pause : readable -> unit =
  [%bs.raw {| function($0) { return $0.pause(); } |} ]

let resume : readable -> unit =
  [%bs.raw {| function($0) { return $0.resume(); } |} ]


let isPaused : readable -> bool =
  [%bs.raw {| function($0) { return $0.isPaused(); } |} ]


let pipe : readable -> writable -> unit =
  [%bs.raw {| function($0,$1) { return $1.pipe($0); } |} ]
