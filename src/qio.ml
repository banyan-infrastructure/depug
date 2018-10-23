type fsModule = Unused0

let fs_ : unit -> fsModule =
  [%bs.raw {| function () { var fs = require('fs'); return fs; } |} ]


let fs_module = fs_ ()

let readText_ : fsModule -> string -> (string -> unit) -> (string -> unit) -> unit =
    [%bs.raw {|
	function (fs,name,reject,resolve) {
            try { 
                return fs.readFile(
		    name,{flags: 'r'}, 
                    function (err,data) {
                        if (err) { 
                            reject('' + err); 
                        } else {
                            if (typeof(data) === 'string') {
                                resolve(data); 
                            } else {
				resolve(data.toString());
			    }
			}
		    });
	    } catch (err) {
		setTimeout(reject('' + err), 0);
	    }
	} |} ]

let readBuffer_ : fsModule -> string -> (string -> unit) -> (Node.Buffer.t -> unit) -> unit =
    [%bs.raw {|
	function ($0,$1,$2,$3) {
	    try {
		    return $0.readFile(
		        $1,{flags: 'b'},
		        function (err,data) {
			        if (err) {
			            $2('' + err);
			        } else {
			            if (typeof(data) === 'string') {
				            $3(data);
			            } else {
				            $3(data);
			            }
			        }
		        });
		} catch (err) {
		    setTimeout($2('' + err), 0);
		}
	} |} ]

let writeText_ : fsModule -> string -> string -> (string -> unit) -> (unit -> unit) -> unit =
    [%bs.raw {| function ($0,$1,$2,$3,$4) {
	    try {
		return $0.writeFile($1,$2,function (err) {
		    if (err) {
			$3('' + err);
		    } else {
			$4([]);
		    }
		});
	    } catch (err) {
		setTimeout(function () { $3('' + err) }, 0);
	    }
	} |} ]


let writeBuffer_ : fsModule -> string -> Node.Buffer.t -> (string -> unit) -> (unit -> unit) -> unit =
    [%bs.raw {|
	      function ($0,$1,$2,$3) {
	    try {
		return $0.writeFile($1,$2,function (err) {
		    if (err) {
			$3('' + err);
		    } else {
			$4([]);
		    }
		});
	    } catch (err) {
		setTimeout(function () { $3('' + err) }, 0);
	    }
	} |} ]

let writableFileStream_ : fsModule -> string -> Stream.writable =
    [%bs.raw {|
	function (fs,name) {
	    return fs.createWriteStream(name);
	} |} ]

let rename_ : fsModule -> string -> string -> (string -> unit) -> (unit -> unit) -> unit =
    [%bs.raw {|
	function (fs,oldname,newname,errf,donef) {
	    return fs.rename(oldname,newname,errf,donef);
	} |} ]

let copyFile_ : fsModule -> string -> string -> (string -> unit) -> (unit -> unit) -> unit =
    [%bs.raw {|
	function (fs,oldname,newname,errf,donef) {
	    return fs.copyFile(oldname,newname,function(err) {
		if (err) {errf(err);} else {donef();}
	    });
	} |} ]
		  

let wrapInPromise (fn : unit -> (string -> unit) -> ('a -> unit) -> unit) =
  Js.Promise.make
    (fun ~resolve:resolve ~reject:reject ->
      fn ()
        (fun err -> (reject (Invalid_argument err))[@bs])
        (fun data -> (resolve data)[@bs])
    )
  
let readText name =
  wrapInPromise (fun _ -> readText_ fs_module name)

let readBuffer name =
  wrapInPromise (fun _ -> readBuffer_ fs_module name)

let writeText name data =
  wrapInPromise (fun _ -> writeText_ fs_module name data)

let writeBuffer name data = 
  wrapInPromise (fun _ -> writeBuffer_ fs_module name data)

let writableFileStream =
  writableFileStream_ fs_module

let rename oldname newname =
  wrapInPromise (fun _ -> rename_ fs_module oldname newname)

let copyFile oldname newname =
  wrapInPromise (fun _ -> copyFile_ fs_module oldname newname)

let unlink_ : fsModule -> string -> (string -> unit) -> (unit -> unit) -> unit =
    [%bs.raw {|
	function($0,$1,$2,$3) {
	    try {
		$0.unlink($1,function(e,v) {
		    if (e) { $2('' + e); } else { $3(v); }
		});
	    } catch (e) { $2('' + e); }
	} |} ]


let unlink name =
  wrapInPromise (fun _ -> unlink_ fs_module name)

let access_ : fsModule -> string -> string -> (string -> unit) -> (int -> unit) -> unit =
    [%bs.raw {|
	function($0,$1,$2,$3,$4) {
	    var mode_int =
		($2.match(/.*r.*/) ? $0.constants.R_OK : 0) |
		($2.match(/.*w.*/) ? $0.constants.W_OK : 0) |
		($2.match(/.*x.*/) ? $0.constants.X_OK : 0) |
		($2.match(/.*f.*/) ? $0.constants.F_OK : 0);
	    try {
		$0.access($1,mode_int,function(e) {
		    if (e) { $3('' + e); } else { $4(0); }
		});
	    } catch (e) { $3('' + e); }
	} |} ]

let access name mode =
  wrapInPromise (fun _ -> access_ fs_module name mode)

let readdir_ : fsModule -> string -> (string -> unit) -> (string array -> unit) -> unit =
    [%bs.raw {|
	function($0,$1,$2,$3) {
	    try {
		$0.readdir($1,function(e,v) {
		    if (e) {
			$2('' + e);
		    } else {
			$3(v);
		    }
		});
	    } catch (e) { $2('' + e); }
	} |} ]

let readdir name =
  wrapInPromise (fun _ -> readdir_ fs_module name)
