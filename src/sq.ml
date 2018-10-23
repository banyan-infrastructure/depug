let strip : string -> string =
  [%bs.raw {| 
            function(s) {
                var stripquotes = require('stripquotes');
                return stripquotes(s);
            }
            |} ]

