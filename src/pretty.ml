let pretty : string -> string =
  [%bs.raw {| 
            function(html) {
               var pretty = require('pretty');
               return pretty(html);
            }
            |} ]
