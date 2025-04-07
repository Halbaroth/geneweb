type 'a astring = string
type safe_string = [ `encoded | `escaped | `safe ] astring
type escaped_string = [ `encoded | `escaped ] astring
type encoded_string = [ `encoded ] astring

let ( ^^^ ) : 'a astring -> 'a astring -> 'a astring =
 fun (a : 'a astring) (b : 'a astring) : 'a astring -> a ^ b

let ( ^>^ ) : 'a astring -> string -> 'a astring =
 fun (a : 'a astring) (b : string) : 'a astring -> a ^ b

let ( ^<^ ) : string -> 'a astring -> 'a astring =
 fun (a : string) (b : 'a astring) : 'a astring -> a ^ b

let ( <^> ) : 'a astring -> 'a astring -> bool = ( <> )

external safe : string -> safe_string = "%identity"
external escaped : string -> escaped_string = "%identity"
external encoded : string -> encoded_string = "%identity"
external as_string : 'a astring -> string = "%identity"

let safe_fn = ( @@ )
