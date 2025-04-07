type +'a astring = private string
type safe_string = [ `encoded | `escaped | `safe ] astring
type escaped_string = [ `encoded | `escaped ] astring
type encoded_string = [ `encoded ] astring

val ( ^^^ ) : 'a astring -> 'a astring -> 'a astring
val ( ^>^ ) : 'a astring -> string -> 'a astring
val ( ^<^ ) : string -> 'a astring -> 'a astring
val ( <^> ) : 'a astring -> 'a astring -> bool
val safe : string -> safe_string
val escaped : string -> escaped_string
val encoded : string -> encoded_string
val as_string : 'a astring -> string
val safe_fn : (string -> string) -> 'a astring -> 'a astring
