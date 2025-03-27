namespace Funco 

open ArgSpec

module ArgMatch = 

    type MatchValue = 
        | Unvalidated of string
        | Str of string 
        | Int of int 
        | Float of float 
        | Bool of bool 
        | NoValue

    type ArgMatch = 
        { name: string 
          value: MatchValue 
          expectedType: ValueType
        }

    let initMatch (arg: ArgSpec) value = 
        { name = arg.name 
          value = Unvalidated value
          expectedType = arg.valType
        }
