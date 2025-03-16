namespace Funco 

open Arg

module ArgMatch = 

    type ArgMatch = 
        { name: string 
          valueStr: string 
          isValidated: bool
          expectedType: ValueType
        }

    let initMatch (arg: Arg) value = 
        { name = arg.name 
          valueStr = value
          isValidated = false 
          expectedType = arg.valType
        }
