namespace Funco 

module Arg = 

    // Used to validate the type when creating an ArgMatch
    type ValueType 
        = String 
        | Int 
        | Float 
        | NoArg 

    // Determines what to do when am arg is parsed 
    type Action 
        = Set 
        | Append 
        | SetTrue 
        | Count 

    type Arg = 
        { name: string 
          short: char option 
          long: string option 
          help: string option 
          valType: ValueType
          action: Action
          required: bool
        } 

    let arg name =
        { name = name 
          short = None 
          long = None 
          help = None 
          valType = String
          action = Set
          required = false 
        }

    // =============== Setter Functions ===============================================

    // TODO tidy up dashes in from of long and short 

    let short name arg = { arg with short = Some name }
    let long name arg = { arg with long = Some name }
    let help text arg = { arg with help = Some text }
    let valueType valueType arg = { arg with valType = valueType }
    let action act arg = { arg with action = act }
    let required req arg = { arg with action = req }

    // =============== Metadata Functions ===============================================

    let takesParameter arg = 
        if List.contains arg.action [SetTrue; Count] then 
            false 
        elif arg.action = SetTrue then 
            false 
        else 
            true 

