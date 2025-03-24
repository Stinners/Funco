namespace Funco 

open System

open Parser 
open Arg
open ArgMatch

module Matches = 

    type Matches = 
        { parser: Parser
          matches: ArgMatch list
          errors: string list 
        }

    let initMatches parser = 
        { parser = parser 
          matches = [] 
          errors = [] 
        }


    type ArgumentType = 
        | Short of string 
        | Long of string 
        | Positional of string 
        | SubCommand of string 
        | DoubleDash 
        | Invalid of string


    let parseArgs (command: string array) (parser: Parser) = 
        let mutable idx = 0 
        let mutable matches = initMatches parser

        let canAdvance () = idx - 1 < Array.length command 
        let advance () = if canAdvance () then idx <- idx + 1 

        let addError msg = matches <- { matches with errors = msg :: matches.errors }
        let addMatch thisMatch = matches <- { matches with matches = thisMatch :: matches.matches }

        let tail (str: string) = str.Substring(1)

        let getArgumentType (token: string) = 
            if token = "--" then 
                DoubleDash
            elif token.StartsWith("--") then 
                Long (token.Substring(2))
            elif token.StartsWith("-") then 
                if token.Length > 1 then Short (token.Substring(1))
                else Invalid $"Isolated '-' not allowed"
            else 
                Positional token

        // Advances by 0 or more
        // TODO impliment reading next parameter 
        // TODO Impliment handling quoted parameters
        let parseShortParam (spec: Arg) (remainder: string) = 
            // Parse Short 'run-on arguments'
            initMatch spec remainder 


       // TODO Impliment reading the next token as the parameter 
       // TODO impliment handling quoted arguments
        let parseParam (spec: Arg) = 
            "foo"

        // TODO Handle parsing quoted arguments
        let parseEqualsParam (spec: Arg) (remainder: string) = 
            remainder

        // Consumes a group of short commands and optionally their parameters 
        // Advances by at least 1
        let parseShort (token: string) = 
            let rec go (token: string) = 
                match getShort token[0] parser with 
                | None -> addError $"Short option {token} is not defined"
                | Some spec -> 
                    let remainder = tail token 
                    if takesParameter spec then 
                        parseShortParam spec remainder |> addMatch
                    else 
                        addMatch (initMatch spec "")
                        if token.Length <> 1 then go remainder 

            advance() 
            go token

        // Advances by 1 or more
        let parseLong (token: string) =
            let equalsIdx = token.IndexOf('=')
            let command = 
                if equalsIdx <> -1 then token.Substring(0,equalsIdx)
                else token

            match getLong command parser with 
            | None -> addError $"Long option {command} is not defined"
            | Some spec -> do 
                let parameter = 
                    if not (takesParameter spec) then "" 
                    elif equalsIdx <> -1 then parseEqualsParam spec (token.Substring(equalsIdx+1))
                    else parseParam spec 
                let thisMatch = initMatch spec parameter
                addMatch thisMatch
            
            advance()

        while canAdvance () do 
            match getArgumentType command[idx] with 
            | Short arg -> parseShort arg
            | Long arg -> parseLong arg 
            | _ -> raise (Exception "Unimplimented Type")
