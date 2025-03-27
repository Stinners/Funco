namespace Funco 

open System

open Parser 
open ArgSpec
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


    // ================== Functions for Extracting Arguments ====================

    let private tryLookup name matches = 
        List.tryFind (fun arg -> arg.name = name) matches |> Option.map(_.value)

    let tryGetInt matches name: int option = 
        match tryLookup name matches with 
        | Some(Int value) -> Some value 
        | _ -> None

    let tryGetFloat matches name: float option = 
        match tryLookup name matches with 
        | Some(Float value) -> Some value 
        | _ -> None 

    let tryGetString matches name: string option = 
        match tryLookup name matches with 
        | Some(Str value) -> Some value 
        | _ -> None 

    let tryGetBool matches name: bool option = 
        match tryLookup name matches with 
        | Some(Bool value) -> Some value 
        | _ -> None 

    let hasMatch matches name = 
        tryLookup name matches |> Option.isSome

    // ================== Parsing Arguments ==========================

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

        // =================== Utililty Functions ============

        let canAdvance () = idx - 1 < Array.length command 
        let advance () = if canAdvance () then idx <- idx + 1 

        let addError msg = matches <- { matches with errors = msg :: matches.errors }
        let addMatch thisMatch = matches <- { matches with matches = thisMatch :: matches.matches }

        let addMatchNoParams spec = addMatch (initMatch spec "") 
        
        let addWithParam spec (parameter: Result<string, string>) =
            match parameter with 
            | Ok parameter -> initMatch spec parameter |> addMatch
            | Error message -> addError message 

        let tail (str: string) = str.Substring(1)

        // =================== Geting Argumet Types ============

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

        // ============== Extracting Parameters ==============

        // Advances by 0 or more
        // TODO impliment reading next parameter 
        // TODO Impliment handling quoted parameters
        let parseShortParam (spec: ArgSpec) (remainder: string): Result<string, string> = 
            // Read the next Element
            if remainder = "" then 
                Ok remainder 
            else 
                Ok ""

       // TODO Impliment reading the next token as the parameter 
       // TODO impliment handling quoted arguments
        let parseParam (spec: ArgSpec) = 
            Ok "foo"

        // TODO Handle parsing quoted arguments
        let parseEqualsParam (spec: ArgSpec) (remainder: string) = 
            Ok remainder

        // ============== Different Parameter Types =============

        // Consumes a group of short commands and optionally their parameters 
        // Advances by at least 1
        let parseShort (token: string) = 
            let rec go (token: string) = 
                match getShort token[0] parser with 
                | None -> addError $"Short option {token} is not defined"
                | Some spec -> 
                    let remainder = tail token 
                    match takesParameter spec, remainder with 
                    | true, _ -> parseShortParam spec remainder |> addWithParam spec 
                    | _, "" -> addMatchNoParams spec
                    | false, _-> do 
                        addMatchNoParams spec
                        go remainder

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
                    if not (takesParameter spec) then Ok "" 
                    elif equalsIdx <> -1 then parseEqualsParam spec (token.Substring(equalsIdx+1))
                    else parseParam spec 
                addWithParam spec parameter
            
            advance()

        // ============== Top Level Loop =============

        while canAdvance () do 
            match getArgumentType command[idx] with 
            | Short arg -> parseShort arg
            | Long arg -> parseLong arg 
            | _ -> raise (Exception "Unimplimented Type")
