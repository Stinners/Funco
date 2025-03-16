namespace Funco 

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


    let parseArgs (command: string array) (parser: Parser) = 

        let addError message matches = { matches with errors = message :: matches.errors }
        let addMatch this matches = { matches with matches = this :: matches.matches }


        let findKeyword (token: string): Option<Arg> = 
            let args = parser.args
            let trimmed = token.TrimStart '-'
            if token.StartsWith "--" then 
                List.tryFind (fun arg -> Option.contains trimmed arg.long) args 
            else if token.StartsWith "-" then 
                List.tryFind (fun arg -> Option.contains trimmed arg.short) args 
            else 
                None 

        
        let makeMatchWithArg (arg: Arg) (entries: string array): Result<ArgMatch, string>
            = Ok (initMatch arg (Array.head entries))


        let rec parse (matches: Matches) (args: string array) = 
            if Array.isEmpty args then 
                matches 
            else 
                let token = Array.head args 
                match findKeyword token with 
                | None -> 
                    parse (addError $"Unrecognized argument: '{token}'" matches) (Array.tail args)

                | Some arg -> 
                    // Consume 2 tokens 
                    if takesArgument arg then 
                        makeMatchWithArg arg args
                        |> next matches args[1..]

                    // Consume one token 
                    else 
                        makeMatchWithArg arg args
                        |> next matches args[2..]

        and next matches args result = 
            match result with 
            | Ok newMatch -> parse (addMatch newMatch matches) args
            | Error msg -> parse (addError msg matches) args


        parse (initMatches parser) command
