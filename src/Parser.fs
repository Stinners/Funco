namespace Funco 

open Arg

module Parser = 

    type Parser = 
        { name: string
          about: string option
          version: string option 
          args: Arg list
        }

    let parser name = 
        { name = name 
          version = None 
          about = None
          args = [] }

    // ============================================================

    let about text parser = { parser with about = Some text }
    let version vers parser = { parser with version = Some vers }

    // ============================================================

    let addArg arg parser = { parser with args = arg :: parser.args }


