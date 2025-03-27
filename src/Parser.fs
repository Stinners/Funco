namespace Funco 

open ArgSpec

module Parser = 

    type Parser = 
        { name: string
          about: string option
          version: string option 
          args: ArgSpec list
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

    let getShort arg parser = List.tryFind (fun short -> Option.contains arg short.short) parser.args
    let getLong arg parser = List.tryFind (fun long -> Option.contains arg long.long) parser.args

    // ============================================================
