﻿namespace SerializersTests

open Xunit

open MongoDB.FSharp
open TestUtils


type ``When registering classes``() =

  let stubbed = getClassMap (fun t -> false)
  
  [<Fact>]
  member this.``It sets serialization options``() =
    let classMap = stubbed typeof<List<string>>
    match classMap with
    | Some cm -> ()
    | None -> fail "expected a classmap"
