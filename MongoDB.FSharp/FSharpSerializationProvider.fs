namespace MongoDB.FSharp

open System
open Microsoft.FSharp.Reflection
open MongoDB.Bson.Serialization


type FsharpSerializationProvider() =
  let fsharpType (typ : Type) =
    typ.GetCustomAttributes(typeof<CompilationMappingAttribute>, true) 
      |> Seq.cast<CompilationMappingAttribute>
      |> Seq.map(fun t -> t.SourceConstructFlags)
      |> Seq.tryHead

  interface IBsonSerializationProvider with
    member this.GetSerializer(typ : Type) =
      let sumTypeCase = fun () ->
        if typ.IsGenericType && typ.GetGenericTypeDefinition() = typedefof<List<_>> then
          typedefof<ListSerializer<_>>.MakeGenericType(typ.GetGenericArguments())
          |> Activator.CreateInstance :?> IBsonSerializer
        elif FSharpType.IsUnion typ then UnionCaseSerializer() :> IBsonSerializer
        else null

      match fsharpType typ with
      | Some SourceConstructFlags.RecordType ->
        match ensureClassMapRegistered typ with
        | Some classMap -> RecordSerializer(classMap) :> IBsonSerializer
        | None          -> null

      | Some SourceConstructFlags.SumType   -> sumTypeCase()
      | Some SourceConstructFlags.UnionCase -> UnionCaseSerializer() :> IBsonSerializer
      | _                                   -> null