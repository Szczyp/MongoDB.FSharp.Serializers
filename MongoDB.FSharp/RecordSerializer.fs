namespace MongoDB.FSharp

open System
open System.Reflection
open Microsoft.FSharp.Reflection
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization


type RecordSerializer(classMap : BsonClassMap) =
  inherit MongoDB.Bson.Serialization.Serializers.BsonBaseSerializer()

  let classMapSerializer = BsonClassMapSerializer(classMap)

  let idProvider = classMapSerializer :> IBsonIdProvider

  override this.Serialize(writer : BsonWriter, nominalType : Type, value : Object, options : IBsonSerializationOptions) =
    classMapSerializer.Serialize(writer, nominalType, value, options)

  override this.Deserialize(reader : BsonReader, nominalType : Type, actualType : Type, options : IBsonSerializationOptions) =
    let names t = FSharpType.GetRecordFields(t) |> Array.map (fun (i : PropertyInfo) -> i.Name) 

    let readItems (reader : BsonReader) names options =
      let items = Array.init (Array.length names) (fun i -> null)

      while reader.ReadBsonType() <> BsonType.EndOfDocument do
        let name = reader.ReadName()
        let memberMap = classMap.GetMemberMapForElement name
        match memberMap with
        | null -> reader.SkipValue()
        | map  -> match Array.tryFindIndex (fun a -> a = map.MemberName) names with
                  | None        -> reader.SkipValue()
                  | Some(index) ->
                    let serializer = memberMap.GetSerializer(memberMap.MemberType)
                    items.[index] <- serializer.Deserialize(reader, memberMap.MemberType, memberMap.SerializationOptions)
      items

    reader.GetCurrentBsonType() |> ignore

    reader.ReadStartDocument()

    let items = readItems reader (names actualType) options

    reader.ReadEndDocument()

    FSharpValue.MakeRecord(actualType, items)


  interface IBsonIdProvider with
    member this.GetDocumentId(document : Object, id : Object byref, nominalType : Type byref, idGenerator : IIdGenerator byref) =
      classMapSerializer.GetDocumentId(document, &id, &nominalType, &idGenerator)

    member this.SetDocumentId(document : Object, id : Object) =
      classMapSerializer.SetDocumentId(document, id)


  interface IBsonDocumentSerializer with
    member this.GetMemberSerializationInfo name =
      classMapSerializer.GetMemberSerializationInfo(name)