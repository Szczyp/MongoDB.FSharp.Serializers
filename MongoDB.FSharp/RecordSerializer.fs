namespace MongoDB.FSharp

open System
open System.Reflection
open Microsoft.FSharp.Reflection
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

    let readItems (reader : BsonReader) properties options =
      properties |> Seq.fold(fun state n ->
        if reader.State <> BsonReaderState.Value then reader.ReadName() |> ignore
        let memberMap = classMap.GetMemberMap(n)
        let serializer = memberMap.GetSerializer(memberMap.MemberType)
        let item = serializer.Deserialize(reader, memberMap.MemberType, memberMap.SerializationOptions)
        item :: state
      ) [] |> Seq.toArray |> Array.rev

    let names = names nominalType

    reader.GetCurrentBsonType() |> ignore

    reader.ReadStartDocument()

    if (Array.tryFind (fun n -> n = "Id") names) = None && reader.ReadName() = "_id"
    then reader.SkipValue()

    let items = readItems reader names options

    reader.ReadEndDocument()

    FSharpValue.MakeRecord(nominalType, items)


  interface IBsonIdProvider with
    member this.GetDocumentId(document : Object, id : Object byref, nominalType : Type byref, idGenerator : IIdGenerator byref) =
      classMapSerializer.GetDocumentId(document, &id, &nominalType, &idGenerator)

    member this.SetDocumentId(document : Object, id : Object) =
      classMapSerializer.SetDocumentId(document, id)