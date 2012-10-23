namespace MongoDB.FSharp

open System
open Microsoft.FSharp.Reflection
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization


type UnionCaseSerializer() =
  inherit MongoDB.Bson.Serialization.Serializers.BsonBaseSerializer()

  let readItems (reader : BsonReader) (types : Type seq) (options : IBsonSerializationOptions) =
    types |> Seq.fold(fun state t ->
      let serializer = BsonSerializer.LookupSerializer(t)
      let item = serializer.Deserialize(reader, t, options)
      item :: state
    ) []
    |> Seq.toArray |> Array.rev

  override this.Serialize(writer : BsonWriter, nominalType : Type, value : Object, options : IBsonSerializationOptions) =
    let info, values = FSharpValue.GetUnionFields(value, nominalType)
    if info.GetFields() |> Array.length = 0
    then writer.WriteString info.Name
    else
      writer.WriteStartDocument()
      writer.WriteName("_t")
      writer.WriteString(info.Name)
      writer.WriteName("_v")
      writer.WriteStartArray()
      values |> Seq.zip(info.GetFields()) |> Seq.iter (fun (field, value) ->
        let itemSerializer = BsonSerializer.LookupSerializer(field.PropertyType)
        itemSerializer.Serialize(writer, field.PropertyType, value, options)
      )
      writer.WriteEndArray()
      writer.WriteEndDocument()

  override this.Deserialize(reader : BsonReader, nominalType : Type, actualType : Type, options : IBsonSerializationOptions) =
    let unionType typeName = 
      FSharpType.GetUnionCases(nominalType) 
      |> Seq.where (fun case -> case.Name = typeName) |> Seq.head

    if reader.GetCurrentBsonType() = BsonType.String
    then reader.ReadString() |> unionType |> fun t -> FSharpValue.MakeUnion(t, [| |])
    else
      reader.ReadStartDocument()
      reader.ReadName("_t")
      let unionType = reader.ReadString() |> unionType
      reader.ReadStartArray()
      let items = readItems reader (unionType.GetFields() |> Seq.map(fun f -> f.PropertyType)) options
      reader.ReadEndArray()
      reader.ReadEndDocument()
      FSharpValue.MakeUnion(unionType, items)

