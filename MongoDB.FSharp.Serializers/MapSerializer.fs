namespace MongoDB.FSharp.Serializers

open System
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization


type MapSerializer<'k, 'v when 'k : comparison>() =
  inherit MongoDB.Bson.Serialization.Serializers.DictionarySerializer<'k, 'v>()

  member this.DeserializePair<'t>(reader, itemOptions) =
    let convention = BsonSerializer.LookupDiscriminatorConvention(typeof<'t>)
    let actualElementType = convention.GetActualType(reader, typeof<'t>)
    let serializer = BsonSerializer.LookupSerializer(actualElementType)
    serializer.Deserialize(reader, typeof<'t>, itemOptions) :?> 't

  override this.Deserialize(reader : BsonReader, nominalType : Type, actualType : Type, options : IBsonSerializationOptions) =
    let pair() =
      reader.ReadStartArray()
      let pair = this.DeserializePair<'k>(reader, options), this.DeserializePair<'v>(reader, options)
      reader.ReadEndArray()
      pair

    reader.GetCurrentBsonType() |> ignore

    reader.ReadStartArray();

    let map = seq { while reader.ReadBsonType() <> BsonType.EndOfDocument do
                      yield pair() } |> Map.ofSeq

    reader.ReadEndArray();

    box map

