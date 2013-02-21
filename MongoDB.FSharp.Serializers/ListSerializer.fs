namespace MongoDB.FSharp.Serializers

open System
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization


type ListSerializer<'T>() =
  inherit MongoDB.Bson.Serialization.Serializers.BsonBaseSerializer(ListSerializationOptions())

  override this.Serialize(writer : BsonWriter, nominalType : Type, value : Object, options : IBsonSerializationOptions) =
    if value = null then
      writer.WriteEmptyArray()
    else
      let actualType = value.GetType()
      this.VerifyTypes(nominalType, actualType, typeof<list<'T>>)

      let lst = value :?> list<'T>
      writer.WriteStartArray()
      
      lst |> List.iter (fun item -> BsonSerializer.Serialize(writer, typeof<'T>, item, options))

      writer.WriteEndArray()


  override this.Deserialize(reader : BsonReader, nominalType : Type, actualType : Type, options : IBsonSerializationOptions) =
    let serializationOptions = this.EnsureSerializationOptions<ListSerializationOptions>(options)
    let itemOptions = serializationOptions.ItemSerializationOptions
    let readArray() =
      seq {
        reader.ReadStartArray()
        let convention = BsonSerializer.LookupDiscriminatorConvention(typeof<'T>)
        while reader.ReadBsonType() <> BsonType.EndOfDocument do
          let actualElementType = convention.GetActualType(reader, typeof<'T>)
          let serializer = BsonSerializer.LookupSerializer(actualElementType)
          let element = serializer.Deserialize(reader, typeof<'T>, itemOptions)
          yield element :?> 'T
        reader.ReadEndArray()
      }

    let readArrayFromObject () =
      reader.ReadStartDocument()
      reader.ReadString("_t") |> ignore
      reader.ReadName("_v")
      let value = this.Deserialize(reader, actualType, actualType, options)
      reader.ReadEndDocument()
      value

    let bsonType = reader.GetCurrentBsonType()
    match bsonType with
    | BsonType.Null     -> reader.ReadNull(); null
    | BsonType.Array    -> readArray() |> List.ofSeq :> Object
    | BsonType.Document -> readArrayFromObject ()
    | _                 -> sprintf "Can't deserialize a %s from BsonType %s" actualType.FullName (bsonType.ToString())
                           |> fun msg -> raise(InvalidOperationException(msg))


  interface IBsonArraySerializer with
    member this.GetItemSerializationInfo() : BsonSerializationInfo =
      let elementName = null
      let nominalType = typeof<'T>
      let serializer = BsonSerializer.LookupSerializer nominalType
      BsonSerializationInfo(elementName, serializer, nominalType, null)


