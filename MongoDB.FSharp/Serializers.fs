namespace MongoDB.FSharp

open MongoDB.Bson.Serialization


module Serializers =
  let mutable isRegistered = false

  /// Registers all F# serializers
  let Register() =
    if not isRegistered then
      BsonSerializer.RegisterSerializationProvider(FsharpSerializationProvider())
      BsonSerializer.RegisterGenericSerializerDefinition(typeof<list<_>>, typeof<ListSerializer<_>>)
      isRegistered <- true
