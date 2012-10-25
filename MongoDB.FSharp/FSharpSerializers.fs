﻿namespace MongoDB.FSharp

open MongoDB.Bson.Serialization


module FSharpSerializers =
  let mutable isRegistered = false

  let Register() =
    if not isRegistered then
      BsonSerializer.RegisterSerializationProvider(FsharpSerializationProvider())
      BsonSerializer.RegisterGenericSerializerDefinition(typeof<list<_>>, typeof<ListSerializer<_>>)
      isRegistered <- true