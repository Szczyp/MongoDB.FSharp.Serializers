namespace ``Acceptance Tests``

open Xunit
open Swensen.Unquote
open Swensen.Unquote.Assertions
open MongoDB.Bson
open MongoDB.Bson.Serialization
open MongoDB.Driver
open MongoDB.Driver.Linq
open MongoDB.FSharp
open System.Linq

open TestUtils

type SimpleSwitch = Off
                  | On

type DimmerSwitch = Off
                  | Dim of int
                  | DimMarquee of int * string
                  | On

type RecordWithList =
  { Id   : BsonObjectId
    Ints : int list }

type RecordType =
  { Id : BsonObjectId
    Name : string
    Int : int }

type RecordWithSwitchList =
  { Id : BsonObjectId
    Switches : SimpleSwitch list }

type IdLessRecordWithSwitchList = { Switches : SimpleSwitch list; Switches2 : SimpleSwitch list }

type ObjectType() =
  member val Id   : BsonObjectId  = BsonObjectId.GenerateNewId()  with get, set
  member val Name : string        = ""                            with get, set
  member val Int  : int           = 0                             with get, set

type ObjectWithList() =
  member val Id   : BsonObjectId  = BsonObjectId.GenerateNewId()  with get, set
  member val List : string list   = []                            with get, set

type ObjectWithOptions() =
  member val Id : BsonObjectId = BsonObjectId.GenerateNewId() with get, set
  member val Age : int option = None with get, set

type ObjectWithDimmer() =
  member val Id : BsonObjectId = BsonObjectId.GenerateNewId() with get, set
  member val Switch : DimmerSwitch = Off with get, set

type ObjectWithDimmers() =
  member val Id : BsonObjectId = BsonObjectId.GenerateNewId() with get, set
  member val Kitchen : DimmerSwitch = Off with get, set
  member val Bedroom1 : DimmerSwitch = Off with get, set
  member val Bedroom2 : DimmerSwitch = Off with get, set

type ``When serializing lists``() = 
  let db = MongoDatabase.Create "mongodb://localhost/test"
  do FSharpSerializers.Register()

  interface System.IDisposable with
    member this.Dispose() =
      db.DropCollection "objects" |> ignore


  [<Fact>]
  member this.``It can serialize an object with a list``() =
    let collection = db.GetCollection<ObjectWithList> "objects"
    let obj = ObjectWithList()
    obj.List <- [ "hello"; "world" ]
    collection.Save obj |> ignore

    let genCollection = db.GetCollection "objects"
    let fromDb = genCollection.FindOne(new QueryDocument("_id", obj.Id))
    let array = fromDb.["List"].AsBsonArray
    Assert.Equal(2, array.Count)
    Assert.Equal<string>("hello", array.Values |> Seq.head |> fun v -> v.AsString)


  [<Fact>]
  member this.``It can serialize a record with a list``() =
    let collection = db.GetCollection<RecordWithList> "objects"
    let obj = { RecordWithList.Id = BsonObjectId.GenerateNewId(); Ints = [1; 2; 3; 4] }
    collection.Save obj |> ignore

    let genCollection = db.GetCollection "objects"
    let fromDb = genCollection.FindOne(new QueryDocument("_id", obj.Id))
    let array = fromDb.["Ints"].AsBsonArray
    Assert.Equal(4, array.Count)
    Assert.Equal(1, array.Values |> Seq.head |> fun v -> v.AsInt32)


  [<Fact>]
  member this.``It can deserialze lists``() =
    let list = BsonArray([ "hello"; "world" ])
    let id = BsonObjectId.GenerateNewId()
    let document = BsonDocument([ BsonElement("_id", id); BsonElement("List", list) ])
    let collection = db.GetCollection "objects"
    collection.Save document |> ignore

    let collection = db.GetCollection<ObjectWithList> "objects"
    let fromDb = collection.FindOne(new QueryDocument("_id", id))
    let array = fromDb.List
    Assert.Equal(2, array.Length)
    Assert.Equal<string>("hello", array.Head)


  [<Fact>]
  member this.``It can deserialze lists from records``() =
    let list = BsonArray([1; 2; 3; 4])
    let id = BsonObjectId.GenerateNewId()
    let document = BsonDocument([ BsonElement("_id", id); BsonElement("Ints", list) ])
    let collection = db.GetCollection "objects"
    collection.Save document |> ignore

    let collection = db.GetCollection<RecordWithList> "objects"
    let fromDb = collection.FindOne(new QueryDocument("_id", id))
    let array = fromDb.Ints
    Assert.Equal(4, array.Length)
    Assert.Equal(1, array.Head)


  [<Fact>]
  member this.``It can serialize records``() =
    let collection = db.GetCollection<RecordType> "objects"
    let obj = { Id = BsonObjectId.GenerateNewId(); Name = "test"; Int = 1 }
    collection.Insert obj |> ignore

    let genCollection = db.GetCollection "objects"
    let fromDb = genCollection.FindOne(new QueryDocument("_id", obj.Id))
    let test = fromDb.["Name"].AsString
    Assert.Equal<string>("test", test)


  [<Fact>]
  member this.``It can deserialize records``() =
    let id = BsonObjectId.GenerateNewId()
    let document = BsonDocument([BsonElement("_id", id); BsonElement("Name", BsonString("value")); BsonElement("Int", BsonInt32(1))])
    let collection = db.GetCollection "objects"
    collection.Save(document) |> ignore

    let collection = db.GetCollection<RecordType>("objects")
    let fromDb = collection.FindOneById(id)
    Assert.NotNull(fromDb)
    Assert.Equal<string>("value", fromDb.Name)


  [<Fact>]
  member this.``It can deserialize select projections on objects``() =
    let collection = db.GetCollection<ObjectType> "objects"
    collection.Save(ObjectType( Id = BsonObjectId.GenerateNewId(), Name = "test", Int = 1 )) |> ignore
    collection.Save(ObjectType( Id = BsonObjectId.GenerateNewId(), Name = "test2", Int = 2 )) |> ignore

    let collection = db.GetCollection<ObjectType>("objects")
    let fromDb = collection.AsQueryable().Select(fun (o : ObjectType) -> o.Name) |> Seq.toList |> List.head
    Assert.NotNull(fromDb)
    Assert.Equal<string>("test", fromDb)


  [<Fact>]
  member this.``It can deserialize select projections on records``() =
    let collection = db.GetCollection<RecordType> "objects"
    collection.Save({ Id = BsonObjectId.GenerateNewId(); Name = "test"; Int = 1 }) |> ignore
    collection.Save({ Id = BsonObjectId.GenerateNewId(); Name = "test2"; Int = 2 }) |> ignore

    let collection = db.GetCollection<RecordType>("objects")
    let fromDb = collection.AsQueryable().Select(fun r -> r.Name) |> Seq.toList |> List.head
    Assert.NotNull(fromDb)
    Assert.Equal<string>("test", fromDb)


  [<Fact>]
  member this.``It can serialize option types``() =
    let collection = db.GetCollection<ObjectWithOptions> "objects"
    let obj = ObjectWithOptions()
    obj.Age <- Some 42
    collection.Save obj |> ignore

    let collection = db.GetCollection "objects"
    let fromDb = collection.FindOneById(obj.Id)
    let age = fromDb.GetElement("Age")
    Assert.NotNull(age);
    Assert.Equal<string>("Some", age.Value.AsBsonDocument.GetElement("_t").Value.AsString)
    let value = age.Value.AsBsonDocument.GetElement("_v").Value
    Assert.True(value.IsBsonArray)
    let array = value.AsBsonArray
    Assert.Equal(1, array.Count)
    Assert.Equal(42, array.[0].AsInt32)


  [<Fact>]
  member this.``It can serialize DimmerSwitch types``() =
    let collection = db.GetCollection<ObjectWithOptions> "objects"
    let obj = ObjectWithDimmer()
    obj.Switch <- DimMarquee(42, "loser")
    collection.Save obj |> ignore

    let collection = db.GetCollection "objects"
    let fromDb = collection.FindOneById(obj.Id)
    let switch = fromDb.GetElement("Switch")
    Assert.NotNull(switch);
    Assert.Equal<string>("DimMarquee", switch.Value.AsBsonDocument.GetElement("_t").Value.AsString)
    let value = switch.Value.AsBsonDocument.GetElement("_v").Value
    Assert.True(value.IsBsonArray)
    let array = value.AsBsonArray
    Assert.Equal(2, array.Count)
    Assert.Equal(42, array.[0].AsInt32)
    Assert.Equal<string>("loser", array.[1].AsString)


  [<Fact>]
  member this.``It can serialize list of simple switches to list of strings``() =
    let collection = db.GetCollection<RecordWithSwitchList> "objects"
    let id = BsonObjectId.GenerateNewId()
    collection.Save { Id = id; Switches = [ SimpleSwitch.On; SimpleSwitch.Off ] } |> ignore

    let collection = db.GetCollection "objects"
    let fromDb = collection.FindOneById(id)
    let switches = fromDb.GetElement("Switches")
    Assert.NotNull(switches);
    Assert.Equal<string>("On", switches.Value.AsBsonArray.[0].AsString)


  [<Fact>]
  member this.``It can serialize to json record without id and a list of simple switches as list of strings``() =
    let json = BsonExtensionMethods.ToJson { Switches = [ SimpleSwitch.On; SimpleSwitch.Off ]; Switches2 = [ SimpleSwitch.Off ] }
    Assert.Equal<string>(@"{ ""Switches"" : [""On"", ""Off""], ""Switches2"" : [""Off""] }", json)


  [<Fact>]
  member this.``It can serialize to json list of simple switches as list of strings``() =
    let json = BsonExtensionMethods.ToJson([SimpleSwitch.On; SimpleSwitch.Off])
    Assert.Equal<string>(@"[""On"", ""Off""]", json)

    
  [<Fact>]
  member this.``It can deserialize from json list of strings as list of simple switches``() =
    let json = BsonExtensionMethods.ToJson([SimpleSwitch.On; SimpleSwitch.Off])
    let list = BsonSerializer.Deserialize<list<SimpleSwitch>>(json)
    Assert.Equal<SimpleSwitch>(SimpleSwitch.On, list.Head)


  [<Fact>]
  member this.``It can deserialize option types``() =
    let id = BsonObjectId.GenerateNewId()
    let arrayPart = BsonArray([ BsonInt32(42) ])
    let structure = BsonDocument(BsonElement("_t", BsonString("Some")), BsonElement("_v", arrayPart))
    let document = BsonDocument(BsonElement("_id", id), BsonElement("Age", structure))
    let collection = db.GetCollection "objects"
    collection.Save(document) |> ignore

    let collection = db.GetCollection<ObjectWithOptions> "objects"
    let fromDb = collection.FindOneById id
    match fromDb.Age with
    | Some 42 -> ()
    | _ -> fail "expected Some 42 but got something else"


  [<Fact>]
  member this.``We can integrate serialize & deserialize on DimmerSwitches``() =
    let collection = db.GetCollection<ObjectWithDimmers> "objects"
    let obj = ObjectWithDimmers()
    obj.Kitchen <- Off
    obj.Bedroom1 <- Dim 42
    obj.Bedroom2 <- DimMarquee(12, "when I was little...")
    collection.Save obj |> ignore

    let fromDb = collection.FindOneById obj.Id
    match fromDb.Kitchen with
    | Off -> ()
    | _ -> fail "Kitchen light wasn't off"

    match fromDb.Bedroom1 with
    | Dim 42 -> ()
    | _ -> fail "Bedroom1 light wasn't dim enough"

    match fromDb.Bedroom2 with
    | DimMarquee(12, "when I was little...") -> ()
    | _ -> fail "Bedroom2 doesn't have the party we thought"
