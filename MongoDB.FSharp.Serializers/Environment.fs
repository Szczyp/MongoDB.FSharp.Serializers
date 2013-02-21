namespace MongoDB.FSharp.Serializers

open System
open System.Reflection
open MongoDB.Bson
open MongoDB.Bson.IO
open MongoDB.Bson.Serialization


[<AutoOpen>]
module BsonWriterExtensions =
  type BsonWriter with
    member inline this.WriteEmptyArray() =
      this.WriteStartArray()
      this.WriteEndArray()


[<AutoOpen>]
module ObjectExtensions =
  type Object with
    member inline this.GetFriendlyTypeName() =
        this.GetType() |> BsonUtils.GetFriendlyTypeName


module Seq =
  let tryHead s =
    if Seq.isEmpty s then
      None
    else
      Some (s |> Seq.head)


[<AutoOpen>]
module Helpers =
  let getClassMap isClassMapRegistered (actualType : Type) =
    let rec getMember (_type : Type) name other =
      let memberInfos = _type.GetMember name
      if not (memberInfos |> Seq.isEmpty) then
          Some(Seq.head memberInfos)
      elif other <> null then
          getMember _type other null
      else
          None

    if not (isClassMapRegistered actualType) then
      let genericType = typedefof<BsonClassMap<_>>.MakeGenericType(actualType)
      let classMap = Activator.CreateInstance(genericType) :?> BsonClassMap

      classMap.AutoMap()
      actualType.GetProperties() |> Seq.where (fun prop -> 
          classMap.AllMemberMaps |> Seq.exists (fun mm -> mm.MemberInfo = (prop :> MemberInfo)) |> not
      )
      |> Seq.where (fun prop -> prop.GetGetMethod() <> null)
      |> Seq.iter (fun prop -> classMap.MapMember(prop :> MemberInfo) |> ignore )

      match getMember actualType "Id" "_id" with
      | Some memberInfo -> classMap.MapIdMember memberInfo |> ignore
      | None -> ()

      classMap.Freeze() |> Some
    else 
      None

  let ensureClassMapRegistered actualType =
    let fn = BsonClassMap.IsClassMapRegistered 
    match getClassMap fn actualType with
    | Some map -> 
        map |> BsonClassMap.RegisterClassMap
        Some map
    | None -> None
