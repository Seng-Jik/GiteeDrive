namespace GiteeDrive
open FSharp.Data

type AccessToken = AccessToken of string option

type SHAHash = SHAHash of string

exception InvalidJsonValue of JsonValue

module private Utils =
    open System.Net.Http

    let httpClient = new HttpClient ()

    let raiseResponseJsonExn (x:JsonValue) =
        let msgExists =
            query {
                for (k,v) in x.Properties() do
                exists (k = "message")
            }
        if msgExists then raise (exn (query {
            for (k,v) in x.Properties() do
            where (k = "message")
            select (v.AsString())
            head
        }))
        else x