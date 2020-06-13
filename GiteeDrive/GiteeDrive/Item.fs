namespace GiteeDrive

type Item =
| File of ItemDetails * size:uint32
| Directory of ItemDetails

and ItemDetails = {
    sha : SHAHash
    path : string

    repoOwner : string
    repo : string
    branch : string
}

module Item =
    open FSharp.Data
    open Utils
    open System.Net.Http

    type private DownloadJson = 
        JsonProvider<"https://gitee.com/api/v5/repos/oschina/git-osc/git/blobs/a7db7e94dc5ffc13194dbe610935063cced130aa">

    let download (AccessToken accessToken) item =
        match item with
        | Directory _ -> invalidArg "item" "Can not download a directory."
        | File ({ sha = sha; repoOwner = repoOwner; repo = repo },_) ->
            let (SHAHash sha) = sha
            Http.RequestString (
                (sprintf "https://gitee.com/api/v5/repos/%s/%s/git/blobs/%s" repoOwner repo sha),
                query = [
                    if accessToken.IsSome then
                        "access_token", accessToken.Value
                ],
                httpMethod = "GET",
                responseEncodingOverride = "UTF-8")
            |> DownloadJson.Parse
            |> fun x ->
                match x.Encoding with
                | "base64" -> System.Convert.FromBase64String x.Content
                | _ -> raise (InvalidJsonValue x.JsonValue)

    let downloadAsync accessToken item = async { return download accessToken item }

    let downloadString accessToken item =
        download accessToken item
        |> System.Text.Encoding.UTF8.GetString

    let downloadStringAsync accessToken item = async { return downloadString accessToken item }


    let update (AccessToken accessToken) commitMsg content item =
        match item with
        | File (item,_) ->
            let url = sprintf "https://gitee.com/api/v5/repos/%s/%s/contents/%s" item.repoOwner item.repo item.path
            let (SHAHash sha) = item.sha
            use content = 
                [
                    "content", System.Convert.ToBase64String content
                    "message", commitMsg
                    "branch", item.branch
                    "sha", sha
                    if accessToken.IsSome then
                        "access_token", accessToken.Value
                ]
                |> dict
                |> fun x -> new FormUrlEncodedContent (x)

            httpClient.PutAsync(url,content).Result.Content.ReadAsStringAsync().Result
            |> JsonValue.TryParse
            |> function
            | None -> ()
            | Some x ->
                x
                |> raiseResponseJsonExn
                |> ignore
        | _ -> invalidArg "item" "item Must be a file item."

    let updateAsync accessToken commitMsg content item = async { return update accessToken commitMsg content item }

    let updateString accessToken commitMsg (content:string) item =
        update accessToken commitMsg (System.Text.Encoding.UTF8.GetBytes content) item

    let updateStringAsync accessToken commitMsg content item = async { return updateString accessToken commitMsg content item }


    let delete (AccessToken accessToken) commitMsg item =
        match item with
        | File (item,_) ->
            let (SHAHash sha) = item.sha
            Http.RequestString (
                (sprintf "https://gitee.com/api/v5/repos/%s/%s/contents/%s" item.repoOwner item.repo item.path),
                query = [
                    "message", commitMsg
                    "branch", item.branch
                    "sha", sha
                    if accessToken.IsSome then
                        "access_token", accessToken.Value
                ],
                httpMethod = "DELETE",
                responseEncodingOverride = "UTF-8")
            |> ignore
        | _ -> invalidArg "item" "item Must be a file item."

    let deleteAsync accessToken commitMsg item = async { return delete accessToken commitMsg item }

