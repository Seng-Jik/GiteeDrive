namespace GiteeDrive

type Repo = {
    owner : string
    repo : string
    branch : string
}

module Repo =
    open FSharp.Data
    open Utils
    open System.Net.Http

    let private getTree (AccessToken accessToken) recursive shaOrBranch repo parentPath =
        Http.RequestString (
            (sprintf "https://gitee.com/api/v5/repos/%s/%s/git/trees/%s" repo.owner repo.repo shaOrBranch),
            query = [
                if accessToken.IsSome then
                    "access_token", accessToken.Value
                if recursive then
                    "recursive", "1"
            ],
            httpMethod = "GET",
            responseEncodingOverride = "UTF-8")
        |> fun x -> 
            JsonValue.Parse x
            |> fun x ->
                let rec parseJsonData (j:JsonValue) =
                    let details = {
                        sha = j.["sha"].AsString() |> SHAHash
                        path = parentPath + j.["path"].AsString()

                        repoOwner = repo.owner
                        repo = repo.repo
                        branch = repo.branch
                    }
                    match j.["type"].AsString() with
                    | "blob" -> File (details, uint32 (j.["size"].AsString()))
                    | "tree" -> Directory details
                    | _ -> raise (InvalidJsonValue j)
                x.["tree"].AsArray()
                |> Array.map parseJsonData
                
    let getRoot accessToken recursive repo = getTree accessToken recursive repo.branch repo ""
    let getRootAsync accessToken recursive repo = async { return getRoot accessToken recursive repo }

    let getDirctory accessToken recursive item repo =
        match item with
        | Directory { sha = s; path = parent } ->
            let (SHAHash s) = s
            getTree accessToken recursive s repo (parent.TrimEnd('/') + "/")
        | _ -> invalidArg "item" "item must be a directory item."

    let getDirctoryAsync accessToken recursive item repo = async { return getDirctory accessToken recursive item repo }

    let createFile (AccessToken accessToken) commitMsg content (path:string) repo =
        let url = sprintf "https://gitee.com/api/v5/repos/%s/%s/contents/%s" repo.owner repo.repo (path.Replace("\\","/"))
        use content = 
            [
                "content", System.Convert.ToBase64String content
                "message", commitMsg
                "branch", repo.branch
                if accessToken.IsSome then
                    "access_token", accessToken.Value
            ]
            |> dict
            |> fun x -> new FormUrlEncodedContent (x)

        httpClient.PostAsync(url,content).Result.Content.ReadAsStringAsync().Result
        |> JsonValue.TryParse
        |> function
        | None -> ()
        | Some x ->
            x
            |> raiseResponseJsonExn
            |> ignore

    let createFileAsync accessToken commitMsg content path repo =
        async { return createFile accessToken commitMsg content path repo }

    let createTextFile accessToken commitMsg (content:string) path repo =
        createFile accessToken commitMsg (System.Text.Encoding.UTF8.GetBytes content) path repo

    let createTextFileAsync accessToken commitMsg content path repo =
        async { return createTextFile accessToken commitMsg content path repo }
