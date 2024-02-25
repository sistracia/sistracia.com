module Writing.Program

open Falco
open Falco.Routing
open Falco.HostBuilder
open Markdig
open HtmlAgilityPack
open Microsoft.AspNetCore.Http
open System
open System.IO

let contentPath = "./content"
let contentExt = "*.md"

[<Struct>]
type FileMeta =
    { Slug: string
      CreationTime: DateTime
      ModificationType: DateTime
      Content: string }

[<Struct>]
type FileResponse =
    { CreationTime: DateTime
      ModificationType: DateTime
      Slug: string }

module Parser =
    let mdToHtml (md: string) : string = Markdown.ToHtml md

    let htmlToHtmlDoc (html: string) : HtmlDocument =
        let doc: HtmlDocument = new HtmlDocument()
        doc.LoadHtml(html)
        doc


    let rec convertHtmlToJson (node: HtmlNode) : Map<string, obj> =
        let mutable json: Map<string, obj> = Map []

        json <- json.Add("tag", node.Name)
        json <- json.Add("text", node.InnerText.Trim())

        if node.HasChildNodes then
            json <-
                json.Add(
                    "children",
                    (Seq.empty, node.ChildNodes)
                    ||> Seq.fold (fun (previous: Map<string, obj> seq) (childNode: HtmlNode) ->
                        Seq.concat [ previous; [ convertHtmlToJson childNode ] ])
                )

        json

    let mdToHtmlDoc = mdToHtml >> htmlToHtmlDoc


module Reader =
    let loadFiles (contentPath: string) (contentExt: string) : Async<FileMeta seq> =
        async {
            return
                Directory.EnumerateFiles(contentPath, contentExt)
                |> Seq.map (fun (file: string) ->
                    { FileMeta.Slug = Path.GetFileNameWithoutExtension file
                      FileMeta.CreationTime = File.GetCreationTime file
                      FileMeta.ModificationType = File.GetLastWriteTime file
                      FileMeta.Content = File.ReadAllText file })
        }

module Content =

    let getHtml (fileMeta: FileMeta) : Async<string> =
        async { return Parser.mdToHtml fileMeta.Content }

    let getJson (fileMeta: FileMeta) : Async<Map<string, obj>> =
        async { return Parser.convertHtmlToJson ((Parser.mdToHtmlDoc fileMeta.Content).DocumentNode) }

let (fileMetas: FileMeta seq) =
    Reader.loadFiles contentPath contentExt |> Async.RunSynchronously

module ApiResponse =
    let metasToResponses: FileResponse seq =
        fileMetas
        |> Seq.map (fun (fileMeta: FileMeta) ->
            { FileResponse.Slug = fileMeta.Slug
              FileResponse.CreationTime = fileMeta.CreationTime
              FileResponse.ModificationType = fileMeta.ModificationType })

    let getHtmlBySlug (slug: string) : Async<string> =
        async {
            match fileMetas |> Seq.tryFind (fun (fileMeta: FileMeta) -> fileMeta.Slug = slug) with
            | Some(fileMeta: FileMeta) -> return! Content.getHtml fileMeta
            | None -> return ""
        }

    let getJsonBySlug (slug: string) : Async<Map<string, obj>> =
        async {
            match fileMetas |> Seq.tryFind (fun (fileMeta: FileMeta) -> fileMeta.Slug = slug) with
            | Some(fileMeta: FileMeta) -> return! Content.getJson fileMeta
            | None -> return Map.empty
        }

module ApiHandler =
    let metaHandler: HttpHandler = Response.ofJson ApiResponse.metasToResponses

    let htmlHandler: HttpHandler =
        fun (ctx: HttpContext) ->
            task {
                let route: RouteCollectionReader = Request.getRoute ctx
                let slug: string = route.GetString "slug"
                let! (htmlResponse: string) = ApiResponse.getHtmlBySlug slug
                return! Response.ofHtmlString htmlResponse ctx
            }

    let jsonHandler: HttpHandler =
        fun (ctx: HttpContext) ->
            task {
                let route: RouteCollectionReader = Request.getRoute ctx
                let slug: string = route.GetString "slug"
                let! (jsonResponse: Map<string, obj>) = ApiResponse.getJsonBySlug slug
                return! Response.ofJson jsonResponse ctx
            }

webHost [||] {
    endpoints
        [ get "/metas" ApiHandler.metaHandler
          get "/html/{slug}" ApiHandler.htmlHandler
          get "/json/{slug}" ApiHandler.jsonHandler ]
}
