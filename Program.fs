module Writing.Program

open Falco
open Falco.Routing
open Falco.HostBuilder
open Falco.Markup
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
    (Reader.loadFiles contentPath contentExt)
    |> Async.RunSynchronously
    |> Seq.sortByDescending _.CreationTime

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

module HtmlView =
    // Template
    let master (title: string) (content: XmlNode list) : XmlNode =
        Elem.html
            [ Attr.lang "en" ]
            [ Elem.head
                  []
                  [ Elem.meta [ Attr.charset "UTF-8" ]
                    Elem.meta [ Attr.name "viewport"; Attr.content "width=device-width, initial-scale=1.0" ]
                    Elem.title [] [ Text.raw title ]
                    Elem.style
                        []
                        [ Text.raw ".mx-auto { margin-right: auto; margin-left: auto; }"
                          Text.raw ".mw-800px { max-width: 800px; }" ] ]
              Elem.body [] content ]

    let homeView: XmlNode =
        master
            "Writting"
            [ Elem.main
                  [ Attr.class' "mx-auto mw-800px" ]
                  [ Elem.h1 [] [ Text.raw "Writting" ]
                    yield!
                        [ for fileMeta in fileMetas do
                              Elem.div
                                  []
                                  [ Elem.h2 [] [ Text.raw fileMeta.Slug ]
                                    Elem.p [] [ Text.raw (fileMeta.CreationTime.ToString()) ]
                                    Elem.p [] [ Text.raw (fileMeta.ModificationType.ToString()) ]
                                    Elem.a
                                        [ Attr.href (sprintf $"{Uri.EscapeDataString(fileMeta.Slug)}") ]
                                        [ Text.raw "Read More" ] ] ] ] ]

    let detailView (content: string) : XmlNode =
        master "Writting" [ Elem.main [ Attr.class' "mx-auto mw-800px" ] [ Text.raw content ] ]


module ApiHandler =
    let mainPageHandler: HttpHandler = Response.ofHtml HtmlView.homeView

    let detailPage: HttpHandler =
        fun (ctx: HttpContext) ->
            task {
                let route: RouteCollectionReader = Request.getRoute ctx
                let slug: string = route.GetString "slug"
                let! (htmlResponse: string) = ApiResponse.getHtmlBySlug slug
                return! Response.ofHtml (HtmlView.detailView htmlResponse) ctx
            }

    let metaHandler: HttpHandler = Response.ofJson ApiResponse.metasToResponses

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
        [ get "/" ApiHandler.mainPageHandler
          get "/{slug}" ApiHandler.detailPage
          get "/metas" ApiHandler.metaHandler
          get "/json/{slug}" ApiHandler.jsonHandler ]
}
