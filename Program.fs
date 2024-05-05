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
open GrayMattr

let contentPath: string = "./content"
let contentExt: string = "*.md"

[<CLIMutable>]
type FileMattr = { Title: string }

[<Struct>]
type FileMeta =
    { Slug: string
      CreationTime: DateTime
      ModificationType: DateTime
      Content: GrayFile<FileMattr> }

[<Struct>]
type FileResponse =
    { CreationTime: DateTime
      ModificationType: DateTime
      Slug: string
      Mattr: FileMattr option }

module Parser =
    let mdToHtml (md: string) : string = Markdown.ToHtml md

    let htmlToHtmlDoc (html: string) : HtmlDocument =
        let doc: HtmlDocument = new HtmlDocument()
        doc.LoadHtml(html)
        doc

    let rec convertHtmlDocToMap (node: HtmlNode) : Map<string, obj> =
        let mutable json: Map<string, obj> = Map []

        match node.Name with
        | "#text" -> json
        | _ ->
            json <- json.Add("tag", node.Name)
            json <- json.Add("href", node.GetAttributeValue("href", ""))
            json <- json.Add("content", node.GetDirectInnerText())

            if node.HasChildNodes then
                let childrenFolder (previous: Map<string, obj> seq) (childNode: HtmlNode) =
                    let childMap: Map<string, obj> = convertHtmlDocToMap childNode

                    let newChildNodes: Map<string, obj> list =
                        match childMap |> Map.isEmpty with
                        | true -> []
                        | false -> [ childMap ]

                    Seq.concat [ previous; newChildNodes ]

                let children: Map<string, obj> seq =
                    Seq.fold childrenFolder Seq.empty node.ChildNodes

                json <- json.Add("children", children)

            json

    let getHtmlBodyElements (node: HtmlNode) : obj =
        (convertHtmlDocToMap node).TryFind "children"

    let mdToHtmlDoc = mdToHtml >> htmlToHtmlDoc

module Reader =
    let loadFiles (contentPath: string) (contentExt: string) : Async<FileMeta seq> =
        async {
            return
                Directory.EnumerateFiles(contentPath, contentExt)
                |> Seq.map (fun (filePath: string) ->
                    { FileMeta.Slug = Path.GetFileNameWithoutExtension filePath
                      FileMeta.CreationTime = File.GetCreationTime filePath
                      FileMeta.ModificationType = File.GetLastWriteTime filePath
                      FileMeta.Content = Mattr.Read<FileMattr> filePath })
        }

module Content =

    let getHtml (fileMeta: FileMeta) : Async<string> =
        async { return Parser.mdToHtml fileMeta.Content.Content }

    let getJson (fileMeta: FileMeta) : Async<obj> =
        async { return Parser.getHtmlBodyElements ((Parser.mdToHtmlDoc fileMeta.Content.Content).DocumentNode) }

module HtmlView =
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

    let postCard (fileMeta: FileMeta) : XmlNode =
        Elem.section
            []
            [ Elem.h2 [] [ Text.raw fileMeta.Slug ]
              Elem.p [] [ Text.raw (fileMeta.CreationTime.ToString()) ]
              Elem.p [] [ Text.raw (fileMeta.ModificationType.ToString()) ]
              Elem.a [ Attr.href (sprintf $"{Uri.EscapeDataString(fileMeta.Slug)}") ] [ Text.raw "Read More" ] ]

    let homeView (fileMetas: FileMeta seq) : XmlNode =
        master
            "Writting"
            [ Elem.main
                  [ Attr.class' "mx-auto mw-800px" ]
                  [ Elem.h1 [] [ Text.raw "Writting" ]
                    yield!
                        [ for fileMeta: FileMeta in fileMetas do
                              postCard fileMeta ] ] ]

    let detailView (content: string) : XmlNode =
        master "Writting" [ Elem.main [ Attr.class' "mx-auto mw-800px" ] [ Text.raw content ] ]

module ApiResponse =
    let limitFileMetas (limit: int) (fileMetas: FileMeta seq) =
        if limit <= 0 then
            fileMetas
        else
            fileMetas |> Seq.take limit

    let getHtmlList (limit: int) (fileMetas: FileMeta seq) =
        fileMetas
        |> limitFileMetas limit
        |> Seq.map (fun (fileMeta: FileMeta) -> fileMeta |> HtmlView.postCard |> renderHtml)


    let getHtmlBySlug (fileMetas: FileMeta seq) (slug: string) : Async<string> =
        async {
            match fileMetas |> Seq.tryFind (fun (fileMeta: FileMeta) -> fileMeta.Slug = slug) with
            | Some(fileMeta: FileMeta) -> return! Content.getHtml fileMeta
            | None -> return ""
        }

    let getJsonList (limit: int) (fileMetas: FileMeta seq) : FileResponse seq =
        fileMetas
        |> limitFileMetas limit
        |> Seq.map (fun (fileMeta: FileMeta) ->
            { FileResponse.Slug = fileMeta.Slug
              FileResponse.CreationTime = fileMeta.CreationTime
              FileResponse.ModificationType = fileMeta.ModificationType
              FileResponse.Mattr = fileMeta.Content.Data })

    let getJsonBySlug (fileMetas: FileMeta seq) (slug: string) : Async<obj> =
        async {
            match fileMetas |> Seq.tryFind (fun (fileMeta: FileMeta) -> fileMeta.Slug = slug) with
            | Some(fileMeta: FileMeta) -> return! Content.getJson fileMeta
            | None -> return Map.empty
        }

module ApiHandler =
    let htmlListHandler (fileMetas: FileMeta seq) : HttpHandler =
        fun (ctx: HttpContext) ->
            let q: QueryCollectionReader = Request.getQuery ctx

            let htmlResponse: string =
                fileMetas |> ApiResponse.getHtmlList (q.GetInt("limit", 0)) |> String.concat ""

            Response.ofHtmlString htmlResponse ctx

    let htmlSlugHandler (fileMetas: FileMeta seq) : HttpHandler =
        fun (ctx: HttpContext) ->
            task {
                let route: RouteCollectionReader = Request.getRoute ctx
                let slug: string = route.GetString "slug"
                let! (htmlResponse: string) = ApiResponse.getHtmlBySlug fileMetas slug
                return! Response.ofHtmlString htmlResponse ctx
            }

    let jsonListHandler (fileMetas: FileMeta seq) : HttpHandler =
        fun (ctx: HttpContext) ->
            let q: QueryCollectionReader = Request.getQuery ctx

            let jsonResponse: FileResponse seq =
                fileMetas |> ApiResponse.getJsonList (q.GetInt("limit", 0))

            Response.ofJson jsonResponse ctx

    let jsonSlugHandler (fileMetas: FileMeta seq) : HttpHandler =
        fun (ctx: HttpContext) ->
            task {
                let route: RouteCollectionReader = Request.getRoute ctx
                let slug: string = route.GetString "slug"
                let! (jsonResponse: obj) = ApiResponse.getJsonBySlug fileMetas slug
                return! Response.ofJson jsonResponse ctx
            }
    let mainPageHandler (fileMetas: FileMeta seq) : HttpHandler =
        fileMetas |> HtmlView.homeView |> Response.ofHtml

    let detailPage (fileMetas: FileMeta seq) : HttpHandler =
        fun (ctx: HttpContext) ->
            task {
                let route: RouteCollectionReader = Request.getRoute ctx
                let slug: string = route.GetString "slug"
                let! (htmlResponse: string) = ApiResponse.getHtmlBySlug fileMetas slug
                return! Response.ofHtml (HtmlView.detailView htmlResponse) ctx
            }

let (fileMetas: FileMeta seq) =
    (Reader.loadFiles contentPath contentExt)
    |> Async.RunSynchronously
    |> Seq.sortByDescending _.CreationTime

webHost [||] {
    endpoints
        [ get "/" (ApiHandler.mainPageHandler fileMetas)
          get "/{slug}" (ApiHandler.detailPage fileMetas)
          get "/html" (ApiHandler.htmlListHandler fileMetas)
          get "/html/{slug}" (ApiHandler.htmlSlugHandler fileMetas)
          get "/json" (ApiHandler.jsonListHandler fileMetas)
          get "/json/{slug}" (ApiHandler.jsonSlugHandler fileMetas) ]
}
