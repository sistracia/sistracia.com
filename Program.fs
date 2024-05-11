module Writing.Program

open Falco
open Falco.Routing
open Falco.HostBuilder
open Falco.Markup
open Markdig
open Markdig.Prism
open HtmlAgilityPack
open Microsoft.AspNetCore.Http
open System
open System.IO
open GrayMattr

let contentPath: string = "./content"
let postsPath: string = $"{contentPath}/posts"
let profilePath = $"{contentPath}/profile.yaml"

let githubURL: string = "https://github.com/sistracia/sistracia.com"
let githubContentOriginURL: string = $"{githubURL}/blob/main/content"
let githubContentHistoryURL: string = $"{githubURL}/commits/main/content"

[<CLIMutable>]
type ProfileMattr =
    { Name: string
      Introduction: string
      Links: string array }

    static member Default =
        { Name = ""
          Introduction = ""
          Links = [||] }

[<CLIMutable>]
type PostMattr =
    { Title: string
      Description: string }

    static member Default = { Title = ""; Description = "" }

[<Struct>]
type PostMeta =
    { Slug: string
      CreationTime: DateTime
      ModificationTime: DateTime
      Content: string
      Mattr: PostMattr }

    member this.FormattedCreationTime = this.CreationTime.ToString("d.M.yyyy")
    member this.FormattedModificationTime = this.ModificationTime.ToString("d.M.yyyy")

[<Struct>]
type PostResponse =
    { CreationTime: DateTime
      ModificationTime: DateTime
      Slug: string
      Mattr: PostMattr }

module Utils =
    let markdigPipeline: MarkdownPipeline =
        MarkdownPipelineBuilder()
            .UseAbbreviations()
            .UseAdvancedExtensions()
            .UseAutoIdentifiers()
            .UseAutoLinks()
            .UseBootstrap()
            .UseCitations()
            .UseCustomContainers()
            .UseDefinitionLists()
            .UseDiagrams()
            .UseEmojiAndSmiley()
            .UseEmphasisExtras()
            .UseFigures()
            .UseFooters()
            .UseFootnotes()
            .UseGenericAttributes()
            .UseGlobalization()
            .UseListExtras()
            .UseMathematics()
            .UseMediaLinks()
            .UseNonAsciiNoEscape()
            .UsePipeTables()
            .UsePragmaLines()
            .UsePreciseSourceLocation()
            .UseReferralLinks()
            .UseTaskLists()
            .UseYamlFrontMatter()
            .UsePrism()
            .Build()

    let mdToHtml (md: string) : string = Markdown.ToHtml(md, markdigPipeline)

    let htmlToHtmlDoc (html: string) : HtmlDocument =
        let doc: HtmlDocument = HtmlDocument()
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

    let limitFileMetas (limit: int) (fileMetas: PostMeta seq) : PostMeta seq =
        if limit <= 0 || limit > (fileMetas |> Seq.length) then
            fileMetas
        else
            fileMetas |> Seq.take limit

    let contentOrigin (slug: string) : string =
        $"{githubContentOriginURL}/{Uri.EscapeDataString(slug)}.md"

    let contentHistory (slug: string) : string =
        $"{githubContentHistoryURL}/{Uri.EscapeDataString(slug)}.md"

    let getFileStr (idx: int) (path: string) =
        let segments: string array = path.Split '/'

        match Array.tryItem (segments.Length - idx) segments with
        | None -> "not-found"
        | Some(segment: string) -> segment

    let getFileDir = getFileStr 2

module Reader =
    let rec getAllMdFiles (directoryPath: string) : string seq =
        let mdFiles: string seq = Directory.EnumerateFiles(directoryPath, "index.md")
        let subDirectories: string seq = Directory.EnumerateDirectories(directoryPath)
        let subDirMdFiles: string seq = subDirectories |> Seq.collect getAllMdFiles

        Seq.append mdFiles subDirMdFiles

    let loadFiles (contentPath: string) : Async<PostMeta seq> =
        async {
            return
                getAllMdFiles contentPath
                |> Seq.map (fun (filePath: string) ->
                    let fileMattr: GrayFile<PostMattr> = (Mattr.Read<PostMattr> filePath)

                    { PostMeta.Slug = Utils.getFileDir filePath
                      PostMeta.CreationTime = File.GetCreationTime filePath
                      PostMeta.ModificationTime = File.GetLastWriteTime filePath
                      PostMeta.Content = fileMattr.Content.Replace("(./", "(./post-assets/")
                      PostMeta.Mattr =
                        match fileMattr.Data with
                        | None -> PostMattr.Default
                        | Some(data: PostMattr) -> data })
        }

module Content =

    let getHtml (fileMeta: PostMeta) : string = Utils.mdToHtml fileMeta.Content

    let getJson (fileMeta: PostMeta) : obj =
        Utils.getHtmlBodyElements ((Utils.mdToHtmlDoc fileMeta.Content).DocumentNode)

module HtmlView =
    let master (title: string) (styleLinks: string array) (content: XmlNode list) : XmlNode =
        Elem.html
            [ Attr.lang "en" ]
            [ Elem.head
                  []
                  [ Elem.meta [ Attr.charset "UTF-8" ]
                    Elem.meta [ Attr.name "viewport"; Attr.content "width=device-width, initial-scale=1.0" ]
                    Elem.title [] [ Text.raw title ]
                    Elem.style
                        []
                        [ Text.raw
                              """
                                        /* Ref: https://www.joshwcomeau.com/css/custom-css-reset/ */
                                        /*
                                            1. Use a more-intuitive box-sizing model.
                                        */
                                        *, *::before, *::after {
                                            box-sizing: border-box;
                                        }
                                        /*
                                            2. Remove default margin
                                        */
                                        * {
                                            margin: 0;
                                        }
                                        /*
                                            Typographic tweaks!
                                            3. Add accessible line-height
                                            4. Improve text rendering
                                        */
                                        body {
                                            line-height: 1.5;
                                            -webkit-font-smoothing: antialiased;
                                        }
                                        /*
                                            5. Improve media defaults
                                        */
                                        img, picture, video, canvas, svg {
                                            display: block;
                                            max-width: 100%;
                                        }
                                        /*
                                            6. Remove built-in form typography styles
                                        */
                                        input, button, textarea, select {
                                            font: inherit;
                                        }
                                        /*
                                            7. Avoid text overflows
                                        */
                                        p, h1, h2, h3, h4, h5, h6 {
                                            overflow-wrap: break-word;
                                        }
                                        /*
                                            8. Create a root stacking context
                                        */
                                        #root {
                                            isolation: isolate;
                                        }
                        """ ]
                    yield!
                        [ for styleLink: string in styleLinks do
                              Elem.link [ Attr.href styleLink; Attr.rel "stylesheet" ] ] ]
              Elem.body [] content ]

    let postCard (fileMeta: PostMeta) : XmlNode =
        Elem.article
            []
            [ Elem.h3
                  []
                  [ Elem.a
                        [ Attr.href (sprintf $"{Uri.EscapeDataString(fileMeta.Slug)}") ]
                        [ Text.raw fileMeta.Mattr.Title ] ]
              Elem.span [] [ Text.raw fileMeta.FormattedCreationTime ]
              Elem.p [] [ Text.raw fileMeta.Mattr.Description ] ]

    let homeView (profile: ProfileMattr) (fileMetas: PostMeta seq) : XmlNode =
        master
            "Writting"
            [| "styles/home.css" |]
            [ Elem.div
                  [ Attr.id "root" ]
                  [ Elem.header
                        []
                        [ Elem.h1 [] [ Text.raw profile.Name ]
                          Elem.p [] [ Text.raw profile.Introduction ]
                          yield!
                              [ for link: string in profile.Links do
                                    Elem.a [ Attr.href link ] [ Text.raw link ] ] ]
                    Elem.main
                        []
                        [ Elem.section
                              [ Attr.id "posts" ]
                              [ Elem.h2 [] [ Text.raw "Posts" ]
                                yield!
                                    [ for fileMeta: PostMeta in fileMetas do
                                          postCard fileMeta ] ] ] ] ]

    let detailView (content: string) (fileMeta: PostMeta) : XmlNode =
        master
            "Writting"
            [| "styles/detail.css"; "styles/prism.css" |]
            [ Elem.div
                  [ Attr.id "root" ]
                  [ Elem.header
                        []
                        [ Elem.h1
                              []
                              [ Elem.a
                                    [ Attr.href (Utils.contentOrigin fileMeta.Slug) ]
                                    [ Text.raw fileMeta.Mattr.Title ] ]
                          Elem.table
                              []
                              [ Elem.tbody
                                    []
                                    [ Elem.tr
                                          []
                                          [ Elem.td [] [ Text.raw "Created" ]
                                            Elem.td
                                                []
                                                [ Elem.a
                                                      [ Attr.href (Utils.contentOrigin fileMeta.Slug) ]
                                                      [ Text.raw fileMeta.FormattedCreationTime ] ] ]
                                      Elem.tr
                                          []
                                          [ Elem.td [] [ Text.raw "Updated" ]
                                            Elem.td
                                                []
                                                [ Elem.a
                                                      [ Attr.href (Utils.contentHistory fileMeta.Slug) ]
                                                      [ Text.raw fileMeta.FormattedModificationTime ] ] ] ] ] ]
                    Elem.main [] [ Text.raw content ] ]
              Elem.script [ Attr.src "scripts/prism.js" ] [] ]

module ApiResponse =

    let getHtmlList (limit: int) (fileMetas: PostMeta seq) : string seq =
        fileMetas
        |> Utils.limitFileMetas limit
        |> Seq.map (fun (fileMeta: PostMeta) -> fileMeta |> HtmlView.postCard |> renderHtml)

    let getBySlug (slug: string) (fileMetas: PostMeta seq) : PostMeta option =
        fileMetas |> Seq.tryFind (fun (fileMeta: PostMeta) -> fileMeta.Slug = slug)

    let getBySlugOrDefault<'T> (somer: PostMeta -> 'T) (none: 'T) (slug: string) (fileMetas: PostMeta seq) : 'T =
        match getBySlug slug fileMetas with
        | Some(fileMeta: PostMeta) -> somer fileMeta
        | None -> none

    let getHtmlBySlug (slug: string) (fileMetas: PostMeta seq) : string =
        getBySlugOrDefault Content.getHtml "" slug fileMetas

    let getJsonList (limit: int) (fileMetas: PostMeta seq) : PostResponse seq =
        fileMetas
        |> Utils.limitFileMetas limit
        |> Seq.map (fun (fileMeta: PostMeta) ->
            { PostResponse.Slug = fileMeta.Slug
              PostResponse.CreationTime = fileMeta.CreationTime
              PostResponse.ModificationTime = fileMeta.ModificationTime
              PostResponse.Mattr = fileMeta.Mattr })

    let getJsonBySlug (slug: string) (fileMetas: PostMeta seq) : obj =
        getBySlugOrDefault Content.getJson "" slug fileMetas

module ApiHandler =
    let htmlListHandler (fileMetas: PostMeta seq) : HttpHandler =
        fun (ctx: HttpContext) ->
            let q: QueryCollectionReader = Request.getQuery ctx

            let htmlResponse: string =
                fileMetas |> ApiResponse.getHtmlList (q.GetInt("limit", 0)) |> String.concat ""

            Response.ofHtmlString htmlResponse ctx

    let htmlSlugHandler (fileMetas: PostMeta seq) : HttpHandler =
        fun (ctx: HttpContext) ->
            task {
                let route: RouteCollectionReader = Request.getRoute ctx
                let slug: string = route.GetString "slug"
                let htmlResponse: string = ApiResponse.getHtmlBySlug slug fileMetas
                return! Response.ofHtmlString htmlResponse ctx
            }

    let jsonListHandler (fileMetas: PostMeta seq) : HttpHandler =
        fun (ctx: HttpContext) ->
            let q: QueryCollectionReader = Request.getQuery ctx

            let jsonResponse: PostResponse seq =
                fileMetas |> ApiResponse.getJsonList (q.GetInt("limit", 0))

            Response.ofJson jsonResponse ctx

    let jsonSlugHandler (fileMetas: PostMeta seq) : HttpHandler =
        fun (ctx: HttpContext) ->
            task {
                let route: RouteCollectionReader = Request.getRoute ctx
                let slug: string = route.GetString "slug"
                let jsonResponse: obj = ApiResponse.getJsonBySlug slug fileMetas
                return! Response.ofJson jsonResponse ctx
            }

    let mainPageHandler (profile: ProfileMattr) (fileMetas: PostMeta seq) : HttpHandler =
        fileMetas
        |> Utils.limitFileMetas 0
        |> HtmlView.homeView profile
        |> Response.ofHtml

    let detailPage (fileMetas: PostMeta seq) : HttpHandler =
        fun (ctx: HttpContext) ->
            task {
                let route: RouteCollectionReader = Request.getRoute ctx
                let slug: string = route.GetString "slug"

                let response =
                    match ApiResponse.getBySlug slug fileMetas with
                    | None -> Response.ofPlainText "Not Found" ctx
                    | Some(fileMeta: PostMeta) ->
                        Response.ofHtml (HtmlView.detailView (Content.getHtml fileMeta) fileMeta) ctx

                return! response
            }

let (fileMetas: PostMeta seq) =
    (Reader.loadFiles postsPath)
    |> Async.RunSynchronously
    |> Seq.sortByDescending _.CreationTime

let (profile: ProfileMattr) =
    match (Mattr.Read<ProfileMattr> profilePath).Data with
    | None -> ProfileMattr.Default
    | Some(data: ProfileMattr) -> data

webHost [||] {
    use_caching
    use_compression
    use_default_files
    use_static_files

    endpoints
        [ get "/" (ApiHandler.mainPageHandler profile fileMetas)
          get "/{slug}" (ApiHandler.detailPage fileMetas)
          get "/html" (ApiHandler.htmlListHandler fileMetas)
          get "/html/{slug}" (ApiHandler.htmlSlugHandler fileMetas)
          get "/json" (ApiHandler.jsonListHandler fileMetas)
          get "/json/{slug}" (ApiHandler.jsonSlugHandler fileMetas) ]
}
