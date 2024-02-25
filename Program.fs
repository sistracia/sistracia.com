module Writing.Program

open Falco
open Falco.Routing
open Falco.HostBuilder
open Markdig
open HtmlAgilityPack
open System.Text.Json
open Microsoft.AspNetCore.Http
open System.IO

let contentPath = "./content"
let contentExt = "*.md"

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

    let dictToJson (jsonObject: obj) : string =
        JsonSerializer.Serialize(jsonObject, JsonSerializerOptions(WriteIndented = true))

    let mdToHtmlDoc = mdToHtml >> htmlToHtmlDoc

    let htmlDocToJson = convertHtmlToJson >> dictToJson

module Reader =
    let loadFiles (contentPath: string) (contentExt: string) : Async<string seq> =
        async {
            return
                Directory.EnumerateFiles(contentPath, contentExt)
                |> Seq.map (fun (file: string) -> File.ReadAllText file)
        }

let htmlStrings: Async<string array> =
    async {
        let! (fileMetas: string seq) = Reader.loadFiles contentPath contentExt

        return!
            fileMetas
            |> Seq.map (fun (file: string) -> async { return Parser.mdToHtml file })
            |> Async.Parallel
    }

let jsonStrings: Async<string array> =
    async {
        let! (fileMetas: string seq) = (Reader.loadFiles contentPath contentExt)

        return!
            fileMetas
            |> Seq.map (fun (file: string) ->
                async { return Parser.htmlDocToJson (Parser.mdToHtmlDoc file).DocumentNode })
            |> Async.Parallel
    }


let htmlHandler: HttpHandler =
    fun (ctx: HttpContext) ->
        task {
            let! (awaitedHtmlStrings: string array) = htmlStrings
            return! Response.ofHtmlString (awaitedHtmlStrings |> Seq.head) ctx
        }


let jsonHandler: HttpHandler =
    fun (ctx: HttpContext) ->
        task {
            let! (awaitedJsonStrings: string array) = jsonStrings
            return! Response.ofPlainText (awaitedJsonStrings |> Seq.head) ctx
        }

webHost [||] { endpoints [ get "/html" htmlHandler; get "/json" jsonHandler ] }
