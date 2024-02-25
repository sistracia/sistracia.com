module Writing.Program

open Falco
open Falco.Routing
open Falco.HostBuilder
open Markdig
open HtmlAgilityPack
open System.Text.Json
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
    let loadFiles (contentPath: string) (contentExt: string) =
        Directory.EnumerateFiles(contentPath, contentExt)
        |> Seq.map (fun (file: string) -> File.ReadAllText file)

let htmlStrings: string seq =
    (Reader.loadFiles contentPath contentExt)
    |> Seq.map (fun (file: string) -> Parser.mdToHtml file)

let jsonStrings: string seq =
    (Reader.loadFiles contentPath contentExt)
    |> Seq.map (fun (file: string) -> Parser.htmlDocToJson (Parser.mdToHtmlDoc file).DocumentNode)


webHost [||] {
    endpoints
        [ get "/html" (Response.ofHtmlString (htmlStrings |> Seq.head))
          get "/json" (Response.ofPlainText (jsonStrings |> Seq.head)) ]
}
