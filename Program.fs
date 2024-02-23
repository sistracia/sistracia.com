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
    let mdToHtml (md: string) = Markdown.ToHtml md

    let htmlToHtmlDoc (html: string) =
        let doc = new HtmlDocument()
        doc.LoadHtml(html)
        doc


    let rec convertHtmlToJson (node: HtmlNode) =
        let mutable json: Map<string, obj> = Map []

        json <- json.Add("tag", node.Name)
        json <- json.Add("text", node.InnerText.Trim())

        if node.HasChildNodes then
            json <-
                json.Add(
                    "children",
                    (Seq.empty, node.ChildNodes)
                    ||> Seq.fold (fun previous childNode -> Seq.concat [ previous; [ convertHtmlToJson childNode ] ])
                )


        json

    let dictToJson (jsonObject: obj) =
        JsonSerializer.Serialize(jsonObject, JsonSerializerOptions(WriteIndented = true))

    let mdToHtmlDoc = mdToHtml >> htmlToHtmlDoc

    let htmlDocToJson = convertHtmlToJson >> dictToJson

module Reader =
    let loadFiles (contentPath: string) (contentExt: string) =
        Directory.EnumerateFiles(contentPath, contentExt)
        |> Seq.map (fun file -> File.ReadAllText file)

let htmlStrings =
    (Reader.loadFiles contentPath contentExt)
    |> Seq.map (fun file -> Parser.mdToHtml file)

let jsonStrings =
    (Reader.loadFiles contentPath contentExt)
    |> Seq.map (fun file -> Parser.htmlDocToJson (Parser.mdToHtmlDoc file).DocumentNode)


webHost [||] {
    endpoints
        [ get "/html" (Response.ofHtmlString (htmlStrings |> Seq.head))
          get "/json" (Response.ofPlainText (jsonStrings |> Seq.head)) ]
}
