module Writing.Program

open Falco
open Falco.Routing
open Falco.HostBuilder

webHost [||] {
    endpoints [
        get "/" (Response.ofPlainText "Hello World")
    ]
}
