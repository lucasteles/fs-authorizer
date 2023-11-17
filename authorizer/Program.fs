open System
open authorizer
open authorizer.CreateAccount
open authorizer.Repositories

[<EntryPoint>]
let main _ =
    AuthorizerApi.start ()
    0
