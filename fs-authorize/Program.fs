open fs_authorize
open fs_authorize.CreateAccount
open fs_authorize.Repositories

[<EntryPoint>]
let main argv =
    AuthorizerApi.start ()
    0