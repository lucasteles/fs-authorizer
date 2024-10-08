module Authorizer.Api

open System
open System.Text.Json
open Authorizer.Adapters
open Authorizer.Repositories

type AuthorizerInput =
    | TransactionInput of TransactionInfo
    | AccountInput of AccountInfo
    | InvalidInput

let parseInput json =
    try
        let raw = Json.deserialize<JsonElement> json

        match raw.TryGetProperty "account", raw.TryGetProperty "transaction" with
        | (true, accountJson), (false, _) -> accountJson.Deserialize<AccountInfo>(Json.options) |> AccountInput
        | (false, _), (true, transactionJson) ->
            transactionJson.Deserialize<TransactionInfo>(Json.options) |> TransactionInput
        | _ -> InvalidInput

    with _ ->
        InvalidInput

let serializeAccount =
    function
    | AuthorizeSuccess o -> box o
    | AuthorizeFailure o -> box o
    | NoAccount o -> box o
    >> Json.serialize


let start () =
    let repository = AccountRepository()
    let printCurrentState = serializeAccount >> printfn "%s"

    let rec readLoop () =
        let parsed = Console.ReadLine() |> parseInput

        match parsed with
        | InvalidInput -> ()
        | AccountInput input ->
            input |> CreateAccount.createAccount repository |> printCurrentState

            readLoop ()
        | TransactionInput input ->
            input
            |> AuthorizeTransaction.authorizeTransaction repository
            |> printCurrentState

            readLoop ()

    readLoop ()
