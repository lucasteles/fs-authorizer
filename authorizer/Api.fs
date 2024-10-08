module Authorizer.Api

open System
open System.Text.Json
open Authorizer.Repositories
open Authorizer.Dto

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
    let accountRepository = AccountRepository()

    let processAccount =
        CreateAccount.createAccount accountRepository.Get accountRepository.Create

    let processTransaction =
        AuthorizeTransaction.authorizeTransaction accountRepository.Update accountRepository.Get

    let printCurrentState = serializeAccount >> printfn "%s"

    let rec readLoop () =
        let parsed = parseInput (Console.ReadLine())

        match parsed with
        | InvalidInput -> ()
        | AccountInput input ->
            processAccount input |> printCurrentState
            readLoop ()
        | TransactionInput input ->
            processTransaction input |> printCurrentState
            readLoop ()

    readLoop ()
