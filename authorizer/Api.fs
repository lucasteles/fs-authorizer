module Authorizer.Api

open System
open Authorizer.Adapters
open Authorizer.Repositories

type AuthorizerInput =
    | TransactionInput of TransactionInfo
    | AccountInput of AccountInfo
    | InvalidInput

let parseInput json =
    try
        let input =
            json
            |> Json.deserialize<
                {| account: AccountInfo option
                   transaction: TransactionInfo option |}
                >

        match input.account, input.transaction with
        | Some accountJson, None -> AccountInput accountJson
        | None, Some transactionJson -> TransactionInput transactionJson
        | _ -> InvalidInput

    with _ ->
        InvalidInput

let parseOutput =
    function
    | AuthorizeSuccess o -> box o
    | AuthorizeFailure o -> o
    | NoAccount o -> o
    >> Json.serialize

let start () =
    let repository = AccountRepository()
    let printCurrentState = parseOutput >> printfn "%s"

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
