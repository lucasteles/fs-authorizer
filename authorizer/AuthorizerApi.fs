module authorizer.AuthorizerApi

open System
open System.Text.Json
open authorizer.Repositories
open authorizer.AccountDto
open authorizer.TransactionDto

type AuthorizerInput =
    | TransactionInput of TransactionInfo
    | AccountInput of AccountInfo
    | InvalidInput

module Json =
    let options =
        JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase, WriteIndented = false)

    let deserialize<'a> (json: string) =
        JsonSerializer.Deserialize<'a>(json, options)

    let serialize thing =
        JsonSerializer.Serialize<_>(thing, options)

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
