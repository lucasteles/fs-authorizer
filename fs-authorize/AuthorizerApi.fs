module fs_authorize.AuthorizerApi

open System
open System.Text.Json
open fs_authorize.Account_dto
open fs_authorize.Repositories
open fs_authorize.Transaction_dto

type AuthorizerInput =
    | TransactionInput of TransactionDto
    | AccountInput of AccountDto
    | InvalidInput

type InputChooseParser = { Account: obj; Transaction: obj }

let desserializeJson<'a> (json: string) =
    JsonSerializer.Deserialize<'a>(json, JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase))

let serializeJson thing =
    JsonSerializer.Serialize<_>(thing, JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase))

let parseInput json =
    try
        match desserializeJson<InputChooseParser> json with
        | { Transaction = _; Account = null } ->
            let input =
                json
                |> desserializeJson<{| Transaction: TransactionDto |}>

            TransactionInput input.Transaction
        | { Transaction = null; Account = _ } ->
            let input =
                json
                |> desserializeJson<{| Account: AccountDto |}>

            AccountInput input.Account
        | _ -> InvalidInput
    with _ -> InvalidInput

let start () =
    let accountRepository = AccountRepository()

    let processAccount =
        CreateAccount.createAccountWorkflow accountRepository.Get accountRepository.Create

    let processTransaction =
        AuthorizeTransaction.authorizeTransactionWorkflow accountRepository.Update accountRepository.Get

    let printCurrentState =
        unwrapOutput >> serializeJson >> printfn "%s"

    let rec readLoop () =
        let parsed = parseInput (Console.ReadLine())

        match parsed with
        | InvalidInput -> ()
        | AccountInput input ->
            processAccount input |> printCurrentState
            readLoop ()
        | TransactionInput input ->
            processTransaction input
            |> printCurrentState
            readLoop ()

    readLoop ()
