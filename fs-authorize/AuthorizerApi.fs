module fs_authorize.AuthorizerApi

open System
open System.Text.Json
open fs_authorize.Account_dto
open fs_authorize.Repositories

type AuthorizerInput =
    | TransactionInput of TransactionDto
    | AccountInput of AccountDto

type InputChooseParser = { Account: obj; Transaction: obj }

let desserializeJson<'a> (json: string) =
    JsonSerializer.Deserialize<'a>(json, JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase))

let serializeJson thing =
    JsonSerializer.Serialize<_>(thing, JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase))

let parseInput json =
    match desserializeJson<InputChooseParser> json with
    | { Transaction = _; Account = null } ->
        let input = json |> desserializeJson<{| Transaction: TransactionDto |}>
        TransactionInput input.Transaction
    | { Transaction = null; Account = _ } ->
        let input = json |> desserializeJson<{| Account: AccountDto |}>
        AccountInput input.Account
    | _ ->  failwith "Invalid input"


let start () =
    let accountRepository = AccountRepository()
    let processAccount =
       CreateAccount.createAccountWorkflow accountRepository.Get accountRepository.Create

    while true do
        let parsed = parseInput (Console.ReadLine())
        match parsed with
        | AccountInput input ->
            processAccount input
            |> unwrapOutput
            |> serializeJson
            |> printfn "%s"

        | TransactionInput input ->
            printfn "nothing yet..."
            ()
