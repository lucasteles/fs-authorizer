module Authorizer.CreateAccount

open Authorizer
open Authorizer.Adapters

type ValidateAccount = Account option -> AccountInfo -> Result<Account, CreateAccountError>
type CreateAccountWorkflow = IAccountRepository -> AccountInfo -> AuthorizeResult

let private adaptError maybeAccount =
    maybeAccount
    |> Option.map AccountInfo.fromAccount
    |> function
        | Some account -> account |> AuthorizeResult.authorizationFailure AuthorizeResult.mapAccountError
        | None ->
            fun _ ->
                CreateAccountError.NoAccount
                |> List.singleton
                |> AuthorizeResult.noAccount AuthorizeResult.mapAccountError

let validateAccount: ValidateAccount =
    fun currentAccount dto ->
        match currentAccount with
        | Some _ -> Error CreateAccountError.AccountAlreadyInitialized
        | None ->
            dto
            |> AccountInfo.accountToDomain
            |> Result.bimap Ok (CreateAccountError.InvalidInput >> Error)

let createAccount: CreateAccountWorkflow =
    fun repository dto ->
        let currentAccount = repository.Get()

        dto
        |> validateAccount currentAccount
        |> Result.tap repository.Create
        |> Result.map AccountInfo.fromAccount
        |> Result.mapErrorToList
        |> Result.bimap AuthorizeResult.authorized (adaptError currentAccount)
