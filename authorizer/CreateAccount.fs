module Authorizer.CreateAccount

open Authorizer
open Authorizer.Adapters

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

let validateAccount maybeAccount dto =
    match maybeAccount with
    | Some _ -> Error CreateAccountError.AccountAlreadyInitialized
    | None ->
        dto
        |> AccountInfo.accountToDomain
        |> Result.bimap Ok (CreateAccountError.InvalidInput >> Error)

let createAccount (repository: IAccountRepository) dto =
    let currentAccount = repository.Get()

    dto
    |> validateAccount currentAccount
    |> Result.tap repository.Create
    |> Result.map AccountInfo.fromAccount
    |> Result.mapErrorToList
    |> Result.bimap AuthorizeResult.authorized (adaptError currentAccount)
