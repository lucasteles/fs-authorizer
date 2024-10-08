module Authorizer.CreateAccount

open Authorizer
open Authorizer.Dto

type ValidateAccount = Account option -> AccountInfo -> Result<Account, CreateAccountError>
type CreateAccountWorkflow = (unit -> Account option) -> (Account -> unit) -> AccountInfo -> AuthorizeResult

let validateAccount: ValidateAccount =
    fun currentAccount dto ->
        match currentAccount with
        | Some _ -> Error CreateAccountError.AccountAlreadyInitialized
        | None ->
            dto
            |> AccountInfo.accountToDomain
            |> Result.bimap Ok (CreateAccountError.InvalidInput >> Error)

let private adaptError errorRenderFn maybeAccount =
    maybeAccount
    |> Option.map AccountInfo.fromAccount
    |> function
        | Some acc -> acc |> AuthorizeResult.authorizationFailure errorRenderFn
        | None ->
            fun _ ->
                CreateAccountError.NoAccount
                |> List.singleton
                |> AuthorizeResult.noAccount errorRenderFn

let createAccount: CreateAccountWorkflow =
    fun getAccount insertAccount dto ->
        let currentAccount = getAccount ()

        dto
        |> validateAccount currentAccount
        |> Result.tap insertAccount
        |> Result.map AccountInfo.fromAccount
        |> Result.mapErrorToList
        |> Result.bimap AuthorizeResult.authorized (adaptError AuthorizeResult.mapAccountError currentAccount)
