module Authorizer.AuthorizeTransaction

open Authorizer.Adapters

let adaptError account =
    account
    |> AccountInfo.fromAccount
    |> (AuthorizeResult.authorizationFailure AuthorizeResult.mapAuthorizationError)

let accountNotInitializedResult =
    AuthorizationError.AccountNotInitialized
    |> List.singleton
    |> AuthorizeResult.noAccount AuthorizeResult.mapAuthorizationError

let invalidTxResult account =
    List.map AuthorizationError.InvalidTx >> adaptError account

let tryAddTransaction tx account =
    account
    |> Validators.validateTransaction tx
    |> Result.map (Account.addTransaction tx)

let updateAccountTx updateFn account tx =
    account
    |> tryAddTransaction tx
    |> Result.map Active
    |> Result.tap updateFn
    |> Result.map AccountInfo.fromAccount
    |> Result.bimap AuthorizeResult.authorized (adaptError account)

let authorizeTransaction (repository: IAccountRepository) dto =
    match repository.Get(), TransactionInfo.fromDto dto with
    | None, _ -> accountNotInitializedResult
    | Some account, Error txErrors -> invalidTxResult account txErrors
    | Some account, Ok tx -> updateAccountTx repository.Update account tx
