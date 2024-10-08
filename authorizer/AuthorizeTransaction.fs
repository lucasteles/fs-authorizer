module Authorizer.AuthorizeTransaction

open Authorizer.Adapters

type AuthorizeTransactionWorkflow = IAccountRepository -> TransactionInfo -> AuthorizeResult

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

let authorizeTransaction: AuthorizeTransactionWorkflow =
    fun repository dto ->
        match TransactionInfo.fromDto dto, repository.Get() with
        | _, None -> accountNotInitializedResult
        | Error txErrors, Some account -> invalidTxResult account txErrors
        | Ok tx, Some account -> updateAccountTx repository.Update account tx
