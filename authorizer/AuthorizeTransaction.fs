module Authorizer.AuthorizeTransaction

open Authorizer.Dto

type UpdateAccount = Account -> unit
type ReadAccount = unit -> Account option
type AuthorizeTransactionWorkflow = UpdateAccount -> ReadAccount -> TransactionInfo -> AuthorizeResult

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
    fun updateAccount getAccount dto ->
        match TransactionInfo.fromDto dto, getAccount () with
        | _, None -> accountNotInitializedResult
        | Error txErrors, Some account -> invalidTxResult account txErrors
        | Ok tx, Some account -> updateAccountTx updateAccount account tx
