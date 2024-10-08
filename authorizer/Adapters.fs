namespace Authorizer.Adapters

open System
open Authorizer

type AccountInfo =
    { ActiveCard: bool
      AvailableLimit: decimal }

type TransactionInfo =
    { Merchant: string
      Amount: decimal
      Time: DateTime }

type AuthorizeResult =
    | AuthorizeSuccess of
        {| Account: AccountInfo
           Violations: string list |}
    | AuthorizeFailure of
        {| Account: AccountInfo
           Violations: string list |}
    | NoAccount of {| Violations: string list |}

module AccountInfo =
    let accountToDomain (accountDto: AccountInfo) =
        let limitResult = accountDto.AvailableLimit |> Money.usd |> Limit.create

        let defAccount limit =
            if accountDto.ActiveCard then
                Active
                    { TotalLimit = limit
                      Transactions = [] }
            else
                Inactive { TotalLimit = limit }

        limitResult |> Result.map defAccount

    let fromAccount account =
        let (Limit limit) = account |> Account.currentAvailableLimit

        let isActive =
            match account with
            | Active _ -> true
            | Inactive _ -> false

        { ActiveCard = isActive
          AvailableLimit = decimal limit }

module TransactionInfo =
    let fromDto dto =
        let merchant =
            dto.Merchant
            |> NonEmptyString50.create
            |> Result.mapError CreateTransactionError.MerchantName
            |> Result.mapErrorToList

        let amount =
            dto.Amount
            |> Money.usd
            |> Amount.create
            |> Result.mapError CreateTransactionError.InvalidAmount
            |> Result.mapErrorToList

        Transaction.create dto.Time <!> merchant <*> amount

module AuthorizeResult =
    let mapAccountError =
        function
        | CreateAccountError.AccountAlreadyInitialized -> "account-already-initialized"
        | CreateAccountError.InvalidInput _ -> "invalid-input"
        | CreateAccountError.NoAccount -> "no-account"

    let mapAuthorizationError =
        function
        | AuthorizationError.InsufficientLimit -> "insufficient-limit"
        | AuthorizationError.CardIsNotActive -> "card-not-active"
        | AuthorizationError.HighFrequencySmallInterval -> "high-frequency-small-interval"
        | AuthorizationError.DoubledTransaction -> "doubled-transaction"
        | AuthorizationError.InvalidTx _ -> "invalid-merchant"
        | AuthorizationError.AccountNotInitialized -> "account-not-initialized"

    let authorizationFailure errorMapFn account violations =
        AuthorizeFailure
            {| Account = account
               Violations = List.map errorMapFn violations |}

    let noAccount renderFn violations =
        NoAccount {| Violations = List.map renderFn violations |}

    let authorized account =
        AuthorizeSuccess {| Account = account; Violations = [] |}
