namespace Authorizer

open System

type Transaction =
    { Merchant: NonEmptyString50
      Amount: Amount
      Time: DateTime }

type InactiveAccount = { TotalLimit: Limit }

type ActiveAccount =
    { TotalLimit: Limit
      Transactions: Transaction list }

type Account =
    | Active of ActiveAccount
    | Inactive of InactiveAccount

type CreateAccountError =
    | AccountAlreadyInitialized
    | NoAccount
    | InvalidInput of Limit.Errors

type CreateTransactionError =
    | MerchantName of NonEmptyString50.Errors
    | InvalidAmount of Amount.Errors

type AuthorizationError =
    | InsufficientLimit
    | CardIsNotActive
    | HighFrequencySmallInterval
    | DoubledTransaction
    | AccountNotInitialized
    | InvalidTx of CreateTransactionError

module Transaction =
    let create time merchantName amount =
        { Amount = amount
          Time = time
          Merchant = merchantName }

    let equivalent (t1: Transaction) (t2: Transaction) =
        (t1.Merchant, t1.Amount) = (t2.Merchant, t2.Amount)

module ActiveAccount =
    let currentAvailableLimit a =
        let transactionTotal =
            a.Transactions |> List.map _.Amount |> List.sum |> Amount.value


        a.TotalLimit |> Limit.subtract transactionTotal

module Account =
    let currentAvailableLimit =
        function
        | Inactive { TotalLimit = available } -> available
        | Active account -> ActiveAccount.currentAvailableLimit account

    let addTransaction tx account =
        { account with
            Transactions = tx :: account.Transactions }
