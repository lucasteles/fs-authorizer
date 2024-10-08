module Authorizer.Validators

open System

type UpdateAccount = Account -> unit
type ReadAccount = unit -> Account option
type NoTransactionShouldBeAcceptedWhenTheCardIsNotActive = Account -> Result<ActiveAccount, AuthorizationError>
type ThereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval = DateTime -> ActiveAccount -> AuthorizationError option
type TransactionAmountShouldNotExceedAvailableLimit = Transaction -> ActiveAccount -> AuthorizationError option

type ThereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval =
    DateTime -> Transaction -> ActiveAccount -> AuthorizationError option


let transactionAmountShouldNotExceedAvailableLimit: TransactionAmountShouldNotExceedAvailableLimit =
    fun transaction activeAccount ->
        let (Amount amount) = transaction.Amount
        let (Limit currentLimit) = activeAccount |> ActiveAccount.currentAvailableLimit

        if amount > currentLimit then
            Some AuthorizationError.InsufficientLimit
        else
            None

let noTransactionShouldBeAcceptedWhenTheCardIsNotActive: NoTransactionShouldBeAcceptedWhenTheCardIsNotActive =
    function
    | Active account -> Ok account
    | Inactive _ -> Error AuthorizationError.CardIsNotActive

let thereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval: ThereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval =
    fun time activeAccount ->
        let maxTransactionIn2Minutes = 3
        let twoMinutesEarlier = time - TimeSpan.FromMinutes(2.)

        let txCount =
            activeAccount.Transactions
            |> List.filter (fun t -> t.Time > twoMinutesEarlier)
            |> List.length

        if txCount >= maxTransactionIn2Minutes then
            Some AuthorizationError.HighFrequencySmallInterval
        else
            None

let thereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval
    : ThereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval =
    fun time transaction activeAccount ->
        let maxTransactionIn2Minutes = 2
        let twoMinutesEarlier = time - TimeSpan.FromMinutes(2.)

        let txCount =
            activeAccount.Transactions
            |> List.filter (fun t -> t.Time > twoMinutesEarlier)
            |> List.filter (Transaction.equivalent transaction)
            |> List.length

        if txCount >= maxTransactionIn2Minutes then
            Some AuthorizationError.DoubledTransaction
        else
            None

let private available tx =
    [ transactionAmountShouldNotExceedAvailableLimit tx
      thereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval tx.Time
      thereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval tx.Time tx ]

let private runAvailable tx activeAccount =
    tx
    |> available
    |> List.choose (fun f -> f activeAccount)
    |> function
        | [] -> Ok activeAccount
        | errs -> Error errs

let validateTransaction tx account =
    account
    |> noTransactionShouldBeAcceptedWhenTheCardIsNotActive
    |> Result.mapErrorToList
    >>= runAvailable tx
