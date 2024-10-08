module Authorizer.Validators

open System


let transactionAmountShouldNotExceedAvailableLimit transaction activeAccount =
    let (Amount amount) = transaction.Amount
    let (Limit currentLimit) = activeAccount |> ActiveAccount.currentAvailableLimit

    if amount > currentLimit then
        Some AuthorizationError.InsufficientLimit
    else
        None

let noTransactionShouldBeAcceptedWhenTheCardIsNotActive =
    function
    | Active account -> Ok account
    | Inactive _ -> Error AuthorizationError.CardIsNotActive

let private earlierMinutes amount (time: DateTime) = time - TimeSpan.FromMinutes(amount)

let thereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval time activeAccount =
    let maxTransactionCount = 3
    let twoMinutesEarlier = time |> earlierMinutes 2

    let txCount =
        activeAccount.Transactions
        |> List.filter (fun t -> t.Time > twoMinutesEarlier)
        |> List.length

    if txCount >= maxTransactionCount then
        Some AuthorizationError.HighFrequencySmallInterval
    else
        None

let thereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval time transaction activeAccount =
    let maxTransactionCount = 2
    let twoMinutesEarlier = time |> earlierMinutes 2

    let txCount =
        activeAccount.Transactions
        |> List.filter (fun t -> t.Time > twoMinutesEarlier)
        |> List.filter (Transaction.equivalent transaction)
        |> List.length

    if txCount >= maxTransactionCount then
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
