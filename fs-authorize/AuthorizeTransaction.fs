module fs_authorize.AuthorizeTransaction

open System
open fs_authorize.Account
open fs_authorize.Account_dto
open fs_authorize.SimpleTypes
open fs_authorize.Transaction
open fs_authorize.Transaction_dto

type NoTransactionShouldBeAcceptedWhenTheCardIsNotActive = Account -> Result<ActiveAccount, TransactionErrors>
type ThereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval =
    DateTime -> ActiveAccount -> Result<ActiveAccount, TransactionErrors>
type ThereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval =
    DateTime -> Transaction -> ActiveAccount -> Result<ActiveAccount, TransactionErrors>
type TransactionAmountShouldNotExceedAvailableLimit =
    Transaction -> ActiveAccount -> Result<ActiveAccount, TransactionErrors>

type AuthorizeTransactionWorkflow =
    (Account -> unit) -> (unit -> Account option) ->  TransactionDto -> OutputDto

let transactionAmountShouldNotExceedAvailableLimit: TransactionAmountShouldNotExceedAvailableLimit =
    fun transaction activeAccount ->
        let (Amount amount) = transaction.Amount
        let (Limit currentLimit) = activeAccount.AvailableLimit

        if amount > currentLimit
        then Error TransactionErrors.InsufficientLimit
        else Ok activeAccount

let noTransactionShouldBeAcceptedWhenTheCardIsNotActive: NoTransactionShouldBeAcceptedWhenTheCardIsNotActive =
    function
    | ActiveAccount account -> Ok account
    | InactiveAccount _ -> Error TransactionErrors.CardIsNotActive

let thereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval: ThereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval =
    fun now activeAccount ->
        let twoMinutesEarlier = now - TimeSpan.FromMinutes(2.)
        let maxTransactionIn2Minutes = 3

        let txCount =
            activeAccount.Transactions
            |> List.filter (fun t -> t.Time > twoMinutesEarlier)
            |> List.length

        if txCount >= maxTransactionIn2Minutes
        then Error TransactionErrors.HighFrequencySmallInterval
        else Ok activeAccount

let thereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval: ThereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval =
    fun now transaction activeAccount ->
        let twoMinutesEarlier = now - TimeSpan.FromMinutes(2.)
        let maxTransactionIn2Minutes = 2

        let compareTx (t1: Transaction) (t2: Transaction) =
            (t1.Merchant, t1.Amount) = (t2.Merchant, t2.Amount)

        let txCount =
            activeAccount.Transactions
            |> List.filter (fun t -> t.Time > twoMinutesEarlier)
            |> List.filter (compareTx transaction)
            |> List.length

        if txCount >= maxTransactionIn2Minutes
        then Error TransactionErrors.DoubledTransaction
        else Ok activeAccount

let private adaptError account =
                 account
                 |> accountToDto
                 |> printableAccountErrorResponse mapTxErrorToString

let authorizeTransactionWorkflow: AuthorizeTransactionWorkflow =
    fun updateAccount getAccount dto ->
        let currentAccount = getAccount ()
        let now = dto.Time
        let workflow tx account =
            noTransactionShouldBeAcceptedWhenTheCardIsNotActive account
            >>= transactionAmountShouldNotExceedAvailableLimit tx
            >>= thereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval now
            >>= thereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval now tx
            |> Result.map (addTransactionToAccount tx)
            |> Result.map ActiveAccount
            |> Result.tap updateAccount
            |> Result.map accountToDto
            |> Result.mapError List.singleton
            |> Result.bimap printableAccountResponse (adaptError account)

        match dtoToTx dto, currentAccount with
        | Ok tx, Some account -> workflow tx account
        | Error e, Some account -> [TransactionErrors.InvalidTx e] |> adaptError account
        | _, None -> [TransactionErrors.AccountNotInitialized] |> printableNoAccountResponse mapTxErrorToString
