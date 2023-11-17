module authorizer.AuthorizeTransaction

open System
open authorizer.SimpleTypes

type AuthorizationError =
    | InsufficientLimit
    | CardIsNotActive
    | HighFrequencySmallInterval
    | DoubledTransaction
    | AccountNotInitialized
    | InvalidTx of CreateTransaction.CreateTransactionError

type InputTransaction = TransactionDto.TransactionInfo
type OutputAccount = AccountDto.AuthorizedAccount

type UpdateAccount = Account -> unit
type ReadAccount = unit -> Account option

type AuthorizeTransactionWorkflow = UpdateAccount -> ReadAccount -> InputTransaction -> OutputAccount

type NoTransactionShouldBeAcceptedWhenTheCardIsNotActive = Account -> Result<ActiveAccount, AuthorizationError>

type ThereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval = DateTime -> ActiveAccount -> AuthorizationError option

type ThereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval =
    DateTime -> Transaction -> ActiveAccount -> AuthorizationError option

type TransactionAmountShouldNotExceedAvailableLimit = Transaction -> ActiveAccount -> AuthorizationError option


let renderAuthorizationError =
    function
    | AuthorizationError.InsufficientLimit -> "insufficient-limit"
    | AuthorizationError.CardIsNotActive -> "card-not-active"
    | AuthorizationError.HighFrequencySmallInterval -> "high-frequency-small-interval"
    | AuthorizationError.DoubledTransaction -> "doubled-transaction"
    | AuthorizationError.InvalidTx _ -> "invalid-merchant"
    | AuthorizationError.AccountNotInitialized -> "account-not-initialized"

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
    fun now activeAccount ->
        let maxTransactionIn2Minutes = 3
        let twoMinutesEarlier = now - (TimeSpan.FromMinutes 2.)

        let txCount =
            activeAccount.Transactions
            |> List.filter (fun t -> t.Time > twoMinutesEarlier)
            |> List.length

        if txCount >= maxTransactionIn2Minutes then
            Some AuthorizationError.HighFrequencySmallInterval
        else
            None

let thereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval: ThereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval =
    fun now transaction activeAccount ->
        let maxTransactionIn2Minutes = 2
        let twoMinutesEarlier = now - (TimeSpan.FromMinutes 2.)

        let txCount =
            activeAccount.Transactions
            |> List.filter (fun t -> t.Time > twoMinutesEarlier)
            |> List.filter (Transaction.equivalent transaction)
            |> List.length

        if txCount >= maxTransactionIn2Minutes then
            Some AuthorizationError.DoubledTransaction
        else
            None

let private adaptError account =
    account
    |> AccountDto.fromAccount
    |> AccountDto.authorizationFailure renderAuthorizationError

let availableValidators tx =
    [ transactionAmountShouldNotExceedAvailableLimit tx
      thereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval tx.Time
      thereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval tx.Time tx ]

let validateTransaction tx account =
    account
    |> noTransactionShouldBeAcceptedWhenTheCardIsNotActive
    |> Result.mapErrorToList
    >>= (fun activeAccount ->
        availableValidators tx
        |> List.choose (fun f -> f activeAccount)
        |> function
            | [] -> Ok activeAccount
            | errs -> Error errs)

let tryAddTransaction tx account =
    validateTransaction tx account |> Result.map (Account.addTransaction tx)

let accountNotInitializedResult =
    AuthorizationError.AccountNotInitialized
    |> List.singleton
    |> AccountDto.noAccount renderAuthorizationError

let invalidTxResult account =
    List.map AuthorizationError.InvalidTx >> adaptError account

let updateAccountTx updateFn account tx =
    account
    |> tryAddTransaction tx
    |> Result.map Active
    |> Result.tap updateFn
    |> Result.map AccountDto.fromAccount
    |> Result.bimap AccountDto.authorized (adaptError account)

let authorizeTransaction: AuthorizeTransactionWorkflow =
    fun updateAccount getAccount dto ->
        match CreateTransaction.fromDto dto, getAccount () with
        | _, None -> accountNotInitializedResult
        | Error txErrors, Some account -> invalidTxResult account txErrors
        | Ok tx, Some account -> updateAccountTx updateAccount account tx
