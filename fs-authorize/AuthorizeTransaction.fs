module fs_authorize.AuthorizeTransaction

open fs_authorize.Account
open fs_authorize.Transaction


type ProcessTransaction =
    ActiveAccount -> Transaction -> Result<ActiveAccount, TransactionErrors>

type TransactionAmountShouldNotExceedAvailableLimit = ProcessTransaction
type NoTransactionShouldBeAcceptedWhenTheCardIsNotActive = ProcessTransaction
type ThereShouldNotBeMoreThan3TransactionsOnA2MinuteInterval = ProcessTransaction
type ThereShouldNoBeMoreThan2SimilarTransactionsInA2MinutesInterval = ProcessTransaction

let transactionAmountShouldNotExceedAvailableLimit: TransactionAmountShouldNotExceedAvailableLimit =
    fun activeAccount transaction ->
        // so exemplo
        Error TransactionErrors.InsufficientLimit
