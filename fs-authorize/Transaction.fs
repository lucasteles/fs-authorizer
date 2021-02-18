namespace fs_authorize.Transaction

open System
open fs_authorize.SimpleTypes

type Transaction =
    { Merchant: NonEmptyString50; Amount: Amount; Time: DateTime  }

type TransactionErrors =
    | InsufficientLimit
    | CardIsNotActive
    | HighFrequencySmallInterval
    | DoubledTransaction
    | InvalidMerchant of NonEmptyString50.Errors

