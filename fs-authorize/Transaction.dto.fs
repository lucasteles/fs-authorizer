module fs_authorize.Transaction_dto

open System
open fs_authorize.SimpleTypes
open fs_authorize.Transaction

type TransactionDto =
    { Merchant: string
      Amount: int
      Time: DateTime }

type CreateTransaction = TransactionDto -> Transaction

let dtoToTx dto =
    let merchant = NonEmptyString50.create dto.Merchant
    let amount = dto.Amount |> toMoney |> Amount

    let buildTx merchantName =
        { Transaction.Amount = amount
          Time = dto.Time
          Merchant = merchantName }

    merchant |> Result.map buildTx

let mapTxErrorToString =
    function
    | TransactionErrors.InsufficientLimit -> "insufficient-limit"
    | TransactionErrors.CardIsNotActive -> "card-not-active"
    | TransactionErrors.HighFrequencySmallInterval -> "high-frequency-small-interval"
    | TransactionErrors.DoubledTransaction -> "doubled-transaction"
    | TransactionErrors.InvalidMerchant _ -> "invalid-merchant"
