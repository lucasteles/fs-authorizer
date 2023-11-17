module authorizer.CreateTransaction

open authorizer.SimpleTypes
open authorizer.TransactionDto


type CreateTransaction = TransactionInfo -> Transaction

type CreateTransactionError =
    | MerchantName of NonEmptyString50.Errors
    | Amount of Amount.Errors

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
        |> Result.mapError CreateTransactionError.Amount
        |> Result.mapErrorToList

    Transaction.create dto.Time <!> merchant <*> amount
