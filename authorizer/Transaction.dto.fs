module authorizer.TransactionDto

open System

type TransactionInfo =
    { Merchant: string
      Amount: decimal
      Time: DateTime }
