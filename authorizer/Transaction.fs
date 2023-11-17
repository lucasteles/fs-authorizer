namespace authorizer

open System
open authorizer.SimpleTypes

type Transaction =
    { Merchant: NonEmptyString50
      Amount: Amount
      Time: DateTime }

module Transaction =

    let create time merchantName amount =
        { Amount = amount
          Time = time
          Merchant = merchantName }

    let equivalent (t1: Transaction) (t2: Transaction) =
        (t1.Merchant, t1.Amount) = (t2.Merchant, t2.Amount)
