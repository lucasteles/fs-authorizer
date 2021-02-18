module fs_authorize.Account

open fs_authorize.SimpleTypes
open fs_authorize.Transaction

type InactiveAccount =
    { AvailableLimit: Limit }

type ActiveAccount =
    { AvailableLimit: Limit
      Transactions: Transaction list }

type Account =
    | ActiveAccount of ActiveAccount
    | InactiveAccount of InactiveAccount

type CreateAccountErrors =
    | AccountAlreadyInitialized
    | InvalidInput of Limit.Errors

type GetCurrentLimit = Account -> int<money>

let getCurrentLimit: GetCurrentLimit =
    function
    | InactiveAccount { AvailableLimit = (Limit v) } -> v
    | ActiveAccount a ->
        let (Limit currLimit) = a.AvailableLimit
        let transactionTotal =
            a.Transactions
            |> List.map (fun { Amount = (Amount v) } -> v)
            |> List.sum
        currLimit - transactionTotal


