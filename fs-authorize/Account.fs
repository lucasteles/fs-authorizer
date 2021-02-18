module fs_authorize.Account

open fs_authorize.SimpleTypes
open fs_authorize.Transaction

type InactiveAccount =
    { TotalLimit: Limit }

type ActiveAccount =
    { TotalLimit: Limit
      Transactions: Transaction list }

type Account =
    | ActiveAccount of ActiveAccount
    | InactiveAccount of InactiveAccount

type CreateAccountErrors =
    | AccountAlreadyInitialized
    | InvalidInput of Limit.Errors

type GetCurrentAvailableLimit = Account -> Limit

type AddTransactionToAccount = Transaction -> ActiveAccount  -> ActiveAccount

let getCurrentAvailableLimit: GetCurrentAvailableLimit =
    function
    | InactiveAccount { TotalLimit = v } -> v
    | ActiveAccount a ->
        let transactionTotal =
            a.Transactions
            |> List.map (fun { Amount = (Amount v) } -> v)
            |> List.sum
        a.TotalLimit |> Limit.subtract transactionTotal

type ActiveAccount with
    member this.AvailableLimit = this |> ActiveAccount |> getCurrentAvailableLimit

let addTransactionToAccount : AddTransactionToAccount =
    fun tx account ->
        { account with Transactions = tx :: account.Transactions }
