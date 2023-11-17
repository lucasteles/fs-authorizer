namespace authorizer

open authorizer.SimpleTypes

type InactiveAccount = { TotalLimit: Limit }

type ActiveAccount =
    { TotalLimit: Limit
      Transactions: Transaction list }

type Account =
    | Active of ActiveAccount
    | Inactive of InactiveAccount

module ActiveAccount =
    let currentAvailableLimit a =
        let transactionTotal =
            a.Transactions |> List.map (fun { Amount = (Amount v) } -> v) |> List.sum

        a.TotalLimit |> Limit.subtract transactionTotal

module Account =
    let currentAvailableLimit =
        function
        | Inactive { TotalLimit = available } -> available
        | Active account -> ActiveAccount.currentAvailableLimit account

    let addTransaction tx account =
        { account with Transactions = tx :: account.Transactions }
