module authorizer.AccountDto

open authorizer.SimpleTypes

type AccountInfo =
    { ActiveCard: bool
      AvailableLimit: decimal }

type AuthorizedAccount =
    | AuthorizeSuccess of
        {| Account: AccountInfo
           Violations: string list |}
    | AuthorizeFailure of
        {| Account: AccountInfo
           Violations: string list |}
    | NoAccount of {| Violations: string list |}


let accountToDomain (accountDto: AccountInfo) =
    let limitResult = accountDto.AvailableLimit |> Money.usd |> Limit.create

    let defAccount limit =
        if accountDto.ActiveCard then
            Active
                { TotalLimit = limit
                  Transactions = [] }
        else
            Inactive { TotalLimit = limit }

    limitResult |> Result.map defAccount

let fromAccount account =
    let (Limit limit) = account |> Account.currentAvailableLimit

    let isActive =
        match account with
        | Active _ -> true
        | Inactive _ -> false

    { ActiveCard = isActive
      AvailableLimit = decimal limit }


let authorizationFailure mapErrorToString account violations =
    AuthorizeFailure
        {| Account = account
           Violations = List.map mapErrorToString violations |}

let noAccount mapErrorToString violations =
    NoAccount {| Violations = List.map mapErrorToString violations |}

let authorized account =
    AuthorizeSuccess {| Account = account; Violations = [] |}
