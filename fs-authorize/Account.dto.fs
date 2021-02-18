module fs_authorize.Account_dto

open System
open fs_authorize.SimpleTypes
open fs_authorize.Account

type AccountDto = { ActiveCard: bool
                    AvailableLimit: int }

type OutputDto =
    | AuthorizeSuccess of {| Account:  AccountDto |}
    | AuthorizeFailure of {| Account: AccountDto; Violations: string list |}
    | NoAccount of {| Violations: string list |}

let unwrapOutput =
    function
    | AuthorizeSuccess o -> box o
    | AuthorizeFailure o -> box o
    | NoAccount o -> box o

let dtoToDomain (accountDto: AccountDto) =
    let limitResult =
        accountDto.AvailableLimit
        |> toMoney
        |> Limit.create

    let defAccount limit =
        if accountDto.ActiveCard then
            ActiveAccount
                { TotalLimit = limit
                  Transactions = [] }
        else
            InactiveAccount
                { TotalLimit = limit }

    limitResult |> Result.map defAccount

let accountToDto account =
    let (Limit limit) = account |> getCurrentAvailableLimit
    let isActive =
        match account with
        | ActiveAccount _ -> true
        | InactiveAccount _ -> false
    { ActiveCard = isActive
      AvailableLimit = int limit }

let mapAccountErrorToString =
    function
    | CreateAccountErrors.AccountAlreadyInitialized -> "account-already-initialized"
    | CreateAccountErrors.InvalidInput _ -> "invalid-input"

let printableAccountErrorResponse mapErrorToString  account violations =
    AuthorizeFailure {| Account = account; Violations = List.map mapErrorToString violations |}

let printableAccountResponse account =
    AuthorizeSuccess {| Account = account |}
