module fs_authorize.Account_dto

open System
open fs_authorize.SimpleTypes
open fs_authorize.Account
open fs_authorize.Transaction

type TransactionDto = { Merchant: string; Amount: int; Time: DateTime  }
type CreateTransaction = TransactionDto -> Transaction

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
                { AvailableLimit = limit
                  Transactions = [] }
        else
            InactiveAccount
                { AvailableLimit = limit }

    limitResult |> Result.map defAccount

let accountToDto account =
    let limit = account |> getCurrentLimit |> int
    let isActive =
        match account with
        | ActiveAccount _ -> true
        | InactiveAccount _ -> false
    { ActiveCard = isActive
      AvailableLimit = limit }
