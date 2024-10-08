namespace Authorizer.Repositories

open Authorizer

type AccountRepository() =
    let mutable account: Account option = None

    interface IAccountRepository with
        member this.Get() = account
        member this.Create(newAccount: Account) = account <- Some newAccount
        member this.Update(newAccount: Account) = account <- Some newAccount
