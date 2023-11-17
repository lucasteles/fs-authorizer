namespace authorizer.Repositories

open authorizer


type AccountRepository() =
    let mutable account: Account option = None

    member this.Get() = account
    member this.Create(newAccount: Account) = account <- Some newAccount
    member this.Update(newAccount: Account) = account <- Some newAccount
