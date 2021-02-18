namespace fs_authorize.Repositories

open fs_authorize.Account


type AccountRepository () =
    let mutable account: Account option = None

    member this.Get() = account
    member this.Create(newAccount : Account) =
        account <- Some newAccount
    member this.Update(newAccount : Account) =
        account <- Some newAccount
