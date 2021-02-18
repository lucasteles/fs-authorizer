module fs_authorize.CreateAccount

open fs_authorize
open fs_authorize.Account_dto
open fs_authorize.Account
open fs_authorize.Transaction

type ValidateAccount = Account option  -> AccountDto -> Result<Account, CreateAccountErrors>
type CreateAccountWorkflow = (unit -> Account option) -> (Account -> unit) -> AccountDto -> OutputDto

let validateAccount: ValidateAccount =
    fun currentAccount dto ->
        match currentAccount with
        | Some _ -> Error CreateAccountErrors.AccountAlreadyInitialized
        | None ->
            dtoToDomain dto
            |> Result.bimap Ok (CreateAccountErrors.InvalidInput >> Error)

let private adaptError maybeAccount =
                 maybeAccount
                 |> Option.map accountToDto
                 |> function
                    | Some a -> printableAccountErrorResponse mapAccountErrorToString a
                    | None -> fun _ -> printableNoAccountResponse mapAccountErrorToString [CreateAccountErrors.NoAccount]

let createAccountWorkflow: CreateAccountWorkflow =
    fun getAccount createAccount dto ->
        let currentAccount = getAccount ()

        dto
        |> validateAccount currentAccount
        |> Result.tap createAccount
        |> Result.map accountToDto
        |> Result.mapError List.singleton
        |> Result.bimap printableAccountResponse (adaptError currentAccount)



