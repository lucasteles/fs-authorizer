module fs_authorize.CreateAccount

open fs_authorize
open fs_authorize.Account_dto
open fs_authorize.Account

type ValidateAccount = Account option  -> AccountDto -> Result<Account, CreateAccountErrors>
type CreateAccountWorkflow = (unit -> Account option) -> (Account -> unit) -> AccountDto -> OutputDto

let validateAccount: ValidateAccount =
    fun currentAccount dto ->
        match currentAccount with
        | Some _ -> Error CreateAccountErrors.AccountAlreadyInitialized
        | None ->
            dtoToDomain dto
            |> Result.bimap Ok (CreateAccountErrors.InvalidInput >> Error)

let createAccountWorkflow: CreateAccountWorkflow =
    fun getAccount createAccount dto ->
        let currentAccount = getAccount ()

        let parseError = Option.map accountToDto currentAccount
                         |> function
                            | Some a -> printableAccountErrorResponse mapAccountErrorToString a
                            | None -> fun _ -> NoAccount {| Violations = ["no-account"] |}

        validateAccount currentAccount dto
        |> Result.tap createAccount
        |> Result.map accountToDto
        |> Result.mapError List.singleton
        |> Result.bimap printableAccountResponse parseError



