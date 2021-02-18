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

let mapErrorToString =
    function
    | CreateAccountErrors.AccountAlreadyInitialized -> "account-already-initialized"
    | CreateAccountErrors.InvalidInput _ -> "invalid-input"

let printableAccountErrorResponse account violations =
    match account with
    | Some account ->
        AuthorizeFailure {| Account = account; Violations = List.map mapErrorToString violations |}
    | None ->
        NoAccount {| Violations = ["no-account"] |}

let printableAccountResponse account =
    AuthorizeSuccess {| Account = account |}

let createAccountWorkflow: CreateAccountWorkflow =
    fun getAccount createAccount dto ->
        let currentAccount = getAccount ()
        let parseError = Option.map accountToDto currentAccount
                         |> printableAccountErrorResponse

        validateAccount currentAccount dto
        |> Result.tap createAccount
        |> Result.map accountToDto
        |> Result.mapError List.singleton
        |> Result.bimap printableAccountResponse parseError



