module authorizer.CreateAccount

open authorizer
open authorizer.AccountDto
open authorizer.SimpleTypes

type CreateAccountError =
    | AccountAlreadyInitialized
    | NoAccount
    | InvalidInput of Limit.Errors

type ValidateAccount = Account option -> AccountInfo -> Result<Account, CreateAccountError>
type CreateAccountWorkflow = (unit -> Account option) -> (Account -> unit) -> AccountInfo -> AuthorizedAccount

let validateAccount: ValidateAccount =
    fun currentAccount dto ->
        match currentAccount with
        | Some _ -> Error CreateAccountError.AccountAlreadyInitialized
        | None ->
            accountToDomain dto
            |> Result.bimap Ok (CreateAccountError.InvalidInput >> Error)

let renderAccountError =
    function
    | CreateAccountError.AccountAlreadyInitialized -> "account-already-initialized"
    | CreateAccountError.InvalidInput _ -> "invalid-input"
    | CreateAccountError.NoAccount -> "no-account"

let private adaptError maybeAccount =
    maybeAccount
    |> Option.map fromAccount
    |> function
        | Some acc -> acc |> authorizationFailure renderAccountError
        | None ->
            fun _ ->
                CreateAccountError.NoAccount
                |> List.singleton
                |> noAccount renderAccountError

let createAccount: CreateAccountWorkflow =
    fun getAccount insertAccount dto ->
        let currentAccount = getAccount ()

        dto
        |> validateAccount currentAccount
        |> Result.tap insertAccount
        |> Result.map fromAccount
        |> Result.mapErrorToList
        |> Result.bimap authorized (adaptError currentAccount)
