namespace Authorizer

open System

[<Measure>]
type USD

module Money =
    type Dollar = decimal<USD>
    let inline usd v = v * 1M<USD>
    let zero = 0M<USD>

type NonEmptyString50 = private NonEmptyString50 of string

module NonEmptyString50 =
    type Errors =
        | EmptyString
        | GreaterThan50Characters

    let value (NonEmptyString50 v) = v

    let create str =
        if String.IsNullOrWhiteSpace str then
            Error Errors.EmptyString
        elif String.length str > 50 then
            Error Errors.GreaterThan50Characters
        else
            str |> NonEmptyString50 |> Ok

[<Struct>]
type Amount =
    private
    | Amount of Money.Dollar

    static member (+)(Amount a, Amount b) = Amount(a + b)
    static member val Zero = Amount Money.zero

module Amount =
    type Errors = NegativeAmount
    let value (Amount v) = v

    let sum (Amount a) (Amount b) = a + b |> Amount

    let create v =
        if v < Money.zero then
            Error Errors.NegativeAmount
        else
            v |> Amount |> Ok

[<Struct>]
type Limit = private Limit of Money.Dollar

module Limit =
    type Errors = ZeroLimit
    let value (Limit v) = v

    let create v =
        if v = Money.zero then
            Error Errors.ZeroLimit
        else
            Limit v |> Ok

    let subtract n (Limit a) = a - n |> Limit

[<AutoOpen>]
module ValueTypes =
    let (|NonEmptyString50|) = NonEmptyString50.value
    let (|Limit|) = Limit.value
    let (|Amount|) = Amount.value
