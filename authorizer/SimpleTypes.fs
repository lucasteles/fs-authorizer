module authorizer.SimpleTypes

open System

[<Measure>]
type USD

module Money =
    type Dollar = decimal<USD>
    let inline usd v = v * 1M<USD>
    let zero = 0M<USD>

type NonEmptyString50 = private NonEmptyString50 of string
type Limit = private Limit of Money.Dollar
type Amount = private Amount of Money.Dollar

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


module Amount =
    type Errors = NegativeAmount
    let value (Amount v) = v

    let create v =
        if v < Money.zero then
            Error Errors.NegativeAmount
        else
            v |> Amount |> Ok

module Limit =
    type Errors = ZeroLimit
    let value (Limit v) = v

    let create v =
        if v = Money.zero then
            Error Errors.ZeroLimit
        else
            Limit v |> Ok

    let sum (Limit a) (Limit b) = a + b |> Limit
    let subtract n (Limit a) = a - n |> Limit

let (|NonEmptyString50|) = NonEmptyString50.value
let (|Limit|) = Limit.value
let (|Amount|) = Amount.value
