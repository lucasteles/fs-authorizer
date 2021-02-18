module fs_authorize.SimpleTypes

open System
[<Measure>] type money

type NonEmptyString50 = private NonEmptyString50 of string
type Limit = private Limit of int<money>
type Amount = Amount of int<money>

let toMoney v = v * 1<money>

module NonEmptyString50  =
    type Errors =  EmptyString | GreaterThan50Characters
    let get (NonEmptyString50 v) = v
    let create str =
        if (String.IsNullOrWhiteSpace(str)) then
            Error Errors.EmptyString
        elif (String.length str > 50) then
            Error Errors.GreaterThan50Characters
        else
            str |> NonEmptyString50 |> Ok
let (|NonEmptyString50|) = NonEmptyString50.get

module Limit  =
    type Errors = ZeroLimit
    let get (Limit v) = v
    let create v =
        if (v = 0<money>) then
            Error Errors.ZeroLimit
        else
            Limit v |> Ok

    let sum (Limit a) (Limit b) = a + b |> Limit
    let subtract n (Limit a) = a - n |> Limit

let (|Limit|) = Limit.get
