namespace authorizer

module Result =
    let bimap onSuccess onError xR =
        match xR with
        | Ok x -> onSuccess x
        | Error err -> onError err

    let tap (f: 'a -> unit) (r: Result<'a, 'e>) =
        match r with
        | Ok x -> f x
        | Error _ -> ()

        r

    let mapErrorToList r = r |> Result.mapError List.singleton

    let applyAggregate f result =
        match f, result with
        | Ok f, Ok x -> f x |> Ok
        | Error errs1, Error errs2 -> (errs1 @ errs2) |> Error
        | Ok _, Error errs
        | Error errs, Ok _ -> errs |> Error

[<AutoOpen>]
module Operators =
    let (>>=) r f = Result.bind f r
    let (<!>) = Result.map
    let (<*>) = Result.applyAggregate
