namespace fs_authorize

[<AutoOpen>]
module Operators =
     let (>>=) r f = Result.bind f r

module Result =
     let bimap onSuccess onError xR =
            match xR with
            | Ok x -> onSuccess x
            | Error err -> onError err
     let tap (f: 'a -> unit) (r: Result<'a, 'e>) =
         Result.map (fun x -> f x; x) r

