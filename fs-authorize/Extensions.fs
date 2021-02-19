namespace fs_authorize

[<AutoOpen>]
module Operators =
     let (>>=) r f =
         match r with
         | Ok x -> f x
         | Error e -> Error e

module Result =
     let bimap onSuccess onError xR =
            match xR with
            | Ok x -> onSuccess x
            | Error err -> onError err
     let tap (f: 'a -> unit) (r: Result<'a, 'e>) =
         Result.map (fun x -> f x; x) r

