module fs_authorize.tests.Helpers

open System
open System.IO

let mockConsole fn inputLines   =
    let input = String.concat "\n" inputLines
    use reader  = new StringReader(input)
    use writer = new StringWriter()
    Console.SetIn(reader)
    Console.SetOut(writer)
    fn() |> ignore
    writer.ToString().Split("\n")
    |> Seq.filter (String.IsNullOrWhiteSpace >> not)
    |> List.ofSeq
