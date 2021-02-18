//open Hopac
//open Logary.Configuration
//open Logary.Adapters.Facade
//open Logary.Targets
open Expecto
open FsUnit

let test' = Swensen.Unquote.Assertions.test

[<Tests>]

let ``introduction tests`` =
  testList "some tests" [
    test "A simple test" {
      let subject = "Hello World"
      Expect.equal subject "Hello World" "The strings should equal"
    }

    testCase "One more test" <| fun _ ->
      let subject = "Hello World"
      subject |> should equal "Hello World"
  ]


[<EntryPoint>]
let main argv =
    (*
  let logary =
    Config.create "fs-authorize.tests" "localhost"
    |> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
    |> Config.processing (Events.events |> Events.sink ["console";])
    |> Config.build
    |> run
  LogaryFacadeAdapter.initialise<Expecto.Logging.Logger> logary *)
  runTestsInAssemblyWithCLIArgs [] argv