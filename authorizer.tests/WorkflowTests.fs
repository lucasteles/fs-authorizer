module Authorizer.WorkflowTests

open NUnit.Framework
open FsUnit
open Authorizer.Tests.Helpers

let interact s = mockConsole Api.start s

[<Test>]
let ``The transaction amount should not exceed available limit`` () =
    let input =
        [ """{ "account": { "activeCard": true, "availableLimit": 100 } }"""
          """{ "transaction": { "merchant": "Burger King", "amount": 20, "time": "2019-02-13T10:00:00.000Z" } }"""
          """{ "transaction": { "merchant": "Habbib's", "amount": 90, "time": "2019-02-13T11:00:00.000Z" } }""" ]

    let output =
        [ """{"account":{"activeCard":true,"availableLimit":100},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":80},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":80},"violations":["insufficient-limit"]}""" ]

    interact input |> should equal output


[<Test>]
let ``Once created, the account should not be updated or recreated`` () =
    let input =
        [ """{ "account": { "activeCard": true, "availableLimit": 100 } }"""
          """{ "account": { "activeCard": true, "availableLimit": 350 } }""" ]

    let output =
        [ """{"account":{"activeCard":true,"availableLimit":100},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":100},"violations":["account-already-initialized"]}""" ]

    interact input |> should equal output


[<Test>]
let ``No transaction should be accepted when the card is not active`` () =
    let input =
        [ """{ "account": { "activeCard": false, "availableLimit": 100 } }"""
          """{ "transaction": { "merchant": "Burger King", "amount": 10, "time": "2019-02-13T10:00:00.000Z" } }""" ]

    let output =
        [ """{"account":{"activeCard":false,"availableLimit":100},"violations":[]}"""
          """{"account":{"activeCard":false,"availableLimit":100},"violations":["card-not-active"]}""" ]

    interact input |> should equal output


[<Test>]
let ``There should not be more than 3 transactions on a 2 minute interval`` () =
    let input =
        [ """{ "account": { "activeCard": true, "availableLimit": 100 } }"""
          """{ "transaction": { "merchant": "Burger King 1", "amount": 10, "time": "2019-02-13T10:00:00.000Z" } }"""
          """{ "transaction": { "merchant": "Burger King 2", "amount": 10, "time": "2019-02-13T10:01:00.000Z" } }"""
          """{ "transaction": { "merchant": "Burger King 3", "amount": 10, "time": "2019-02-13T10:01:10.000Z" } }"""
          """{ "transaction": { "merchant": "Burger King 4", "amount": 10, "time": "2019-02-13T10:01:30.000Z" } }""" ]

    let output =
        [ """{"account":{"activeCard":true,"availableLimit":100},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":90},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":80},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":70},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":70},"violations":["high-frequency-small-interval"]}""" ]

    interact input |> should equal output

[<Test>]
let ``There should not be more than 2 similar transactions (same amount and merchant) in a 2 minutes interval`` () =
    let input =
        [ """{ "account": { "activeCard": true, "availableLimit": 100 } }"""
          """{ "transaction": { "merchant": "Burger King", "amount": 10, "time": "2019-02-13T10:00:00.000Z" } }"""
          """{ "transaction": { "merchant": "Burger King", "amount": 10, "time": "2019-02-13T10:00:00.000Z" } }"""
          """{ "transaction": { "merchant": "Burger King", "amount": 10, "time": "2019-02-13T10:01:30.000Z" } }""" ]

    let output =
        [ """{"account":{"activeCard":true,"availableLimit":100},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":90},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":80},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":80},"violations":["doubled-transaction"]}""" ]

    interact input |> should equal output

[<Test>]
let ``should list more than one violation`` () =
    let input =
        [ """{ "account": { "activeCard": true, "availableLimit": 100 } }"""
          """{ "transaction": { "merchant": "Burger King", "amount": 40, "time": "2019-02-13T10:00:00.000Z" } }"""
          """{ "transaction": { "merchant": "Burger King", "amount": 40, "time": "2019-02-13T10:00:00.000Z" } }"""
          """{ "transaction": { "merchant": "Burger King", "amount": 40, "time": "2019-02-13T10:01:30.000Z" } }""" ]

    let output =
        [ """{"account":{"activeCard":true,"availableLimit":100},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":60},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":20},"violations":[]}"""
          """{"account":{"activeCard":true,"availableLimit":20},"violations":["insufficient-limit","doubled-transaction"]}""" ]

    interact input |> should equal output
