open FsUnit
open Xunit
open FsUnit.Xunit


module Tests =

    [<Fact>]
    let ``Passing test`` () =
        true |> should equal true
