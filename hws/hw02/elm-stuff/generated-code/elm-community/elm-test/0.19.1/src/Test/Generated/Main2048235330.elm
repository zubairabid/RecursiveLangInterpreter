module Test.Generated.Main2048235330 exposing (main)

import SampleTests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "SampleTests" [SampleTests.test_product,
    SampleTests.test_countoccur,
    SampleTests.test_invert,
    SampleTests.test_repeat,
    SampleTests.test_merge,
    SampleTests.test_every,
    SampleTests.test_preorder] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 373255299326450, processes = 4, paths = ["/home/zubair/Documents/Work/Acads/Projects/popl-monsoon-2020/hws/hw02/tests/SampleTests.elm"]}