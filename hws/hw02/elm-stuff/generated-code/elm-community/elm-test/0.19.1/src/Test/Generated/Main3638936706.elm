module Test.Generated.Main3638936706 exposing (main)

import SampleTests

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "SampleTests" [SampleTests.test_invert,
    SampleTests.test_repeat] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 309178088357683, processes = 4, paths = ["/home/zubair/Documents/Work/Acads/Projects/popl-monsoon-2020/hws/hw02/tests/SampleTests.elm"]}