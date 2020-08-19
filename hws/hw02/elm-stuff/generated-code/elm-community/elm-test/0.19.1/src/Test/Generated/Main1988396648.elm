module Test.Generated.Main1988396648 exposing (main)

import Example

import Test.Reporter.Reporter exposing (Report(..))
import Console.Text exposing (UseColor(..))
import Test.Runner.Node
import Test

main : Test.Runner.Node.TestProgram
main =
    [     Test.describe "Example" [Example.suite] ]
        |> Test.concat
        |> Test.Runner.Node.run { runs = Nothing, report = (ConsoleReport UseColor), seed = 83814828678873, processes = 4, paths = ["/home/zubair/Documents/Work/Acads/Projects/popl-monsoon-2020/hws/hw02/tests/Example.elm"]}