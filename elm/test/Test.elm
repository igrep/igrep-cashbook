import Graphics.Element exposing (Element)

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

main : Element
main = runDisplay <| suite "This is a test"
  [ test "hoge" <| assertEqual 1 1
  , suite "Nested" <|
      [ test "foo" <| assert True
      , test "bar" <| assertEqual "aa" "bb"
      ]
  ]
