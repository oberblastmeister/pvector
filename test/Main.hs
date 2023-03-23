import qualified Properties
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "main"
      [ Properties.tests
      ]
