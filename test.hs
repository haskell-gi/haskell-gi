
import Control.Monad (when)
import Data.Algorithm.Diff
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Text.Show.Pretty

import qualified Data.Map as M
import qualified Data.Sequence as S

import GI.API
import GI.Code
import GI.CodeGen
import GI.Internal.ArgInfo
import GI.Type
import GI.Value

testConfig = Config {
  imports = [],
  prefixes = M.fromList [("test", "test")],
  names = M.empty,
  input = M.empty }

a @?== b = do
  when (a /= b) $ assertFailure diff

   where
     diff = diffStr (getDiff (lines $ ppShow b) (lines $ ppShow a)) ""
     diffStr xs _ = '\n' : concatMap
                    (\(di, s) -> diChar di : ' ' : s ++ "\n") xs

     diChar B = ' '
     diChar F = '-'
     diChar S = '+'

testCodeGen name api code = (codeToList $ runCodeGen' testConfig $ genCode M.empty name api) @?== code

testConstant = testCase "constant" $
  testCodeGen (Name "test" "foo") (APIConst (Constant (VInt8 42)))
  [ Line "-- constant foo"
  , Line "test_foo :: Int8"
  , Line "test_foo = 42"
  ]

testFunction = testCase "function" $
  testCodeGen (Name "test" "foo_bar") (APIFunction $ Function {
      fnSymbol = "foo_bar",
      fnFlags = [],
      fnCallable =
        Callable {
           returnType = TBasicType TVoid,
           returnMayBeNull = False,
           returnTransfer = TransferNothing,
           returnAttributes = [],
           args = [
             Arg {
                argName = "x",
                argType = TBasicType TInt8,
                direction = DirectionIn,
                mayBeNull = False,
                transfer = TransferNothing,
                scope = ScopeTypeInvalid }]}})
  [ Line "-- function foo_bar"
  , ForeignImport $ Sequence $ S.fromList
      [ Line "foreign import ccall \"foo_bar\" foo_bar :: "
      , Indent $ Sequence $ S.fromList
        [ Line "Int8 ->                                 -- x"
        , Line "IO (Ptr ())"
        ]
      ]
  , Group $ Sequence $ S.fromList
    [ Line "testFooBar ::"
    , Indent $ Sequence $ S.fromList
      [ Line "Int8 ->                                 -- x"
      , Line "IO (Ptr ())"
      ]
    , Line "testFooBar x = do"
    , Indent $ Sequence $ S.fromList
      [ Line "result <- foo_bar x"
      , Line "return result"
      ]
    ]]

testEnum = testCase "enum" $ testCodeGen name api expected
  where
    name = Name "test" "enum"
    api = APIEnum $ Enumeration [("foo", 1), ("bar", 2)]
    expected =
      [ Line "-- enum enum"
      , Group $ Sequence $ S.fromList
          [ Line "data TestEnum = "
          , Indent $ Sequence $ S.fromList
            [ Line "  TestEnumFoo"
            , Line "| TestEnumBar"
            ]
          ]
      , Group $ Sequence $ S.fromList
          [ Line "instance Enum TestEnum where"
          , Indent $ Sequence $ S.fromList
            [Line "fromEnum TestEnumFoo = 1",
             Line "fromEnum TestEnumBar = 2"]
          , Line ""
          , Indent $ Sequence $ S.fromList
            [Line "toEnum 1 = TestEnumFoo"
            , Line "toEnum 2 = TestEnumBar"]
          ]]

main = defaultMain [
  testConstant,
  testFunction,
  testEnum]
