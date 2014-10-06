module Test.Data.Argonaut where

  import Data.Argonaut
  import Data.Argonaut.Core (Json())
  import Data.Either
  import Data.Tuple
  import Data.Array
  import Debug.Trace
  import qualified Data.StrMap as M

  import Test.QuickCheck
  import Test.QuickCheck.LCG

  genJNull :: Gen Json
  genJNull = pure jsonNull

  genJBool :: Gen Json
  genJBool = fromBoolean <$> arbitrary

  genJNumber :: Gen Json
  genJNumber = fromNumber <$> arbitrary

  genJString :: Gen Json
  genJString = fromString <$> arbitrary

  genJArray :: Size -> Gen Json
  genJArray sz = fromArray <$> vectorOf sz (genJson $ sz - 1)

  genJObject :: Size -> Gen Json
  genJObject sz = do
    v <- vectorOf sz (genJson $ sz - 1)
    k <- vectorOf (length v) (arbitrary :: Gen AlphaNumString)
    return $  let 
                f (AlphaNumString s) = s
                k' = f <$> k
              in fromObject <<< M.fromList $ zipWith Tuple k' v

  genJson :: Size -> Gen Json
  genJson 0 = oneOf genJNull [genJBool, genJNumber, genJString]
  genJson n = frequency (Tuple 1 genJNull) rest where
    rest = [Tuple 2 genJBool,
            Tuple 2 genJNumber,
            Tuple 3 genJString,
            Tuple 1 (genJArray n),
            Tuple 1 (genJObject n)]

  instance arbitraryJson :: Arbitrary Json where
    arbitrary = sized genJson

  prop_encode_then_decode :: Json -> Boolean
  prop_encode_then_decode json =
    Right json == (decodeJson $ encodeJson $ json)

  prop_decode_then_encode :: Json -> Boolean
  prop_decode_then_encode json =
    let decoded = (decodeJson json) :: Either String Json in
    Right json == (decoded >>= (encodeJson >>> pure))

  main = do
    trace "Sample of JSON"
    showSample (genJson 10)

    trace "Testing that any JSON can be encoded and then decoded"
    quickCheck prop_encode_then_decode

    trace "Testing that any JSON can be decoded and then encoded"
    quickCheck prop_decode_then_encode
