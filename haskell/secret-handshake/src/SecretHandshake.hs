module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n
  | testBit n 4 = reverse . handshake . clearBit n $ 4
  | otherwise = fmap snd . filter fst $ zip
    (testBit n <$> [0..3])
    ["wink", "double blink", "close your eyes", "jump"]
