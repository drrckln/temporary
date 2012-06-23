import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0


int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2: int2bin (n `div` 2)


make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

paritybit :: [Bit] -> [Bit]
paritybit bits | odd . sum . filter (==1) $ bits = bits ++ [1]
               | otherwise                       = bits ++ [0]




encode :: String -> [Bit]
encode = concat . map (paritybit . make8 . int2bin . ord)



chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

paritycheck :: [[Bit]] -> [[Bit]]
paritycheck [] = []
paritycheck (x:xs) | length x /= 9  = error "incomplete byte"
                   | not $ check x  = error "parity bit incorrect"
                   | otherwise      = take 8 x : paritycheck xs
                   where oddit byte = odd . sum . filter (==1) . take 8 $ byte
                         check byte = oddit byte == (last byte == 1)
                                    
                                 
decode :: [Bit] -> String
decode = map (chr . bin2int) . paritycheck . chop9


transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel bits = tail bits


