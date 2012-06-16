csrshift :: Int -> [(Char,Char)]
csrshift 0 = zip (['A','B'..'Z'] ++ ['a','b'..'z']) 
                 (['A','B'..'Z'] ++ ['a','b'..'z'])
csrshift n = zip (['A','B'..'Z'] ++ ['a','b'..'z']) 
                 (drop n ['A','B'..'Z'] ++ take n ['A','B'..'Z']
                                        ++ drop n ['a','b'..'z']
                                        ++ take n ['a','b'..'z'])


caesar :: String -> Int -> String
caesar [] _  = []
caesar  x 0  = x
caesar original@(x:xs) n
    | not (x `elem` (['A','B'..'Z'] ++ ['a','b'..'z'])) = x : caesar xs n
    | n < 0           = caesar original (n `mod` 26)
    | otherwise       = shift x list : caesar xs (n `mod` 26)
    where list        = csrshift (n `mod` 26)
          shift x ls  = head [ b | (a,b) <- ls, a == x]
