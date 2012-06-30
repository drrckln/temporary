goto :: Pos -> IO ()
goto (x,y) =
    putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs


seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (a:as) = do a
                 seqn as

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"



-- set the board size
width :: Int
width = 60

height :: Int
height = 45


-- type synonyms
type Pos = (Int, Int)
type Board = [Pos]


-- an example starting case 
glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]


-- display cells
showcells :: Board -> IO ()
showcells b = seqn [writeat p "o" | p <- b]



-- checks for life by matching position against board of live positions
isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)


-- creates a list of neighbors, uses tuples for positions
neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1),(x,y-1),
                          (x+1,y-1),(x-1,y),
                          (x+1,y),(x-1,y+1),
                          (x,y+1),(x+1,y+1)]

-- wraps around (torus)
wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)


-- calculates the number of live neighbors for one position
liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

-- checks if a position survived
survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbs b p `elem` [2,3]]

-- given a board, calculates a list of the newly 'born' cells
births :: Board -> [Pos]
births b = [p | p <- rmdups . concat $ map neighbs b, isEmpty b p, liveneighbs b p == 3]


-- removes duplicates
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x: rmdups (filter (/=x) xs)

-- creates list of next generation's live positions (iterates by 1)
nextgen :: Board -> Board
nextgen b = survivors b ++ births b

-- dead 
dead :: Board -> [Pos]
dead b = [d| d <- b, not (d `elem` (survivors b))]

-- delete cells
delcells :: Board -> IO ()
delcells b = seqn [writeat p " " | p <- dead b]


-- actually executes the program
life :: Board -> IO ()
life b = do showcells b
            wait 10000
            delcells b
            life (nextgen b)

-- time stepper
wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]

-- starts by clearing screen
start :: Board -> IO ()
start b = do cls
             life b


-- editor
addin :: Pos -> Board -> Board
addin p b = p : b

remout :: Pos -> Board -> Board
remout p b = filter (/= p) b

editor :: Board -> IO Board
editor b = do cls
              edit (0,0) b


edit :: Pos -> Board -> IO Board
edit p b = do showcells b
              goto p
              c <- getChar
              goto p
              putChar ' '
              figure p b c


figure :: Pos -> Board -> Char -> IO Board
figure p@(x,y) b c 
    | c == 'h'  = do edit (wrap (x-1,y)) b
    | c == 'j'  = do edit (wrap (x,y+1)) b
    | c == 'k'  = do edit (wrap (x,y-1)) b
    | c == 'l'  = do edit (wrap (x+1,y)) b
    | c == ' '  = do toggle p b
    | c == 's'  = do return b
    | otherwise = do error "invalid"
                     edit p b

toggle :: Pos -> Board -> IO Board
toggle p b
    | p `elem` b = do edit p (remout p b) 
    | otherwise  = do edit p (addin p b)
    

