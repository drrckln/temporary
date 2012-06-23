type Parser a = String -> [(a, String)]

--the parser return v always succeeds with the result value v, without consuming any of the input string
return' :: a -> Parser a
return' v = \inp -> [(v,inp)]

--the dual parser failure always fails, regardless of the contents of the input string
failure :: Parser a
failure = \inp -> []

--Our final basic parser is item, which fails if the input string is empty, and succeeds with the first character as the result value otherwise
item :: Parser Char
item = \inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x,xs)]


parse :: Parser a -> String -> [(a,String)]
parse p inp = p inp


--Sequencing: In practice, however, it turns out to be more convenient to combine the sequencing of parsers with the processing of their result values, by means of a sequencing operator >= (read as “then”) defined as follows:

thenop :: Parser a -> (a -> Parser b) -> Parser c
p `thenop` f = \inp -> case parse p inp of
                             [] -> []
                             [(v,out)] -> parse (f v) out

{- A typical parser built using >= has the following structure: (Haskell's special syntax)

do v1 <- p1 
   v2 <- p2
   .
   .
   vn <- pn 
   return (f v1 v2 ... vn)

For example, a parser that consumes three characters, discards the second, and returns the first and third as a pair can now be defined as follows:

p :: Parser (Char,Char)
p = do x <- item
       item 
       y <- item
       return (x,y)

do allows you to combine a bunch of actions into one action, so we're building a parser out of parsers here.
-}

{-
--Another natural way of combining two parsers is to apply the first parser to the input string, and if this fails to apply the second instead. Such a choice operator +++ (read as “or else”) can be defined as follows:

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp case parse p inp of
                    [] -> parse q inp
                    [(v,out)] -> [(v,out)]

--First of all, we define a parser sat p for single characters that satisfy the predicate p:

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure


digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)


String :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-}
