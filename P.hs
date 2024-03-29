module P where

import Data.List
import Data.Char
import FPH
import Lexicon

data ParseTree a b =  Ep | Leaf a | Branch b [ParseTree a b]
                   deriving Eq

instance (Show a, Show b) => Show (ParseTree a b) where
  show Ep            = "[]"
  show (Leaf t)      = show t
  show (Branch l ts) = "[." ++ show l  ++ " "
                            ++ show ts ++ "]"

snowwhite = Branch "S"
            [Branch "NP" [Leaf "Snow White"],
             Branch "VP" [Branch "TV" [Leaf "love"],
                          Branch "NP" [Leaf "the dwarfs"]]]

type Pos = [Int]

pos ::  ParseTree a b -> [Pos]
pos Ep            = [[]]
pos (Leaf _)      = [[]]
pos (Branch _ ts) = [] : [ i:p | (i,t) <- zip [0..] ts,
                                     p <- pos t ]

subtree :: ParseTree a b -> Pos -> ParseTree a b
subtree t             []     = t
subtree (Branch _ ts) (i:is) = subtree (ts!!i) is

subtrees :: ParseTree a b -> [ParseTree a b]
subtrees t = [ subtree t p | p <- pos t ]

type Rel a = [(a,a)]

properdominance :: ParseTree a b -> Rel Pos
properdominance t = [ (p,q) | p <- pos t,
                              q <- pos t,
                              p /= q,
                              prefix p q ]

dominance :: ParseTree a b -> Rel Pos
dominance t = [ (p,q) | p <- pos t,
                        q <- pos t,
                        prefix p q ]

sisters :: Pos -> Pos -> Bool
sisters [i]    [j]    = i /= j
sisters (i:is) (j:js) = i == j && sisters is js
sisters  _      _     = False

sisterhood :: ParseTree a b -> Rel Pos
sisterhood t = [ (p,q) | p <- pos t,
                         q <- pos t,
                         sisters p q ]

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

cCommand :: ParseTree a b -> Rel Pos
cCommand t = (sisterhood t) @@ (dominance t)

branchingPos :: ParseTree a b -> [Pos]
branchingPos t = let ps = pos t in
  [ p | p <- ps, (p++[0]) `elem` ps, (p++[1]) `elem` ps ]

precede :: Pos -> Pos -> Bool
precede (i:is) (j:js) = i < j || (i == j && precede is js)
precede  _      _     = False

precedence :: ParseTree a b -> Rel Pos
precedence t = [ (p,q) | p <- pos t,
                         q <- pos t,
                         precede p q ]

split2 :: [a] -> [([a],[a])]
split2 []     = [([],[])]
split2 (x:xs) = [([],(x:xs))]
             ++ (map (\(ys,zs) -> ((x:ys),zs)) (split2 xs))

splitN :: Int -> [a] -> [[[a]]]
splitN n xs
  | n <= 1    = error "cannot split"
  | n == 2    = [ [ys,zs] | (ys,zs) <- split2 xs ]
  | otherwise = [ ys:rs   | (ys,zs) <- split2 xs,
                             rs     <- splitN (n-1) zs ]

recognize :: String -> Bool
recognize = \ xs ->
    null xs || xs == "a" || xs == "b" || xs == "c"
    || or [ recognize ys | ["a",ys,"a"] <- splitN 3 xs ]
    || or [ recognize ys | ["b",ys,"b"] <- splitN 3 xs ]
    || or [ recognize ys | ["c",ys,"c"] <- splitN 3 xs ]

gener :: Int -> String -> [String]
gener 0 alphabet = [[]]
gener n alphabet = [ x:xs | x  <- alphabet,
                            xs <- gener (n-1) alphabet ]

gener' :: Int -> String -> [String]
gener' n alphabet = gener   n    alphabet
                 ++ gener' (n+1) alphabet

generateAll  :: String -> [String]
generateAll alphabet = gener' 0 alphabet

generate = filter recognize (generateAll alphabet)
  where alphabet = ['a','b','c']

parse :: String -> [ParseTree String String]
parse = \ xs ->
    [Leaf "[]" | null xs ]
 ++ [Leaf "a"  | xs == "a" ]
 ++ [Leaf "b"  | xs == "b" ]
 ++ [Leaf "c"  | xs == "c" ]
 ++ [Branch "A" [Leaf "a", t, Leaf "a"] |
                ["a",ys,"a"] <- splitN 3 xs,
                t            <- parse ys   ]
 ++ [Branch "A" [Leaf "b", t, Leaf "b"] |
                ["b",ys,"b"] <- splitN 3 xs,
                t            <- parse ys   ]
 ++ [Branch "A" [Leaf "c", t, Leaf "c"] |
                ["c",ys,"c"] <- splitN 3 xs,
                t            <- parse ys   ]

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed r xs = [(r,xs)]

failp :: Parser a b
failp xs = []

symbol :: Eq a => a -> Parser a a
symbol c []                 = []
symbol c (x:xs) | c == x    = [(x,xs)]
                | otherwise = []

token :: Eq a => [a] -> Parser a [a]
token cs xs | cs == take n xs   = [(cs,drop n xs)]
            | otherwise         = []
         where n = length cs

satisfy :: (a -> Bool) -> Parser a a
satisfy p []                 = []
satisfy p (x:xs) | p x       = [(x,xs)]
                 | otherwise = []

digit :: Parser Char Char
digit = satisfy isDigit

just :: Parser a b -> Parser a b
just p = filter (null.snd) . p

infixr 4 <|>

(<|>) :: Parser a b -> Parser a b -> Parser a b
(p1 <|> p2) xs = p1 xs ++ p2 xs

(<*>) :: Parser a [b] -> Parser a [b] -> Parser a [b]
(p <*> q) xs = [ (r1 ++ r2,zs) | (r1,ys) <- p xs,
                                 (r2,zs) <- q ys ]

pS,pNP,pVP,pD,pN :: Parser String String

pS  = pNP P.<*> pVP
pNP = symbol "Alice"  <|> symbol "Dorothy" <|> (pD P.<*> pN)
pVP = symbol "smiled" <|> symbol "laughed"
pD  = symbol "every"  <|> symbol "some"    <|> symbol "no"
pN  = symbol "dwarf"  <|> symbol "wizard"

infixl 7 <$>

(<$>) :: (a -> b) -> Parser s a -> Parser s b
(f <$> p) xs = [ (f x,ys) | (x,ys) <- p xs ]

digitize :: Parser Char Int
digitize = f P.<$> digit
  where f c = ord c - ord '0'

type PARSER a b = Parser a (ParseTree a b)

epsilonT :: PARSER a b
epsilonT = succeed Ep

symbolT :: Eq a => a -> PARSER a b
symbolT s = (\ x -> Leaf x) P.<$> symbol s

infixl 6 <:>

(<:>) :: Parser a b -> Parser a [b] -> Parser a [b]
(p <:> q) xs = [ (r:rs,zs) | (r,ys)  <- p xs,
                             (rs,zs) <- q ys ]

collect :: [Parser a b] -> Parser a [b]
collect []     = succeed []
collect (p:ps) = p <:> collect ps

parseAs :: b -> [PARSER a b] -> PARSER a b
parseAs label ps = (\ xs -> Branch label xs) P.<$> collect ps

sent, np, vp, det, cn :: PARSER String Char
sent =  parseAs 'S' [np,vp]
np   =  symbolT "Alice"  <|> symbolT "Dorothy"
    <|> parseAs 'N' [det,cn]
det  =  symbolT "every"  <|> symbolT "some" <|> symbolT "no"
cn   =  symbolT "man"    <|> symbolT "woman"
vp   =  symbolT "smiled" <|> symbolT "laughed"

palindrome :: PARSER Char Char
palindrome =
  epsilonT <|> symbolT 'a' <|> symbolT 'b' <|> symbolT 'c'
           <|> parseAs 'A' [symbolT 'a', palindrome, symbolT 'a']
           <|> parseAs 'A' [symbolT 'b', palindrome, symbolT 'b']
           <|> parseAs 'A' [symbolT 'c', palindrome, symbolT 'c']

many :: Parser a b -> Parser a [b]
many p = (p <:> many p) <|> (succeed [])

parseManyAs :: b -> PARSER a b -> PARSER a b
parseManyAs l p = (\ xs -> Branch l xs) P.<$> many p

colour, answer, guess, reaction, turn, game
   :: PARSER String String

colour   =  symbolT "red"   <|> symbolT "yellow"
        <|> symbolT "blue"  <|> symbolT "green"
answer   =  symbolT "black" <|> symbolT "white"
guess    =  parseAs "GUESS" [colour,colour,colour,colour]
reaction =  parseManyAs "REACTION" answer
turn     =  parseAs "TURN" [guess,reaction]
game     =  turn <|> parseAs "GAME" [turn,game]

gender, modifier, structure, tense, question, exclamation, prepType
         :: Agreement -> Agreement
gender   = filter (`elem` [Masc,Fem,Neutr])
modifier = filter (`elem` [Dfree,Dban])
structure = filter (`elem` [De1,De2,De4])
tense = filter (`elem` [Past,PresCon,Fut])
question = filter(`elem` [Ma,Me,Ne,Ba])
exclamation = filter(`elem` [Ah,Ya,Wa])
prepType = filter(`elem` [At,With,From,To])

instance Show Cat where
  show (Cat "_"  label agr subcatlist) = label ++ show agr
  show (Cat phon label agr subcatlist) = phon  ++ " "
                                               ++ label ++ show agr

phon :: Cat -> String
phon (Cat ph _ _ _) = ph

catLabel :: Cat -> CatLabel
catLabel (Cat _ label _ _) = label

fs :: Cat -> Agreement
fs (Cat _ _ agr _) = agr

subcatList :: Cat -> [Cat]
subcatList (Cat _ _ _ cats) = cats

combine :: Cat -> Cat -> [Agreement]
combine cat1 cat2 =
 [ feats | length (gender      feats) <= 1,
           length (modifier      feats) <= 1,
           length (structure      feats) <= 1,
           length (tense       feats) <= 1,
           length (question    feats) <= 1,
           length (exclamation feats) <= 1,
           length (prepType    feats) <= 1 ]
  where
    feats = (nub . sort) (fs cat1 ++ fs cat2)

agree :: Cat -> Cat -> Bool
agree cat1 cat2 = not (null (combine cat1 cat2))

assign :: Feat -> Cat -> [Cat]
assign f c@(Cat phon label fs subcatlist) =
  [Cat phon label fs' subcatlist |
         fs' <- combine c (Cat "" "" [f] []) ]

scan :: String -> String
scan []                      = []
scan (x:xs) | x `elem` ".,?" = ' ':x:scan xs
            | otherwise      =     x:scan xs

type Words = [String]

lexer :: String -> Words
lexer = preproc . words . (map toLower) . scan

preproc :: Words -> Words
preproc []                 = []
preproc ["."]              = []
preproc ["?"]              = []
preproc (",":xs)           = preproc xs
preproc (x:xs)             = x : preproc xs

lookupWord :: (String -> [Cat]) -> String -> [Cat]
lookupWord db w = [ c | c <- db w ]

collectCats :: (String -> [Cat]) -> Words -> [[Cat]]
collectCats db words =
  let
    listing = map (\ x -> (x,lookupWord db x)) words
    unknown = map fst (filter (null.snd) listing)
  in
    if unknown /= [] then
      error ("unknown words: " ++ show unknown)
    else initCats (map snd listing)

initCats :: [[Cat]] -> [[Cat]]
initCats []         = [[]]
initCats (cs:rests) = [ c:rest | c    <- cs,
                                 rest <- initCats rests ]

t2c :: ParseTree Cat Cat -> Cat
t2c (Leaf   c)   = c
t2c (Branch c _) = c

agreeC :: ParseTree Cat Cat -> ParseTree Cat Cat -> Bool
agreeC t1 t2 = agree (t2c t1) (t2c t2)

leafP :: CatLabel -> PARSER Cat Cat
leafP label []     = []
leafP label (c:cs) = [(Leaf c,cs) | catLabel c == label ]

assignT :: Feat ->  ParseTree Cat Cat
                -> [ParseTree Cat Cat]
assignT f (Leaf   c)    = [Leaf   c'    | c' <- assign f c]
assignT f (Branch c ts) = [Branch c' ts | c' <- assign f c]

parseSent :: PARSER Cat Cat
parseSent = sRule <|> exclSRule <|> qSRule

exclSRule :: PARSER Cat Cat
exclSRule = \ xs ->
       [ (Branch (Cat "_" "S" [] []) [s,mak],zs) |
         (s,ys) <- sRule xs,
         (mak, zs) <- leafP "EMAK" ys]

qSRule :: PARSER Cat Cat
qSRule = \ xs ->
       [ (Branch (Cat "_" "S" [] []) [s,mak],zs) |
         (s,ys) <- sRule xs,
         (mak, zs) <- leafP "QMAK" ys]

sRule :: PARSER Cat Cat
sRule = \ xs ->
       [ (Branch (Cat "_" "S" [] []) [np,vp],zs) |
         (np,ys) <- parseNP xs,
         (vp,zs) <- parseVP ys,
         agreeC np vp,
         subcatList (t2c vp) == [] ]

npRule :: PARSER Cat Cat
npRule = \ xs ->
  [ (Branch (Cat "_" "NP" fs []) [det,np],zs) |
    (det,ys) <- parseDET xs,
    (np,zs)  <- leafP "NP"  ys,
    fs       <- combine (t2c det) (t2c np),
    agreeC det np ]

parseNP :: PARSER Cat Cat
parseNP = leafP "NP" <|> npRule

ppRule :: PARSER Cat Cat
ppRule = \ xs ->
   [ (Branch (Cat "_" "PP" fs []) [prep,np],zs) |
     (prep,ys) <- leafP "PREP" xs,
     (np,zs)   <- parseNP ys,
      fs       <- combine (t2c prep) (t2c np) ]

parsePP :: PARSER Cat Cat
parsePP = ppRule

parseDET :: PARSER Cat Cat
parseDET = leafP "DET"

parsePrep :: PARSER Cat Cat
parsePrep = leafP "PREP"

parseTen :: PARSER Cat Cat
parseTen = leafP "TEN"

parseVP :: PARSER Cat Cat
parseVP = vpBasicRule <|>  vpTenRule <|> futVpRule <|> ppVpRule

vpTenRule :: PARSER Cat Cat  -- verb + le/zhe --> Past/ PresCon
vpTenRule = \xs ->
    [ (Branch (Cat "_" "VP" (fs (t2c ten)) []) (vp:(ten:nps)),ws) |
    (vp,ys) <- leafP "VP" xs,
    (ten,zs) <- parseTen ys,
    subcatlist  <- [subcatList (t2c vp)],
    (nps,ws)    <- parseNPs zs,
    match subcatlist (map t2c nps)]

vpBasicRule :: PARSER Cat Cat
vpBasicRule = \xs ->
     [ (Branch (Cat "_" "VP" (fs (t2c vp)) []) (vp:nps),zs) |
       (vp,ys)     <- leafP "VP" xs,
       subcatlist  <- [subcatList (t2c vp)],
       (nps,zs)    <- parseNPs ys,
       match subcatlist (map t2c nps)]

parsePlainVP :: PARSER Cat Cat
parsePlainVP = vpBasicRule <|> vpTenRule

ppVpRule :: PARSER Cat Cat
ppVpRule = \xs ->
    [(Branch (Cat "_" "VP" (fs (t2c vp)) []) [pp,vp],zs) |
      (pp,ys)  <- parsePP xs,
      (vp,zs)  <- parsePlainVP ys]

futVpRule :: PARSER Cat Cat    -- yao + verb --> Future
futVpRule = \xs ->
    [ (Branch (Cat "_" "VP" (fs (t2c ten)) []) [ten,vp],zs) |
      (ten,ys) <- parseTen xs,
      (vp,zs) <- parseVP ys]

match :: [Cat] -> [Cat] -> Bool
match []     []     = True
match _      []     = False
match []      _     = False
match (x:xs) (y:ys) = catLabel x == catLabel y
            && agree x y
              && match xs ys

parseNPs :: [Cat] -> [([ParseTree Cat Cat],[Cat])]
parseNPs = many parseNP

prs :: String -> [ParseTree Cat Cat]
prs string = let ws = lexer string
     in  [ s | catlist <- collectCats lexicon ws,
            (s,[])  <- parseSent catlist ]

type StackParser a b = [a] -> [a] -> [(b,[a],[a])]

type SPARSER a b = StackParser a (ParseTree a b)

infixr 4 <||>

(<||>) :: StackParser a b -> StackParser a b
               -> StackParser a b
(p1 <||> p2) stack xs = p1 stack xs ++ p2 stack xs

infixl 6 <::>

(<::>) :: StackParser a b  -> StackParser a [b]
                           -> StackParser a [b]
(p <::> q) us xs = [(r:rs,ws,zs) | (r,vs,ys)  <- p us xs,
                                   (rs,ws,zs) <- q vs ys ]

succeedS :: b -> StackParser a b
succeedS r us xs = [(r,us,xs)]

manyS :: StackParser a b -> StackParser a [b]
manyS p = (p <::> manyS p) <||> succeedS []

push :: Cat -> SPARSER Cat Cat -> SPARSER Cat Cat
push c p stack = p (c:stack)

pop :: CatLabel -> SPARSER Cat Cat
pop c []     xs                   = []
pop c (u:us) xs | catLabel u == c = [(Leaf u, us, xs)]
                | otherwise       = []

leafPS :: CatLabel -> SPARSER Cat Cat
leafPS l _ []         = []
leafPS l s (c:cs) = [(Leaf c,s,cs) | catLabel c == l ]

prsTXT :: SPARSER Cat Cat
prsTXT = conjR <||> prsS

conjR :: SPARSER Cat Cat
conjR = \ us xs ->
   [ (Branch (Cat "_" "TXT" [] []) [s, conj, txt], ws, zs) |
       (s,vs,ys)      <- prsS us xs,
       (conj,vs1,ys1) <- leafPS "CONJ" vs ys,
       (txt,ws,zs)    <- prsTXT vs1 ys1            ]

prsS :: SPARSER Cat Cat
prsS = spR <||> cond1R <||> cond2R

spR :: SPARSER Cat Cat
spR = \ us xs ->
 [ (Branch (Cat "_" "S" (fs (t2c np)) []) [np,vp],ws,zs) |
       (np,vs,ys) <- prsNP us xs,
       (vp,ws,zs) <- prsVP vs ys,
       agreeC np vp,
       subcatList (t2c vp) == [] ]

cond1R :: SPARSER Cat Cat
cond1R = \ us xs ->
   [ (Branch (Cat "_" "S" [] []) [cond,s1,s2], ws, zs) |
       (cond,vs,ys) <- leafPS "COND" us xs,
       (s1,vs1,ys1) <- prsS vs ys,
       (s2,ws,zs)   <- prsS vs1 ys1 ]

cond2R :: SPARSER Cat Cat
cond2R = \ us xs ->
     [ (Branch (Cat "_" "S" [] []) [cond,s1,s2], ws, zs) |
         (cond,vs,ys) <- leafPS "COND" us xs,
         (s1,vs1,ys1) <- prsS vs ys,
         (_,vs2,ys2)  <- leafPS "THEN" vs1 ys1,
         (s2,ws,zs)   <- prsS vs2 ys2 ]

prsNP :: SPARSER Cat Cat
prsNP = leafPS "NP" <||> npR <||> pop "NP"

npR :: SPARSER Cat Cat
npR = \ us xs ->
  [ (Branch (Cat "_" "NP" fs []) [det,cn], (us++ws), zs) |
      (det,vs,ys) <- prsDET [] xs,
      (cn,ws,zs)  <- prsCN vs ys,
       fs         <- combine (t2c det) (t2c cn),
      agreeC det cn ]

prsDET :: SPARSER Cat Cat
prsDET = leafPS "DET"

prsCN :: SPARSER Cat Cat
prsCN = leafPS "CN" <||> cnrelR

prsVP :: SPARSER Cat Cat
prsVP = vpR

vpR :: SPARSER Cat Cat
vpR = \us xs ->
 [(Branch (Cat "_" "VP" (fs (t2c vp)) []) (vp:xps),ws,zs) |
             (vp,vs,ys)  <- leafPS "VP" us xs,
             subcatlist  <- [subcatList (t2c vp)],
             (xps,ws,zs) <- prsNPsorPPs vs ys,
             match subcatlist (map t2c xps) ]

prsAUX :: SPARSER Cat Cat
prsAUX = leafPS "AUX" <||> pop "AUX"

prsPP :: SPARSER Cat Cat
prsPP = ppR <||> pop "PP"

ppR :: SPARSER Cat Cat
ppR = \us xs ->
  [ (Branch (Cat "_" "PP" fs []) [prep,np], ws, zs) |
      (prep,vs,ys) <- prsPREP us xs,
      (np,ws,zs)   <- prsNP vs ys,
       fs          <- combine (t2c prep) (t2c np) ]

prsPREP :: SPARSER Cat Cat
prsPREP = leafPS "PREP"

prsNPorPP :: SPARSER Cat Cat
prsNPorPP = prsNP <||> prsPP

prsNPsorPPs :: [Cat] -> [Cat]
       -> [([ParseTree Cat Cat],[Cat],[Cat])]
prsNPsorPPs = manyS prsNPorPP

cnrelR :: SPARSER Cat Cat
cnrelR = \us xs ->
     [ (Branch (Cat "_" "CN" (fs (t2c cn)) [])
               [cn,rel], ws, zs) |
                 (cn,vs,ys)  <- leafPS "CN" us xs,
                 (rel,ws,zs) <- prsREL vs ys,
                 agreeC cn rel ]

prsREL :: SPARSER Cat Cat
prsREL = relclauseR <||> thatlessR

relclauseR :: SPARSER Cat Cat
relclauseR = \us xs ->
  [(Branch (Cat "_" "COMP" fs []) [rel,s], ws, zs) |
      (rel,vs,ys) <- leafPS "REL" us xs,
       fs         <- [fs (t2c rel)],
       gap        <- [Cat "#" "NP" fs []],
       (s,ws,zs)  <- push gap prsS vs ys ]

thatlessR :: SPARSER Cat Cat
thatlessR = \ us xs ->
        [ (Branch (Cat "_" "COMP" [] []) [s], vs, ys) |
           gap       <- [Cat "#" "NP" [] []],
           (s,vs,ys) <- push gap prsS us xs]

prsYN :: SPARSER Cat Cat
prsYN = \us xs ->
   [(Branch (Cat "_" "YN" [] []) [aux,s], ws,zs) |
       (aux,vs,ys) <- prsAUX us xs,
       gap         <- [Cat "#" "AUX" (fs (t2c aux)) [] ],
       (s,ws,zs)   <- push gap prsS vs ys ]

parses :: String -> [ParseTree Cat Cat]
parses str = let ws = lexer str
             in  [ s | catlist   <- collectCats lexicon ws,
                       (s,[],[]) <- prsTXT [] catlist
                                 ++ prsYN  [] catlist]

testSuite1 :: [String]
testSuite1 =
 [ "Alice admired Dorothy.",
   "Did Alice admire Dorothy?",
   "Who did Alice admire?",
   "Atreyu gave the sword to the princess.",
   "Did Atreyu give the sword to the princess?",
   "Who did Atreyu give the sword to?",
   "To whom did Atreyu give the sword?",
   "Goldilocks helped the girl "
    ++ "that Atreyu gave the sword to.",

  "Did Goldilocks help the girl "
    ++ "that Atreyu gave the sword to.",
   "Goldilocks helped the boy that helped the princess "
    ++ "that Atreyu gave the sword to." ]

testSuite2 :: [String]
testSuite2 =
 [ "Dorothy admired the boy that Alice helped Atreyu",
   "Dorothy admired the boy that helped",
   "Dorothy admired the girl that "
    ++ "Atreyu helped the princess that gave the sword to" ]

data Term = Const String | Var Int deriving (Eq,Ord)

data GQ = Sm | All | Th | Most | Many | Few
        deriving (Eq,Show,Ord)

data Abstract = MkAbstract Int LF deriving (Eq,Ord)

data LF = Rel String [Term]
        | Eq   Term Term
        | Neg  LF
        | Impl LF LF
        | Equi LF LF
        | Conj [LF]
        | Disj [LF]
        | Qt GQ Abstract Abstract
     deriving (Eq,Ord)

instance Show Term where
  show (Const name) = name
  show (Var i)      = 'x': show i

instance Show Abstract where
  show (MkAbstract i lf) =
   "(\\ x" ++ show i ++ " " ++ show lf ++ ")"

instance Show LF where
  show (Rel r args)   = r ++ show args
  show (Eq t1 t2)     = show t1 ++ "==" ++ show t2
  show (Neg lf)       = '~': (show lf)
  show (Impl lf1 lf2) = "(" ++ show lf1 ++ "==>"
                            ++ show lf2 ++ ")"
  show (Equi lf1 lf2) = "(" ++ show lf1 ++ "<=>"
                            ++ show lf2 ++ ")"
  show (Conj [])      = "true"
  show (Conj lfs)     = "conj" ++ concat [ show lfs ]
  show (Disj [])      = "false"
  show (Disj lfs)     = "disj" ++ concat [ show lfs ]
  show (Qt gq a1 a2)   = show gq ++ (' ' : show a1)
                                 ++ (' ' : show a2)

transS :: ParseTree Cat Cat -> LF
transS (Branch (Cat _ "S" _ _) [np,vp]) =
  (transNP np) (transVP vp)

transS (Branch (Cat _ "YN" _ _)
       [Leaf (Cat "did"    "AUX" _ []),s]) = transS s
transS (Branch (Cat _ "YN" _ _)
       [Leaf (Cat "didn't" "AUX" _ []),s]) = Neg (transS s)

transNP :: ParseTree Cat Cat ->
                (Term -> LF) -> LF
transNP (Leaf (Cat "#"  "NP" _ _)) = \ p -> p (Var 0)
transNP (Leaf (Cat name "NP" _ _)) = \ p -> p (Const name)
transNP (Branch (Cat _ "NP" _ _) [det,cn]) =
                             (transDET det) (transCN cn)

transDET :: ParseTree Cat Cat -> (Term -> LF)
                              -> (Term -> LF)
                              -> LF
transDET (Leaf (Cat "every" "DET" _ _)) =
  \ p q -> let i = fresh[p,q] in
  Qt All       (MkAbstract i (p (Var i)))
               (MkAbstract i (q (Var i)))

transDET (Leaf (Cat "all" "DET" _ _)) =
  \ p q -> let i = fresh[p,q] in
  Qt All       (MkAbstract i (p (Var i)))
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "some" "DET" _ _)) =
  \ p q -> let i = fresh[p,q] in
  Qt Sm      (MkAbstract i (p (Var i)))
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "a" "DET" _ _)) =
  \ p q -> let i = fresh[p,q] in
  Qt Sm      (MkAbstract i (p (Var i)))
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "several" "DET" _ _)) =
  \ p q -> let i = fresh[p,q] in
  Qt Sm      (MkAbstract i (p (Var i)))
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "no" "DET" _ _)) =
  \ p q -> let i = fresh[p,q] in
  Neg (Qt Sm (MkAbstract i (p (Var i)))
               (MkAbstract i (q (Var i))))
transDET (Leaf (Cat "the" "DET" _ _)) =
  \ p q -> let i = fresh[p,q] in
  Qt Th        (MkAbstract i (p (Var i)))
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "most" "DET" _ _)) =
  \ p q -> let i = fresh[p,q] in
  Qt Most      (MkAbstract i (p (Var i)))
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "many" "DET" _ _)) =
  \ p q -> let i = fresh[p,q] in
  Qt Many      (MkAbstract i (p (Var i)))
               (MkAbstract i (q (Var i)))
transDET (Leaf (Cat "few" "DET" _ _)) =
  \ p q -> let i = fresh[p,q] in
  Neg (Qt Many (MkAbstract i (p (Var i)))
               (MkAbstract i (q (Var i))))

transDET (Leaf (Cat "which" "DET" _ _)) =
  \ p q -> Conj [p (Var 0),q (Var 0)]

transCN :: ParseTree Cat Cat -> Term -> LF
transCN (Leaf   (Cat name "CN" _ _))          = \ x ->
                                              Rel name [x]
transCN (Branch (Cat _    "CN" _ _) [cn,rel]) = \ x ->
                       Conj [transCN cn x, transREL rel x]

transREL :: ParseTree Cat Cat -> Term -> LF
transREL (Branch (Cat _ "COMP" _ _ ) [rel,s]) =
  \ x -> sub x (transS s)
transREL (Branch (Cat _ "COMP" _ _ ) [s])     =
  \ x -> sub x (transS s)

transPP :: ParseTree Cat Cat -> (Term -> LF) -> LF
transPP (Leaf   (Cat "#" "PP" _ _)) = \ p -> p (Var 0)
transPP (Branch (Cat _   "PP" _ _) [prep,np]) = transNP np

transVP :: ParseTree Cat Cat -> Term -> LF
transVP (Branch (Cat _ "VP" _ _)
                [Leaf (Cat name "VP" _ [])]) =
        \ t -> Rel name [t]
transVP (Branch (Cat _ "VP" _ _)
                [Leaf (Cat name "VP" _ [_]),np]) =
        \ subj -> transNP np (\ obj -> Rel name [subj,obj])

transVP (Branch (Cat _ "VP" _ _)
                [Leaf (Cat name "VP" _ [_,_]),np,pp]) =
        \ subj   -> transNP np
        (\ obj   -> transPP pp
         (\ iobj -> Rel name [subj,obj,iobj]))
transVP (Branch (Cat _ "VP" _ _)
                [Leaf (Cat "did" "AUX" _ []),vp]) =
        transVP vp
transVP (Branch (Cat _ "VP" _ _)
                [Leaf (Cat "didn't" "AUX" _ []),vp]) =
        \x -> Neg ((transVP vp) x)
transVP (Branch (Cat _ "VP" _ _)
                [Leaf (Cat "#" "AUX" _ []),vp]) =
        transVP vp

transWH :: ParseTree Cat Cat -> Abstract
transWH (Branch (Cat _ "WH" _ _ ) [wh,s]) =
  MkAbstract 0 (Conj [transW wh, transS s])

transW :: ParseTree Cat Cat -> LF
transW (Branch (Cat _ "NP" fs _) [det,cn]) =
                            transCN cn (Var 0)
transW (Leaf (Cat _ "NP" fs _))
      | Masc      `elem` fs = Rel "man"    [Var 0]
      | Fem       `elem` fs = Rel "woman"  [Var 0]
      | otherwise           = Rel "thing"  [Var 0]

transW (Branch (Cat _ "PP" fs _) [prep,np])
      | Masc      `elem` fs = Rel "man"    [Var 0]
      | Fem       `elem` fs = Rel "woman"  [Var 0]
      | otherwise           = Rel "thing"  [Var 0]

subst :: Term -> Term -> Term
subst x (Const name)         = Const name
subst x (Var n) | n == 0     = x
                | otherwise  = Var n
                | x == Var n = error "bad substitution"

sub :: Term -> LF -> LF
sub x (Rel name ts)     = Rel name (map (subst x) ts)
sub x (Eq t1 t2)        = Eq (subst x t1) (subst x t2)
sub x (Neg lf)          = Neg (sub x lf)
sub x (Impl lf1 lf2)    = Impl (sub x lf1) (sub x lf2)
sub x (Equi lf1 lf2)    = Equi (sub x lf1) (sub x lf2)
sub x (Conj lfs)        = Conj (map (sub x) lfs)
sub x (Disj lfs)        = Disj (map (sub x) lfs)
sub x (Qt gq abs1 abs2) = Qt gq (sb x abs1) (sb x abs2)

sb :: Term -> Abstract -> Abstract
sb x (MkAbstract 0 lf) = MkAbstract 0 lf
sb x (MkAbstract n lf) = MkAbstract n (sub x lf)

bInLF :: LF -> [Int]
bInLF (Rel _ _)         = []
bInLF (Eq _  _)         = []

bInLF (Neg lf)          = bInLF lf
bInLF (Impl lf1 lf2)    = bInLFs [lf1,lf2]
bInLF (Equi lf1 lf2)    = bInLFs [lf1,lf2]
bInLF (Conj lfs)        = bInLFs lfs
bInLF (Disj lfs)        = bInLFs lfs
bInLF (Qt gq abs1 abs2) = bInAs [abs1,abs2]

bInLFs :: [LF] -> [Int]
bInLFs = nub . concat . map bInLF

bInA :: Abstract -> [Int]
bInA (MkAbstract i lf) = i: bInLF lf

bInAs :: [Abstract] -> [Int]
bInAs = nub . concat . map bInA

freshIndex  :: [LF] -> Int
freshIndex lfs = i+1
  where i      = foldr max 0 (bInLFs lfs)

fresh :: [Term -> LF] -> Int
fresh preds   = freshIndex (map ($ dummy) preds)
  where dummy = Const ""

process :: String -> [LF]
process string = map transS (parses string)

processW :: String -> [Abstract]
processW string = map transWH (parses string)
