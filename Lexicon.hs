module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = Masc  | Fem  | Neutr
            | Sg    | Pl
            | Fst   | Snd  | Thrd
            | Pers  | Refl | Wh
            | De1 |  De2 |  De4                           -- structure particles
            | Le  |  Zhe  |  Yao                          -- tense particles
            | Ma  |  Me  |  Ne  |  Ba                     -- question markers
            | Ah |  Ya |  Wa |  Na 			              -- exclamation markers
            | At  |  With  |  From | To                   -- prepositions
            deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]

lexicon "wo"    = [Cat "wo"    "NP" [Pers,Fst,Sg]          []]      -- i/me
lexicon "women" = [Cat "women" "NP" [Pers,Fst,Pl]          []]      -- we/us
lexicon "ni"    = [Cat "ni"    "NP" [Pers,Snd]             []]      -- you
lexicon "ta1"   = [Cat "ta1"   "NP" [Pers,Thrd,Sg,Masc]    []]      -- he/him
lexicon "ta2"   = [Cat "ta2"   "NP" [Pers,Thrd,Sg,Fem]     []]      -- she/her
lexicon "ta3"   = [Cat "ta3"   "NP" [Pers,Thrd,Sg,Neutr]   []]      -- it
lexicon "tamen" = [Cat "tamen" "NP" [Pers,THrd,Pl]         []]      -- they/them

lexicon "woziji"     = [Cat "woziji"    "NP" [Refl,Sg,Fst]        []]       -- myself
lexicon "womenziji"  = [Cat "womenziji" "NP" [Refl,Pl,Fst]        []]       -- ourselves
lexicon "niziji"     = [Cat "niziji"    "NP" [Refl,Sg,Snd]        []]       -- yourself
lexicon "nimenziji"  = [Cat "nimenziji" "NP" [Refl,Pl,Snd]        []]       -- yourselves
lexicon "ta1ziji"    = [Cat "ta1ziji"   "NP" [Refl,Sg,Thrd,Masc]  []]       -- himself
lexicon "ta2ziji"    = [Cat "ta2ziji"   "NP" [Refl,Sg,Thrd,Fem]   []]       -- herself
lexicon "ta3ziji"    = [Cat "ta3ziji"   "NP" [Refl,Sg,Thrd,Neutr] []]       -- itself
lexicon "tamenziji"  = [Cat "tamenziji" "NP" [Refl,Pl,Thrd]       []]       -- themselves

lexicon "baixuegongzhu"  = [Cat "baixuegongzhu"  "NP" [Thrd,Fem,Sg]  []]    -- snowwhite
lexicon "ailisi"         = [Cat "ailisi"         "NP" [Thrd,Fem,Sg]  []]    -- alice
lexicon "duoluoxi"       = [Cat "duoluoxi"       "NP" [Thrd,Fem,Sg]  []]    -- dorothy
lexicon "jinfaguniang"   = [Cat "jinfaguniang"   "NP" [Thrd,Fem,Sg]  []]    -- goldilocks
lexicon "xioamuke"       = [Cat "xiaomuke"       "NP" [Thrd,Masc,Sg] []]    -- littlemook
lexicon "ateleiyou"      = [Cat "ateleiyou"      "NP" [Thrd,Masc,Sg] []]    -- atreyu

lexicon "meige"      = [Cat "meige"     "DET" [Sg] []]      -- every
lexicon "suoyou"     = [Cat "suoyou"    "DET" [Pl] []]      -- all
lexicon "yixie"      = [Cat "yixie"     "DET" [Pl] []]      -- some
lexicon "yige"       = [Cat "yige"      "DET" [Sg] []]      -- a
lexicon "meiyou"     = [Cat "meiyou"    "DET" []   []]      -- no
lexicon "dabufen"    = [Cat "dabufen"   "DET" [Pl] []]      -- most
lexicon "henduo"     = [Cat "henduo"    "DET" [Pl] []]      -- many
lexicon "henshao"    = [Cat "henshao"   "DET" [Pl] []]      -- few
lexicon "zhege"      = [Cat "zhege"     "DET" [Sg] []]      -- this
lexicon "nage"       = [Cat "nage"      "DET" [Sg] []]      -- that
lexicon "zhexie"     = [Cat "zhexie"    "DET" [Pl] []]      -- these
lexicon "naxie"      = [Cat "naxie"     "DET" [Pl] []]      -- those

lexicon "dongxi"        = [Cat "dongxi"     "CN" [Neutr,Thrd]     []]   -- thing
lexicon "ren"           = [Cat "ren"        "CN" [Masc,Thrd]      []]   -- person
lexicon "nanhai"        = [Cat "nanhai"     "CN" [Masc,Thrd]      []]   -- boy
lexicon "nanhaimen"     = [Cat "nanhaimen"  "CN" [Pl,Masc,Thrd]   []]   -- boys
lexicon "nanren"        = [Cat "nanren"     "CN" [Sg,Masc,Thrd]   []]   -- man
lexicon "nanrenmen"     = [Cat "nanrenmen"  "CN" [Pl,Masc,Thrd]   []]   -- men
lexicon "nvhai"         = [Cat "nvhai"      "CN" [Sg,Fem,Thrd]    []]   -- girl
lexicon "nvhaimen"      = [Cat "nvhaimen"   "CN" [Pl,Fem,Thrd]    []]   -- girls
lexicon "nvren"         = [Cat "nvren"      "CN" [Sg,Fem,Thrd]    []]   -- woman
lexicon "nvrenmen"      = [Cat "nvrenmen"   "CN" [Pl,Fem,Thrd]    []]   -- women
lexicon "gongzhu"       = [Cat "gongzhu"    "CN" [Sg,Fem,Thrd]    []]   -- princess
lexicon "gongzhumen"    = [Cat "gongzhumen" "CN" [Pl,Fem,Thrd]    []]   -- princesses
lexicon "xiaoairen"     = [Cat "xiaoairen"    "CN" [Sg,Masc,Thrd] []]   -- dwarf
lexicon "xiaoairenmen"  = [Cat "xiaoairenmen" "CN" [Pl,Masc,Thrd] []]   -- dwarves
lexicon "juren"         = [Cat "juren"      "CN" [Sg,Masc,Thrd]   []]   -- giant
lexicon "jurenmen"      = [Cat "jurenmen"   "CN" [Pl,Masc,Thrd]   []]   -- giants

lexicon "wushi"    = [Cat "wushi"    "CN" [Sg,Masc,Thrd]  []]  -- wizard
lexicon "wushimen" = [Cat "wushimen" "CN" [Pl,Masc,Thrd]  []]  -- wizards
lexicon "jian"     = [Cat "jian"     "CN" [Neutr,Thrd]    []]  -- sword/sowrds
lexicon "dao"      = [Cat "dao"      "CN" [Neutr,Thrd]    []]  -- dagger/daggers

lexicon "weixiao"        = [Cat "weixiao"        "VP" []       []]   -- smile
lexicon "xiao"           = [Cat "xiao"           "VP" []       []]   -- laugh

lexicon "ai"       =
    [Cat "ai"         "VP" []   [Cat "_" "NP" [] []]   -- love
lexicon "jingpei"  =
    [Cat "jingpei"    "VP" []   [Cat "_" "NP" [] []]   -- admire
lexicon "bangzhu"  =
    [Cat "bangzhu"    "VP" []   [Cat "_" "NP" [] []]   -- help
lexicon "dabai"  =
    [Cat "dabai"      "VP" []   [Cat "_" "NP" [] []]   -- defeat

lexicon "gei"  =
    [Cat "gei"     "VP" []      [Cat "_" "NP"    [] [],      -- give
                                 Cat "_" "NP"    [] []]]
lexicon "mai1"  =
    [Cat "_"       "PP" [From]  [Cat "mai1" "VP" [] [],      -- buy
                                 Cat "_" "NP"    [] []]]
lexicon "mai2"  =
    [Cat "mai2"    "VP" [To]    [Cat "_" "NP"    [] [],      -- sell
                                 Cat "_" "PP"    [] []]]
lexicon "ti"    =
    [Cat "ti"      "PP" [With]  [Cat "ti" "VP"   [] [],      -- kick
                                 Cat "_" "NP"    [] []]]
lexicon "na"    =
    [Cat "_"       "PP" [From]  [Cat "na" "VP"   [] [],      -- take
                                 Cat "_" "NP"    [] []]]

lexicon "yong"  = [Cat "yong"    "PREP" [With] []]  -- with
lexicon "xiang" = [Cat "xiang"   "PREP" [To]   []]  -- to
lexicon "cong"  = [Cat "cong"    "PREP" [From] []]  -- from

lexicon _ = []
