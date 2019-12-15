module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = Masc  | Fem  | Neutr | MascOrFem 
          | Sg    | Pl 
          | Fst   | Snd  | Thrd 
          | Nom   | AccOrDat 
          | Pers  | Refl | Wh 
          | Past  | Pres | Fut   | Perf | Infl
          | On    | With | By    | To   | From  
          deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]

lexicon "i"    = [Cat "i"    "NP" [Pers,Fst,Sg,Nom]            []]
lexicon "me"   = [Cat "me"   "NP" [Pers,Fst,Sg,AccOrDat]       []]
lexicon "we"   = [Cat "we"   "NP" [Pers,Fst,Pl,Nom]            []]
lexicon "us"   = [Cat "us"   "NP" [Pers,Fst,Pl,AccOrDat]       []]
lexicon "you"  = [Cat "you"  "NP" [Pers,Snd]                   []]
lexicon "he"   = [Cat "he"   "NP" [Pers,Thrd,Sg,Nom,Masc]      []]
lexicon "him"  = [Cat "him"  "NP" [Pers,Thrd,Sg,AccOrDat,Masc] []]
lexicon "she"  = [Cat "she"  "NP" [Pers,Thrd,Sg,Nom,Fem]       []]
lexicon "her"  = [Cat "her"  "NP" [Pers,Thrd,Sg,AccOrDat,Fem]  []]
lexicon "it"   = [Cat "it"   "NP" [Pers,Thrd,Sg,Neutr]         []]
lexicon "they" = [Cat "they" "NP" [Pers,Thrd,Pl,Nom]           []]
lexicon "them" = [Cat "them" "NP" [Pers,Thrd,Pl,AccOrDat]      []]

lexicon "myself"     = [Cat "myself"     "NP" [Refl,Sg,Fst,AccOrDat]        []]
lexicon "ourselves"  = [Cat "ourselves"  "NP" [Refl,Pl,Fst,AccOrDat]        []]
lexicon "yourself"   = [Cat "yourself"   "NP" [Refl,Sg,Snd,AccOrDat]        []]
lexicon "yourselves" = [Cat "yourselves" "NP" [Refl,Pl,Snd,AccOrDat]        []]
lexicon "himself"    = [Cat "himself"    "NP" [Refl,Sg,Thrd,AccOrDat,Masc]  []]
lexicon "herself"    = [Cat "herself"    "NP" [Refl,Sg,Thrd,AccOrDat,Fem]   []]
lexicon "itself"     = [Cat "itself"     "NP" [Refl,Sg,Thrd,AccOrDat,Neutr] []]
lexicon "themselves" = [Cat "themselves" "NP" [Refl,Pl,Thrd,AccOrDat]       []]

--lexicon "who"   = [Cat "who"   "NP"  [Wh,Thrd,MascOrFem]             [], 
--                   Cat "who"   "REL" [MascOrFem]                     []]
--lexicon "whom"  = [Cat "whom"  "NP"  [Sg,Wh,Thrd,AccOrDat,MascOrFem] [], 
--                   Cat "whom"  "REL" [Sg,MascOrFem,AccOrDat]         []]
--lexicon "what"  = [Cat "what"  "NP"  [Wh,Thrd,AccOrDat,Neutr]        []]
--lexicon "that"  = [Cat "that"  "REL" []                              [], 
--                   Cat "that"  "DET" [Sg]                            []]
--lexicon "which" = [Cat "which" "REL" [Neutr]                         [], 
--                   Cat "which" "DET" [Wh]                            []]

lexicon "snowwhite"  = [Cat "snowwhite"  "NP" [Thrd,Fem,Sg]  []]
lexicon "alice"      = [Cat "alice"      "NP" [Thrd,Fem,Sg]  []]
lexicon "dorothy"    = [Cat "dorothy"    "NP" [Thrd,Fem,Sg]  []]
lexicon "goldilocks" = [Cat "goldilocks" "NP" [Thrd,Fem,Sg]  []]
lexicon "littlemook" = [Cat "littlemook" "NP" [Thrd,Masc,Sg] []]
lexicon "atreyu"     = [Cat "atreyu"     "NP" [Thrd,Masc,Sg] []]

lexicon "every"   = [Cat "every"   "DET" [Sg] []]
lexicon "all"     = [Cat "all"     "DET" [Pl] []]
lexicon "some"    = [Cat "some"    "DET" []   []]
lexicon "several" = [Cat "several" "DET" [Pl] []]
lexicon "a"       = [Cat "a"       "DET" [Sg] []]
lexicon "no"      = [Cat "no"      "DET" []   []]
lexicon "the"     = [Cat "the"     "DET" []   []]

lexicon "most"  = [Cat "most"  "DET" [Pl] []]
lexicon "many"  = [Cat "many"  "DET" [Pl] []]
lexicon "few"   = [Cat "few"   "DET" [Pl] []]
lexicon "this"  = [Cat "this"  "DET" [Sg] []]
lexicon "these" = [Cat "these" "DET" [Pl] []]
lexicon "those" = [Cat "those" "DET" [Pl] []]

--lexicon "less_than" = [Cat "less_than" "DF" [Pl] []]
--lexicon "more_than" = [Cat "more_than" "DF" [Pl] []]

lexicon "thing"      = [Cat "thing"      "CN" [Sg,Neutr,Thrd] []]
lexicon "things"     = [Cat "things"     "CN" [Pl,Neutr,Thrd] []]
lexicon "person"     = [Cat "person"     "CN" [Sg,Masc,Thrd]  []]
lexicon "persons"    = [Cat "persons"    "CN" [Pl,Masc,Thrd]  []]
lexicon "boy"        = [Cat "boy"        "CN" [Sg,Masc,Thrd]  []]
lexicon "boys"       = [Cat "boys"       "CN" [Pl,Masc,Thrd]  []]
lexicon "man"        = [Cat "man"        "CN" [Sg,Masc,Thrd]  []]
lexicon "men"        = [Cat "men"        "CN" [Pl,Masc,Thrd]  []]
lexicon "girl"       = [Cat "girl"       "CN" [Sg,Fem,Thrd]   []]
lexicon "girls"      = [Cat "girls"      "CN" [Pl,Fem,Thrd]   []]
lexicon "woman"      = [Cat "woman"      "CN" [Sg,Fem,Thrd]   []]
lexicon "women"      = [Cat "women"      "CN" [Pl,Fem,Thrd]   []]
lexicon "princess"   = [Cat "princess"   "CN" [Sg,Fem,Thrd]   []]
lexicon "princesses" = [Cat "princesses" "CN" [Pl,Fem,Thrd]   []]
lexicon "dwarf"      = [Cat "dwarf"      "CN" [Sg,Masc,Thrd]  []]
lexicon "dwarfs"     = [Cat "dwarfs"     "CN" [Pl,Masc,Thrd]  []]
lexicon "dwarves"    = [Cat "dwarves"    "CN" [Pl,Masc,Thrd]  []]
lexicon "giant"      = [Cat "giant"      "CN" [Sg,Masc,Thrd]  []]
lexicon "giants"     = [Cat "giants"     "CN" [Pl,Masc,Thrd]  []]

lexicon "wizard"  = [Cat "wizard"  "CN" [Sg,Masc,Thrd]  []]
lexicon "wizards" = [Cat "wizards" "CN" [Pl,Masc,Thrd]  []]
lexicon "sword"   = [Cat "sword"   "CN" [Sg,Neutr,Thrd] []]
lexicon "swords"  = [Cat "swords"  "CN" [Pl,Neutr,Thrd] []]
lexicon "dagger"  = [Cat "dagger"  "CN" [Sg,Neutr,Thrd] []]
lexicon "daggers" = [Cat "daggers" "CN" [Pl,Neutr,Thrd] []]

--lexicon "did"    = [Cat "did"    "AUX" [] []]
--lexicon "didn't" = [Cat "didn't" "AUX" [] []]

lexicon "smiled"         = [Cat "smiled"         "VP" [Past]         []]
lexicon "will_smile"     = [Cat "will_smile"     "VP" [Fut]          []]
lexicon "smile"          = [Cat "smile"          "VP" [Infl]         [],
                            Cat "smile"          "VP" [Pres,Pl]      [],
                            Cat "smile"          "VP" [Pres,Sg,Fst]  [],
                            Cat "smile"          "VP" [Pres,Sg,Snd]  []]
lexicon "smiles"         = [Cat "smiles"         "VP" [Pres,Sg,Thrd] []]
lexicon "have_smiled"    = [Cat "have_smiled"    "VP" [Perf,Pl]      [],
                            Cat "have_smiled"    "VP" [Perf,Sg,Fst]  [],
                            Cat "have_smiled"    "VP" [Perf,Sg,Snd]  []]
lexicon "has_smiled"     = [Cat "has_smiled"     "VP" [Perf,Sg,Thrd] []]
lexicon "laughed"        = [Cat "laughed"        "VP" [Past]         []]
lexicon "will_laugh"     = [Cat "will_laugh"     "VP" [Fut]          []]
lexicon "laugh"          = [Cat "laugh"          "VP" [Infl]         [],
                            Cat "laugh"          "VP" [Pres,Pl]      [],
                            Cat "laugh"          "VP" [Pres,Sg,Fst]  [],
                            Cat "laugh"          "VP" [Pres,Sg,Snd]  []]
lexicon "laughs"         = [Cat "laughs"         "VP" [Pres,Sg,Thrd] []]
lexicon "have_laughed"   = [Cat "have_laughed"   "VP" [Perf,Pl]      [],
                            Cat "have_laughed"   "VP" [Perf,Sg,Fst]  [],
                            Cat "have_laughed"   "VP" [Perf,Sg,Snd]  []]
lexicon "has_laughed"    = [Cat "has_laughed"    "VP" [Perf,Sg,Thrd] []]
lexicon "cheered"        = [Cat "cheered"        "VP" [Past]         []]
lexicon "will_cheer"     = [Cat "will_cheer"     "VP" [Fut]          []]
lexicon "cheer"          = [Cat "cheer"          "VP" [Infl]         [],
                            Cat "cheer"          "VP" [Pres,Pl]      [],
                            Cat "cheer"          "VP" [Pres,Sg,Fst]  [],
                            Cat "cheer"          "VP" [Pres,Sg,Snd]  []]
lexicon "cheers"         = [Cat "cheers"         "VP" [Pres,Sg,Thrd] []]
lexicon "have_cheered"   = [Cat "have_cheered"   "VP" [Perf,Pl]      [],
                            Cat "have_cheered"   "VP" [Perf,Sg,Fst]  [],
                            Cat "have_cheered"   "VP" [Perf,Sg,Snd]  []]
lexicon "has_cheered"    = [Cat "has_cheered"    "VP" [Perf,Sg,Thrd] []]
lexicon "shuddered"      = [Cat "shuddered"      "VP" [Past]         []]
lexicon "will_shudder"   = [Cat "will_shudder"   "VP" [Fut]          []]
lexicon "shudder"        = [Cat "shudder"        "VP" [Infl]         [],
                            Cat "shudder"        "VP" [Pres,Pl]      [],
                            Cat "shudder"        "VP" [Pres,Sg,Fst]  [],
                            Cat "shudder"        "VP" [Pres,Sg,Snd]  []]
lexicon "shudders"       = [Cat "shudders"       "VP" [Pres,Sg,Thrd] []]
lexicon "have_shuddered" = [Cat "have_shuddered" "VP" [Perf,Pl]      [],
                            Cat "have_shuddered" "VP" [Perf,Sg,Fst]  [],
                            Cat "have_shuddered" "VP" [Perf,Sg,Snd]  []]
lexicon "has_shuddered"  = [Cat "has_shuddered"  "VP" [Perf,Sg,Thrd] []]

lexicon "loved"        =
 [Cat "loved"        "VP" [Past]         [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_love"    =
 [Cat "will_love"    "VP" [Fut]          [Cat "_" "NP" [AccOrDat] []]]
lexicon "love"         =
 [Cat "love"         "VP" [Infl]         [Cat "_" "NP" [AccOrDat] []],
  Cat "love"         "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "love"         "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "love"         "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "loves"        =
 [Cat "loves"        "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_loved"   =
 [Cat "have_loved"   "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "have_loved"   "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "have_loved"   "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_loved"    =
 [Cat "has_loved"    "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "admired"      =
 [Cat "admired"      "VP" [Past]         [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_admire"  =
 [Cat "will_admire"  "VP" [Fut]          [Cat "_" "NP" [AccOrDat] []]]
lexicon "admire"       =
 [Cat "admire"       "VP" [Infl]         [Cat "_" "NP" [AccOrDat] []],
  Cat "admire"       "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "admire"       "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "admire"       "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "admires"      =
 [Cat "admires"      "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_admired" =
 [Cat "have_admired" "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "have_admired" "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "have_admired" "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_admired"  =
 [Cat "has_admired"  "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "helped"        =
 [Cat "helped"        "VP" [Past]         [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_help"     =
 [Cat "will_help"     "VP" [Fut]          [Cat "_" "NP" [AccOrDat] []]]
lexicon "help"          =
 [Cat "help"          "VP" [Infl]         [Cat "_" "NP" [AccOrDat] []],
  Cat "help"          "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "help"          "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "help"          "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "helps"         =
 [Cat "helps"         "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_helped"   =
 [Cat "have_helped"   "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "have_helped"   "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "have_helped"   "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_helped"    =
 [Cat "has_helped"    "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeated"      =
 [Cat "defeated"      "VP" [Past]         [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_defeat"   =
 [Cat "will_defeat"   "VP" [Fut]          [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeat"        =
 [Cat "defeat"        "VP" [Infl]         [Cat "_" "NP" [AccOrDat] []],
  Cat "defeat"        "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "defeat"        "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "defeat"        "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeats"       =
 [Cat "defeats"       "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_defeated" =
 [Cat "have_defeated" "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "have_defeated" "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "have_defeated" "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_defeated"  =
 [Cat "has_defeated"  "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

lexicon "gave"       =
 [Cat "gave"       "VP" [Past]         [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "gave"       "VP" [Past]         [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]
lexicon "will_give"  =
 [Cat "will_give"  "VP" [Fut]          [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "will_give"  "VP" [Fut]          [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]
lexicon "give"       =
 [Cat "give"       "VP" [Infl]         [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "give"       "VP" [Infl]         [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []],
  Cat "give"       "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "give"       "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []],
  Cat "give"       "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "give"       "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []],
  Cat "give"       "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "give"       "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]
lexicon "gives"      =
 [Cat "gives"      "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "gives"      "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]
lexicon "have_given" =
 [Cat "have_given" "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "have_given" "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []],
  Cat "have_given" "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "have_given" "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []],
  Cat "have_given" "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "have_given" "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]
lexicon "has_given"  =
 [Cat "has_given"  "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "has_given"  "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]
lexicon "sold"       =
 [Cat "sold"       "VP" [Past]         [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "sold"       "VP" [Past]         [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]
lexicon "will_sell"  =
 [Cat "will_sell"  "VP" [Fut]          [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "will_sell"  "VP" [Fut]          [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]
lexicon "sell"       =
 [Cat "sell"       "VP" [Infl]         [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "sell"       "VP" [Infl]         [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []],
  Cat "sell"       "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "sell"       "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []],
  Cat "sell"       "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "sell"       "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []],
  Cat "sell"       "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "sell"       "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]
lexicon "sells"      =
 [Cat "sells"      "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "sells"      "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]
lexicon "have_sold"  =
 [Cat "have_sold"  "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "have_sold"  "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []],
  Cat "have_sold"  "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "have_sold"  "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []],
  Cat "have_sold"  "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "have_sold"  "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]
lexicon "has_sold"   =
 [Cat "has_sold"   "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "PP" [To]       []],
  Cat "has_sold"   "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                        Cat "_" "NP" [AccOrDat] []]]

lexicon "kicked"      =
 [Cat "kicked"      "VP" [Past]         [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [With]     []],
  Cat "kicked"      "VP" [Past]         [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_kick"   =
 [Cat "will_kick"   "VP" [Fut]          [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [With]     []],
  Cat "will_kick"   "VP" [Fut]          [Cat "_" "NP" [AccOrDat] []]]
lexicon "kick"        =
 [Cat "kick"        "VP" [Infl]         [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [With]     []],
  Cat "kick"        "VP" [Infl]         [Cat "_" "NP" [AccOrDat] []],
  Cat "kick"        "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [With]     []],
  Cat "kick"        "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "kick"        "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [With]     []],
  Cat "kick"        "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "kick"        "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [With]     []],
  Cat "kick"        "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "kicks"       =
 [Cat "kicks"       "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [With]     []],
  Cat "kicks"       "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_kicked" =
 [Cat "have_kicked" "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [With]     []],
  Cat "have_kicked" "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "have_kicked" "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [With]     []],
  Cat "have_kicked" "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "have_kicked" "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [With]     []],
  Cat "have_kicked" "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_kicked"  =
 [Cat "has_kicked"  "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [With]     []],
  Cat "has_kicked"  "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "took"        =
 [Cat "took"        "VP" [Past]         [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [From]     []],
  Cat "took"        "VP" [Past]         [Cat "_" "NP" [AccOrDat] []]]
lexicon "will_take"   =
 [Cat "will_take"   "VP" [Fut]          [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [From]     []],
  Cat "will_take"   "VP" [Fut]          [Cat "_" "NP" [AccOrDat] []]]
lexicon "take"        =
 [Cat "take"        "VP" [Infl]         [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [From]     []],
  Cat "take"        "VP" [Infl]         [Cat "_" "NP" [AccOrDat] []],
  Cat "take"        "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [From]     []],
  Cat "take"        "VP" [Pres,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "take"        "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [From]     []],
  Cat "take"        "VP" [Pres,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "take"        "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [From]     []],
  Cat "take"        "VP" [Pres,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "takes"       =
 [Cat "takes"       "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [From]     []],
  Cat "takes"       "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]
lexicon "have_taken"  =
 [Cat "have_taken"  "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [From]     []],
  Cat "have_taken"  "VP" [Perf,Pl]      [Cat "_" "NP" [AccOrDat] []],
  Cat "have_taken"  "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [From]     []],
  Cat "have_taken"  "VP" [Perf,Sg,Fst]  [Cat "_" "NP" [AccOrDat] []],
  Cat "have_taken"  "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [From]     []],
  Cat "have_taken"  "VP" [Perf,Sg,Snd]  [Cat "_" "NP" [AccOrDat] []]]
lexicon "has_taken"   =
 [Cat "has_taken"   "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [],
                                         Cat "_" "PP" [From]     []],
  Cat "has_taken"   "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]]

--lexicon "on"   = [Cat "on"   "PREP" [On]   []]
lexicon "with" = [Cat "with" "PREP" [With] []]
--lexicon "by"   = [Cat "by"   "PREP" [By]   []]
lexicon "to"   = [Cat "to"   "PREP" [To]   []]
lexicon "from" = [Cat "from" "PREP" [From] []]

--lexicon "and"  = [Cat "and"  "CONJ" [] []]
--lexicon "."    = [Cat "."    "CONJ" [] []]
--lexicon "if"   = [Cat "if"   "COND" [] []]
--lexicon "then" = [Cat "then" "THEN" [] []]

lexicon _ = []
