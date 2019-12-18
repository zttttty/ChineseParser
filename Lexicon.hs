module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = Masc  | Fem  | Neutr
            | Sg    | Pl
            | Fst   | Snd  | Thrd
            | Pers  | Refl
            | De1 |  De2 |  De4                           -- structure particles
            | Past  |  PresCon  |  Fut                    -- tense (past, present continuous, future)
            | Ma  |  Me  |  Ne  |  Ba                     -- question markers
            | Ah |  Ya |  Wa                              -- exclamation markers
            | At  |  With  |  From | To                   -- prepositions
            deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]

lexicon "wo"    = [Cat "wo"    "NP" [Pers,Fst,Sg]          []]      -- i/me 我
lexicon "women" = [Cat "women" "NP" [Pers,Fst,Pl]          []]      -- we/us 我们
lexicon "ni"    = [Cat "ni"    "NP" [Pers,Snd]             []]      -- you 你
lexicon "ta1"   = [Cat "ta1"   "NP" [Pers,Thrd,Sg,Masc]    []]      -- he/him 他
lexicon "ta2"   = [Cat "ta2"   "NP" [Pers,Thrd,Sg,Fem]     []]      -- she/her 她
lexicon "ta3"   = [Cat "ta3"   "NP" [Pers,Thrd,Sg,Neutr]   []]      -- it 它
lexicon "tamen1" = [Cat "tamen1" "NP" [Pers,Thrd,Pl]       []]      -- they/them (general animate)他们
lexicon "tamen2" = [Cat "tamen2" "NP" [Pers,Thrd,Pl,Fem]   []]      -- they/them (only fem) 她们
lexicon "tamen3" = [Cat "tamen3" "NP" [Pers,Thrd,Pl,Neutr] []]      -- they/them (only inanimate)它们

lexicon "woziji"     = [Cat "woziji"    "NP" [Refl,Sg,Fst]        []]       -- myself 我自己
lexicon "womenziji"  = [Cat "womenziji" "NP" [Refl,Pl,Fst]        []]       -- ourselves 我们自己
lexicon "niziji"     = [Cat "niziji"    "NP" [Refl,Sg,Snd]        []]       -- yourself 你自己
lexicon "nimenziji"  = [Cat "nimenziji" "NP" [Refl,Pl,Snd]        []]       -- yourselves 你们自己
lexicon "ta1ziji"    = [Cat "ta1ziji"   "NP" [Refl,Sg,Thrd,Masc]  []]       -- himself 他自己
lexicon "ta2ziji"    = [Cat "ta2ziji"   "NP" [Refl,Sg,Thrd,Fem]   []]       -- herself 她自己
lexicon "ta3ziji"    = [Cat "ta3ziji"   "NP" [Refl,Sg,Thrd,Neutr] []]       -- itself 它自己
lexicon "tamenziji"  = [Cat "tamenziji" "NP" [Refl,Pl,Thrd]       []]       -- themselves (general animate) 他们自己
lexicon "tamenziji2" = [Cat "tamenziji2" "NP" [Refl,Pl,Thrd,Fem]  []]       -- themselves (only fem) 她们自己
lexicon "tamenziji3" = [Cat "tamenziji3" "NP" [Refl,Pl,Thrd,Neutr][]]       -- themselves (only inanimate)它们自己

lexicon "baixuegongzhu"  = [Cat "baixuegongzhu"  "NP" [Thrd,Fem,Sg]  []]    -- snowwhite 白雪公主
lexicon "ailisi"         = [Cat "ailisi"         "NP" [Thrd,Fem,Sg]  []]    -- alice 爱丽斯
lexicon "duoluoxi"       = [Cat "duoluoxi"       "NP" [Thrd,Fem,Sg]  []]    -- dorothy 多萝西
lexicon "jinfaguniang"   = [Cat "jinfaguniang"   "NP" [Thrd,Fem,Sg]  []]    -- goldilocks 金发姑娘
lexicon "xioamuke"       = [Cat "xiaomuke"       "NP" [Thrd,Masc,Sg] []]    -- littlemook 小穆克
lexicon "ateleiyou"      = [Cat "ateleiyou"      "NP" [Thrd,Masc,Sg] []]    -- atreyu 阿特雷尤

lexicon "meige"      = [Cat "meige"     "DET" [Sg] []]      -- every 每个
lexicon "suoyou"     = [Cat "suoyou"    "DET" [Pl] []]      -- all 所有
lexicon "yixie"      = [Cat "yixie"     "DET" [Pl] []]      -- some 一些
lexicon "yige"       = [Cat "yige"      "DET" [Sg] []]      -- a 一个
lexicon "meiyou"     = [Cat "meiyou"    "DET" []   []]      -- no 没有
lexicon "dabufen"    = [Cat "dabufen"   "DET" [Pl] []]      -- most 大部分
lexicon "henduo"     = [Cat "henduo"    "DET" [Pl] []]      -- many 很多
lexicon "henshao"    = [Cat "henshao"   "DET" [Pl] []]      -- few 很少
lexicon "zhege"      = [Cat "zhege"     "DET" [Sg] []]      -- this 这个
lexicon "nage"       = [Cat "nage"      "DET" [Sg] []]      -- that 那个
lexicon "zhexie"     = [Cat "zhexie"    "DET" [Pl] []]      -- these 这些
lexicon "naxie"      = [Cat "naxie"     "DET" [Pl] []]      -- those 那些

lexicon "dongxi"        = [Cat "dongxi"     "CN" [Neutr,Thrd]     []]   -- thing 东西
lexicon "ren"           = [Cat "ren"        "CN" [Masc,Thrd]      []]   -- person 人
lexicon "nanhai"        = [Cat "nanhai"     "CN" [Masc,Thrd]      []]   -- boy 男孩
lexicon "nanhaimen"     = [Cat "nanhaimen"  "CN" [Pl,Masc,Thrd]   []]   -- boys 男孩们
lexicon "nanren"        = [Cat "nanren"     "CN" [Sg,Masc,Thrd]   []]   -- man 男人
lexicon "nanrenmen"     = [Cat "nanrenmen"  "CN" [Pl,Masc,Thrd]   []]   -- men 男人们
lexicon "nvhai"         = [Cat "nvhai"      "CN" [Sg,Fem,Thrd]    []]   -- girl 女孩
lexicon "nvhaimen"      = [Cat "nvhaimen"   "CN" [Pl,Fem,Thrd]    []]   -- girls 女孩们
lexicon "nvren"         = [Cat "nvren"      "CN" [Sg,Fem,Thrd]    []]   -- woman 女人
lexicon "nvrenmen"      = [Cat "nvrenmen"   "CN" [Pl,Fem,Thrd]    []]   -- women 女人们
lexicon "gongzhu"       = [Cat "gongzhu"    "CN" [Sg,Fem,Thrd]    []]   -- princess 公主
lexicon "gongzhumen"    = [Cat "gongzhumen" "CN" [Pl,Fem,Thrd]    []]   -- princesses 公主们
lexicon "xiaoairen"     = [Cat "xiaoairen"    "CN" [Sg,Masc,Thrd] []]   -- dwarf 小矮人
lexicon "xiaoairenmen"  = [Cat "xiaoairenmen" "CN" [Pl,Masc,Thrd] []]   -- dwarves 小矮人们
lexicon "juren"         = [Cat "juren"      "CN" [Sg,Masc,Thrd]   []]   -- giant 巨人
lexicon "jurenmen"      = [Cat "jurenmen"   "CN" [Pl,Masc,Thrd]   []]   -- giants 巨人们

lexicon "wushi"    = [Cat "wushi"    "CN" [Sg,Masc,Thrd]  []]  -- wizard 巫师
lexicon "wushimen" = [Cat "wushimen" "CN" [Pl,Masc,Thrd]  []]  -- wizards 巫师们
lexicon "jian"     = [Cat "jian"     "CN" [Neutr,Thrd]    []]  -- sword/sowrds 剑
lexicon "dao"      = [Cat "dao"      "CN" [Neutr,Thrd]    []]  -- dagger/daggers 刀

lexicon "weixiao"        = [Cat "weixiao"        "VP" []       []]   -- smile 微笑
lexicon "xiao"           = [Cat "xiao"           "VP" []       []]   -- laugh 笑
lexicon "shuijiao"       = [Cat "shuijiao"       "VP" []       []]   -- sleep 睡觉

lexicon "ai"       =
    [Cat "ai"       "VP"  []    [Cat "_" "NP"  [] []]]   -- love 爱

lexicon "jingpei"  =
    [Cat "jingpei"    "VP" []    [Cat "_" "NP" [] []]]   -- admire 敬佩

lexicon "bangzhu"  =
    [Cat "bangzhu"    "VP" []    [Cat "_" "NP" [] []]]   -- help 帮助

lexicon "dabai"  =
   [Cat "dabai"    "VP" []    [Cat "_" "NP" [] []]]   -- defeat 打败

lexicon "gei"  =
    [Cat "gei"     "VP" []      [Cat "_" "NP"    [] [],      -- give 给
                                 Cat "_" "NP"    [] []]]
lexicon "mai1"  =
    [Cat "_"       "PP" [From]  [Cat "mai1" "VP" [] [],      -- buy 买
                                 Cat "_" "NP"    [] []]]
lexicon "mai2"  =
    [Cat "mai2"    "VP" [To]    [Cat "_" "NP"    [] [],      -- sell
                                 Cat "_" "PP"    [] []]]
lexicon "ti"    =
    [Cat "_"      "VP" []  [Cat "_" "NP"    [] []],
     Cat "_"      "VP" [With]  [Cat "_" "NP"    [] []]]
     
lexicon "na"    =
    [Cat "_"       "PP" [From]  [Cat "na" "VP"   [] [],      -- take
                                 Cat "_" "NP"    [] []]]

lexicon "yong"  = [Cat "yong"    "PREP" [With] []]  -- with
lexicon "xiang" = [Cat "xiang"   "PREP" [To]   []]  -- to
lexicon "cong"  = [Cat "cong"    "PREP" [From] []]  -- from

lexicon "de1"   = [Cat "de1"     "STRU" [De1]  []]  -- structure particles
lexicon "de2"   = [Cat "de2"     "STRU" [De2]  []]
lexicon "de4"   = [Cat "de4"     "STRU" [De4]  []]

lexicon "le"    = [Cat "le"      "TEN"  [Past] []]  -- tense particles
lexicon "zhe"   = [Cat "zhe"     "TEN"  [PresCon]  []]
lexicon "yao"   = [Cat "yao"     "TEN"  [Fut]  []]

lexicon "ma"    = [Cat "ma"      "MAK"  [Ma]   []]  -- question markers
lexicon "me"    = [Cat "me"      "MAK"  [Me]   []]
lexicon "ne"    = [Cat "ne"      "MAK"  [Ne]   []]
lexicon "ba"    = [Cat "ba"      "MAK"  [Ba]   []]

lexicon "ah"    = [Cat "ah"      "MAK"  [Ah]   []]  -- exclamation markers
lexicon "ya"    = [Cat "ya"      "MAK"  [Ya]   []]
lexicon "wa"    = [Cat "wa"      "MAK"  [Wa]   []]

lexicon _ = []
