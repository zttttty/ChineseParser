module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = Masc  | Fem  | Neutr
            | Dfree | Dban                                -- Det marker (Dfree: NP can be modified by Det, vice versa)
            | De1 |  De2 |  De4                           -- structure particles
            | Past  |  PresCon  |  Fut                    -- tense (past, present continuous, future)
            | Ma  |  Me  |  Ne  |  Ba                     -- question markers
            | Ah |  Ya |  Wa                              -- exclamation markers
            | At  |  With  |  From | To                   -- prepositions
            deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]

lexicon "wo"    = [Cat "wo"       "NP" [Dban]         []]      -- i/me 我
lexicon "women" = [Cat "women"    "NP" [Dban]         []]      -- we/us 我们
lexicon "ni"    = [Cat "ni"       "NP" [Dban]         []]      -- you 你
lexicon "ta1"   = [Cat "ta1"      "NP" [Masc,Dban]    []]      -- he/him 他
lexicon "ta2"   = [Cat "ta2"      "NP" [Fem,Dban]     []]      -- she/her 她
lexicon "ta3"   = [Cat "ta3"      "NP" [Neutr,Dban]   []]      -- it 它
lexicon "tamen1" = [Cat "tamen1"  "NP" [Dban]         []]      -- they/them (general animate)他们
lexicon "tamen2" = [Cat "tamen2"  "NP" [Fem,Dban]     []]      -- they/them (only fem) 她们
lexicon "tamen3" = [Cat "tamen3"  "NP" [Neutr,Dban]   []]      -- they/them (only inanimate)它们

lexicon "taziji1"  = [Cat "taziji1"    "NP" [Masc,Dban]        []]       -- himself 他自己
lexicon "taziji2"  = [Cat "taziji2"    "NP" [Fem,Dban]         []]       -- herself 她自己
lexicon "taziji3"  = [Cat "taziji3"    "NP" [Neutr,Dban]       []]       -- itself 它自己

lexicon "baixuegongzhu"  = [Cat "baixuegongzhu"  "NP" [Fem,Dfree]  []]    -- snowwhite 白雪公主
lexicon "ailisi"         = [Cat "ailisi"         "NP" [Fem,Dfree]  []]    -- alice 爱丽斯
lexicon "duoluoxi"       = [Cat "duoluoxi"       "NP" [Fem,Dfree]  []]    -- dorothy 多萝西
lexicon "jinfaguniang"   = [Cat "jinfaguniang"   "NP" [Fem,Dfree]  []]    -- goldilocks 金发姑娘
lexicon "xioamuke"       = [Cat "xiaomuke"       "NP" [Masc,Dfree] []]    -- littlemook 小穆克
lexicon "ateleiyou"      = [Cat "ateleiyou"      "NP" [Masc,Dfree] []]    -- atreyu 阿特雷尤

lexicon "meige"      = [Cat "meige"     "DET" [Dfree] []]      -- every 每个
lexicon "suoyou"     = [Cat "suoyou"    "DET" [Dfree] []]      -- all 所有
lexicon "yixie"      = [Cat "yixie"     "DET" [Dfree] []]      -- some 一些
lexicon "yige"       = [Cat "yige"      "DET" [Dfree] []]      -- a 一个
lexicon "meiyou"     = [Cat "meiyou"    "DET" [Dfree] []]      -- no 没有
lexicon "henduo"     = [Cat "henduo"    "DET" [Dfree] []]      -- many 很多
lexicon "henshao"    = [Cat "henshao"   "DET" [Dfree] []]      -- few 很少
lexicon "zhege"      = [Cat "zhege"     "DET" [Dfree] []]      -- this 这个
lexicon "zhexie"     = [Cat "zhexie"    "DET" [Dfree] []]      -- these 这些

lexicon "ren"           = [Cat "ren"        "NP" [Dfree]           []]   -- person 人
lexicon "nanhai"        = [Cat "nanhai"     "NP" [Masc,Dfree]      []]   -- boy 男孩
lexicon "nanhaimen"     = [Cat "nanhaimen"  "NP" [Masc,Dban]       []]   -- boys 男孩们
lexicon "nvhai"         = [Cat "nvhai"      "NP" [Fem,Dfree]       []]   -- girl 女孩
lexicon "nvhaimen"      = [Cat "nvhaimen"   "NP" [Fem,Dban]        []]   -- girls 女孩们
lexicon "gongzhu"       = [Cat "gongzhu"    "NP" [Fem,Dfree]       []]   -- princess 公主
lexicon "gongzhumen"    = [Cat "gongzhumen" "NP" [Fem,Dban]        []]   -- princesses 公主们
lexicon "juren"         = [Cat "juren"      "NP" [Masc,Dfree]      []]   -- giant 巨人
lexicon "jurenmen"      = [Cat "jurenmen"   "NP" [Masc,Dban]       []]   -- giants 巨人们

lexicon "jian"        = [Cat "jian"        "NP" [Neutr,Dfree]    []]  -- sword/sowrds 剑
lexicon "dao"         = [Cat "dao"         "NP" [Neutr,Dfree]    []]  -- dagger/daggers 刀
lexicon "tuoxie"      = [Cat "tuoxie"      "NP" [Neutr,Dfree]    []]  -- slipper 拖鞋
lexicon "ziliao"      = [Cat "ziliao"      "NP" [Neutr,Dfree]    []]  -- resourse 资料
lexicon "pingguo"     = [Cat "pingguo"     "NP" [Neutr,Dfree]    []]  -- apple 苹果
lexicon "diannao"     = [Cat "diannao"     "NP" [Neutr,Dfree]    []]  -- computer 电脑

lexicon "weixiao"        = [Cat "weixiao"        "VP" []       []]   -- smile 微笑

lexicon "xiao"           = [Cat "xiao"           "VP" []       []]   -- laugh 笑

lexicon "shuijiao"       = [Cat "shuijiao"       "VP" []       []]   -- sleep 睡觉

lexicon "ai"       =
    [Cat "ai"       "VP"  []    [Cat "_" "NP"  [] []]]   -- love 爱

lexicon "jingpei"  =
    [Cat "jingpei"  "VP" []    [Cat "_" "NP" [] []]]   -- admire 敬佩

lexicon "bangzhu"  =
    [Cat "bangzhu"  "VP" []    [Cat "_" "NP" [] []]]   -- help 帮助

lexicon "dabai"    =
    [Cat "dabai"    "VP" []    [Cat "_" "NP" [] []]]   -- defeat 打败

lexicon "chazhao"  =
    [Cat "chazhao"  "VP" []    [Cat "_" "NP"  [] []]]   -- search 查找

lexicon "mai"  =
    [Cat "mai"      "VP" []    [Cat "_" "NP"    [] []]]  -- sell 卖

lexicon "ti"    =
    [Cat "ti"      "VP" []     [Cat "_" "NP"    [] []]]  -- kick 踢

lexicon "na"    =
    [Cat "na"      "VP" []     [Cat "_" "NP"    [] []]]  -- take 拿

lexicon "yong"  = [Cat "yong"    "PREP" [With] [Cat "ti"      "VP"    [] []]]    -- with 用

lexicon "xiang" = [Cat "xiang"   "PREP" [To]   [Cat "mai"      "VP"    [] []]]    -- to 从
lexicon "cong"  = [Cat "cong"    "PREP" [From] [Cat "na"       "VP"    [] []]]    -- from 向

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
