{-# OPTIONS_GHC -w #-}
module PortCapParser
(
  PC(PC),
  parsePortCap
) where

import Data.Char

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
    = HappyTerminal (Token)
    | HappyErrorToken Int
    | HappyAbsSyn4 t4
    | HappyAbsSyn5 t5
    | HappyAbsSyn6 t6
    | HappyAbsSyn7 t7
    | HappyAbsSyn8 t8
    | HappyAbsSyn9 t9
    | HappyAbsSyn10 t10
    | HappyAbsSyn11 t11
    | HappyAbsSyn12 t12

action_0 (14) = happyShift action_10
action_0 (21) = happyShift action_11
action_0 (4) = happyGoto action_12
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 (12) = happyGoto action_9
action_0 _ = happyFail

action_1 (14) = happyShift action_10
action_1 (21) = happyShift action_11
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 (9) = happyGoto action_6
action_1 (10) = happyGoto action_7
action_1 (11) = happyGoto action_8
action_1 (12) = happyGoto action_9
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 _ = happyReduce_9

action_4 _ = happyReduce_3

action_5 _ = happyReduce_4

action_6 _ = happyReduce_5

action_7 _ = happyReduce_6

action_8 _ = happyReduce_7

action_9 _ = happyReduce_8

action_10 _ = happyReduce_10

action_11 (15) = happyShift action_14
action_11 (16) = happyShift action_15
action_11 (17) = happyShift action_16
action_11 (18) = happyShift action_17
action_11 (19) = happyShift action_18
action_11 (20) = happyShift action_19
action_11 _ = happyFail

action_12 (13) = happyShift action_13
action_12 (22) = happyAccept
action_12 _ = happyFail

action_13 (14) = happyShift action_10
action_13 (21) = happyShift action_11
action_13 (5) = happyGoto action_31
action_13 (6) = happyGoto action_3
action_13 (7) = happyGoto action_4
action_13 (8) = happyGoto action_5
action_13 (9) = happyGoto action_6
action_13 (10) = happyGoto action_7
action_13 (11) = happyGoto action_8
action_13 (12) = happyGoto action_9
action_13 _ = happyFail

action_14 _ = happyReduce_20

action_15 (21) = happyShift action_30
action_15 (9) = happyGoto action_28
action_15 (10) = happyGoto action_29
action_15 _ = happyReduce_15

action_16 (21) = happyShift action_27
action_16 (8) = happyGoto action_24
action_16 (9) = happyGoto action_25
action_16 (10) = happyGoto action_26
action_16 _ = happyReduce_11

action_17 (21) = happyShift action_23
action_17 (10) = happyGoto action_22
action_17 _ = happyReduce_18

action_18 _ = happyReduce_23

action_19 (21) = happyShift action_21
action_19 (12) = happyGoto action_20
action_19 _ = happyReduce_21

action_20 _ = happyReduce_22

action_21 (19) = happyShift action_18
action_21 _ = happyFail

action_22 _ = happyReduce_19

action_23 (15) = happyShift action_14
action_23 _ = happyFail

action_24 _ = happyReduce_12

action_25 _ = happyReduce_13

action_26 _ = happyReduce_14

action_27 (15) = happyShift action_14
action_27 (16) = happyShift action_15
action_27 (18) = happyShift action_17
action_27 _ = happyFail

action_28 _ = happyReduce_16

action_29 _ = happyReduce_17

action_30 (15) = happyShift action_14
action_30 (18) = happyShift action_17
action_30 _ = happyFail

action_31 _ = happyReduce_2

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
     =  HappyAbsSyn4
         ([happy_var_1]
    )
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
    _
    (HappyAbsSyn4  happy_var_1)
     =  HappyAbsSyn4
         (happy_var_3 : happy_var_1
    )
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
     =  HappyAbsSyn5
         (happy_var_1
    )
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
     =  HappyAbsSyn5
         (happy_var_1
    )
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
     =  HappyAbsSyn5
         (happy_var_1
    )
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyAbsSyn10  happy_var_1)
     =  HappyAbsSyn5
         (happy_var_1
    )
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 (HappyAbsSyn11  happy_var_1)
     =  HappyAbsSyn5
         (happy_var_1
    )
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 (HappyAbsSyn12  happy_var_1)
     =  HappyAbsSyn5
         (happy_var_1
    )
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 (HappyAbsSyn6  happy_var_1)
     =  HappyAbsSyn5
         (happy_var_1
    )
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 _
     =  HappyAbsSyn6
         (pcnoop
    )

happyReduce_11 = happySpecReduce_2  7 happyReduction_11
happyReduction_11 _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn7
         (pcr happy_var_1
    )
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_3)
    _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn7
         (merge (pcr happy_var_1) happy_var_3
    )
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
    _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn7
         (merge (pcr happy_var_1) happy_var_3
    )
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_3)
    _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn7
         (merge (pcr happy_var_1) happy_var_3
    )
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  8 happyReduction_15
happyReduction_15 _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn8
         (pcrw happy_var_1
    )
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  8 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_3)
    _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn8
         (merge (pcrw happy_var_1) happy_var_3
    )
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  8 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_3)
    _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn8
         (merge (pcr happy_var_1) happy_var_3
    )
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  9 happyReduction_18
happyReduction_18 _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn9
         (pcw happy_var_1
    )
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
    _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn9
         (merge (pcw happy_var_1) happy_var_3
    )
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  10 happyReduction_20
happyReduction_20 _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn10
         (pcru happy_var_1
    )
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  11 happyReduction_21
happyReduction_21 _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn11
         (pcc happy_var_1
    )
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  11 happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_3)
    _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn11
         (merge (pca happy_var_1) happy_var_3
    )
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  12 happyReduction_23
happyReduction_23 _
    (HappyTerminal (TokenInt happy_var_1))
     =  HappyAbsSyn12
         (pca happy_var_1
    )
happyReduction_23 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
    action 22 22 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
    let cont i = action i i tk (HappyState action) sts stk tks in
    case tk of {
    TokenOR -> cont 13;
    TokenNoop -> cont 14;
    TokenRU -> cont 15;
    TokenRW -> cont 16;
    TokenR -> cont 17;
    TokenW -> cont 18;
    TokenA -> cont 19;
    TokenC -> cont 20;
    TokenInt happy_dollar_dollar -> cont 21;
    _ -> happyError' (tk:tks)
    }

happyError_ 22 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token = TokenOR
           | TokenNoop
           | TokenRU
           | TokenRW
           | TokenR
           | TokenW
           | TokenA
           | TokenC
           | TokenInt Int
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isDigit c = let (num,rest) = span isDigit (c:cs)
                  in TokenInt (read num) : lexer rest
    | isAlpha c = lexa (c:cs)

lexa ('O':'R':cs)         = TokenOR   : lexer cs
lexa ('N':'O':'O':'P':cs) = TokenNoop : lexer cs
lexa ('R':'U':cs)         = TokenRU   : lexer cs
lexa ('R':'W':cs)         = TokenRW   : lexer cs
lexa ('R':cs)             = TokenR    : lexer cs
lexa ('W':cs)             = TokenW    : lexer cs
lexa ('A':cs)             = TokenA    : lexer cs
lexa ('C':cs)             = TokenC    : lexer cs

-- r rw w ru c a
data PC = PC Int Int Int Int Int Int deriving (Eq, Ord, Show)

pcr  r  = PC r  0  0  0  0  0
pcrw rw = PC 0  rw 0  0  0  0
pcw  w  = PC 0  0  w  0  0  0
pcru ru = PC 0  0  0  ru 0  0
pcc  c  = PC 0  0  0  0  c  0
pca  a  = PC 0  0  0  0  0  a
pcnoop  = PC 0  0  0  0  0  0

merge (PC r rw w ru c a) (PC r' rw' w' ru' c' a')
    = PC (r+r') (rw+rw') (w+w') (ru+ru') (c+c') (a+a')

parsePortCap :: String -> [PC]
parsePortCap str = reverse $ parser $ lexer $ map toUpper str
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
    happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
     (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
     sts1@(((st1@(HappyState (action))):(_))) ->
            let r = fn stk in  -- it doesn't hurt to always seq here...
               happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--    trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                        (saved_tok `HappyStk` _ `HappyStk` stk) =
--    trace ("discarding state, depth " ++ show (length stk))  $
    action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
    action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--    happySeq = happyDoSeq
-- otherwise it emits
--     happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
