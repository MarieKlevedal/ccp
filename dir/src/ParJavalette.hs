{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParJavalette where
import AbsJavalette
import LexJavalette
import ErrM

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Ident)
	| HappyAbsSyn5 (String)
	| HappyAbsSyn6 (Integer)
	| HappyAbsSyn7 (Double)
	| HappyAbsSyn8 (Program)
	| HappyAbsSyn9 ([Def])
	| HappyAbsSyn10 (Def)
	| HappyAbsSyn11 ([Arg])
	| HappyAbsSyn12 (Arg)
	| HappyAbsSyn13 (Block)
	| HappyAbsSyn14 ([Stm])
	| HappyAbsSyn15 (Stm)
	| HappyAbsSyn16 (Item)
	| HappyAbsSyn17 ([Item])
	| HappyAbsSyn18 (Type)
	| HappyAbsSyn19 ([Type])
	| HappyAbsSyn20 (Exp)
	| HappyAbsSyn27 ([Exp])
	| HappyAbsSyn28 (Lit)
	| HappyAbsSyn29 (AddOp)
	| HappyAbsSyn30 (MulOp)
	| HappyAbsSyn31 (RelOp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (52) = happyShift action_7
action_0 (53) = happyShift action_8
action_0 (57) = happyShift action_9
action_0 (60) = happyShift action_10
action_0 (8) = happyGoto action_3
action_0 (9) = happyGoto action_4
action_0 (10) = happyGoto action_5
action_0 (18) = happyGoto action_6
action_0 _ = happyFail

action_1 (65) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (69) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_5

action_5 (52) = happyShift action_7
action_5 (53) = happyShift action_8
action_5 (57) = happyShift action_9
action_5 (60) = happyShift action_10
action_5 (9) = happyGoto action_12
action_5 (10) = happyGoto action_5
action_5 (18) = happyGoto action_6
action_5 _ = happyReduce_6

action_6 (65) = happyShift action_2
action_6 (4) = happyGoto action_11
action_6 _ = happyFail

action_7 _ = happyReduce_34

action_8 _ = happyReduce_33

action_9 _ = happyReduce_32

action_10 _ = happyReduce_35

action_11 (36) = happyShift action_13
action_11 _ = happyFail

action_12 _ = happyReduce_7

action_13 (52) = happyShift action_7
action_13 (53) = happyShift action_8
action_13 (57) = happyShift action_9
action_13 (60) = happyShift action_10
action_13 (11) = happyGoto action_14
action_13 (12) = happyGoto action_15
action_13 (18) = happyGoto action_16
action_13 _ = happyReduce_9

action_14 (37) = happyShift action_19
action_14 _ = happyFail

action_15 (41) = happyShift action_18
action_15 _ = happyReduce_10

action_16 (65) = happyShift action_2
action_16 (4) = happyGoto action_17
action_16 _ = happyFail

action_17 _ = happyReduce_12

action_18 (52) = happyShift action_7
action_18 (53) = happyShift action_8
action_18 (57) = happyShift action_9
action_18 (60) = happyShift action_10
action_18 (11) = happyGoto action_22
action_18 (12) = happyGoto action_15
action_18 (18) = happyGoto action_16
action_18 _ = happyReduce_9

action_19 (62) = happyShift action_21
action_19 (13) = happyGoto action_20
action_19 _ = happyFail

action_20 _ = happyReduce_8

action_21 (14) = happyGoto action_23
action_21 _ = happyReduce_14

action_22 _ = happyReduce_11

action_23 (32) = happyShift action_39
action_23 (36) = happyShift action_40
action_23 (42) = happyShift action_41
action_23 (45) = happyShift action_42
action_23 (52) = happyShift action_7
action_23 (53) = happyShift action_8
action_23 (55) = happyShift action_43
action_23 (56) = happyShift action_44
action_23 (57) = happyShift action_9
action_23 (58) = happyShift action_45
action_23 (59) = happyShift action_46
action_23 (60) = happyShift action_10
action_23 (61) = happyShift action_47
action_23 (62) = happyShift action_21
action_23 (64) = happyShift action_48
action_23 (65) = happyShift action_2
action_23 (66) = happyShift action_49
action_23 (67) = happyShift action_50
action_23 (68) = happyShift action_51
action_23 (4) = happyGoto action_24
action_23 (5) = happyGoto action_25
action_23 (6) = happyGoto action_26
action_23 (7) = happyGoto action_27
action_23 (13) = happyGoto action_28
action_23 (15) = happyGoto action_29
action_23 (18) = happyGoto action_30
action_23 (20) = happyGoto action_31
action_23 (21) = happyGoto action_32
action_23 (22) = happyGoto action_33
action_23 (23) = happyGoto action_34
action_23 (24) = happyGoto action_35
action_23 (25) = happyGoto action_36
action_23 (26) = happyGoto action_37
action_23 (28) = happyGoto action_38
action_23 _ = happyFail

action_24 (36) = happyShift action_80
action_24 (40) = happyShift action_81
action_24 (43) = happyShift action_82
action_24 (48) = happyShift action_83
action_24 _ = happyReduce_39

action_25 _ = happyReduce_42

action_26 _ = happyReduce_60

action_27 _ = happyReduce_61

action_28 _ = happyReduce_17

action_29 _ = happyReduce_15

action_30 (65) = happyShift action_2
action_30 (4) = happyGoto action_77
action_30 (16) = happyGoto action_78
action_30 (17) = happyGoto action_79
action_30 _ = happyFail

action_31 _ = happyReduce_46

action_32 _ = happyReduce_48

action_33 (34) = happyShift action_74
action_33 (38) = happyShift action_75
action_33 (44) = happyShift action_76
action_33 (30) = happyGoto action_73
action_33 _ = happyReduce_50

action_34 (39) = happyShift action_71
action_34 (42) = happyShift action_72
action_34 (29) = happyGoto action_70
action_34 _ = happyReduce_52

action_35 (33) = happyShift action_63
action_35 (35) = happyShift action_64
action_35 (46) = happyShift action_65
action_35 (47) = happyShift action_66
action_35 (49) = happyShift action_67
action_35 (50) = happyShift action_68
action_35 (51) = happyShift action_69
action_35 (31) = happyGoto action_62
action_35 _ = happyReduce_54

action_36 (63) = happyShift action_61
action_36 _ = happyReduce_56

action_37 (45) = happyShift action_60
action_37 _ = happyFail

action_38 _ = happyReduce_40

action_39 (36) = happyShift action_40
action_39 (55) = happyShift action_43
action_39 (59) = happyShift action_46
action_39 (65) = happyShift action_2
action_39 (66) = happyShift action_49
action_39 (67) = happyShift action_50
action_39 (68) = happyShift action_51
action_39 (4) = happyGoto action_53
action_39 (5) = happyGoto action_25
action_39 (6) = happyGoto action_26
action_39 (7) = happyGoto action_27
action_39 (20) = happyGoto action_59
action_39 (28) = happyGoto action_38
action_39 _ = happyFail

action_40 (32) = happyShift action_39
action_40 (36) = happyShift action_40
action_40 (42) = happyShift action_41
action_40 (55) = happyShift action_43
action_40 (59) = happyShift action_46
action_40 (65) = happyShift action_2
action_40 (66) = happyShift action_49
action_40 (67) = happyShift action_50
action_40 (68) = happyShift action_51
action_40 (4) = happyGoto action_53
action_40 (5) = happyGoto action_25
action_40 (6) = happyGoto action_26
action_40 (7) = happyGoto action_27
action_40 (20) = happyGoto action_31
action_40 (21) = happyGoto action_32
action_40 (22) = happyGoto action_33
action_40 (23) = happyGoto action_34
action_40 (24) = happyGoto action_35
action_40 (25) = happyGoto action_36
action_40 (26) = happyGoto action_58
action_40 (28) = happyGoto action_38
action_40 _ = happyFail

action_41 (36) = happyShift action_40
action_41 (55) = happyShift action_43
action_41 (59) = happyShift action_46
action_41 (65) = happyShift action_2
action_41 (66) = happyShift action_49
action_41 (67) = happyShift action_50
action_41 (68) = happyShift action_51
action_41 (4) = happyGoto action_53
action_41 (5) = happyGoto action_25
action_41 (6) = happyGoto action_26
action_41 (7) = happyGoto action_27
action_41 (20) = happyGoto action_57
action_41 (28) = happyGoto action_38
action_41 _ = happyFail

action_42 _ = happyReduce_16

action_43 _ = happyReduce_63

action_44 (36) = happyShift action_56
action_44 _ = happyFail

action_45 (32) = happyShift action_39
action_45 (36) = happyShift action_40
action_45 (42) = happyShift action_41
action_45 (45) = happyShift action_55
action_45 (55) = happyShift action_43
action_45 (59) = happyShift action_46
action_45 (65) = happyShift action_2
action_45 (66) = happyShift action_49
action_45 (67) = happyShift action_50
action_45 (68) = happyShift action_51
action_45 (4) = happyGoto action_53
action_45 (5) = happyGoto action_25
action_45 (6) = happyGoto action_26
action_45 (7) = happyGoto action_27
action_45 (20) = happyGoto action_31
action_45 (21) = happyGoto action_32
action_45 (22) = happyGoto action_33
action_45 (23) = happyGoto action_34
action_45 (24) = happyGoto action_35
action_45 (25) = happyGoto action_36
action_45 (26) = happyGoto action_54
action_45 (28) = happyGoto action_38
action_45 _ = happyFail

action_46 _ = happyReduce_62

action_47 (36) = happyShift action_52
action_47 _ = happyFail

action_48 _ = happyReduce_13

action_49 _ = happyReduce_2

action_50 _ = happyReduce_3

action_51 _ = happyReduce_4

action_52 (32) = happyShift action_39
action_52 (36) = happyShift action_40
action_52 (42) = happyShift action_41
action_52 (55) = happyShift action_43
action_52 (59) = happyShift action_46
action_52 (65) = happyShift action_2
action_52 (66) = happyShift action_49
action_52 (67) = happyShift action_50
action_52 (68) = happyShift action_51
action_52 (4) = happyGoto action_53
action_52 (5) = happyGoto action_25
action_52 (6) = happyGoto action_26
action_52 (7) = happyGoto action_27
action_52 (20) = happyGoto action_31
action_52 (21) = happyGoto action_32
action_52 (22) = happyGoto action_33
action_52 (23) = happyGoto action_34
action_52 (24) = happyGoto action_35
action_52 (25) = happyGoto action_36
action_52 (26) = happyGoto action_100
action_52 (28) = happyGoto action_38
action_52 _ = happyFail

action_53 (36) = happyShift action_80
action_53 _ = happyReduce_39

action_54 (45) = happyShift action_99
action_54 _ = happyFail

action_55 _ = happyReduce_23

action_56 (32) = happyShift action_39
action_56 (36) = happyShift action_40
action_56 (42) = happyShift action_41
action_56 (55) = happyShift action_43
action_56 (59) = happyShift action_46
action_56 (65) = happyShift action_2
action_56 (66) = happyShift action_49
action_56 (67) = happyShift action_50
action_56 (68) = happyShift action_51
action_56 (4) = happyGoto action_53
action_56 (5) = happyGoto action_25
action_56 (6) = happyGoto action_26
action_56 (7) = happyGoto action_27
action_56 (20) = happyGoto action_31
action_56 (21) = happyGoto action_32
action_56 (22) = happyGoto action_33
action_56 (23) = happyGoto action_34
action_56 (24) = happyGoto action_35
action_56 (25) = happyGoto action_36
action_56 (26) = happyGoto action_98
action_56 (28) = happyGoto action_38
action_56 _ = happyFail

action_57 _ = happyReduce_44

action_58 (37) = happyShift action_97
action_58 _ = happyFail

action_59 _ = happyReduce_45

action_60 _ = happyReduce_27

action_61 (32) = happyShift action_39
action_61 (36) = happyShift action_40
action_61 (42) = happyShift action_41
action_61 (55) = happyShift action_43
action_61 (59) = happyShift action_46
action_61 (65) = happyShift action_2
action_61 (66) = happyShift action_49
action_61 (67) = happyShift action_50
action_61 (68) = happyShift action_51
action_61 (4) = happyGoto action_53
action_61 (5) = happyGoto action_25
action_61 (6) = happyGoto action_26
action_61 (7) = happyGoto action_27
action_61 (20) = happyGoto action_31
action_61 (21) = happyGoto action_32
action_61 (22) = happyGoto action_33
action_61 (23) = happyGoto action_34
action_61 (24) = happyGoto action_35
action_61 (25) = happyGoto action_36
action_61 (26) = happyGoto action_96
action_61 (28) = happyGoto action_38
action_61 _ = happyFail

action_62 (32) = happyShift action_39
action_62 (36) = happyShift action_40
action_62 (42) = happyShift action_41
action_62 (55) = happyShift action_43
action_62 (59) = happyShift action_46
action_62 (65) = happyShift action_2
action_62 (66) = happyShift action_49
action_62 (67) = happyShift action_50
action_62 (68) = happyShift action_51
action_62 (4) = happyGoto action_53
action_62 (5) = happyGoto action_25
action_62 (6) = happyGoto action_26
action_62 (7) = happyGoto action_27
action_62 (20) = happyGoto action_31
action_62 (21) = happyGoto action_32
action_62 (22) = happyGoto action_33
action_62 (23) = happyGoto action_95
action_62 (28) = happyGoto action_38
action_62 _ = happyFail

action_63 _ = happyReduce_74

action_64 (32) = happyShift action_39
action_64 (36) = happyShift action_40
action_64 (42) = happyShift action_41
action_64 (55) = happyShift action_43
action_64 (59) = happyShift action_46
action_64 (65) = happyShift action_2
action_64 (66) = happyShift action_49
action_64 (67) = happyShift action_50
action_64 (68) = happyShift action_51
action_64 (4) = happyGoto action_53
action_64 (5) = happyGoto action_25
action_64 (6) = happyGoto action_26
action_64 (7) = happyGoto action_27
action_64 (20) = happyGoto action_31
action_64 (21) = happyGoto action_32
action_64 (22) = happyGoto action_33
action_64 (23) = happyGoto action_34
action_64 (24) = happyGoto action_35
action_64 (25) = happyGoto action_94
action_64 (28) = happyGoto action_38
action_64 _ = happyFail

action_65 _ = happyReduce_69

action_66 _ = happyReduce_70

action_67 _ = happyReduce_73

action_68 _ = happyReduce_71

action_69 _ = happyReduce_72

action_70 (32) = happyShift action_39
action_70 (36) = happyShift action_40
action_70 (42) = happyShift action_41
action_70 (55) = happyShift action_43
action_70 (59) = happyShift action_46
action_70 (65) = happyShift action_2
action_70 (66) = happyShift action_49
action_70 (67) = happyShift action_50
action_70 (68) = happyShift action_51
action_70 (4) = happyGoto action_53
action_70 (5) = happyGoto action_25
action_70 (6) = happyGoto action_26
action_70 (7) = happyGoto action_27
action_70 (20) = happyGoto action_31
action_70 (21) = happyGoto action_32
action_70 (22) = happyGoto action_93
action_70 (28) = happyGoto action_38
action_70 _ = happyFail

action_71 _ = happyReduce_64

action_72 _ = happyReduce_65

action_73 (32) = happyShift action_39
action_73 (36) = happyShift action_40
action_73 (42) = happyShift action_41
action_73 (55) = happyShift action_43
action_73 (59) = happyShift action_46
action_73 (65) = happyShift action_2
action_73 (66) = happyShift action_49
action_73 (67) = happyShift action_50
action_73 (68) = happyShift action_51
action_73 (4) = happyGoto action_53
action_73 (5) = happyGoto action_25
action_73 (6) = happyGoto action_26
action_73 (7) = happyGoto action_27
action_73 (20) = happyGoto action_31
action_73 (21) = happyGoto action_92
action_73 (28) = happyGoto action_38
action_73 _ = happyFail

action_74 _ = happyReduce_68

action_75 _ = happyReduce_66

action_76 _ = happyReduce_67

action_77 (48) = happyShift action_91
action_77 _ = happyReduce_28

action_78 (41) = happyShift action_90
action_78 _ = happyReduce_30

action_79 (45) = happyShift action_89
action_79 _ = happyFail

action_80 (32) = happyShift action_39
action_80 (36) = happyShift action_40
action_80 (42) = happyShift action_41
action_80 (55) = happyShift action_43
action_80 (59) = happyShift action_46
action_80 (65) = happyShift action_2
action_80 (66) = happyShift action_49
action_80 (67) = happyShift action_50
action_80 (68) = happyShift action_51
action_80 (4) = happyGoto action_53
action_80 (5) = happyGoto action_25
action_80 (6) = happyGoto action_26
action_80 (7) = happyGoto action_27
action_80 (20) = happyGoto action_31
action_80 (21) = happyGoto action_32
action_80 (22) = happyGoto action_33
action_80 (23) = happyGoto action_34
action_80 (24) = happyGoto action_35
action_80 (25) = happyGoto action_36
action_80 (26) = happyGoto action_87
action_80 (27) = happyGoto action_88
action_80 (28) = happyGoto action_38
action_80 _ = happyReduce_57

action_81 (45) = happyShift action_86
action_81 _ = happyFail

action_82 (45) = happyShift action_85
action_82 _ = happyFail

action_83 (32) = happyShift action_39
action_83 (36) = happyShift action_40
action_83 (42) = happyShift action_41
action_83 (55) = happyShift action_43
action_83 (59) = happyShift action_46
action_83 (65) = happyShift action_2
action_83 (66) = happyShift action_49
action_83 (67) = happyShift action_50
action_83 (68) = happyShift action_51
action_83 (4) = happyGoto action_53
action_83 (5) = happyGoto action_25
action_83 (6) = happyGoto action_26
action_83 (7) = happyGoto action_27
action_83 (20) = happyGoto action_31
action_83 (21) = happyGoto action_32
action_83 (22) = happyGoto action_33
action_83 (23) = happyGoto action_34
action_83 (24) = happyGoto action_35
action_83 (25) = happyGoto action_36
action_83 (26) = happyGoto action_84
action_83 (28) = happyGoto action_38
action_83 _ = happyFail

action_84 (45) = happyShift action_107
action_84 _ = happyFail

action_85 _ = happyReduce_21

action_86 _ = happyReduce_20

action_87 (41) = happyShift action_106
action_87 _ = happyReduce_58

action_88 (37) = happyShift action_105
action_88 _ = happyFail

action_89 _ = happyReduce_18

action_90 (65) = happyShift action_2
action_90 (4) = happyGoto action_77
action_90 (16) = happyGoto action_78
action_90 (17) = happyGoto action_104
action_90 _ = happyFail

action_91 (32) = happyShift action_39
action_91 (36) = happyShift action_40
action_91 (42) = happyShift action_41
action_91 (55) = happyShift action_43
action_91 (59) = happyShift action_46
action_91 (65) = happyShift action_2
action_91 (66) = happyShift action_49
action_91 (67) = happyShift action_50
action_91 (68) = happyShift action_51
action_91 (4) = happyGoto action_53
action_91 (5) = happyGoto action_25
action_91 (6) = happyGoto action_26
action_91 (7) = happyGoto action_27
action_91 (20) = happyGoto action_31
action_91 (21) = happyGoto action_32
action_91 (22) = happyGoto action_33
action_91 (23) = happyGoto action_34
action_91 (24) = happyGoto action_35
action_91 (25) = happyGoto action_36
action_91 (26) = happyGoto action_103
action_91 (28) = happyGoto action_38
action_91 _ = happyFail

action_92 _ = happyReduce_47

action_93 (34) = happyShift action_74
action_93 (38) = happyShift action_75
action_93 (44) = happyShift action_76
action_93 (30) = happyGoto action_73
action_93 _ = happyReduce_49

action_94 _ = happyReduce_53

action_95 (39) = happyShift action_71
action_95 (42) = happyShift action_72
action_95 (29) = happyGoto action_70
action_95 _ = happyReduce_51

action_96 _ = happyReduce_55

action_97 _ = happyReduce_43

action_98 (37) = happyShift action_102
action_98 _ = happyFail

action_99 _ = happyReduce_22

action_100 (37) = happyShift action_101
action_100 _ = happyFail

action_101 (32) = happyShift action_39
action_101 (36) = happyShift action_40
action_101 (42) = happyShift action_41
action_101 (45) = happyShift action_42
action_101 (52) = happyShift action_7
action_101 (53) = happyShift action_8
action_101 (55) = happyShift action_43
action_101 (56) = happyShift action_44
action_101 (57) = happyShift action_9
action_101 (58) = happyShift action_45
action_101 (59) = happyShift action_46
action_101 (60) = happyShift action_10
action_101 (61) = happyShift action_47
action_101 (62) = happyShift action_21
action_101 (65) = happyShift action_2
action_101 (66) = happyShift action_49
action_101 (67) = happyShift action_50
action_101 (68) = happyShift action_51
action_101 (4) = happyGoto action_24
action_101 (5) = happyGoto action_25
action_101 (6) = happyGoto action_26
action_101 (7) = happyGoto action_27
action_101 (13) = happyGoto action_28
action_101 (15) = happyGoto action_110
action_101 (18) = happyGoto action_30
action_101 (20) = happyGoto action_31
action_101 (21) = happyGoto action_32
action_101 (22) = happyGoto action_33
action_101 (23) = happyGoto action_34
action_101 (24) = happyGoto action_35
action_101 (25) = happyGoto action_36
action_101 (26) = happyGoto action_37
action_101 (28) = happyGoto action_38
action_101 _ = happyFail

action_102 (32) = happyShift action_39
action_102 (36) = happyShift action_40
action_102 (42) = happyShift action_41
action_102 (45) = happyShift action_42
action_102 (52) = happyShift action_7
action_102 (53) = happyShift action_8
action_102 (55) = happyShift action_43
action_102 (56) = happyShift action_44
action_102 (57) = happyShift action_9
action_102 (58) = happyShift action_45
action_102 (59) = happyShift action_46
action_102 (60) = happyShift action_10
action_102 (61) = happyShift action_47
action_102 (62) = happyShift action_21
action_102 (65) = happyShift action_2
action_102 (66) = happyShift action_49
action_102 (67) = happyShift action_50
action_102 (68) = happyShift action_51
action_102 (4) = happyGoto action_24
action_102 (5) = happyGoto action_25
action_102 (6) = happyGoto action_26
action_102 (7) = happyGoto action_27
action_102 (13) = happyGoto action_28
action_102 (15) = happyGoto action_109
action_102 (18) = happyGoto action_30
action_102 (20) = happyGoto action_31
action_102 (21) = happyGoto action_32
action_102 (22) = happyGoto action_33
action_102 (23) = happyGoto action_34
action_102 (24) = happyGoto action_35
action_102 (25) = happyGoto action_36
action_102 (26) = happyGoto action_37
action_102 (28) = happyGoto action_38
action_102 _ = happyFail

action_103 _ = happyReduce_29

action_104 _ = happyReduce_31

action_105 _ = happyReduce_41

action_106 (32) = happyShift action_39
action_106 (36) = happyShift action_40
action_106 (42) = happyShift action_41
action_106 (55) = happyShift action_43
action_106 (59) = happyShift action_46
action_106 (65) = happyShift action_2
action_106 (66) = happyShift action_49
action_106 (67) = happyShift action_50
action_106 (68) = happyShift action_51
action_106 (4) = happyGoto action_53
action_106 (5) = happyGoto action_25
action_106 (6) = happyGoto action_26
action_106 (7) = happyGoto action_27
action_106 (20) = happyGoto action_31
action_106 (21) = happyGoto action_32
action_106 (22) = happyGoto action_33
action_106 (23) = happyGoto action_34
action_106 (24) = happyGoto action_35
action_106 (25) = happyGoto action_36
action_106 (26) = happyGoto action_87
action_106 (27) = happyGoto action_108
action_106 (28) = happyGoto action_38
action_106 _ = happyReduce_57

action_107 _ = happyReduce_19

action_108 _ = happyReduce_59

action_109 (54) = happyShift action_111
action_109 _ = happyReduce_24

action_110 _ = happyReduce_26

action_111 (32) = happyShift action_39
action_111 (36) = happyShift action_40
action_111 (42) = happyShift action_41
action_111 (45) = happyShift action_42
action_111 (52) = happyShift action_7
action_111 (53) = happyShift action_8
action_111 (55) = happyShift action_43
action_111 (56) = happyShift action_44
action_111 (57) = happyShift action_9
action_111 (58) = happyShift action_45
action_111 (59) = happyShift action_46
action_111 (60) = happyShift action_10
action_111 (61) = happyShift action_47
action_111 (62) = happyShift action_21
action_111 (65) = happyShift action_2
action_111 (66) = happyShift action_49
action_111 (67) = happyShift action_50
action_111 (68) = happyShift action_51
action_111 (4) = happyGoto action_24
action_111 (5) = happyGoto action_25
action_111 (6) = happyGoto action_26
action_111 (7) = happyGoto action_27
action_111 (13) = happyGoto action_28
action_111 (15) = happyGoto action_112
action_111 (18) = happyGoto action_30
action_111 (20) = happyGoto action_31
action_111 (21) = happyGoto action_32
action_111 (22) = happyGoto action_33
action_111 (23) = happyGoto action_34
action_111 (24) = happyGoto action_35
action_111 (25) = happyGoto action_36
action_111 (26) = happyGoto action_37
action_111 (28) = happyGoto action_38
action_111 _ = happyFail

action_112 _ = happyReduce_25

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn4
		 (Ident happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn6
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn7
		 ((read ( happy_var_1)) :: Double
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (PProg happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ((:[]) happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  9 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 10 happyReduction_8
happyReduction_8 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (FnDef happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_0  11 happyReduction_9
happyReduction_9  =  HappyAbsSyn11
		 ([]
	)

happyReduce_10 = happySpecReduce_1  11 happyReduction_10
happyReduction_10 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ((:[]) happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  11 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  12 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn12
		 (DArg happy_var_1 happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  13 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (DBlock (reverse happy_var_2)
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_0  14 happyReduction_14
happyReduction_14  =  HappyAbsSyn14
		 ([]
	)

happyReduce_15 = happySpecReduce_2  14 happyReduction_15
happyReduction_15 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  15 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn15
		 (SEmpty
	)

happyReduce_17 = happySpecReduce_1  15 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn15
		 (SBlock happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  15 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn15
		 (SDecl happy_var_1 happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 4 15 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (SAss happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_3  15 happyReduction_20
happyReduction_20 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn15
		 (SIncr happy_var_1
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  15 happyReduction_21
happyReduction_21 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn15
		 (SDecr happy_var_1
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  15 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (SRet happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  15 happyReduction_23
happyReduction_23 _
	_
	 =  HappyAbsSyn15
		 (SVRet
	)

happyReduce_24 = happyReduce 5 15 happyReduction_24
happyReduction_24 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (SIf happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 7 15 happyReduction_25
happyReduction_25 ((HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (SIfElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 15 happyReduction_26
happyReduction_26 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (SWhile happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_2  15 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn15
		 (SExp happy_var_1
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (IDecl happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (IInit happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:[]) happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  17 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  18 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn18
		 (TInt
	)

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn18
		 (TDoub
	)

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn18
		 (TBool
	)

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn18
		 (TVoid
	)

happyReduce_36 = happySpecReduce_0  19 happyReduction_36
happyReduction_36  =  HappyAbsSyn19
		 ([]
	)

happyReduce_37 = happySpecReduce_1  19 happyReduction_37
happyReduction_37 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ((:[]) happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  19 happyReduction_38
happyReduction_38 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn19
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (EVar happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn20
		 (ELit happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happyReduce 4 20 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (EApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_1  20 happyReduction_42
happyReduction_42 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn20
		 (EString happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  20 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  21 happyReduction_44
happyReduction_44 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (ENeg happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  21 happyReduction_45
happyReduction_45 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (ENot happy_var_2
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  21 happyReduction_46
happyReduction_46 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  22 happyReduction_47
happyReduction_47 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EMul happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  22 happyReduction_48
happyReduction_48 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  23 happyReduction_49
happyReduction_49 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EAdd happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  23 happyReduction_50
happyReduction_50 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  24 happyReduction_51
happyReduction_51 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (ERel happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  24 happyReduction_52
happyReduction_52 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  25 happyReduction_53
happyReduction_53 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EAnd happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  25 happyReduction_54
happyReduction_54 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_3  26 happyReduction_55
happyReduction_55 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EOr happy_var_1 happy_var_3
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  26 happyReduction_56
happyReduction_56 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_0  27 happyReduction_57
happyReduction_57  =  HappyAbsSyn27
		 ([]
	)

happyReduce_58 = happySpecReduce_1  27 happyReduction_58
happyReduction_58 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn27
		 ((:[]) happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  27 happyReduction_59
happyReduction_59 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn27
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  28 happyReduction_60
happyReduction_60 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn28
		 (LInt happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  28 happyReduction_61
happyReduction_61 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn28
		 (LDoub happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  28 happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn28
		 (LTrue
	)

happyReduce_63 = happySpecReduce_1  28 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn28
		 (LFalse
	)

happyReduce_64 = happySpecReduce_1  29 happyReduction_64
happyReduction_64 _
	 =  HappyAbsSyn29
		 (Plus
	)

happyReduce_65 = happySpecReduce_1  29 happyReduction_65
happyReduction_65 _
	 =  HappyAbsSyn29
		 (Minus
	)

happyReduce_66 = happySpecReduce_1  30 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn30
		 (Times
	)

happyReduce_67 = happySpecReduce_1  30 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn30
		 (Div
	)

happyReduce_68 = happySpecReduce_1  30 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn30
		 (Mod
	)

happyReduce_69 = happySpecReduce_1  31 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn31
		 (Lt
	)

happyReduce_70 = happySpecReduce_1  31 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn31
		 (LtEq
	)

happyReduce_71 = happySpecReduce_1  31 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn31
		 (Gt
	)

happyReduce_72 = happySpecReduce_1  31 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn31
		 (GtEq
	)

happyReduce_73 = happySpecReduce_1  31 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn31
		 (Eq
	)

happyReduce_74 = happySpecReduce_1  31 happyReduction_74
happyReduction_74 _
	 =  HappyAbsSyn31
		 (NEq
	)

happyNewToken action sts stk [] =
	action 69 69 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 32;
	PT _ (TS _ 2) -> cont 33;
	PT _ (TS _ 3) -> cont 34;
	PT _ (TS _ 4) -> cont 35;
	PT _ (TS _ 5) -> cont 36;
	PT _ (TS _ 6) -> cont 37;
	PT _ (TS _ 7) -> cont 38;
	PT _ (TS _ 8) -> cont 39;
	PT _ (TS _ 9) -> cont 40;
	PT _ (TS _ 10) -> cont 41;
	PT _ (TS _ 11) -> cont 42;
	PT _ (TS _ 12) -> cont 43;
	PT _ (TS _ 13) -> cont 44;
	PT _ (TS _ 14) -> cont 45;
	PT _ (TS _ 15) -> cont 46;
	PT _ (TS _ 16) -> cont 47;
	PT _ (TS _ 17) -> cont 48;
	PT _ (TS _ 18) -> cont 49;
	PT _ (TS _ 19) -> cont 50;
	PT _ (TS _ 20) -> cont 51;
	PT _ (TS _ 21) -> cont 52;
	PT _ (TS _ 22) -> cont 53;
	PT _ (TS _ 23) -> cont 54;
	PT _ (TS _ 24) -> cont 55;
	PT _ (TS _ 25) -> cont 56;
	PT _ (TS _ 26) -> cont 57;
	PT _ (TS _ 27) -> cont 58;
	PT _ (TS _ 28) -> cont 59;
	PT _ (TS _ 29) -> cont 60;
	PT _ (TS _ 30) -> cont 61;
	PT _ (TS _ 31) -> cont 62;
	PT _ (TS _ 32) -> cont 63;
	PT _ (TS _ 33) -> cont 64;
	PT _ (TV happy_dollar_dollar) -> cont 65;
	PT _ (TL happy_dollar_dollar) -> cont 66;
	PT _ (TI happy_dollar_dollar) -> cont 67;
	PT _ (TD happy_dollar_dollar) -> cont 68;
	_ -> happyError' (tk:tks)
	}

happyError_ 69 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
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
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
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
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

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
