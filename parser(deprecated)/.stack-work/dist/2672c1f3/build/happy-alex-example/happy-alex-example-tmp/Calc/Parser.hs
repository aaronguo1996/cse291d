{-# OPTIONS_GHC -w #-}
{-# OPTIONS -fglasgow-exts -cpp #-}
module Calc.Parser where

import Calc.Base
import Calc.Data
import qualified Data.Array as Happy_Data_Array
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
happyIn4 :: (Program) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> (Program)
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
happyIn5 :: (Exp) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> (Exp)
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
happyIn6 :: (CaseNil) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> (CaseNil)
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
happyIn7 :: (CaseCons) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> (CaseCons)
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
happyIn8 :: (Var) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> (Var)
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x92\x00\x92\x00\x84\x00\x00\x00\x69\x00\x22\x01\xa5\x00\x00\x00\x77\x00\x22\x01\x77\x00\x00\x00\x00\x00\x00\x00\x77\x00\x22\x01\x22\x01\x00\x00\x52\x00\x22\x01\x6e\x00\x63\x00\x37\x00\x4d\x00\xa5\x00\x22\x01\x22\x01\x22\x01\x22\x01\x22\x01\x22\x01\x22\x01\x22\x01\x22\x01\x22\x01\xdb\x00\xdb\x00\xdb\x00\x8a\x00\xc0\x00\xf6\x00\x22\x01\x22\x01\x0c\x01\x0c\x01\x22\x01\x22\x01\x47\x00\x22\x01\x00\x00\x6f\x00\x45\x00\x3c\x00\x1c\x00\x01\x00\x22\x01\x22\x01\x22\x01\x3f\x00\x00\x00\x38\x00\x6f\x00\x6f\x00\x6f\x00\x33\x00\x29\x00\x22\x01\x6f\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xd2\x00\x30\x00\x00\x00\x00\x00\x00\x00\x46\x01\x45\x01\x00\x00\x2b\x00\x41\x01\x2a\x00\x00\x00\x00\x00\x00\x00\x02\x00\x40\x01\x3f\x01\x00\x00\x3a\x01\x3a\x01\x00\x00\x00\x00\x3a\x01\x00\x00\x3a\x01\x38\x01\x34\x01\x33\x01\x2f\x01\x2d\x01\x1e\x01\x1d\x01\x08\x01\x07\x01\xf9\x00\xed\x00\xed\x00\xed\x00\xed\x00\xed\x00\xed\x00\xed\x00\xed\x00\xed\x00\xed\x00\xec\x00\xde\x00\x17\x00\xc3\x00\x00\x00\xa8\x00\x00\x00\x00\x00\xa8\x00\xa8\x00\x8d\x00\x72\x00\x70\x00\x10\x00\x00\x00\x00\x00\x55\x00\x55\x00\x55\x00\xfd\xff\x00\x00\x1f\x00\x04\x00\x00\x00"#

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\x00\x00\x00\x00\x00\x00\xe5\xff\x00\x00\x00\x00\xfe\xff\xec\xff\x00\x00\x00\x00\x00\x00\xe9\xff\xe8\xff\xeb\xff\x00\x00\x00\x00\x00\x00\xea\xff\x00\x00\xee\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xf5\xff\xf6\xff\xf4\xff\xf7\xff\xf8\xff\xf3\xff\xef\xff\xf0\xff\xf1\xff\xf2\xff\x00\x00\x00\x00\x00\x00\x00\x00\xed\xff\xfa\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xfb\xff\x00\x00\xe7\xff\xfc\xff\xfd\xff\x00\x00\x00\x00\x00\x00\xe6\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x04\x00\x01\x00\x02\x00\x03\x00\x01\x00\x04\x00\x06\x00\x04\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x03\x00\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x02\x00\x19\x00\x1a\x00\x1b\x00\x01\x00\x1d\x00\x03\x00\x01\x00\x05\x00\x06\x00\x04\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x04\x00\x04\x00\x14\x00\x15\x00\x16\x00\x17\x00\x04\x00\x19\x00\x1a\x00\x1b\x00\x01\x00\x1d\x00\x03\x00\x04\x00\x13\x00\x06\x00\x0b\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x11\x00\x0b\x00\x14\x00\x15\x00\x16\x00\x17\x00\x13\x00\x19\x00\x1a\x00\x1b\x00\x01\x00\x1d\x00\x03\x00\x01\x00\x12\x00\x06\x00\x04\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x1d\x00\x18\x00\x14\x00\x15\x00\x16\x00\x17\x00\x07\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x01\x00\x01\x00\x03\x00\x01\x00\x04\x00\x06\x00\x04\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x13\x00\x0b\x00\x14\x00\x15\x00\x16\x00\x17\x00\x1e\x00\x19\x00\x1a\x00\x1b\x00\x01\x00\x1d\x00\x03\x00\x01\x00\xff\xff\x06\x00\x04\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x18\x00\x0b\x00\x14\x00\x15\x00\x16\x00\x17\x00\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\x1d\x00\x03\x00\x01\x00\xff\xff\x06\x00\x04\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\xff\xff\xff\xff\x14\x00\x15\x00\x16\x00\x17\x00\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\x1d\x00\x03\x00\x01\x00\xff\xff\x06\x00\x04\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x00\x00\xff\xff\x14\x00\x15\x00\x04\x00\x17\x00\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\x1d\x00\x03\x00\x01\x00\xff\xff\x06\x00\x04\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x01\x00\x01\x00\x14\x00\x04\x00\x04\x00\x17\x00\xff\xff\x19\x00\x1a\x00\x1b\x00\x01\x00\x1d\x00\x03\x00\x01\x00\xff\xff\x06\x00\x04\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0d\x00\x0e\x00\x0f\x00\x10\x00\x11\x00\x01\x00\x01\x00\x14\x00\x04\x00\x04\x00\x01\x00\xff\xff\x03\x00\xff\xff\x1b\x00\x06\x00\x1d\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\xff\xff\xff\xff\x0e\x00\x0f\x00\x10\x00\xff\xff\x01\x00\x01\x00\x14\x00\x04\x00\x04\x00\x01\x00\xff\xff\x03\x00\xff\xff\x1b\x00\x06\x00\x1d\x00\x08\x00\x09\x00\x0a\x00\x0b\x00\x01\x00\xff\xff\x01\x00\x04\x00\x10\x00\x04\x00\x01\x00\x01\x00\x14\x00\x04\x00\x04\x00\x01\x00\xff\xff\x01\x00\x04\x00\x1b\x00\x04\x00\x1d\x00\x01\x00\x01\x00\x01\x00\x04\x00\x04\x00\x04\x00\x01\x00\x01\x00\xff\xff\x04\x00\x04\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x41\x00\x09\x00\x38\x00\x0a\x00\x18\x00\x14\x00\x0b\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0f\x00\x1e\x00\x3b\x00\x3c\x00\x10\x00\x1f\x00\x20\x00\x21\x00\x33\x00\x22\x00\x23\x00\x11\x00\x09\x00\x12\x00\x0a\x00\x43\x00\x39\x00\x0b\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0f\x00\x1e\x00\x15\x00\x17\x00\x10\x00\x1f\x00\x20\x00\x21\x00\x02\x00\x22\x00\x23\x00\x11\x00\x09\x00\x12\x00\x0a\x00\x2f\x00\x43\x00\x0b\x00\x04\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0f\x00\x1e\x00\x41\x00\x04\x00\x10\x00\x1f\x00\x20\x00\x21\x00\x3a\x00\x22\x00\x23\x00\x11\x00\x09\x00\x12\x00\x0a\x00\x18\x00\x3b\x00\x0b\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0f\x00\x1e\x00\x35\x00\x2e\x00\x10\x00\x1f\x00\x20\x00\x21\x00\x30\x00\x22\x00\x23\x00\x11\x00\x32\x00\x12\x00\x09\x00\x3d\x00\x0a\x00\x3e\x00\x07\x00\x0b\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0f\x00\x1e\x00\x31\x00\x04\x00\x10\x00\x1f\x00\x20\x00\x21\x00\xff\xff\x22\x00\x23\x00\x11\x00\x09\x00\x12\x00\x0a\x00\x3f\x00\x00\x00\x0b\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0f\x00\x1e\x00\x06\x00\x04\x00\x10\x00\x1f\x00\x00\x00\x21\x00\x00\x00\x22\x00\x23\x00\x11\x00\x09\x00\x12\x00\x0a\x00\x18\x00\x00\x00\x0b\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0f\x00\x1e\x00\x00\x00\x00\x00\x10\x00\x1f\x00\x20\x00\x21\x00\x00\x00\x22\x00\x23\x00\x11\x00\x09\x00\x12\x00\x0a\x00\x32\x00\x00\x00\x0b\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0f\x00\x1e\x00\x04\x00\x00\x00\x10\x00\x00\x00\x02\x00\x21\x00\x00\x00\x22\x00\x23\x00\x11\x00\x09\x00\x12\x00\x0a\x00\x35\x00\x00\x00\x0b\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0f\x00\x1e\x00\x36\x00\x18\x00\x10\x00\x07\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x09\x00\x12\x00\x0a\x00\x23\x00\x00\x00\x0b\x00\x07\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x1a\x00\x1b\x00\x1c\x00\x1d\x00\x0f\x00\x1e\x00\x24\x00\x25\x00\x10\x00\x07\x00\x07\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x11\x00\x0b\x00\x12\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x00\x00\x00\x00\x1c\x00\x1d\x00\x0f\x00\x00\x00\x26\x00\x27\x00\x10\x00\x07\x00\x07\x00\x09\x00\x00\x00\x0a\x00\x00\x00\x11\x00\x0b\x00\x12\x00\x0c\x00\x0d\x00\x0e\x00\x04\x00\x28\x00\x00\x00\x29\x00\x07\x00\x0f\x00\x07\x00\x2a\x00\x2b\x00\x10\x00\x07\x00\x07\x00\x2c\x00\x00\x00\x18\x00\x07\x00\x11\x00\x07\x00\x12\x00\x12\x00\x13\x00\x16\x00\x07\x00\x07\x00\x07\x00\x18\x00\x06\x00\x00\x00\x07\x00\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 26) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26)
	]

happy_n_terms = 31 :: Int
happy_n_nonterms = 5 :: Int

happyReduce_1 = happySpecReduce_3  0# happyReduction_1
happyReduction_1 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn4
		 (DefEq happy_var_1 happy_var_3
	)}}

happyReduce_2 = happyReduce 6# 1# happyReduction_2
happyReduction_2 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	case happyOut5 happy_x_6 of { happy_var_6 -> 
	happyIn5
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_3 = happyReduce 6# 1# happyReduction_3
happyReduction_3 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut5 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	case happyOut5 happy_x_6 of { happy_var_6 -> 
	happyIn5
		 (ITE happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_4 = happyReduce 6# 1# happyReduction_4
happyReduction_4 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut6 happy_x_4 of { happy_var_4 -> 
	case happyOut7 happy_x_6 of { happy_var_6 -> 
	happyIn5
		 (Case happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_5 = happyReduce 4# 1# happyReduction_5
happyReduction_5 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_2 of { happy_var_2 -> 
	case happyOut5 happy_x_4 of { happy_var_4 -> 
	happyIn5
		 (Lambda happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_6 = happySpecReduce_2  1# happyReduction_6
happyReduction_6 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (App happy_var_1 happy_var_2
	)}}

happyReduce_7 = happySpecReduce_3  1# happyReduction_7
happyReduction_7 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (And happy_var_1 happy_var_3
	)}}

happyReduce_8 = happySpecReduce_3  1# happyReduction_8
happyReduction_8 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (Or happy_var_1 happy_var_3
	)}}

happyReduce_9 = happySpecReduce_3  1# happyReduction_9
happyReduction_9 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (OpGT happy_var_1 happy_var_3
	)}}

happyReduce_10 = happySpecReduce_3  1# happyReduction_10
happyReduction_10 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (OpLT happy_var_1 happy_var_3
	)}}

happyReduce_11 = happySpecReduce_3  1# happyReduction_11
happyReduction_11 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (OpEq happy_var_1 happy_var_3
	)}}

happyReduce_12 = happySpecReduce_3  1# happyReduction_12
happyReduction_12 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (Cons happy_var_1 happy_var_3
	)}}

happyReduce_13 = happySpecReduce_3  1# happyReduction_13
happyReduction_13 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (Add happy_var_1 happy_var_3
	)}}

happyReduce_14 = happySpecReduce_3  1# happyReduction_14
happyReduction_14 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (Sub happy_var_1 happy_var_3
	)}}

happyReduce_15 = happySpecReduce_3  1# happyReduction_15
happyReduction_15 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (Mul happy_var_1 happy_var_3
	)}}

happyReduce_16 = happySpecReduce_3  1# happyReduction_16
happyReduction_16 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_1 of { happy_var_1 -> 
	case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn5
		 (Div happy_var_1 happy_var_3
	)}}

happyReduce_17 = happySpecReduce_2  1# happyReduction_17
happyReduction_17 happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (Not happy_var_2
	)}

happyReduce_18 = happySpecReduce_3  1# happyReduction_18
happyReduction_18 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_2 of { happy_var_2 -> 
	happyIn5
		 (Brack happy_var_2
	)}

happyReduce_19 = happySpecReduce_1  1# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut8 happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (ExpVar happy_var_1
	)}

happyReduce_20 = happySpecReduce_1  1# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOutTok happy_x_1 of { (Token _ (TokenInt happy_var_1)) -> 
	happyIn5
		 (Int happy_var_1
	)}

happyReduce_21 = happySpecReduce_1  1# happyReduction_21
happyReduction_21 happy_x_1
	 =  happyIn5
		 (Nil
	)

happyReduce_22 = happySpecReduce_1  1# happyReduction_22
happyReduction_22 happy_x_1
	 =  happyIn5
		 (ExpTrue
	)

happyReduce_23 = happySpecReduce_1  1# happyReduction_23
happyReduction_23 happy_x_1
	 =  happyIn5
		 (ExpFalse
	)

happyReduce_24 = happySpecReduce_3  2# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut5 happy_x_3 of { happy_var_3 -> 
	happyIn6
		 (CaseNil happy_var_3
	)}

happyReduce_25 = happyReduce 5# 3# happyReduction_25
happyReduction_25 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut8 happy_x_1 of { happy_var_1 -> 
	case happyOut8 happy_x_3 of { happy_var_3 -> 
	case happyOut5 happy_x_5 of { happy_var_5 -> 
	happyIn7
		 (CaseCons happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_26 = happySpecReduce_1  4# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOutTok happy_x_1 of { (Token _ (TokenVar happy_var_1)) -> 
	happyIn8
		 (Var happy_var_1
	)}

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = happyDoAction i tk action sts stk in
	case tk of {
	Token _ TokenEOF -> happyDoAction 30# tk action sts stk;
	Token _ TokenLet -> cont 1#;
	Token _ TokenIn -> cont 2#;
	Token _ TokenIf -> cont 3#;
	Token _ TokenThen -> cont 4#;
	Token _ TokenElse -> cont 5#;
	Token _ TokenCase -> cont 6#;
	Token _ TokenOf -> cont 7#;
	Token _ TokenTrue -> cont 8#;
	Token _ TokenFalse -> cont 9#;
	Token _ (TokenInt happy_dollar_dollar) -> cont 10#;
	Token _ (TokenVar happy_dollar_dollar) -> cont 11#;
	Token _ TokenPlus -> cont 12#;
	Token _ TokenMinus -> cont 13#;
	Token _ TokenTimes -> cont 14#;
	Token _ TokenDiv -> cont 15#;
	Token _ TokenLambda -> cont 16#;
	Token _ TokenCons -> cont 17#;
	Token _ TokenSemi -> cont 18#;
	Token _ TokenRarrow -> cont 19#;
	Token _ TokenNot -> cont 20#;
	Token _ TokenLand -> cont 21#;
	Token _ TokenLor -> cont 22#;
	Token _ TokenEq -> cont 23#;
	Token _ TokenAssign -> cont 24#;
	Token _ TokenGT -> cont 25#;
	Token _ TokenLT -> cont 26#;
	Token _ TokenOB -> cont 27#;
	Token _ TokenCB -> cont 28#;
	Token _ TokenNil -> cont 29#;
	_ -> happyError' tk
	})

happyError_ 30# tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (thenP)
happyReturn :: () => a -> Parser a
happyReturn = (returnP)
happyThen1 = happyThen
happyReturn1 :: () => a -> Parser a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> Parser a
happyError' tk = (\token -> happyError) tk

happyParser = happySomeParser where
  happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (happyOut4 x))

happySeq = happyDontSeq



{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\sherl\\AppData\\Local\\Programs\\stack\\x86_64-windows\\ghc-7.10.3\\lib/include\\ghcversion.h" #-}

















{-# LINE 12 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}





-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif
{-# LINE 46 "templates\\GenericTemplate.hs" #-}


data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList





{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is 0#, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}


          case action of
                0#           -> {- nothing -}
                                     happyFail i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}

                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}


                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = indexShortOffAddr happyActOffsets st
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st


indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#





data HappyAddr = HappyA# Happy_GHC_Exts.Addr#




-----------------------------------------------------------------------------
-- HappyState data type (not arrays)

{-# LINE 170 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = indexShortOffAddr happyGotoOffsets st1
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i



          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = indexShortOffAddr happyGotoOffsets st
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (0# is the error token)

-- parse error if we are in recovery and we fail again
happyFail 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  0# tk old_st (HappyCons ((action)) (sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        happyDoAction 0# tk action sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ( (Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

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
