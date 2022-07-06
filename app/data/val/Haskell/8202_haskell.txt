module Win32Font
{-
	( CharSet
	, PitchAndFamily
	, OutPrecision
	, ClipPrecision
	, FontQuality
	, FontWeight

	, createFont, deleteFont

	, StockFont, getStockFont
	, oEM_FIXED_FONT, aNSI_FIXED_FONT, aNSI_VAR_FONT, sYSTEM_FONT
	, dEVICE_DEFAULT_FONT, sYSTEM_FIXED_FONT
	) where
-}
	where

import StdDIS
import Win32Types
import GDITypes


----------------------------------------------------------------
-- Types
----------------------------------------------------------------

type CharSet        = UINT
type PitchAndFamily = UINT
type OutPrecision   = UINT
type ClipPrecision  = UINT
type FontQuality    = UINT
type FontWeight     = Word32
type FaceName       = String


-- # A FaceName is a string no more that LF_FACESIZE in length
-- # (including null terminator).
-- %const Int LF_FACESIZE         # == 32
-- %sentinel_array : FaceName : CHAR : char : $0 = '\0' : ('\0' == $0) : LF_FACESIZE

----------------------------------------------------------------
-- Constants
----------------------------------------------------------------

aNSI_CHARSET :: CharSet
aNSI_CHARSET =
  unsafePerformIO(
    prim_Win32Font_cpp_aNSI_CHARSET >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_aNSI_CHARSET :: IO (Word32)
dEFAULT_CHARSET :: CharSet
dEFAULT_CHARSET =
  unsafePerformIO(
    prim_Win32Font_cpp_dEFAULT_CHARSET >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_dEFAULT_CHARSET :: IO (Word32)
sYMBOL_CHARSET :: CharSet
sYMBOL_CHARSET =
  unsafePerformIO(
    prim_Win32Font_cpp_sYMBOL_CHARSET >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_sYMBOL_CHARSET :: IO (Word32)
sHIFTJIS_CHARSET :: CharSet
sHIFTJIS_CHARSET =
  unsafePerformIO(
    prim_Win32Font_cpp_sHIFTJIS_CHARSET >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_sHIFTJIS_CHARSET :: IO (Word32)
hANGEUL_CHARSET :: CharSet
hANGEUL_CHARSET =
  unsafePerformIO(
    prim_Win32Font_cpp_hANGEUL_CHARSET >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_hANGEUL_CHARSET :: IO (Word32)
cHINESEBIG5_CHARSET :: CharSet
cHINESEBIG5_CHARSET =
  unsafePerformIO(
    prim_Win32Font_cpp_cHINESEBIG5_CHARSET >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_cHINESEBIG5_CHARSET :: IO (Word32)
oEM_CHARSET :: CharSet
oEM_CHARSET =
  unsafePerformIO(
    prim_Win32Font_cpp_oEM_CHARSET >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_oEM_CHARSET :: IO (Word32)

dEFAULT_PITCH :: PitchAndFamily
dEFAULT_PITCH =
  unsafePerformIO(
    prim_Win32Font_cpp_dEFAULT_PITCH >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_dEFAULT_PITCH :: IO (Word32)
fIXED_PITCH :: PitchAndFamily
fIXED_PITCH =
  unsafePerformIO(
    prim_Win32Font_cpp_fIXED_PITCH >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fIXED_PITCH :: IO (Word32)
vARIABLE_PITCH :: PitchAndFamily
vARIABLE_PITCH =
  unsafePerformIO(
    prim_Win32Font_cpp_vARIABLE_PITCH >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_vARIABLE_PITCH :: IO (Word32)
fF_DONTCARE :: PitchAndFamily
fF_DONTCARE =
  unsafePerformIO(
    prim_Win32Font_cpp_fF_DONTCARE >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fF_DONTCARE :: IO (Word32)
fF_ROMAN :: PitchAndFamily
fF_ROMAN =
  unsafePerformIO(
    prim_Win32Font_cpp_fF_ROMAN >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fF_ROMAN :: IO (Word32)
fF_SWISS :: PitchAndFamily
fF_SWISS =
  unsafePerformIO(
    prim_Win32Font_cpp_fF_SWISS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fF_SWISS :: IO (Word32)
fF_MODERN :: PitchAndFamily
fF_MODERN =
  unsafePerformIO(
    prim_Win32Font_cpp_fF_MODERN >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fF_MODERN :: IO (Word32)
fF_SCRIPT :: PitchAndFamily
fF_SCRIPT =
  unsafePerformIO(
    prim_Win32Font_cpp_fF_SCRIPT >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fF_SCRIPT :: IO (Word32)
fF_DECORATIVE :: PitchAndFamily
fF_DECORATIVE =
  unsafePerformIO(
    prim_Win32Font_cpp_fF_DECORATIVE >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fF_DECORATIVE :: IO (Word32)
familyMask :: PitchAndFamily
familyMask =
  unsafePerformIO(
    prim_Win32Font_cpp_familyMask >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_familyMask :: IO (Word32)
pitchMask :: PitchAndFamily
pitchMask =
  unsafePerformIO(
    prim_Win32Font_cpp_pitchMask >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_pitchMask :: IO (Word32)

oUT_DEFAULT_PRECIS :: OutPrecision
oUT_DEFAULT_PRECIS =
  unsafePerformIO(
    prim_Win32Font_cpp_oUT_DEFAULT_PRECIS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_oUT_DEFAULT_PRECIS :: IO (Word32)
oUT_STRING_PRECIS :: OutPrecision
oUT_STRING_PRECIS =
  unsafePerformIO(
    prim_Win32Font_cpp_oUT_STRING_PRECIS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_oUT_STRING_PRECIS :: IO (Word32)
oUT_CHARACTER_PRECIS :: OutPrecision
oUT_CHARACTER_PRECIS =
  unsafePerformIO(
    prim_Win32Font_cpp_oUT_CHARACTER_PRECIS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_oUT_CHARACTER_PRECIS :: IO (Word32)
oUT_STROKE_PRECIS :: OutPrecision
oUT_STROKE_PRECIS =
  unsafePerformIO(
    prim_Win32Font_cpp_oUT_STROKE_PRECIS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_oUT_STROKE_PRECIS :: IO (Word32)
oUT_TT_PRECIS :: OutPrecision
oUT_TT_PRECIS =
  unsafePerformIO(
    prim_Win32Font_cpp_oUT_TT_PRECIS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_oUT_TT_PRECIS :: IO (Word32)
oUT_DEVICE_PRECIS :: OutPrecision
oUT_DEVICE_PRECIS =
  unsafePerformIO(
    prim_Win32Font_cpp_oUT_DEVICE_PRECIS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_oUT_DEVICE_PRECIS :: IO (Word32)
oUT_RASTER_PRECIS :: OutPrecision
oUT_RASTER_PRECIS =
  unsafePerformIO(
    prim_Win32Font_cpp_oUT_RASTER_PRECIS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_oUT_RASTER_PRECIS :: IO (Word32)
oUT_TT_ONLY_PRECIS :: OutPrecision
oUT_TT_ONLY_PRECIS =
  unsafePerformIO(
    prim_Win32Font_cpp_oUT_TT_ONLY_PRECIS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_oUT_TT_ONLY_PRECIS :: IO (Word32)

cLIP_DEFAULT_PRECIS :: ClipPrecision
cLIP_DEFAULT_PRECIS =
  unsafePerformIO(
    prim_Win32Font_cpp_cLIP_DEFAULT_PRECIS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_cLIP_DEFAULT_PRECIS :: IO (Word32)
cLIP_CHARACTER_PRECIS :: ClipPrecision
cLIP_CHARACTER_PRECIS =
  unsafePerformIO(
    prim_Win32Font_cpp_cLIP_CHARACTER_PRECIS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_cLIP_CHARACTER_PRECIS :: IO (Word32)
cLIP_STROKE_PRECIS :: ClipPrecision
cLIP_STROKE_PRECIS =
  unsafePerformIO(
    prim_Win32Font_cpp_cLIP_STROKE_PRECIS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_cLIP_STROKE_PRECIS :: IO (Word32)
cLIP_MASK :: ClipPrecision
cLIP_MASK =
  unsafePerformIO(
    prim_Win32Font_cpp_cLIP_MASK >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_cLIP_MASK :: IO (Word32)
cLIP_LH_ANGLES :: ClipPrecision
cLIP_LH_ANGLES =
  unsafePerformIO(
    prim_Win32Font_cpp_cLIP_LH_ANGLES >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_cLIP_LH_ANGLES :: IO (Word32)
cLIP_TT_ALWAYS :: ClipPrecision
cLIP_TT_ALWAYS =
  unsafePerformIO(
    prim_Win32Font_cpp_cLIP_TT_ALWAYS >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_cLIP_TT_ALWAYS :: IO (Word32)
cLIP_EMBEDDED :: ClipPrecision
cLIP_EMBEDDED =
  unsafePerformIO(
    prim_Win32Font_cpp_cLIP_EMBEDDED >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_cLIP_EMBEDDED :: IO (Word32)

dEFAULT_QUALITY :: FontQuality
dEFAULT_QUALITY =
  unsafePerformIO(
    prim_Win32Font_cpp_dEFAULT_QUALITY >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_dEFAULT_QUALITY :: IO (Word32)
dRAFT_QUALITY :: FontQuality
dRAFT_QUALITY =
  unsafePerformIO(
    prim_Win32Font_cpp_dRAFT_QUALITY >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_dRAFT_QUALITY :: IO (Word32)
pROOF_QUALITY :: FontQuality
pROOF_QUALITY =
  unsafePerformIO(
    prim_Win32Font_cpp_pROOF_QUALITY >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_pROOF_QUALITY :: IO (Word32)

fW_DONTCARE :: FontWeight
fW_DONTCARE =
  unsafePerformIO(
    prim_Win32Font_cpp_fW_DONTCARE >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fW_DONTCARE :: IO (Word32)
fW_THIN :: FontWeight
fW_THIN =
  unsafePerformIO(
    prim_Win32Font_cpp_fW_THIN >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fW_THIN :: IO (Word32)
fW_EXTRALIGHT :: FontWeight
fW_EXTRALIGHT =
  unsafePerformIO(
    prim_Win32Font_cpp_fW_EXTRALIGHT >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fW_EXTRALIGHT :: IO (Word32)
fW_LIGHT :: FontWeight
fW_LIGHT =
  unsafePerformIO(
    prim_Win32Font_cpp_fW_LIGHT >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fW_LIGHT :: IO (Word32)
fW_NORMAL :: FontWeight
fW_NORMAL =
  unsafePerformIO(
    prim_Win32Font_cpp_fW_NORMAL >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fW_NORMAL :: IO (Word32)
fW_MEDIUM :: FontWeight
fW_MEDIUM =
  unsafePerformIO(
    prim_Win32Font_cpp_fW_MEDIUM >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fW_MEDIUM :: IO (Word32)
fW_SEMIBOLD :: FontWeight
fW_SEMIBOLD =
  unsafePerformIO(
    prim_Win32Font_cpp_fW_SEMIBOLD >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fW_SEMIBOLD :: IO (Word32)
fW_BOLD :: FontWeight
fW_BOLD =
  unsafePerformIO(
    prim_Win32Font_cpp_fW_BOLD >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fW_BOLD :: IO (Word32)
fW_EXTRABOLD :: FontWeight
fW_EXTRABOLD =
  unsafePerformIO(
    prim_Win32Font_cpp_fW_EXTRABOLD >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fW_EXTRABOLD :: IO (Word32)
fW_HEAVY :: FontWeight
fW_HEAVY =
  unsafePerformIO(
    prim_Win32Font_cpp_fW_HEAVY >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fW_HEAVY :: IO (Word32)
fW_REGULAR :: FontWeight
fW_REGULAR =
  unsafePerformIO(
    prim_Win32Font_cpp_fW_REGULAR >>= \ (res1) ->
    (return (res1)))
primitive prim_Win32Font_cpp_fW_REGULAR :: IO (Word32)

----------------------------------------------------------------
-- Functions
----------------------------------------------------------------

createFont :: INT -> INT -> INT -> INT -> FontWeight -> Bool -> Bool -> Bool -> CharSet -> OutPrecision -> ClipPrecision -> FontQuality -> PitchAndFamily -> FaceName -> IO HFONT
createFont gc_arg1 gc_arg2 gc_arg3 gc_arg4 arg5 gc_arg5 gc_arg6 gc_arg7 arg9 arg10 arg11 arg12 arg13 gc_arg8 =
  case ( fromIntegral  gc_arg1) of { arg1 ->
  case ( fromIntegral  gc_arg2) of { arg2 ->
  case ( fromIntegral  gc_arg3) of { arg3 ->
  case ( fromIntegral  gc_arg4) of { arg4 ->
  (marshall_bool_ gc_arg5) >>= \ (arg6) ->
  (marshall_bool_ gc_arg6) >>= \ (arg7) ->
  (marshall_bool_ gc_arg7) >>= \ (arg8) ->
  (marshall_string_ gc_arg8) >>= \ (arg14) ->
  prim_Win32Font_cpp_createFont arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 >>= \ (res1,gc_failed,gc_failstring) ->
  if ( gc_failed /= (0::Int))
  then unmarshall_string_ gc_failstring >>=  ioError  . userError
  else (return (res1))}}}}
primitive prim_Win32Font_cpp_createFont :: Int -> Int -> Int -> Int -> Word32 -> Int -> Int -> Int -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Addr -> IO (Addr,Int,Addr)

-- test :: IO ()
-- test = do
--   f <- createFont_adr (100,100) 0 False False "Arial"
--   putStrLn "Created first font"
--   f <- createFont_adr (100,100) (-90) False False "Bogus"
--   putStrLn "Created second font"
-- 
-- createFont_adr (width, height) escapement bold italic family = 
--  createFont height width
-- 		     (round (escapement * 1800/pi))
-- 		     0                     -- orientation
-- 		     weight
-- 		     italic False False    -- italic, underline, strikeout
-- 		     aNSI_CHARSET
-- 		     oUT_DEFAULT_PRECIS
-- 		     cLIP_DEFAULT_PRECIS
-- 		     dEFAULT_QUALITY
-- 		     dEFAULT_PITCH
-- 		     family
--  where
--   weight | bold      = fW_BOLD
-- 	    | otherwise = fW_NORMAL


-- missing CreateFontIndirect from WinFonts.ss; GSL ???

deleteFont :: HFONT -> IO ()
deleteFont arg1 =
  prim_Win32Font_cpp_deleteFont arg1
primitive prim_Win32Font_cpp_deleteFont :: Addr -> IO ()

----------------------------------------------------------------

type StockFont      = WORD

oEM_FIXED_FONT :: StockFont
oEM_FIXED_FONT =
  unsafePerformIO(
    prim_Win32Font_cpp_oEM_FIXED_FONT >>= \ (res1) ->
    let gc_res1 = ( fromIntegral  (res1)) in
    (return (gc_res1)))
primitive prim_Win32Font_cpp_oEM_FIXED_FONT :: IO (Word32)
aNSI_FIXED_FONT :: StockFont
aNSI_FIXED_FONT =
  unsafePerformIO(
    prim_Win32Font_cpp_aNSI_FIXED_FONT >>= \ (res1) ->
    let gc_res1 = ( fromIntegral  (res1)) in
    (return (gc_res1)))
primitive prim_Win32Font_cpp_aNSI_FIXED_FONT :: IO (Word32)
aNSI_VAR_FONT :: StockFont
aNSI_VAR_FONT =
  unsafePerformIO(
    prim_Win32Font_cpp_aNSI_VAR_FONT >>= \ (res1) ->
    let gc_res1 = ( fromIntegral  (res1)) in
    (return (gc_res1)))
primitive prim_Win32Font_cpp_aNSI_VAR_FONT :: IO (Word32)
sYSTEM_FONT :: StockFont
sYSTEM_FONT =
  unsafePerformIO(
    prim_Win32Font_cpp_sYSTEM_FONT >>= \ (res1) ->
    let gc_res1 = ( fromIntegral  (res1)) in
    (return (gc_res1)))
primitive prim_Win32Font_cpp_sYSTEM_FONT :: IO (Word32)
dEVICE_DEFAULT_FONT :: StockFont
dEVICE_DEFAULT_FONT =
  unsafePerformIO(
    prim_Win32Font_cpp_dEVICE_DEFAULT_FONT >>= \ (res1) ->
    let gc_res1 = ( fromIntegral  (res1)) in
    (return (gc_res1)))
primitive prim_Win32Font_cpp_dEVICE_DEFAULT_FONT :: IO (Word32)
sYSTEM_FIXED_FONT :: StockFont
sYSTEM_FIXED_FONT =
  unsafePerformIO(
    prim_Win32Font_cpp_sYSTEM_FIXED_FONT >>= \ (res1) ->
    let gc_res1 = ( fromIntegral  (res1)) in
    (return (gc_res1)))
primitive prim_Win32Font_cpp_sYSTEM_FIXED_FONT :: IO (Word32)

getStockFont :: StockFont -> IO HFONT
getStockFont gc_arg1 =
  case ( fromIntegral  gc_arg1) of { arg1 ->
  prim_Win32Font_cpp_getStockFont arg1 >>= \ (res1) ->
  (return (res1))}
primitive prim_Win32Font_cpp_getStockFont :: Word32 -> IO (Addr)

----------------------------------------------------------------
-- End
----------------------------------------------------------------
needPrims_hugs 2
