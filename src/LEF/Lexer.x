{
module LEF.Lexer
    ( Lexer (..)
    , Token (..)
    , lexer
    ) where

import Data.Char (ord)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import LEF.Tokens
}

$any     = [.\n\r]
@newline = [\n\r] | \r\n
@comment = "/*" $any* "*/"
         | "//" .* @newline

@nul_eof = \0 $any*

@preprocessor = \# .* @newline

-- C# actually defines a letter to be any character (or escape sequence)
-- from the Unicode classes Lu, Ll, Lt, Lm, Lo or Nl. Identifiers must
-- start with a letter or an underscore, but can then also contain
-- characters from the classes Mn, Mc, Nd, Pc or Cf.
$ident_start = [a-zA-Z_\@]
$ident_part  = [a-zA-Z_0-9]
$const_part  = [A-Z_]

$digit     = [0-9]
$hex_digit = [0-9a-fA-F]
$sign      = [\+\-]

@int_suffix  = [uU][lL]? | [lL][uU]?
@real_suffix = [fFdDmM]
@exponent    = [eE] $sign? $digit+

@simple_escape  = \\ [0abfnrtv\'\"\\]
@hex_escape     = \\x $hex_digit{1,4}
@unicode_escape = \\u $hex_digit{4} | \\U $hex_digit{8}
@escapes        = @simple_escape | @hex_escape | @unicode_escape

@character          = [^\'\\] | @escapes
@string_character   = [^\"\\] | @escapes
@verbatim_character = $any # \" | \"\"


tokens :-

$white+       ;
@comment      ;
@nul_eof      ;
@preprocessor ;

-- T.T
\; ;

-- Keywords
END                { constTok Tok_End       }
LIBRARY            { constTok Tok_Library   }
VERSION            { constTok Tok_Version   }
NAMESCASESENSITIVE { constTok Tok_Namescasesensitive }
BUSBITCHARS        { constTok Tok_BusBitChars }
DIVIDERCHAR        { constTok Tok_DividerChar }
UNITS              { constTok Tok_Units       }
DATABASE           { constTok Tok_Database    }
ON                 { constTok Tok_On          }
OFF                { constTok Tok_Off         }
MICRONS	           { constTok Tok_Microns     }
USEMINSPACING	   { constTok Tok_UseMinSpacing }
OBS	               { constTok Tok_Obs }
PIN	               { constTok Tok_Pin }
CLEARANCEMEASURE   { constTok Tok_ClearanceMeasure }
MANUFACTURINGGRID  { constTok Tok_ManufacturingGrid }
LAYER            { constTok Tok_Layer }
TYPE             { constTok Tok_Type }
SPACING          { constTok Tok_Spacing }
DIRECTION        { constTok Tok_Direction }
PITCH            { constTok Tok_Pitch }
OFFSET           { constTok Tok_Offset }
PATH             { constTok Tok_Path }
WIDTH            { constTok Tok_Width }
RESISTANCE       { constTok Tok_Resistance }
EDGECAPACITANCE  { constTok Tok_EdgeCapacitance }
CAPACITANCE      { constTok Tok_Capacitance }
VIA	             { constTok Tok_Via }
RECT             { constTok Tok_Rect }
VIARULE	         { constTok Tok_ViaRule }
TO               { constTok Tok_To }
BY               { constTok Tok_By }
OVERHANG         { constTok Tok_Overhang }
METALOVERHANG	 { constTok Tok_MetalOverhang }
SITE             { constTok Tok_Site }
SYMMETRY         { constTok Tok_Symmetry }
CLASS            { constTok Tok_Class }
SIZE             { constTok Tok_Size }
MACRO            { constTok Tok_Macro }
FOREIGN	         { constTok Tok_Foreign }
ORIGIN           { constTok Tok_Origin }
USE              { constTok Tok_Use }
SHAPE            { constTok Tok_Shape }
PORT             { constTok Tok_Port }
LIBRARY          { constTok Tok_Library }

-- Integer literals
\-    $digit+     @int_suffix? { textTok Tok_Number }
      $digit+     @int_suffix? { textTok Tok_Number }

-- Real literals
\-  $digit+ \. $digit+ @exponent? @real_suffix? { textTok Tok_Number }
    $digit+ \. $digit+ @exponent? @real_suffix? { textTok Tok_Number }
            \. $digit+ @exponent? @real_suffix? { textTok Tok_Number }
               $digit+ @exponent  @real_suffix? { textTok Tok_Number }
               $digit+            @real_suffix  { textTok Tok_Number }

-- Character / String literals
\" @string_character* \"     { textTok (Tok_String . T.drop 1 . T.init)   }

-- Identifiers
$ident_start $ident_part*      { textTok Tok_Ident }

{
wrap :: (str -> tok) -> AlexPosn -> str -> Lexer tok
wrap f (AlexPn _ line col) s = L (line, col) (f s)

constTok = wrap . const
bstrTok f = wrap (f . T.encodeUtf8)
textTok = wrap

lexer :: String -> T.Text -> [Lexer Token]
lexer file text = go (alexStartPos, '\n', text `T.snoc` '\n')
  where
    go inp@(pos, _, cs) = case {-# SCC "alexScan" #-} alexScan inp 0 of
        AlexEOF                -> []
        AlexError inp'         -> error (errMsg inp')
        AlexSkip  inp'   _     -> go inp'
        AlexToken inp' len act -> act pos (T.take len cs) : go inp'

    errMsg (AlexPn _ line col, _, cs) =
        file ++ ": lexical error (line " ++ show line ++ ", col " ++ show col ++ ")\n"
             ++ "    near " ++ show (T.unpack $ T.take 40 cs)

-----------------------------------------------------------

type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  T.Text)       -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_,c,_) = c

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,_,cs) | T.null cs  = Nothing
                     | {-# SCC "alexSkip" #-} alexSkip c = alexGetChar (p', c, cs')
                     | otherwise  = p' `seq` cs' `seq` Just (c, (p', c, cs'))
  where
    c   = T.head cs
    cs' = T.tail cs
    p'  = alexMove p c

alexGetByte :: AlexInput -> Maybe (Int,AlexInput)
alexGetByte i = case alexGetChar i of
  Nothing -> Nothing
  Just (c, j) -> Just (ord c, j)

alexSkip :: Char -> Bool
alexSkip '\xFEFF' = True
alexSkip _        = False

-----------------------------------------------------------

data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+7) `div` 8)*8+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)
}
