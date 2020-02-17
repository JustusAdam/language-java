{
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-unused-binds #-}
module Language.Java.Lexer (LexerM, runLexerM, getPosition, Token(..), lexer) where

import Numeric
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "monad-bytestring"

$digit      = [0-9]
$nonzero    = [1-9]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]

@lineterm = [\n\r] | \r\n

-- TODO: this doesn't notice a comment that ends "**/"
@tradcomm = "/*" ( ~[\*] | \*+ (~[\/\*] | \n) | \n )* \*+ "/"
@linecomm = "//" .* @lineterm
@comm = @tradcomm | @linecomm

$javaLetter = [a-zA-Z\_\$]
$javaDigit = $digit
$javaLetterOrDigit = [a-zA-Z0-9\_\$]

@octEscape = [0123]? $octdig{1,2}
@hexEscape = u $hexdig{4}
@charEscape = \\ (@octEscape | @hexEscape | [btnfr\"\'\\])

@expsuffix = [\+\-]? $digit+
@exponent = [eE] @expsuffix
@pexponent = [pP] @expsuffix

tokens  :-

    $white+         ;
    @comm           ;

    "@interface"    { direct KW_AnnInterface }
    abstract        { direct KW_Abstract     }
    assert          { direct KW_Assert       }
    boolean         { direct KW_Boolean      }
    break           { direct KW_Break        }
    byte            { direct KW_Byte         }
    case            { direct KW_Case         }
    catch           { direct KW_Catch        }
    char            { direct KW_Char         }
    class           { direct KW_Class        }
    const           { direct KW_Const        }
    continue        { direct KW_Continue     }
    default         { direct KW_Default      }
    do              { direct KW_Do           }
    double          { direct KW_Double       }
    else            { direct KW_Else         }
    enum            { direct KW_Enum         }
    extends         { direct KW_Extends      }
    final           { direct KW_Final        }
    finally         { direct KW_Finally      }
    float           { direct KW_Float        }
    for             { direct KW_For          }
    goto            { direct KW_Goto         }
    if              { direct KW_If           }
    implements      { direct KW_Implements   }
    import          { direct KW_Import       }
    instanceof      { direct KW_Instanceof   }
    int             { direct KW_Int          }
    interface       { direct KW_Interface    }
    long            { direct KW_Long         }
    native          { direct KW_Native       }
    new             { direct KW_New          }
    package         { direct KW_Package      }
    private         { direct KW_Private      }
    protected       { direct KW_Protected    }
    public          { direct KW_Public       }
    return          { direct KW_Return       }
    short           { direct KW_Short        }
    static          { direct KW_Static       }
    strictfp        { direct KW_Strictfp     }
    super           { direct KW_Super        }
    switch          { direct KW_Switch       }
    synchronized    { direct KW_Synchronized }
    this            { direct KW_This         }
    throw           { direct KW_Throw        }
    throws          { direct KW_Throws       }
    transient       { direct KW_Transient    }
    try             { direct KW_Try          }
    void            { direct KW_Void         }
    volatile        { direct KW_Volatile     }
    while           { direct KW_While        }

    0               { direct $ IntTok 0        }
    0 [lL]          { direct $ LongTok 0       }
    0 $digit+       { tokenOverInputStr $ IntTok . pickyReadOct }
    0 $digit+ [lL]  { tokenOverInputStr $ LongTok . pickyReadOct }
    $nonzero $digit*        { tokenOverInputStr $ IntTok . read }
    $nonzero $digit* [lL]   { tokenOverInputStr $ LongTok . read . init }
    0 [xX] $hexdig+         { tokenOverInputStr $ IntTok . fst . head . readHex . drop 2 }
    0 [xX] $hexdig+ [lL]    { tokenOverInputStr $ LongTok . fst . head . readHex . init . drop 2 }

    $digit+ \. $digit* @exponent? [dD]?           { tokenOverInputStr $ \s -> DoubleTok (fst . head $ readFloat $ '0':s) }
            \. $digit+ @exponent? [dD]?           { tokenOverInputStr $ \s -> DoubleTok (fst . head $ readFloat $ '0':s) }
    $digit+ \. $digit* @exponent? [fF]            { tokenOverInputStr $ \s -> FloatTok  (fst . head $ readFloat $ '0':s) }
            \. $digit+ @exponent? [fF]            { tokenOverInputStr $ \s -> FloatTok  (fst . head $ readFloat $ '0':s) }
    $digit+ @exponent                             { tokenOverInputStr $ \s -> DoubleTok (fst . head $ readFloat s) }
    $digit+ @exponent? [dD]                       { tokenOverInputStr $ \s -> DoubleTok (fst . head $ readFloat s) }
    $digit+ @exponent? [fF]                       { tokenOverInputStr $ \s -> FloatTok  (fst . head $ readFloat s) }
    0 [xX] $hexdig* \.? $hexdig* @pexponent [dD]? { tokenOverInputStr $ \s -> DoubleTok (readHexExp (drop 2 s)) }
    0 [xX] $hexdig* \.? $hexdig* @pexponent [fF]  { tokenOverInputStr $ \s -> FloatTok  (readHexExp (drop 2 s)) }

    true            { direct $ BoolTok True    }
    false           { direct $ BoolTok False   }

    ' (@charEscape | ~[\\\']) '               { tokenOverInputStr $ \s -> CharTok (readCharTok s) }

    \" (@charEscape | ~[\\\"])* \"            { tokenOverInputStr $ \s -> StringTok (readStringTok s) }

    null            {direct NullTok }

    $javaLetter $javaLetterOrDigit*     { tokenOverInputStr $ \s -> IdentTok s }

    \(              { direct OpenParen       }
    \)              { direct CloseParen      }
    \[              { direct OpenSquare      }
    \]              { direct CloseSquare     }
    \{              { direct OpenCurly       }
    \}              { direct CloseCurly      }
    \;              { direct SemiColon       }
    \,              { direct Comma           }
    \.              { direct Period          }
    "->"            { direct LambdaArrow     }
    "::"            { direct MethodRefSep    }

    "="             { direct Op_Equal        }
    ">"             { direct Op_GThan        }
    "<"             { direct Op_LThan        }
    "!"             { direct Op_Bang         }
    "~"             { direct Op_Tilde        }
    "?"             { direct Op_Query        }
    ":"             { direct Op_Colon        }
    "=="            { direct Op_Equals       }
    "<="            { direct Op_LThanE       }
    ">="            { direct Op_GThanE       }
    "!="            { direct Op_BangE        }
    "&&"            { direct Op_AAnd         }
    "||"            { direct Op_OOr          }
    "++"            { direct Op_PPlus        }
    "--"            { direct Op_MMinus       }
    "+"             { direct Op_Plus         }
    "-"             { direct Op_Minus        }
    "*"             { direct Op_Star         }
    "/"             { direct Op_Slash        }
    "&"             { direct Op_And          }
    "|"             { direct Op_Or           }
    "^"             { direct Op_Caret        }
    "%"             { direct Op_Percent      }
    "<<"            { direct Op_LShift       }
    "+="            { direct Op_PlusE        }
    "-="            { direct Op_MinusE       }
    "*="            { direct Op_StarE        }
    "/="            { direct Op_SlashE       }
    "&="            { direct Op_AndE         }
    "|="            { direct Op_OrE          }
    "^="            { direct Op_CaretE       }
    "%="            { direct Op_PercentE     }
    "<<="           { direct Op_LShiftE      }
    ">>="           { direct Op_RShiftE      }
    ">>>="          { direct Op_RRShiftE     }
    "@"             { direct Op_AtSign       }


{

type LexerM = Alex

direct tok = only (pure tok)

onlyError msg = only (alexError msg)

only f _ _ = f

tokenOverInputStr f = withMatchedInput (pure . f . BS.unpack)

withMatchedInput f (_, _, input, _) len = f (BS.take len input)

alexEOF = pure EOF

pickyReadOct :: String -> Integer
pickyReadOct s =
  if not $ null remStr
  then lexicalError $ "Non-octal digit '" ++ take 1 remStr ++ "' in \"" ++ s ++ "\"."
  else n
    where (n,remStr) = head $ readOct s

readHexExp :: (Floating a, Eq a) => String -> a
readHexExp initial =
    let (m, suf) = head $ readHex initial
        (e, _) = case suf of
                      p:s | toLower p == 'p' -> head $ readHex s
                      _                      -> (0, "")
     in m ** e

readCharTok :: String -> Char
readCharTok s = head . convChar . dropQuotes $ s
readStringTok :: String -> String
readStringTok = convChar . dropQuotes

dropQuotes :: String -> String
dropQuotes s = take (length s - 2) (tail s)

-- Converts a sequence of (unquoted) Java character literals, including
-- escapes, into the sequence of corresponding Chars. The calls to
-- 'lexicalError' double-check that this function is consistent with
-- the lexer rules for character and string literals. This function
-- could be expressed as another Alex lexer, but it's simple enough
-- to implement by hand.
convChar :: String -> String
convChar ('\\':'u':s@(d1:d2:d3:d4:s')) =
  -- TODO: this is the wrong place for handling unicode escapes
  -- according to the Java Language Specification. Unicode escapes can
  -- appear anywhere in the source text, and are best processed
  -- before lexing.
  if all isHexDigit [d1,d2,d3,d4]
  then toEnum (read ['0','x',d1,d2,d3,d4]):convChar s'
  else lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':'u':s) =
  lexicalError $ "bad unicode escape \"\\u" ++ take 4 s ++ "\""
convChar ('\\':c:s) =
  if isOctDigit c
  then convOctal maxRemainingOctals
  else (case c of
          'b' -> '\b'
          'f' -> '\f'
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          '\'' -> '\''
          '\\' -> '\\'
          '"' -> '"'
          _ -> badEscape):convChar s
  where maxRemainingOctals =
          if c <= '3' then 2 else 1
        convOctal n =
          let octals = takeWhile isOctDigit $ take n s
              noctals = length octals
              toChar = toEnum . fst . head . readOct
          in toChar (c:octals):convChar (drop noctals s)
        badEscape = lexicalError $ "bad escape \"\\" ++ c:"\""
convChar ("\\") =
  lexicalError "bad escape \"\\\""
convChar (x:s) = x:convChar s
convChar "" = ""

lexicalError :: String -> a
lexicalError = error . ("lexical error: " ++)

data Token
    -- Keywords
    = KW_Abstract
    | KW_AnnInterface
    | KW_Assert
    | KW_Boolean
    | KW_Break
    | KW_Byte
    | KW_Case
    | KW_Catch
    | KW_Char
    | KW_Class
    | KW_Const
    | KW_Continue
    | KW_Default
    | KW_Do
    | KW_Double
    | KW_Else
    | KW_Enum
    | KW_Extends
    | KW_Final
    | KW_Finally
    | KW_Float
    | KW_For
    | KW_Goto
    | KW_If
    | KW_Implements
    | KW_Import
    | KW_Instanceof
    | KW_Int
    | KW_Interface
    | KW_Long
    | KW_Native
    | KW_New
    | KW_Package
    | KW_Private
    | KW_Protected
    | KW_Public
    | KW_Return
    | KW_Short
    | KW_Static
    | KW_Strictfp
    | KW_Super
    | KW_Switch
    | KW_Synchronized
    | KW_This
    | KW_Throw
    | KW_Throws
    | KW_Transient
    | KW_Try
    | KW_Void
    | KW_Volatile
    | KW_While

    -- Separators
    | OpenParen
    | CloseParen
    | OpenSquare
    | CloseSquare
    | OpenCurly
    | CloseCurly
    | SemiColon
    | Comma
    | Period
    | LambdaArrow
    | MethodRefSep

    -- Literals
    | IntTok  Integer
    | LongTok Integer
    | DoubleTok Double
    | FloatTok Double
    | CharTok Char
    | StringTok String
    | BoolTok Bool
    | NullTok

    -- Identifiers
    | IdentTok String

    -- Operators
    | Op_Equal
    | Op_GThan
    | Op_LThan
    | Op_Bang
    | Op_Tilde
    | Op_Query
    | Op_Colon
    | Op_Equals
    | Op_LThanE
    | Op_GThanE
    | Op_BangE
    | Op_AAnd
    | Op_OOr
    | Op_PPlus
    | Op_MMinus
    | Op_Plus
    | Op_Minus
    | Op_Star
    | Op_Slash
    | Op_And
    | Op_Or
    | Op_Caret
    | Op_Percent
    | Op_LShift
    | Op_PlusE
    | Op_MinusE
    | Op_StarE
    | Op_SlashE
    | Op_AndE
    | Op_OrE
    | Op_CaretE
    | Op_PercentE
    | Op_LShiftE
    | Op_RShiftE
    | Op_RRShiftE
    | Op_AtSign

    | EOF
  deriving (Show, Eq)

lexer :: (Token -> LexerM a) -> LexerM a
lexer cont = alexMonadScan >>= cont

getPosition :: LexerM (Int, Int)
getPosition = Alex $ \s -> pure (s, let AlexPn _ line col = alex_pos s in (line, col))

runLexerM :: BS.ByteString -> LexerM a -> Either String a
runLexerM = runAlex

}
