{
module PortCapParser
(
  PC(PC),
  parsePortCap
) where

import Data.Char
}

%name parser
%tokentype { Token }
%error { parseError }

%token
      'OR'            { TokenOR }
      'NOOP'          { TokenNoop }
      'RU'            { TokenRU }
      'RW'            { TokenRW }
      'R'             { TokenR }
      'W'             { TokenW }
      'A'             { TokenA }
      'C'             { TokenC }
      int             { TokenInt $$ }

%%

Pc : Pc1              { [$1] }
   | Pc 'OR' Pc1      { $3 : $1 }

Pc1 : Pcr             { $1 }
    | Pcrw            { $1 }
    | Pcw             { $1 }
    | Pcru            { $1 }
    | Pcc             { $1 }
    | Pca             { $1 }
    | Pcnoop          { $1 }

Pcnoop : 'NOOP'       { pcnoop }

Pcr : int 'R'         { pcr $1 }
    | int 'R' Pcrw    { merge (pcr $1) $3 }
    | int 'R' Pcw     { merge (pcr $1) $3 }
    | int 'R' Pcru    { merge (pcr $1) $3 }

Pcrw : int 'RW'       { pcrw $1 }
     | int 'RW' Pcw   { merge (pcrw $1) $3 }
     | int 'RW' Pcru  { merge (pcr $1) $3 }

Pcw  : int 'W'        { pcw $1 }
     | int 'W' Pcru   { merge (pcw $1) $3 }

Pcru : int 'RU'       { pcru $1 }

Pcc : int 'C'         { pcc $1 }
    | int 'C' Pca     { merge (pca $1) $3 }

Pca : int 'A'         { pca $1 }

{
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
}