{
module Parser where

import Data.Char
import Data.List

import Prop
}

%name parser
%tokentype {Token}
%error {parseError}

%token 
      cons            { TokenCons $$ }
      var             { TokenVar $$ }
      '('             { TokenOB }
      ')'             { TokenCB }
      and             { TokenAnd }
      or              { TokenOr }
      impl            { TokenImpl }
      syss            { TokenSyss }
      not             { TokenNot }
      

%left syss
%right impl
%left and or
%left not
%%

Prop   : cons                           { Cons $1 }
      | var                             { Var $1 }
      | '(' Prop ')'                    { $2 }
      | Prop and Prop                   { And $1 $3 }
      | Prop or Prop                    { Or $1 $3 }
      | Prop impl Prop                  { Impl $1 $3 }
      | Prop syss Prop                  { Syss $1 $3 }
      | not Prop                        { Not $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokenCons Bool
      | TokenVar String
      | TokenOB
      | TokenCB
      | TokenAnd
      | TokenOr
      | TokenImpl
      | TokenSyss
      | TokenNot
      deriving Show


lexer :: String -> [Token]
lexer [] = []
lexer ('T':cs) = TokenCons True : lexer cs
lexer ('⊥':cs) = TokenCons False : lexer cs
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
lexer ('¬':cs) = TokenNot : lexer cs
lexer ('∧':cs) = TokenAnd : lexer cs
lexer ('∨':cs) = TokenOr : lexer cs
lexer ('→':cs) = TokenImpl : lexer cs
lexer ('↔':cs) = TokenSyss : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexVar cs =
   case span isAlpha cs of
      (var,rest)   -> TokenVar var : lexer rest



--main = getContents >>= print . parser . lexer

}
