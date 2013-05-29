module Imp.Syntax where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Token


-- Type Definitions --

--Integer type definition --
-- Op = opposite, Mult = multiplication, Div = division, 
-- Sub = substraction, Ad = addition, Mod = modulo
data IntExp = 	Var String | Intr Integer | Op IntExp | 
		Mult IntExp IntExp | Div IntExp IntExp | 
		Mod IntExp IntExp | Sub IntExp IntExp | 
		Ad IntExp IntExp deriving Show

-- Boolean expressions type definition --
-- Or = or, And = y Impl = implication, Equiv = equivalence, Grth = greater than,
-- LsTh = lesser than, GrThEq = greater than or equal, LsThEq = lesser than or equal,
-- Dis = distinct, Eq = equal
data BoolExp = 	BoolCte Bool | Not BoolExp | And BoolExp BoolExp | 
		Or BoolExp BoolExp | Impl BoolExp BoolExp | 
		Equiv BoolExp BoolExp | GrTh IntExp IntExp | 
		LsTh IntExp IntExp | GrThEq IntExp IntExp | 
		LsThEq IntExp IntExp | Dis IntExp IntExp | 
		Eq IntExp IntExp  deriving Show

-- Sentence type definition --
data Comm = 	Skip | String := IntExp | Output String | Input String | 
		If BoolExp Comm Comm | While BoolExp Comm | Seq [Comm] | 
		For String IntExp IntExp Comm | Newvar String IntExp Comm | 
		Fail String | Catch String Comm Comm deriving Show

-- Definition of words and characters reserved for my imperative language --
definicion = emptyDef{ identStart = letter, identLetter = alphaNum, 
		opStart = oneOf "-><|&=:/%*+", 
		opLetter = oneOf "-><|&=:/%*+", 
		reservedOpNames = ["=>","<=>","==","/=","<","<=",">",">=",
				   "&","|","*","+","/","%","-",":=","not"],
		reservedNames = ["true","false","skip","if","then","else",
				 "fi","while","do","od","for","to","fail",
				 "catch","in","with","newvar","{","}","!","?"]
		}

-- tokenizer definition --             
TokenParser{ parens = myparens, identifier = myidentifier, integer = myinteger,
	     reservedOp = myreservedOp, reserved = myreserved, 
	     semiSep1 = mysemiSep1, whiteSpace = mywhiteSpace } 
	   = makeTokenParser definicion

-- Boolean expressions parser
bool_parser :: Parser BoolExp
bool_parser = buildExpressionParser tabla_bool terminos_bool

-- the order in this table establishes the precedence
tabla_bool = [  [Prefix (myreservedOp "not" >> return Not)],
		[Infix (myreservedOp "&" >> return And) AssocLeft],
		[Infix (myreservedOp "|" >> return Or) AssocLeft],
		[Infix (myreservedOp "=>" >> return Impl) AssocLeft],
		[Infix (myreservedOp "<=>" >> return Equiv) AssocLeft]
	]

terminos_bool = try relacion_expresion
       <|> myparens bool_parser
       <|> (myreserved "true" >> return (BoolCte True))
       <|> (myreserved "false" >> return (BoolCte False))

           
-- Integer expressions parser --
expresion_parser :: Parser IntExp
expresion_parser = buildExpressionParser tabla_expresion terminos_expresion

-- the order in this table establishes the precedence
tabla_expresion =   [ [Prefix (myreservedOp "-" >> return Op)],
		      [Infix (myreservedOp "/" >> return Div) AssocLeft],
		      [Infix (myreservedOp "*" >> return Mult) AssocLeft],
		      [Infix (myreservedOp "%" >> return Mod) AssocLeft],
		      [Infix (myreservedOp "-" >> return Sub) AssocLeft],
		      [Infix (myreservedOp "+" >> return Ad) AssocLeft]
		]

terminos_expresion = myparens expresion_parser
       <|> fmap Var myidentifier
       <|> fmap Intr myinteger

relacion_expresion =
  do expresion_1 <- expresion_parser
     operacion <- relacion
     expresion_2 <- expresion_parser
     return $ operacion expresion_1 expresion_2

relacion = (myreservedOp ">" >> return GrTh)
       <|> (myreservedOp "<" >> return LsTh)
       <|> (myreservedOp "==" >> return Eq)
       <|> (myreservedOp "/=" >> return Dis) 
       <|> (myreservedOp ">=" >> return GrThEq)
       <|> (myreservedOp "<=" >> return LsThEq)

-- Sentence parser --
sentencia_parser :: Parser Comm
sentencia_parser = fmap Seq (mysemiSep1 sentencia)
sentencia = (	myreserved "skip" >> return Skip)
		<|> do { v <- myidentifier; myreservedOp ":="; e <- expresion_parser;
			return (v := e)
			}
		<|> do { myreserved "if"; b <- bool_parser; myreserved "then";
			s1 <- sentencia_parser; myreserved "else";
			s2 <- sentencia_parser; myreserved "fi";
			return (If b s1 s2)
			}
		<|> do { myreserved "while"; b <- bool_parser; myreserved "do";
			s <- sentencia_parser; myreserved "od";
			return (While b s)
			}
		<|> do { myreserved "for"; v <- myidentifier; myreservedOp ":=";
			e <- expresion_parser; myreserved "to"; 
			b <- expresion_parser; myreserved "do";
			s <- sentencia_parser; myreserved "od";
			return (For v e b s)
			}
		<|> do { myreserved "newvar"; v <- myidentifier; myreservedOp ":=";
			e <- expresion_parser; myreserved "{";
			s <- sentencia_parser; myreserved "}";
			return (Newvar v e s)
			}
		<|> do { myreserved "fail"; l <- myidentifier;
			return (Fail l)
			}
		<|> do { myreserved "catch"; l <- myidentifier; myreserved "in";
			s1 <- sentencia_parser; myreserved "with";
			s2 <- sentencia_parser;
			return (Catch l s1 s2)
			}
		<|> do { myreserved "?"; v <- myidentifier;
			return (Input v)
			}
		<|> do { myreserved "!"; v <- myidentifier;
			return (Output v)
			}

-- Main parser --
main_parser :: Parser Comm
main_parser = mywhiteSpace >> sentencia_parser <* eof

-- Parse expresion --
parsear_expresion :: String -> IO ()
parsear_expresion input = case parse main_parser "" input of
             {  Left errmsg -> print errmsg;
		Right answer -> print answer
             }
parse_command :: String -> Comm
parse_command input = case parse main_parser "" input of
             { -- Left errmsg ->  errmsg;
		Right answer -> answer
             }

