{-# LANGUAGE TupleSections #-}
module HsDirac where

import Data.List (group, intercalate, sort)
import Data.Ratio (Ratio, denominator, numerator, (%))
import System.Environment (getArgs)
import Data.Maybe (maybeToList)
import Data.Char (toLower)

data TopLevel
  = Constant String String String
  | Formula String Expr
  | Module String [VarDecl] [Command]
  | Label String Expr

data Expr
  = Disj [Expr]
  | Conj [Expr]
  | Call String [Expr]
  | Expr :+: Expr
  | Var String
  | Int Int
  | Bool Bool
  | Expr :=: Expr
  | Expr :>=: Expr
  | Expr :>: Expr
  | Expr :<: Expr
  | Cond Expr Expr Expr
  | Not Expr

data VarDecl
  = IntVar String Int Int Expr
  | BoolVar String Bool

data Command = Command
  { action :: String,
    guard :: Expr,
    updates :: [Update]
  }

data Update
  = Update
      { prob :: String,
        assigns :: [Assignment]
      }
  | AlwaysUpdate [Assignment]

data Assignment = Assign String Expr

instance Show TopLevel where
  show (Constant t n v) = unwords ["const", t, n, "=", v] ++ ";"
  show (Formula n e) = unwords ["formula", n, "=", show e] ++ ";"
  show (Module name vars commands) =
    unwords ["module", name]
      ++ "\n"
      ++ unlines (map show vars)
      ++ unlines (map show commands)
      ++ "endmodule"
  show (Label name expr) = unwords ["label", "\"" ++ name ++ "\"", "=", show expr] ++ ";"

instance Show Expr where
  show (Disj es) = intercalate " | " (map show es)
  show (Conj es) = intercalate " & " (map show es)
  show (Call fn ps) = fn ++ "(" ++ intercalate ", " (map show ps) ++ ")"
  show (a :+: b) = show a ++ " + " ++ show b
  show (a :=: b) = show a ++ " = " ++ show b
  show (a :<: b) = show a ++ " < " ++ show b
  show (a :>: b) = show a ++ " > " ++ show b
  show (a :>=: b) = show a ++ " >= " ++ show b
  show (Var v) = v
  show (Int i) = show i
  show (Bool b) = map toLower $ show b
  show (Cond c t f) = show c ++ " ? " ++ show t ++ " : " ++ show f
  show (Not x) = "!" ++ show x

instance Show VarDecl where
  show (IntVar n lower upper init) =
    unwords
      [n, ":", "[", show lower, "..", show upper, "]", "init", show init]
      ++ ";"
  show (BoolVar n init) = n ++ ": bool init " ++ map toLower (show init) ++ ";"

instance Show Command where
  show (Command action guard updates) =
    "["
      ++ action
      ++ "] "
      ++ show guard
      ++ " ->\n"
      ++ intercalate " +\n" (map (('\t' :) . show) updates)
      ++ ";"

instance Show Update where
  show (Update prob assigns) = prob ++ ":\t" ++ intercalate " & " (map show assigns)
  show (AlwaysUpdate assigns) = intercalate " & " $ map show assigns

instance Show Assignment where
  show (Assign var val) = "(" ++ var ++ "' = " ++ show val ++ ")"



name :: VarDecl -> String
name (IntVar n _ _ _) = n
name (BoolVar n _) = n

ref :: VarDecl -> Expr
ref = Var . name

printProgram :: [TopLevel] -> String
printProgram xs = "dtmc\n" ++ unlines (map show xs)

data GameConfig = GC
  { players :: Int,
    boardsize :: Int,
    getAction :: Int -> String,
    transitions :: [(Int, Int)],
    gamemaster :: Maybe TopLevel
  }

readAction :: String -> (String, String)
readAction xs = let [p,q] = words xs in (p,q)

factory :: Int -> TopLevel
factory i = Module ("F" ++ show i) [BoolVar c False] [work, strike] where
    p = "p" ++ show i
    q = "q" ++ show i
    c = "c" ++ show i
    work = Command "a" (Not $ Var c) [Update p [Assign c (Bool True)],
                                        Update ("1-"++p) [Assign c (Bool False)]]
    strike = Command "a" (Var c) [Update q [Assign c (Bool False)],
                                Update ("1-"++q) [Assign c (Bool True)]]



generate :: Int -> [(String, String)] -> String
generate n c = printProgram $ as:(ps++qs++fs) where
    ps = zipWith (\i p -> Constant "double" ("p" ++ show i) p) [1..n] (map fst c)
    qs = zipWith (\i q -> Constant "double" ("q" ++ show i) q) [1..n] (map snd c)
    fs = map factory [1..n]
    as = Formula "allStrike" $ Conj $ map (\i -> Var ("c" ++ show i)) [1..n]


main :: IO ()
main = do
    [nfactories] <- map read <$> getArgs
    constants <- map readAction . lines <$> getContents
    putStr $ generate nfactories constants
