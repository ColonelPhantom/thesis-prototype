{-# LANGUAGE TupleSections #-}
module HsDirac where

import Data.List (group, intercalate, sort)
import Data.Ratio (Ratio, denominator, numerator, (%))
import System.Environment (getArgs)
import Data.Maybe (maybeToList)

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
      { prob :: Ratio Int,
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
  show (Bool b) = show b
  show (Cond c t f) = show c ++ " ? " ++ show t ++ " : " ++ show f

instance Show VarDecl where
  show (IntVar n lower upper init) =
    unwords
      [n, ":", "[", show lower, "..", show upper, "]", "init", show init]
      ++ ";"
  show (BoolVar n init) = n ++ ": bool init " ++ show init ++ ";"

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
  show (Update prob assigns) = ratio ++ ":\t" ++ intercalate " & " (map show assigns)
    where
      ratio = show (numerator prob) ++ "/" ++ show (denominator prob)
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

-- Now the Dirac Dice specific stuff (finally)
diceProbs :: ([(Int, Int)], Int)
diceProbs = (map (,1) [1..6], 6)

player :: GameConfig -> Int -> TopLevel
player gc i = Module player [pos] [command]
  where
    player = "player" ++ show i
    pos = IntVar ("p" ++ show i) 0 (boardsize gc) (Var $ "start" ++ show i)
    nextpos moved = Cond (new :>: Int (boardsize gc)) (ref pos) (move $ transitions gc) where
        new = ref pos :+: Int moved
        move [] = new
        move ((from,to):xs) = Cond (new :=: Int from) (Int to) (move xs)
    command =
      Command
        (getAction gc i)
        (Var "!finished") -- yeah, i know
        ( flip map (fst diceProbs) $ \(dice, prob) ->
            Update
              (prob % snd diceProbs)
              [Assign (name pos) (nextpos dice)]
        )

singlestep :: Int -> Int -> [(Int, Int)] -> GameConfig
singlestep p b t = GC p b action t (Just gamemaster) where
    action i = "step" ++ show i
    gamemaster = Module "gamemaster" [player] steps
      where
        player = IntVar "pl" 0 (p-1) (Int 0)
        steps = map step [0 .. p-1]
        step i =
          Command
            (action i)
            (Conj [Var "!finished", ref player :=: Int i])
            [AlwaysUpdate [Assign (name player) (Int $ if i /= (p-1) then i + 1 else 0)]]

generate :: GameConfig -> [Int] -> String
generate gc inits = printProgram $ is ++ [finished] ++ ps ++ maybeToList (gamemaster gc) ++ wins
  where
    is = map (\(i, x) -> Constant "int" ("start" ++ show i) (show x)) $ take (players gc) $ zip [0 ..] inits
    finished = Formula "finished" $ Disj $ map (\i -> Var ("p" ++ show i) :=: Int (boardsize gc)) [0 .. players gc - 1]
    ps = map (player gc) [0 .. players gc - 1]
    wins = map win [0 .. players gc - 1]
    win i = Label ("win" ++ show i) $ Conj $ (Var ("p" ++ show i) :>=: Int (boardsize gc)):first where
        first = map nonwin [1 .. i-1]
        nonwin i = Var ("p" ++ show i) :<: Int (boardsize gc)

readTransition :: String -> (Int, Int)
readTransition s = (read a, read b) where [a,b] = words s

main :: IO ()
main = do
  [nplayers, positions] <- map read <$> getArgs
  transitions <- map readTransition . lines <$> getContents
  putStr $ generate (singlestep nplayers positions transitions) (repeat 0)
