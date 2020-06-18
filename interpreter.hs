-- Very simple Poly language interpreter written in Haskell
-- Dependency: ghc, stack
-- Usage:
--   Write input in line 39
--   $ make haskell

{-
    expr  = Const( float  val  )
        | Var  ( string name )
        | Add  ( expr lhs, expr rhs )
        | Mul  ( expr lhs, expr rhs )
        | Let  ( string name, expr rhs, expr body )
-}


data Expr
  = Const Int
  | Add Expr Expr
  | Mul Expr Expr
  | Var String
  | Let String Expr Expr
  deriving (Show)

lookupEnv :: String -> [(String, Int)] -> Int
lookupEnv target ((str,val):list)
  = if (target == str) then val else lookupEnv target list 
lookupEnv target []
  = error ("Could not find Val " ++ target)

eval :: Expr -> [(String, Int)] -> Int
eval (Const x) env = x
eval (Add x y) env = (eval x env) + (eval y env)
eval (Mul x y) env = (eval x env) * (eval y env)
eval (Var x) env = lookupEnv x env
eval (Let name value body) env = eval body ((name, (eval value env)):env)

main :: IO ()
main = do
-- Write input here
  let input = Let "a" (Const 2) (Add (Var "a") (Const 3))
  let output = eval input []
  print output
