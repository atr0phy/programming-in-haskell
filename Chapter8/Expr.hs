module Chapter8.Expr where

data Expr = Val Int | Add Expr Expr | Mult Expr Expr

value :: Expr -> Int
value e = eval e []
-- value (Val n  ) = n
-- value (Add (Val x) (Val y)) = value x + value y
-- value (Mult (Val x) (Val y)) = value x * value y

type Cont = [Op]
data Op = EVALADD Expr | EVALMULT Expr | ADD Int | MULT Int

eval :: Expr -> Cont -> Int
eval (Val n   ) c = exec c n
eval (Add  x y) c = eval x (EVALADD y : c)
eval (Mult x y) c = eval x (EVALMULT y : c)

exec :: Cont -> Int -> Int
exec []               n = n
exec (EVALADD  y : c) n = eval y (ADD n : c)
exec (EVALMULT y : c) n = eval y (MULT n : c)
exec (ADD      n : c) m = exec c (n + m)
exec (MULT     n : c) m = exec c (n * m)

expr1 = value (Add (Val 2) (Val 1))
expr2 = value (Mult (Add (Val 2) (Val 3)) (Add (Val 1) (Val 3)))
