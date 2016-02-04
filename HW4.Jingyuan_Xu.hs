--Jingyuan Xu
--932428597
module HW4 where
--Approach one
--Q1

data Reg=A|B
  deriving (Eq, Show)

data Expr= I Int
 |R Reg
 |Add Expr Expr
 |Less Expr Expr
 |Not Expr
 |And Expr Expr
 deriving (Eq, Show)

data Stmt= RE Reg Expr
 |IF Expr Stmt Stmt
 deriving (Eq, Show)

type Prog=[Stmt]

type State = (Int,Int)   -- the state of registers A and B

data Val = In Int         -- integer result
 | Bo Bool        -- boolean result
 | Error
  deriving (Eq, Show)
--Q2 Implement the three semantic valuation functions

-- |
--   >>> expr (I 3) (1,2)
--   In 3
--

expr :: Expr -> State -> Val
expr (I x) (a,b)= In x
expr (R A) (a,b)=In a
expr (R B) (a,b)=In b

expr (Add x y) (a,b)=case (expr x (a,b), expr y (a,b)) of
                          (In i, In j)->In ((+)i j)
                          _           ->Error
expr (Less x y)(a,b)= case (expr x (a,b), expr y (a,b)) of
                            (In i, In j)->Bo ((<=)i j)
                            _           ->Error

expr (Not x)(a,b)= case (expr x (a,b)) of
				   (Bo i) -> if (i==True) then Bo False else Bo True
				   _      ->Error

expr (And x y)(a,b)=case (expr x (a,b), expr y (a,b)) of
--                          (Bo i,Bo j)->if(i==True&&j==True) then Bo True else Bo False
                          (Bo i, Bo j) ->Bo (i&&j)
                          _            ->Error

stmt :: Stmt -> State -> Maybe State
stmt (RE A e) (a,b)=Just (a,b)
stmt (RE B e) (a,b)=Just (a,b)
stmt (IF e s1 s2) (a,b)= case(expr e (a,b)) of
--						 Bo i->if(i==True) then stmt s1 (a,b) else stmt s2 (a,b)
						 Bo True  -> stmt s1 (a,b)
                         Bo False -> stmt s2 (a,b)
                         _        -> Nothing




prog :: Prog -> State -> Maybe State
prog [] (a,b)= Just(a,b)
prog (x:xs)(a,b)=case (stmt x (a,b)) of
                       Just (c,d) -> prog xs (c,d)
                       _       -> Nothing

--A := 3;          { set register A to 3 }
--B := A+2;        { set register B to A+2, which will be 5 }
--if A <= B        { if A is less than or equal to B ... }
--A := A + A     { ... then double A }
--B := B + B;    { ... otherwise double B }
--B := A+B;        { set B to A+B }
--{ at the end of the program: A=6, B=11 }
test:: Prog
test=[RE A (I 3),RE B (Add(R A)(I 2)),IF (Less (R A) (R B)) (RE A (Add(R A)(R A))) (RE B (Add(R B)(R B))), RE B (Add(R A)(R B))]


