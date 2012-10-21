{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read,error)
import ITMOPrelude.Algebra
---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x = x
example1' = \x -> x
example1'' = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y = x %+ y
example2' x = \y -> x %+ y
example2'' = \x -> \y -> x %+ y
example2''' = \x y -> x %+ y
example2'''' = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True || _ = True
False || x = x

instance Monoid Bool where
	mempty = True
	mappend = (&&)
	
data OrBool = Or Bool

instance Monoid OrBool where
	mempty = Or False
	mappend (Or a) (Or b) = Or (a || b)
-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat  deriving (Show,Read)

natZero = Zero -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp Zero Zero = EQ
natCmp Zero (Succ _) = LT
natCmp (Succ _) Zero = GT
natCmp (Succ a) (Succ b) = natCmp a b  

-- n совпадает с m
natEq :: Nat -> Nat -> Bool
natEq Zero Zero = True
natEq Zero (Succ _) = False
natEq (Succ _) Zero = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero Zero = False
natLt Zero (Succ m) = True
natLt (Succ n) Zero = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
m -. Zero = m
(Succ n) -. (Succ m) = if' (natLt n m) Zero (n -. m)

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDiv1 :: Nat -> Nat -> Nat
natDiv1 Zero n = Zero
natDiv1 n m = case (natLt n m) of
	True -> Zero 
	False -> natOne +. (natDiv1 (n -. m) m)

natMod1 :: Nat -> Nat -> Nat
natMod1 n m = n -. ((natDiv1 n m) *. m)

natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod _ Zero = error "NatDivMod: division by zero"
natDivMod n m = Pair (natDiv1 n m) (natMod1 n m)
	
natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток

-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd n m= gcd m (natMod n m)

instance Monoid Nat where
	mempty = Zero
	mappend = (+.)
	
data MulNat = MulN Nat

instance Monoid MulNat where
	mempty = MulN Zero
	mappend (MulN a) (MulN b) = MulN (a *. b)
-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data UnZero = One | USucc UnZero deriving (Show,Read)

u :: Nat -> UnZero
u Zero = error "Zero hasn't been exsepted"
u (Succ Zero) = One
u (Succ n) = USucc (u n)

revU :: UnZero -> Nat
revU (USucc One) = Succ(Succ Zero)
revU (USucc n) = Succ (revU n)

uCmp :: UnZero -> UnZero -> Tri
uCmp One One = EQ
uCmp One (USucc _) = LT
uCmp (USucc _) One = GT
uCmp (USucc n) (USucc m) = uCmp n m

uSum :: UnZero -> UnZero -> UnZero
uSum One m = USucc m
uSum (USucc n) m = USucc (uSum n m)

uMin :: UnZero -> UnZero -> UnZero
uMin (USucc m) One = USucc m
uMin (USucc n) (USucc m) = uMin n m

uLt :: UnZero -> UnZero -> Bool
uLt n m = case (uCmp n m) of
	LT -> True
	_ -> False
	
uMul :: UnZero -> UnZero -> UnZero
uMul One m = m
uMul n m = uSum n (uMul n (uMin m One) )
 
data Int = IntZero | Pos UnZero | Neg UnZero deriving (Show,Read)

intZero = IntZero -- 0
intOne = Pos One -- 1
intNegOne = Neg One -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg IntZero = IntZero
intNeg (Pos m) = Neg m
intNeg (Neg m) = Pos m

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp IntZero IntZero = EQ
intCmp IntZero (Pos _) = LT
intCmp IntZero (Neg _) = GT
intCmp (Pos _) IntZero = GT
intCmp (Neg _) IntZero = LT
intCmp (Pos a) (Pos b) = uCmp a b 
intCmp (Neg a) (Neg b) = uCmp b a 
intCmp (Pos _) (Neg _) = GT
intCmp (Neg _) (Pos _) = LT 

intEq :: Int -> Int -> Bool
intEq n m = case (intCmp n m) of
	EQ -> True
	_ -> False


intLt :: Int -> Int -> Bool
intLt n m = case (intCmp n m) of
	LT -> True
	_ -> False

	
infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
m .+. IntZero = m
IntZero .+. m = m
(Neg n) .+. (Neg m) = Neg (uSum n m)
(Pos n) .+. (Pos m) = Pos (uSum n m)
(Pos n) .+. (Neg m) = case (uCmp n m) of
	 GT -> Pos (uMin n m)
	 LT -> Neg (uMin m n)
	 EQ -> IntZero
(Neg n) .+. (Pos m) = case (uCmp n m) of
	LT -> Neg (uMin n m)
	GT -> Pos (uMin m n)
	EQ -> IntZero

(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Pos n) .*. (Neg m) = Neg (uMul n m)
(Neg n) .*. (Pos m) = Neg (uMul n m)
(Pos n) .*. (Pos m) = Pos (uMul n m)
(Neg n) .*. (Neg m) = Pos (uMul n m)

instance Monoid Int where
	mempty = IntZero
	mappend = (.+.)
	
instance Group Int where
	gempty = IntZero
	gappend = (.+.)
	greverse = intNeg
	
data MulInt = MulI Int

instance Monoid MulInt where
	mempty = MulI intOne
	mappend (MulI a) (MulI b) = MulI (a .*. b)
-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat IntZero _) = error "ratInv: has no inversion element"
ratInv (Rat (Pos x) y) = Rat (Pos (u y)) (revU x)
ratInv (Rat (Neg x) y) = Rat (Neg (u y)) (revU x)

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat x y) (Rat a b) = intCmp (x .*. (Pos (u b)))(a .*. (Pos (u y)))

ratEq :: Rat -> Rat -> Bool
ratEq (Rat x y) (Rat a b) = intEq (x .*. (Pos (u b)))(a .*. (Pos (u y))) 

ratLt :: Rat -> Rat -> Bool
ratLt (Rat x y) (Rat a b) = intLt (x .*. (Pos (u b)))(a .*. (Pos (u y))) 

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat x y) %+ (Rat a b) = Rat ((x .*. (Pos (u b))) .+. (a .*. (Pos (u y)))) (y *. b)

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat x y) %* (Rat a b) = Rat (x .*. a) (y *. b)

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

instance Monoid Rat where
	mempty = Rat IntZero natOne
	mappend = (%+)
	
instance Group Rat where
	gempty = Rat IntZero natOne
	gappend = (%+)
	greverse = ratNeg
	
data MulRat = MulR Rat

instance Monoid MulRat where
	mempty = MulR (Rat intOne natOne)
	mappend (MulR a) (MulR b) = MulR (a %* b)
	
instance Group MulRat where
	gempty = MulR (Rat IntZero natOne)
	gappend (MulR a) (MulR b) = MulR (a %* b)
	greverse (MulR a) = MulR (ratInv a)
-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3 a b c = gcd a (gcd b c)
example3' a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4 a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b