{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- � ������ ������� ��������� ����������� ������������� ���
-- ���������������� ������
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- �����-�� �������. ��������, ��� � ���� ������� ����
-- ������������ ������� Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- ����������� ��������� ��� ���������������� ������
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- ������ �� �� ���� ����������

-- ���� ������ ������ ������������ ����������� �������������, ��
-- � ��� ����� ������� ��� ������
-- (����� ������, ��� �� ����� ������� ����� ������ ��������,
-- ���� �������)

--��������� ������ ��������� ����������
free (Var v) = [ v ]
free (Lam v t) = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

newname fv = head . filter (not . flip elem fv) . iterate ('_':)

--�����������
subst :: Term -> Variable -> Term -> Term
subst t@(Var v) var what = if v == var then what else t
subst (App t t') var what = App (subst t var what) (subst t' var what)
subst t@(Lam v b) var what = if v == var then t else (Lam v (subst b var what))

--���� ��������
betaReduction :: Term -> Variable -> Term -> Term
betaReduction t v t' = subst t v (rename (free t) t')
  where rename vars term = case term of
          Var _ -> term
          App x x' -> App (rename vars x) (rename vars x')
          Lam var x -> Lam newv newx
            where nameUsed = elem var vars
                  newv = if nameUsed then newname (vars ++ (free x)) var else var
                  newx = if nameUsed then subst (Var newv) var x else x

--����������� ���������� �����
isNormal :: Term -> Bool
isNormal (Var _) = True
isNormal (Lam _ t) = isNormal t
isNormal (App (Lam _ _) _) = False
isNormal (App t t') = isNormal t && isNormal t' 

--����� ����-�������
hasRedex :: Term -> Bool
hasRedex (Var _) = False
hasRedex (Lam _ t) = hasRedex t
hasRedex (App (Lam _ _) _) = True
hasRedex (App t t') = hasRedex t || hasRedex t'

--�������� ��������
isWeaked :: Term -> Bool
isWeaked (Var _) = True
isWeaked (Lam _ _) = True
isWeaked (App (Lam _ _) _) = False
isWeaked (App t _) = isWeaked t

--������������� ��������
applic :: Term -> Term
applic (Var v) = Var v
applic (App (Lam v t) t') = if (hasRedex t) then (Lam v (applic t)) else (betaReduction t v t')
applic (App t t') = App (applic t) (applic t')

--���������� ��������
normal :: Term -> Term
normal (Var v) = Var v
normal (Lam v t) = Lam v (normal t)
normal (App (Lam v t) t') = betaReduction t v t'
normal (App t t') = if (hasRedex t) then (App (normal t) t') else (App t (normal t'))

--�������� ������ �������� �����
weaked :: Term -> Term
weaked (Var v) = Var v
weaked (Lam v t) = Lam v t
weaked (App (Lam v t) t') = betaReduction t v t'
weaked (App t t') = weaked t

------------------------------------------------------------
-- �� ����������� ����, ��� ��������� ����������� ���������
-- ��������� ������������ (��� ��� ��������� ������������
-- ����� ����� �������������� � �������� �������
-- ��������� (n); ���� �� n ����� ������������� �� ������,
-- �� ������� ������� error, ������ ��� �������):

wh, no, wa, sa :: Integer -> Term -> Term

-- �������� ������������� ��������
sa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
sa n t = if (isNormal t) then t else sa (n - 1) (applic t)

-- ������������ ���������� ��������
no 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
no n t = if (isNormal t) then t else no (n - 1) (normal t)

-- �������� � ������ �������� ���������� �����
wh 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wh n t = if (isWeaked t) then t else wh (n - 1) (weaked t)

-- (*) (�� �����������) �������� "������" ������������� ��������.
-- ���������� �� �������� �������������� ���, ��� �� ����� ������
-- ����� � ������ ����� ����������, ����� ��� ��������.
wa = undefined

-- ���������: c������� ������ ������ �������������� ���������� �� �����������,
-- ������ ����� ������������ ���� ���������� (� ��������� �� �����-���������)
-- ��� ��� ������������� ������ � ��������������� Term � ���� � �������.

-- ������������ ���� ���� �������� (� ������� �������� ��
-- �����������, ��)
orders =
    [ ("wh", wh)
    , ("no", no)
-- , ("wa", wa) -- ����� ����������������, ��
    , ("sa", sa) ]

------------------------------------------------------------
-- ����������� ���, ���� �������� ���������
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- ���� ����� ��������� �����
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- ������� ������������� ���������, ���� ��� ��� �������
--
-- ������� ���������� ��������, ��� ��������� � ����� �����
-- ��������� ���������� ��������, �� ��������� Haskell ��
-- ������ �� ��������� ����������������� ����������.
--
-- ����� ��� �������� ����������� � ������� ���� � �������
-- seq � ���������� ����� (���� ��������� ��� ��� ������ ��
-- �����������, �� �����-�� ����).	