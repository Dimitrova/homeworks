{-# LANGUAGE ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- В данном задании требуется реализовать интерпретатор для
-- нетипизированной лямбды
--------------------------------------------------------------------------------

module UnTyLambda.Interpreter where

-- Какие-то импорты. Заметьте, что в этом задании надо
-- использовать обычную Prelude
import Prelude hiding (catch)
import Control.Exception

------------------------------------------------------------
-- Определение дататайпа для нетипизированной лямбды
type Variable = String
data Term = Var Variable | Lam Variable Term | App Term Term deriving (Show,Read)

------------------------------------------------------------
-- Дальше всё на ваше усмотрение

-- Если внутри будете использовать именованное представление, то
-- я тут решил немного вам помочь
-- (иначе говоря, код из этого раздела можно совсем выкинуть,
-- если хочется)

--Возвращет список свободных переменных
free (Var v) = [ v ]
free (Lam v t) = filter (/= v) . free $ t
free (App t t') = (free t) ++ (free t')

newname fv = head . filter (not . flip elem fv) . iterate ('_':)

--Подстановка
subst :: Term -> Variable -> Term -> Term
subst t@(Var v) var what = if v == var then what else t
subst (App t t') var what = App (subst t var what) (subst t' var what)
subst t@(Lam v b) var what = if v == var then t else (Lam v (subst b var what))

--Бета редукция
betaReduction :: Term -> Variable -> Term -> Term
betaReduction t v t' = subst t v (rename (free t) t')
  where rename vars term = case term of
          Var _ -> term
          App x x' -> App (rename vars x) (rename vars x')
          Lam var x -> Lam newv newx
            where nameUsed = elem var vars
                  newv = if nameUsed then newname (vars ++ (free x)) var else var
                  newx = if nameUsed then subst (Var newv) var x else x

--Определение нормальной формы
isNormal :: Term -> Bool
isNormal (Var _) = True
isNormal (Lam _ t) = isNormal t
isNormal (App (Lam _ _) _) = False
isNormal (App t t') = isNormal t && isNormal t' 

--Поиск бета-редекса
hasRedex :: Term -> Bool
hasRedex (Var _) = False
hasRedex (Lam _ t) = hasRedex t
hasRedex (App (Lam _ _) _) = True
hasRedex (App t t') = hasRedex t || hasRedex t'

--Проверка слабости
isWeaked :: Term -> Bool
isWeaked (Var _) = True
isWeaked (Lam _ _) = True
isWeaked (App (Lam _ _) _) = False
isWeaked (App t _) = isWeaked t

--Аппликативная редукция
applic :: Term -> Term
applic (Var v) = Var v
applic (App (Lam v t) t') = if (hasRedex t) then (Lam v (applic t)) else (betaReduction t v t')
applic (App t t') = App (applic t) (applic t')

--Нормальная редукция
normal :: Term -> Term
normal (Var v) = Var v
normal (Lam v t) = Lam v (normal t)
normal (App (Lam v t) t') = betaReduction t v t'
normal (App t t') = if (hasRedex t) then (App (normal t) t') else (App t (normal t'))

--Редукция слабой головной формы
weaked :: Term -> Term
weaked (Var v) = Var v
weaked (Lam v t) = Lam v t
weaked (App (Lam v t) t') = betaReduction t v t'
weaked (App t t') = weaked t

------------------------------------------------------------
-- За исключением того, что требуется реализовать следующие
-- стратегии нормализации (они все принимают максимальное
-- число шагов интерпретатора в качестве первого
-- параметра (n); если за n шагов нормализовать не удаётся,
-- то следует бросать error, тестер его поймает):

wh, no, wa, sa :: Integer -> Term -> Term

-- Редукция аппликативным порядком
sa 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
sa n t = if (isNormal t) then t else sa (n - 1) (applic t)

-- Нормализация нормальным порядком
no 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
no n t = if (isNormal t) then t else no (n - 1) (normal t)

-- Редукция в слабую головную нормальную форму
wh 0 t = error $ "Too long sequence at [" ++ show t ++ "]"
wh n t = if (isWeaked t) then t else wh (n - 1) (weaked t)

-- (*) (не обязательно) Редукция "слабым" аппликативным порядком.
-- Отличается от обычного аппликативного тем, что не лезет внутрь
-- лямбд и правые части аппликаций, когда это возможно.
wa = undefined

-- Замечание: cкорость работы вашего интерпретатора специально не оценивается,
-- потому можно использовать свой изоморфный (с точностью до альфа-конверсии)
-- тип для представления термов и преобразовывать Term в него и обратно.

-- Перечисление всех этих порядков (в порядке отличном от
-- определения, да)
orders =
    [ ("wh", wh)
    , ("no", no)
-- , ("wa", wa) -- Можно раскоментировать, да
    , ("sa", sa) ]

------------------------------------------------------------
-- Игнорируйте это, если выглядит непонятно
pall term = mapM_ $ \(d, x) -> putStr (d ++ ": ") >> catch (let t = x 1000 term in seq t (print t)) (\(e :: SomeException) -> print e)
testfuncs funcs = mapM_ $ \t -> putStr "===== " >> print t >> pall t funcs

------------------------------------------------------------
-- Сюда можно добавлять тесты
lamxx = Lam "x" $ App (Var "x") (Var "x")
omega = App lamxx lamxx

test = testfuncs orders
    [ Var "a"
    , Lam "x" $ (Lam "y" $ Var "y") `App` (Var "x")
    , (Lam "x" $ Lam "y" $ Var "x") `App` (Var "y")
    , omega
    ]

------------------------------------------------------------
-- Немного теоретических замечаний, если они вас волнуют
--
-- Следует специально отметить, что поскольку в конце теста
-- результат вычисления печатают, то ленивость Haskell не
-- влияет на семантику интерпретируемого исчисления.
--
-- Чтобы это особенно подчеркнуть в тестере выше я написал
-- seq в интересном месте (хотя конкретно это там ничего не
-- гарантирует, на самом-то деле).	