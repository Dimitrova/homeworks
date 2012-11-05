{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.IO where

import ITMOPrelude.Primitive
import ITMOPrelude.List
import ITMOPrelude.Categories

data RealWorld = RealWorld
    { stdIn :: List Nat
    , stdOut :: List Nat
    , exitCode :: Nat }

type IO a = State RealWorld a

getNat :: IO Nat
getNat = State (\s -> (s {stdIn = tail (stdIn s)}, (head (stdIn s))))

putNat :: Nat -> IO ()
putNat x = State (\s -> (s {stdOut = Cons x (stdOut s)}, ()))

setExitCode :: Nat -> IO ()
setExitCode x = State (\s -> (s {exitCode = x}, ()))