--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module InferMonad
  ( Infer,
    returnI,
    eachI,
    thenI,
    guardI,
    useI,
    getSubI,
    substituteI,
    unifyI,
    freshI,
    freshesI,
  )
where

import Control.Applicative
import Control.Monad
import MaybeM
import StateX (StateX, eachSX, getSX, putSX, returnSX, thenSX, toSX, useSX)
import Substitution
import Type

type Counter = Int

data Infer x = MkI {rep :: StateX Sub (StateX Counter (Maybe ((x, Sub), Counter)))}

returnI :: x -> Infer x
returnI x = MkI (returnSX (returnSX returnM) x)

eachI :: Infer x -> (x -> y) -> Infer y
xI `eachI` f = MkI (eachSX (eachSX eachM) (rep xI) f)

thenI :: Infer x -> (x -> Infer y) -> Infer y
xI `thenI` kI = MkI (thenSX (thenSX thenM) (rep xI) (rep . kI))

failI :: Infer x
failI = MkI (toSX (eachSX eachM) (toSX eachM failM))

useI :: x -> Infer x -> x
useI xfail =
  useM xfail
    . useSX eachM 0
    . useSX (eachSX eachM) emptySub
    . rep

guardI :: Bool -> Infer x -> Infer x
guardI b xI = if b then xI else failI

putSubI :: Sub -> Infer ()
putSubI s = MkI (putSX (returnSX returnM) s)

getSubI :: Infer Sub
getSubI = MkI (getSX (returnSX returnM))

putCounterI :: Counter -> Infer ()
putCounterI c = MkI (toSX (eachSX eachM) (putSX returnM c))

getCounterI :: Infer Counter
getCounterI = MkI (toSX (eachSX eachM) (getSX returnM))

substituteI :: MonoType -> Infer MonoType
substituteI t =
  getSubI `thenI`
  (\s -> returnI (applySub s t))

unifyI :: MonoType -> MonoType -> Infer ()
unifyI t u =
  getSubI `thenI`
  (\s -> let sM = unifySub t u s
          in existsM sM
              `guardI` (putSubI (theM sM) `thenI`
                        (\() -> returnI ())
                        )
  )

-- フレッシュな型変数を生成
freshI :: Infer MonoType
freshI =
  getCounterI `thenI`
  (\c -> putCounterI (c + 1) `thenI`
  (\() -> returnI $ TVar ("a" ++ show c))
  )

-- n 個のフレッシュな型変数を生成してリストで返す
freshesI :: Int -> Infer [MonoType]
freshesI 0 = returnI []
freshesI n =
  freshI `thenI`
  (\x -> freshesI (n -1) `thenI`
  (\xs -> returnI (x : xs))
  )

instance Applicative Infer where
  pure = return
  (<*>) = ap

instance Monad Infer where
  return = returnI
  (>>=) = thenI

instance Functor Infer where
  fmap f x = f <$> x
