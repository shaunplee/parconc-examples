{-# LANGUAGE BangPatterns, CPP #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fwarn-unused-imports #-}
-- -Wall

-- A module for stream processing built on top of Control.Monad.Par

-- (In the future may want to look into the stream interface used by
--  the stream fusion framework.)

module Stream
 (
   Stream, streamFromList, streamMap, streamFold, streamFilter
 ) where

import Control.Monad.Par.Scheds.Trace as P
import Control.DeepSeq

--------------------------------------------------------------------------------
-- Types

-- <<IList
data IList a
  = Nil
  | Cons a (IVar (IList a))
  | Fork (Par ()) (IList a)

type Stream a = IVar (IList a)
-- >>

instance NFData a => NFData (IList a) where
--  rnf Nil = r0
  rnf Nil = ()
  rnf (Cons a b) = rnf a `seq` rnf b
  rnf (Fork _ b) = rnf b

-- -----------------------------------------------------------------------------
-- Stream operators

-- <<streamFromList
streamFromList :: (Show a, NFData a) => Int -> Int -> [a] -> Par (Stream a)
streamFromList chunkSize forkDist xs = do
  var <- new                            -- <1>
  fork $ loop chunkSize forkDist xs var -- <2>
  return var                            -- <3>
 where
  loop :: (Show a, NFData a) => Int -> Int -> [a] -> IVar (IList a) -> Par ()
  loop _ _ [] var = put var Nil         -- <4>
  loop cRem 0 (x:xs) var = do  -- add a Fork
    tail <- new
    put var (Fork (buildMore (chunkSize - forkDist - 1) xs tail) (Cons x tail))
    loop (cRem - 1) (chunkSize - 1) xs tail
  loop 0 _ _ _ = return () -- quit this loop if chunk is done
  loop cRem fDist (x:xs) var = do       -- <5>
    tail <- new                         -- <6>
    put var (Cons x tail)               -- <7>
    loop (cRem - 1) (fDist - 1) xs tail -- <8>
  buildMore :: (Show a, NFData a) => Int -> [a] -> IVar (IList a) -> Par ()
  buildMore _ [] var = return ()
  buildMore 0 ys var = loop chunkSize forkDist ys var
  buildMore fdRem (_:ys) restP = do
    next <- get restP
    case next of
      Nil -> return ()
      (Cons _ newTail) -> buildMore (fdRem - 1) ys newTail
      (Fork _ _) -> error "Two Forks in a row (streamFromList)"

-- >>

-- <<streamMap
streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap fn instrm = do
  outstrm <- new
  fork $ loop instrm outstrm
  return outstrm
 where
  loop instrm outstrm = do
    ilst <- get instrm
    case ilst of
      Nil -> put outstrm Nil
      Cons h t -> do
        newtl <- new
        put outstrm (Cons (fn h) newtl)
        loop t newtl
      Fork _ Nil -> put outstrm Nil
      Fork f (Cons h t) -> do
        newt1 <- new
        put outstrm (Fork f (Cons (fn h) newt1))
        loop t newt1
      Fork _ (Fork _ _) -> error "Two Forks in a row (streamMap)"
-- >>


-- | Reduce a stream to a single value.  This function will not return
--   until it reaches the end-of-stream.
-- <<streamFold
streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold fn !acc instrm = do
  ilst <- get instrm
  case ilst of
    Nil      -> return acc
    Cons h t -> streamFold fn (fn acc h) t
    Fork _ Nil -> return acc
    Fork f (Cons h t) -> do
      fork f
      streamFold fn (fn acc h) t
-- >>

streamFilter :: NFData a => (a -> Bool) -> Stream a -> Par (Stream a)
streamFilter p instr = do
    outstr <- new
    fork $ loop instr outstr
    return outstr
  where
    loop instr outstr = do
      v <- get instr
      case v of
        Nil -> put outstr Nil
        Cons x instr'
          | p x -> do
             tail <- new
             put_ outstr (Cons x tail)
             loop instr' tail
          | otherwise -> do
             loop instr' outstr
