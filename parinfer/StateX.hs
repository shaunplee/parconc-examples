--
-- Adapted from the program "infer", believed to have been originally
-- authored by Philip Wadler, and used in the nofib benchmark suite
-- since at least the late 90s.
--

module StateX
  ( StateX,
    returnSX,
    eachSX,
    thenSX,
    toSX,
    putSX,
    getSX,
    useSX,
  )
where

data StateX s a = MkSX {rep :: (s -> a)}

returnSX :: ((a, b) -> a1) -> a -> StateX b a1
returnSX returnX x = MkSX (\s -> returnX (x, s))

eachSX :: (t -> ((t1, b) -> (a, b)) -> a1) -> StateX s t -> (t1 -> a) -> StateX s a1
eachSX eachX xSX f = MkSX (\s -> rep xSX s `eachX` (\(x, s') -> (f x, s')))

thenSX :: (t -> ((t1, s) -> a) -> a1) -> StateX s1 t -> (t1 -> StateX s a) -> StateX s1 a1
thenSX thenX xSX kSX = MkSX (\s -> rep xSX s `thenX` (\(x, s') -> rep (kSX x) s'))

toSX :: (t -> (a -> (a, b)) -> a1) -> t -> StateX b a1
toSX eachX xX = MkSX (\s -> xX `eachX` (\x -> (x, s)))

putSX :: (((), b) -> a) -> b -> StateX s a
putSX returnX s' = MkSX (\s -> returnX ((), s'))

getSX :: ((b, b) -> a) -> StateX b a
getSX returnX = MkSX (\s -> returnX (s, s))

useSX :: (t -> ((a, b) -> a) -> t1) -> s -> StateX s t -> t1
useSX eachX s xSX = rep xSX s `eachX` fst
