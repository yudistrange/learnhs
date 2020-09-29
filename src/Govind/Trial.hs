module Trial where

import Control.Monad.State

reverseWithCount :: [a] -> State Int [a]
reverseWithCount list = state (\s -> (reverse list, s + 1))

newtype MyState s a = MyState {exec :: s -> (s, a)}

instance Functor (MyState s) where
  fmap fn (MyState {exec = sfx}) = MyState {exec = (\s -> (s, fn (snd (sfx s))))}

instance Applicative (MyState s) where
  pure x = MyState {exec = (\s -> (s, x))}
  (<*>) (MyState execfx) (MyState execo) =
    MyState {exec = (\s ->
                       let (s', x) = execo s
                           (s'', fx) = execfx s' in
                         (s'', fx x))}

instance Monad (MyState s) where
  (>>=) (MyState stateExec) fn =
    MyState {exec = (\s ->
                       let (s', a) = stateExec s in
                         exec (fn a) s')}

getS :: MyState s s
getS = MyState {exec = (\s -> (s, s))}

putS :: s -> MyState s ()
putS st = MyState {exec = (\_ -> (st, ()))}

modifyS :: (s -> s) -> MyState s ()
modifyS fn = MyState {exec = (\s -> (fn s, ()))}
