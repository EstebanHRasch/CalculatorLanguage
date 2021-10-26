-- just in case we want to use the stateerrormonad file in the
-- hints folder

module StateError where

import Control.Monad(ap)

data StateError msg s a  =  StateError (s -> (Either msg (s,a)))


runStateError (StateError sa) = sa

{-instance Show (StateError msg s a) where
  show (StateError sa) = case StateError (\s -> sa s of 
    Left msg -> show msg
    Right (s, a) -> show a)
-}
instance Functor (StateError msg s) where
  -- map the function f to the result of the Parser
  fmap f (StateError sa) =  StateError (\s -> case sa s of
    Left msg -> Left msg
    Right (s, a) -> Right (s, f a) )


--ignore this for now
instance Applicative (StateError msg s) where
  pure = return
  (<*>) = ap


instance Monad (StateError msg s) where
  return a =  StateError (\s -> Right (s, a))
  (StateError sa) >>= f = StateError (\s -> case sa s of
    Left msg -> Left msg
    Right (s, a) -> runStateError (f a) s)



--local :: StateError msg dfn s a -> s -> StateError msg dfn s a
--local se new = StateError (\dfn s -> Right (s, dfn))

err :: String -> StateError String s a
err str = StateError (\s -> Left str)

--getDfns :: StateError msg s dfn
--getDfns = StateError (\s -> Right (s, dfn))

getState :: StateError msg s s
getState = StateError (\s -> Right (s, s))

putState :: s -> StateError msg s ()
putState s = StateError (\_ -> Right (s, ()))

-- many more possibilities...

