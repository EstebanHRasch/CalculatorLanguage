module EnvUnsafe where

import Control.Monad(ap)

--This monad will form the plumbing for the evaluation function

data Unsafe a = Error String | Ok a deriving (Show, Eq)

data EnvUnsafe e a = EnvUnsafe (e -> Unsafe a)

-- function that just runs the function contained in EnvUnsafe
runEnvUnsafe ::  (EnvUnsafe e a) -> e -> Unsafe a
runEnvUnsafe (EnvUnsafe eu) e = eu e

instance Functor (EnvUnsafe e) where
  -- fmap :: (a -> b) -> EnvUnsafe a -> EnvUnsafe b
  fmap f (EnvUnsafe eu) = EnvUnsafe $ \e -> case eu e of
                                              Ok a -> Ok (f a)
                                              Error s -> Error s     
                                             
  -- make sure your implementation follows the functor laws

--ignore this for now
instance Applicative (EnvUnsafe e) where
  pure = return
  (<*>) = ap

instance Monad (EnvUnsafe e) where
  --return :: a -> EnvUnsafe a
  return a = EnvUnsafe (\e -> Ok a)

  --(>>=) :: EnvUnsafe a -> (a -> EnvUnsafe b) -> EnvUnsafe b
  (EnvUnsafe eu) >>= f = EnvUnsafe $ \e -> case eu e of
                                             Error s -> Error s
                                             Ok a -> runEnvUnsafe (f a) e

  -- make sure your implementation follows the Monad laws


-- a way to easily return an error (for instance in do notation)
err :: String -> EnvUnsafe e a
err s = EnvUnsafe $ \e -> Error s


-- a way to easily get the entire environment (for instance in do notation)
getEnv :: EnvUnsafe e e
getEnv = EnvUnsafe $ \e -> Ok e


putEnv :: e -> EnvUnsafe e e
putEnv e = EnvUnsafe $ \_ -> Ok e

-- TO RUN THE TESTS ON EVAL --
-- x = <eval statement> 
-- y = Map.empty (environment you want to run your eval on)
-- runEnvUnsafe x y