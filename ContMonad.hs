module ContMonad where

{- http://www.nct9.ne.jp/m_hiroi/func/haskell38.html -}

import Control.Monad
import Control.Monad.Cont
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

addCps :: Integer -> Integer -> Cont r Integer
addCps x y = return (x + y)

mulCps :: Integer -> Integer -> Cont r Integer
mulCps x y = return (x * y)

addSquare :: Integer -> Integer -> Cont r Integer
addSquare a b = do
  x <- addCps a b
  y <- mulCps x x
  return y

factCps :: Integer -> Cont r Integer
factCps 0 = return 1
factCps n = factCps (n - 1) >>= \m -> return $ m * n

fiboCps :: Integer -> Cont r Integer
fiboCps 0 = return 1
fiboCps 1 = return 1
fiboCps n = do
  a <- fiboCps (n - 1)
  b <- fiboCps (n - 2)
  return $ a + b

flattenCps :: [[a]] -> Cont [b] [a]
flattenCps [] = return []
flattenCps ([]:_) = cont $ \_ -> []
flattenCps (x:xs) = flattenCps xs >>= \y -> return $ (x ++ y)

bar1, bar2, bar3, foo :: (Int -> Cont r Int) -> Cont r Int

bar1 cont = return 1
bar2 cont = cont 2
bar3 cont = return 3
foo cont = do
  bar1 cont
  bar2 cont
  bar3 cont

test = callCC (\k -> foo k)

flattenCps' :: [[a]] -> Cont [b] [a]
flattenCps' xs =
  callCC (\k -> let flatten' [] = return []
                    flatten' ([]:_) = k []
                    flatten' (x:xs) = flatten' xs >>= \y -> return $ x ++ y
                in flatten' xs)


bar10, bar20, bar30, foo00 :: (Int -> ContT r IO Int) -> ContT r IO Int

bar10 cont = do
  liftIO $ putStrLn "call bar10"
  return 10

bar20 cont = do
  liftIO $ putStrLn "call bar20"
  cont 20

bar30 cont = do
  liftIO $ putStrLn "call bar30"
  return 30

foo00 cont = do
  bar10 cont
  bar20 cont
  bar30 cont

test00 = callCC (\k -> foo00 k)

{- https://qiita.com/sparklingbaby/items/2eacabb4be93b9b64755 -}


callCC' :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC' f = ContT $ \g ->
  runContT (f $ ContT . const . g) g

getCC :: ContT r m (ContT r m a)
getCC = callCC' $ \exit ->
  let a = exit a
  in return a

getCCTest = do
  goto <- getCC
  line <- lift getLine
  lift $ putStrLn line
  when (line /= "bye")
    goto

getCC' :: a -> ContT r m (a, a -> ContT r m b)
getCC' a = callCC' $ \exit ->
  let f b = exit (b, f)
  in return (a, f)

getCC'Test = do
  (x, goto) <- getCC' 0
  lift $ print x
  when (x < 10) $
    goto $ x + 1
