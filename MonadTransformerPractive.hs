import Control.Applicative
import Control.Monad
data EitherIO e a = EitherIO { runEitherIO :: IO (Either e a) }
-- fmap :: Functor f => (a -> b) -> f a -> f b
-- fmap :: Functor f => (a -> b) -> (EitherIO e a) -> (EitherIO e b)
-- fmap :: Functor f => (a -> b) -> (IO a) -> (IO b)
instance Functor (EitherIO e) where
  fmap f ex = wrapped
    where
      unwrapped = runEitherIO ex
      fmapped   = fmap (fmap f) unwrapped
      wrapped   = EitherIO fmapped

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- Expected type: IO (Either e a -> Either e b)
-- Actual type: IO (Either e (a -> b))
instance Applicative (EitherIO e) where
  pure      = EitherIO . pure . Right
  {- (<*>) fn fa = EitherIO $ (\x -> (<*>) x)  <$> (runEitherIO fn) <*> (runEitherIO fa) -}
  (<*>) fn fa = EitherIO $ liftA2 (<*>) (runEitherIO fn) (runEitherIO fa)

instance Monad (EitherIO e) where
  return       = EitherIO . return . Right
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>=) :: Monad m => IO (Either e c) -> c -> IO (Either e d) -> IO (Either e d)
-- (>>=) ma fn = (runEitherIO ma) >>= runEitherIO . fn 
-- (>>=) :: Monad m => Either e a -> (a -> Either e b) -> Either e b
  (>>=) ma fn = EitherIO $ runEitherIO ma >>= getIt
    where
      {- getIt :: Either a b -> IO (Either a c) -}
      getIt (Right b) = runEitherIO $ fn b
      getIt (Left a) = return $ Left a

  {- (>>=) ma fn = do -}
    {- x <- runEitherIO ma -}
    {- case x of -}
         {- Right a -> fn a -}
         {- Left  b -> EitherIO $ return $ Left b -}
