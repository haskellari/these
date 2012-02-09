-----------------------------------------------------------------------------
-- | Module     :  Control.Monad.Trans.Chronicle
--
--   The 'ChronicleT' monad, a hybrid error/writer monad that allows 
--   both accumulating outputs and aborting computation with a final output.
-----------------------------------------------------------------------------
module Control.Monad.Trans.Chronicle ( 
                                     -- * The Chronicle monad
                                       Chronicle, chronicle, runChronicle
                                     -- * The ChronicleT monad transformer
                                     , ChronicleT(..)
                                     -- * Chronicle operations
                                     , dictate, confess
                                     , memento, absolve, retcon
                                     ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Functor.Apply (Apply(..))
import Data.Functor.Bind (Bind(..))
import Data.Functor.Identity
import Data.Monoid (Monoid(..))

import Data.These

-- --------------------------------------------------------------------------
-- | A chronicle monad parameterized by the output type @c@.
--
--   The 'return' function produces a computation with no output, and '>>='
--   combines multiple outputs with 'mappend'.
type Chronicle c = ChronicleT c Identity

chronicle :: These c a -> Chronicle c a
chronicle = ChronicleT . Identity

runChronicle :: Chronicle c a -> These c a
runChronicle = runIdentity . runChronicleT

-- --------------------------------------------------------------------------
-- | The `ChronicleT` monad transformer.
--
--   The 'return' function produces a computation with no output, and '>>='
--   combines multiple outputs with 'mappend'.
newtype ChronicleT c m a = ChronicleT { runChronicleT :: m (These c a) }

instance (Functor m) => Functor (ChronicleT c m) where
    fmap f (ChronicleT c) =  ChronicleT (fmap f <$> c)

instance (Monoid c, Apply m) => Apply (ChronicleT c m) where
    ChronicleT f <.> ChronicleT x = ChronicleT ((<.>) <$> f <.> x)

instance (Monoid c, Applicative m) => Applicative (ChronicleT c m) where
    pure = ChronicleT . pure . pure
    ChronicleT f <*> ChronicleT x = ChronicleT (liftA2 (<*>) f x)

instance (Monoid c, Apply m, Monad m) => Bind (ChronicleT c m) where
    (>>-) = (>>=)

instance (Monoid c, Monad m) => Monad (ChronicleT c m) where
    return = ChronicleT . return . return
    m >>= k = ChronicleT $ 
        do cx <- runChronicleT m
           case cx of 
               This  a   -> return (This a)
               That    x -> runChronicleT (k x)
               These a x -> do cy <- runChronicleT (k x)
                               return $ case cy of
                                            This  b   -> This (mappend a b)
                                            That    y -> These a y
                                            These b y -> These (mappend a b) y

instance (Monoid c) => MonadTrans (ChronicleT c) where
    lift m = ChronicleT (That `liftM` m)

instance (Monoid c, MonadIO m) => MonadIO (ChronicleT c m) where
    liftIO = lift . liftIO

-- | @'dictate' c@ is an action that records the output @c@.
--   
--   Equivalent to 'tell' for the 'Writer' monad.
dictate :: (Monoid c, Monad m) => c -> ChronicleT c m ()
dictate c = ChronicleT $ return (These c ())

-- | @'confess' c@ is an action that ends with a final output @c@.
--   
--   Equivalent to 'throwError' for the 'Error' monad.
confess :: (Monoid c, Monad m) => c -> ChronicleT c m a
confess c = ChronicleT $ return (This c)

-- | @'memento' m@ is an action that executes the action @m@ and records any 
--   output it produced. The value of the computation will be the final record 
--   if @m@ ended via 'confess', or the value of @m@ otherwise.
memento :: (Monoid c, Monad m) => ChronicleT c m a -> ChronicleT c m (Either c a)
memento m = ChronicleT $ 
    do cx <- runChronicleT m
       return $ case cx of
                    This  a   -> These a (Left a)
                    That    x -> That (Right x)
                    These a x -> These a (Right x)

-- | @'absolve' x m@ is an action that executes the action @m@ and discards any
--   output it produced. The default value @x@ will be used if @m@ ended via 
--   'confess'.
absolve :: (Monoid c, Monad m) => a -> ChronicleT c m a -> ChronicleT c m a
absolve x m = ChronicleT $ 
    do cy <- runChronicleT m
       return $ case cy of
                    This  _   -> That x
                    That    y -> That y
                    These _ y -> That y

-- | @'retcon' f m@ is an action that executes the action @m@ and applies the
--   function @f@ to its output, leaving the return value unchanged.
--   
--   Equivalent to 'censor' for the 'Writer' monad.
retcon :: (Monoid c, Monad m) => (c -> c) -> ChronicleT c m a -> ChronicleT c m a
retcon f m = ChronicleT $ mapThis f `liftM` runChronicleT m

