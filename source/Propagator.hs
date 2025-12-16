{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

module Propagator where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus, join)
import Control.Monad.Primitive (PrimMonad (..))
import Data.Kind (Type)
import Data.Primitive (writeMutVar)
import Data.Primitive.MutVar (MutVar, atomicModifyMutVar', modifyMutVar', newMutVar, readMutVar)

type Cell :: (Type -> Type) -> Type -> Type
newtype Cell m x = Cell {_cRef :: MutVar (PrimState m) (Maybe x, m ())}
  deriving newtype (Eq)

fresh :: forall x m. (MonadPlus m, PrimMonad m) => m (Cell m x)
fresh = do
  ref <- newMutVar (Nothing, return ())
  pure (Cell ref)

with :: (MonadPlus m, PrimMonad m) => Cell m x -> (x -> m ()) -> m ()
with (Cell ref) f = readMutVar ref >>= \(x, _) -> mapM_ f x

watch :: (MonadPlus m, PrimMonad m) => Cell m x -> (x -> m ()) -> m ()
watch cell@(Cell ref) k = join $ atomicModifyMutVar' ref \(x, ks) ->
  ((x, ks *> with cell k), with cell k <|> writeMutVar ref (x, ks) *> empty)

write :: (MonadPlus m, PrimMonad m, Eq x) => Cell m x -> x -> m ()
write (Cell ref) x = join $ atomicModifyMutVar' ref \case
  (Just y, ks)
    | x == y -> ((Just x, ks), pure ())
    | x /= y -> ((Just x, ks), empty)
  (Nothing, ks) -> ((Just x, ks), ks <|> writeMutVar ref (Nothing, ks) *> empty)

unify :: (MonadPlus m, PrimMonad m, Eq x) => Cell m x -> Cell m x -> m ()
unify x y = watch x (write y) *> watch y (write x)

unsafeRead :: (PrimMonad m) => Cell m x -> m (Maybe x)
unsafeRead (Cell ref) = fmap fst (readMutVar ref)
