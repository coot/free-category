{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module LoginStateMachine where

import Prelude hiding (id, (.))

import Control.Arrow (Kleisli (..))
import Control.Category (Category (..), (<<<))
import Data.Functor.Identity (Identity (..))

import Control.Category.Free (Cat (..), FreeAlgebra2 (..))
import Data.Algebra.Free (AlgebraType, AlgebraType0, proof)

{-------------------------------------------------------------------------------
-- Categories with monadic actions
-------------------------------------------------------------------------------}

class (Category c) => Lifting c m | c -> m where
  lift :: m (c a b) -> c a b

data FreeLifting :: (* -> *) -> (* -> * -> *) -> * -> * -> * where
  Cat  :: Cat f a b -> FreeLifting m f a b
  Lift :: m (FreeLifting m f a b) -> FreeLifting m f a b

instance Functor m => Category (FreeLifting m gr) where
  id = Cat Id
  Cat f . Cat g = Cat (f . g)
  Lift (mf) . g = Lift ((. g) <$> mf)

-- |
-- This means that one can interpret `AccessCat` in any Kleisli category!
instance Monad m => Lifting (Kleisli m) m where
  lift m = Kleisli (\a -> m >>= \(Kleisli f) -> f a)

instance Functor m => Lifting (FreeLifting m gr) m where
  lift = Lift

type instance AlgebraType0 (FreeLifting m) tr = (Monad m)
type instance AlgebraType  (FreeLifting m) c  = Lifting c m
instance Monad m => FreeAlgebra2 (FreeLifting m) where
  liftFree2    = Cat . liftFree2
  foldNatFree2 nat (Cat cab)   = foldNatFree2 nat cab
  foldNatFree2 nat (Lift mcab) = lift $ foldNatFree2 nat <$> mcab

  codom2  = proof
  forget2 = proof

{-------------------------------------------------------------------------------
-- Example State Machine, inspired by:
-- `State Machines All The Way Down` by Edwin Bradly, 2017
-- https://www.youtube.com/watch?v=xq7ZuSRgCR4
-------------------------------------------------------------------------------}

data LoginResult = Success | LoginError

data StateType where
  LoggedInType  :: StateType
  LoggedOutType :: StateType

data SStateType (a :: StateType) where
  SLoggedIn  :: SStateType 'LoggedInType
  SLoggedOut :: SStateType 'LoggedOutType

data State (st :: StateType) where
  LoggedIn  :: State 'LoggedInType
  LoggedOut :: State 'LoggedOutType

data Tr a from to where
  Login   :: SStateType to -> Tr a (State 'LoggedOutType) (State to)
  Logout  :: Maybe a -> Tr a (State 'LoggedInType) (State 'LoggedOutType)
  Access  :: Tr a (State 'LoggedInType) (State 'LoggedInType)


login :: Monad m
      => SStateType st
      -> FreeLifting m (Tr a) (State 'LoggedOutType) (State st)
login = liftFree2 . Login

logout :: Monad m
       => Maybe a
       -> FreeLifting m (Tr a) (State 'LoggedInType) (State 'LoggedOutType)
logout = liftFree2 . Logout

access :: Monad m
       => FreeLifting m (Tr a) (State 'LoggedInType) (State 'LoggedInType)
access = liftFree2 Access

type Username = String

newtype HandleLogin m a = HandleLogin {
    handleLogin :: Username -> m (Maybe (HandleAccess m a))
  }

data HandleAccess m a where
  AccessHandler
    :: m a                         -- access secret
    -> (a -> m (HandleAccess m a)) -- handle secret
    -> HandleAccess m a
  LogoutHandler :: HandleAccess m a

accessSecret
  :: forall m a. Monad m
  => Username
  -> HandleLogin m a
  -> FreeLifting m (Tr a) (State 'LoggedOutType) (State 'LoggedOutType)
accessSecret username HandleLogin{handleLogin} = lift $ do
  st <- handleLogin username
  case st of
    Just accessHandler -> return $ handle accessHandler Nothing . login SLoggedIn
    Nothing -> return id
 where
  handle :: HandleAccess m a -> Maybe a -> FreeLifting m (Tr a) (State 'LoggedInType) (State 'LoggedOutType)
  handle LogoutHandler ma = logout ma
  handle (AccessHandler accessHandler dataHandler) _ = lift $ do
    a <- accessHandler
    accessHandler' <- dataHandler a
    return $ handle accessHandler' (Just a)

natPure :: forall a from to. Tr a from to -> Kleisli Identity from to
natPure (Login SLoggedIn)     = Kleisli $ \_ -> Identity LoggedIn
natPure (Login SLoggedOut)    = Kleisli $ \_ -> Identity LoggedOut
natPure (Logout _)            = Kleisli $ \_ -> Identity LoggedOut
natPure Access                = Kleisli $ \_ -> Identity LoggedIn

interpretPure
  :: FreeLifting Identity (Tr a) from to
  -> Kleisli Identity from to
interpretPure = foldNatFree2 natPure

interpretIO
  :: (forall x y. (Tr a) x y -> Kleisli IO x y)
  -> FreeLifting IO (Tr a) from to
  -> Kleisli IO from to
interpretIO natIO = foldNatFree2 natIO
