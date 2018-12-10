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

-- | Category transformer, which adds @'Lifting'@ instance to the underlying
-- base category.
data FreeLifting :: (* -> *) -> (k -> k -> *) -> k -> k -> * where
  Base :: c a b -> FreeLifting m c a b
  Lift :: m (FreeLifting m c a b) -> FreeLifting m c a b

-- | Wrap a transition into a free category @'Cat'@ and then in
-- @'FreeLifting'@
--
-- prop> liftCat tr = Base (tr :.: Id)
--
liftCat :: Monad m => tr a b -> FreeLifting m (Cat tr) a b
liftCat = liftFree2 . liftFree2

 -- | Fold @'FreeLifing'@ category based on a free category @'Cat' tr@ using
 -- a natural transformation @tr x y -> c x y@.
 --
foldNatLift
  :: (Monad m, Lifting c m)
  => (forall x y. tr x y -> c x y)
  -> FreeLifting m (Cat tr) x y
  -> c x y
foldNatLift nat = foldNatFree2 (foldNatFree2 nat)

instance (Functor m, Category c) => Category (FreeLifting m c) where
  id = Base id
  Base f . Base g = Base (f . g)
  Lift (mf) . g   = Lift ((. g) <$> mf)

-- | This means that one can interpret `AccessCat` in any Kleisli category!
--
instance Monad m => Lifting (Kleisli m) m where
  lift m = Kleisli (\a -> m >>= \(Kleisli f) -> f a)

instance (Functor m, Category c) => Lifting (FreeLifting m c) m where
  lift = Lift

type instance AlgebraType0 (FreeLifting m) c = (Monad m, Category c)
type instance AlgebraType  (FreeLifting m) c  = Lifting c m
instance Monad m => FreeAlgebra2 (FreeLifting m) where
  liftFree2    = Base
  foldNatFree2 nat (Base cab)  = nat cab
  foldNatFree2 nat (Lift mcab) = lift $ foldNatFree2 nat <$> mcab

  codom2  = proof
  forget2 = proof

{-------------------------------------------------------------------------------
-- Example State Machine, inspired by:
-- `State Machines All The Way Down` by Edwin Bradly, 2017
-- https://www.youtube.com/watch?v=xq7ZuSRgCR4
-------------------------------------------------------------------------------}

data LoginResult = Success | LoginError

-- | Type level representation of the states.
--
data StateType where
  LoggedInType  :: StateType
  LoggedOutType :: StateType

data SStateType (a :: StateType) where
  SLoggedIn  :: SStateType 'LoggedInType
  SLoggedOut :: SStateType 'LoggedOutType

-- | Term level representation of the states.
-- @'LoggedOut'@ let one carry out a value.
--
data State a (st :: StateType) where
  LoggedIn  :: State a 'LoggedInType
  LoggedOut :: Maybe a -> State a 'LoggedOutType

-- | Graph of transitions in the state machine.
-- In abstract representation the states do not show up, the only way to record
-- some data is to addit to the transition.  Thus @'Logout'@ can carry data.
-- When interpreted in a some category (e.g. @'Kleisli' m@) then the data will
-- be avalable on @'LoggedOut{} :: 'State' a st@.
data Tr a from to where
  Login   :: SStateType to -> Tr a (State a 'LoggedOutType) (State a to)
  Logout  :: Maybe a -> Tr a (State a 'LoggedInType) (State a 'LoggedOutType)
  Access  :: Tr a (State a 'LoggedInType) (State a 'LoggedInType)

login :: Monad m
      => SStateType st
      -> FreeLifting m (Cat (Tr a)) (State a 'LoggedOutType) (State a st)
login = liftCat . Login

logout :: Monad m
       => Maybe a
       -> FreeLifting m (Cat (Tr a)) (State a 'LoggedInType) (State a 'LoggedOutType)
logout = liftCat . Logout

access :: Monad m
       => FreeLifting m (Cat (Tr a)) (State a 'LoggedInType) (State a 'LoggedInType)
access = liftCat Access

type Username = String

-- * Data representation of the state machine.

newtype HandleLogin m authToken a = HandleLogin {
    handleLogin :: authToken -> m (Maybe (HandleAccess m a))
  }

data HandleAccess m a where
  AccessHandler
    :: m a                         -- access secret
    -> (a -> m (HandleAccess m a)) -- handle secret
    -> HandleAccess m a
  LogoutHandler :: HandleAccess m a

exampleHandleLogin
  :: Applicative m
  => String
  -> HandleLogin m String String
exampleHandleLogin passwd = HandleLogin $ \authToken ->
  if authToken == passwd
    then pure (Just handleAccess)
    else pure Nothing
 where
  handleAccess = AccessHandler (pure "secret") (\_ -> pure LogoutHandler)


-- * Abstract access function, also an isomorphism between @'HandleLogin' m a@
-- and @'FreeLifting m (Tr a) (State a 'LoggedoutType) (State
-- a 'LoggedOutType).

accessSecret
  :: forall m authToken a. Monad m
  => authToken
  -> HandleLogin m authToken a
  -> FreeLifting m (Cat (Tr a)) (State a 'LoggedOutType) (State a 'LoggedOutType)
accessSecret authToken HandleLogin{handleLogin} = lift $ do
  st <- handleLogin authToken
  case st of
    Just accessHandler -> return $ handle accessHandler Nothing . login SLoggedIn
    Nothing -> return id
 where
  handle :: HandleAccess m a -> Maybe a -> FreeLifting m (Cat (Tr a)) (State a 'LoggedInType) (State a 'LoggedOutType)
  handle LogoutHandler ma = logout ma
  handle (AccessHandler accessHandler dataHandler) _ = lift $ do
    a <- accessHandler
    accessHandler' <- dataHandler a
    return $ handle accessHandler' (Just a)

-- | Get data following using the protocol defined by the state machine.
getData
  :: Monad m
  => (forall x y. Tr a x y -> Kleisli m x y)
  -> HandleLogin m authToken a
  -> authToken
  -> m (Maybe a)
getData nat handleLogin authToken = case foldNatLift nat (accessSecret authToken handleLogin) of
  Kleisli fn -> do
    LoggedOut ma <- fn (LoggedOut Nothing)
    return ma

-- * Interpreters

natPure :: forall m a from to. Monad m => Tr a from to -> Kleisli m from to
natPure (Login SLoggedIn)  = Kleisli $ \_ -> return LoggedIn
natPure (Login SLoggedOut) = Kleisli $ \_ -> return (LoggedOut Nothing)
natPure (Logout ma)        = Kleisli $ \_ -> return (LoggedOut ma)
natPure Access             = Kleisli $ \_ -> return LoggedIn

-- | A trivial program, which extracts a trivial secret.
main :: IO ()
main = do
  putStrLn "Provide a password:"
  authToken <- getLine
  let secret = runIdentity $ getData natPure (exampleHandleLogin "password") authToken
  putStrLn ("Secret: " ++ show secret)
