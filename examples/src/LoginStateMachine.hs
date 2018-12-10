{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LoginStateMachine where

import Prelude hiding (id, (.))

import Control.Arrow (Kleisli (..))
import Control.Category (Category (..))

import Control.Category.Free (Cat)

-- Import classes and combintators used in this example
import Control.Category.Lifting

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

runLoggedOut :: State a 'LoggedOutType -> Maybe a
runLoggedOut (LoggedOut a) = a

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

-- | Get data following the protocol defined by the state machine.
-- 
-- Note: in GHC-8.6.1 we'd need @'MonadFail'@ which prevents from running this in
-- @'Identity'@ monad.  To avoid this we use @'runLoggedOut'@ function.
getData
  :: forall m authToken a.
     ( Monad m )
  => (forall x y. Tr a x y -> Kleisli m x y)
  -> HandleLogin m authToken a
  -> authToken
  -> m (Maybe a)
getData nat handleLogin authToken = case foldNatLift nat (accessSecret authToken handleLogin) of
  Kleisli fn -> do
    ma <- runLoggedOut <$> fn (LoggedOut Nothing)
    return ma

-- * Interpreters
-- To write an interpret it is enough to supply a natural transformation from
-- @'Tr' a from to@ to @'Kleisli' m@ for some monad @m@.

-- | A pure natural transformation from @'Tr'@ to @'Kleisli' m@ for some
-- @'Monad' m@.  Note, that even though @'Kleisli'@ category seems redundant
-- here, as we don't use the monad in the transformation, we need
-- a transformation into a category that satisfies the @'Lifing'@ constraint.
-- This is bause we will need the monad whn @'foldNatLift'@ will walk over the
-- constructors of '@FreeLifting'@ category.
--
natPure :: forall m a from to. Monad m => Tr a from to -> Kleisli m from to
natPure = liftKleisli . nat
 where
  -- a natural trasformation to @'->'@
  nat :: Tr a from to -> (from -> to)
  nat (Login SLoggedIn)  = \_ -> LoggedIn
  nat (Login SLoggedOut) = \_ -> (LoggedOut Nothing)
  nat (Logout ma)        = \_ -> (LoggedOut ma)
  nat Access             = \_ -> LoggedIn

-- | A trivial program, which extracts a trivial secret.
main :: IO ()
main = do
  putStrLn "Provide a password:"
  authToken <- getLine
  secret <- getData natPure (exampleHandleLogin "password") authToken
  putStrLn ("Secret: " ++ show secret)
