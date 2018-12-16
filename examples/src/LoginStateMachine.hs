{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module LoginStateMachine where

import Prelude hiding (id, (.))

import Control.Arrow (Kleisli (..))
import Control.Category (Category (..))
import Control.Monad (void)
import Numeric.Natural (Natural)
import Data.Functor (($>))
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Test.QuickCheck

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

-- | Graph of transitions in the state machine.  In abstract representation the
-- states do not show up, the only way to record some data is to addit to the
-- transition.  Thus @'Logout'@ can carry data.  When interpreted in a some
-- category (e.g. @'Kleisli' m@) then the data will be avalable on
-- @'LoggedOut{} :: 'State' a st@.
--
data Tr a from to where
  Login  :: SStateType to -> Tr a (State a 'LoggedOutType) (State a to)
  Logout :: Maybe a -> Tr a (State a 'LoggedInType) (State a 'LoggedOutType)
  Access :: Tr a (State a 'LoggedInType) (State a 'LoggedInType)

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

data HandleLogin m authtoken a = HandleLogin {
    handleLogin
      :: m (Either (HandleLogin m authtoken a) (HandleAccess m a)),
      -- ^ either failure with a login continuation or handle access to the
      -- secret data
    handleAccessDenied
      :: m ()
      -- ^ handle access denied
  }

data HandleAccess m a where
  AccessHandler
    :: m a                         -- access secret
    -> (a -> m (HandleAccess m a)) -- handle secret
    -> HandleAccess m a
  LogoutHandler :: HandleAccess m a

handleLoginIO
  :: String
  -> HandleLogin IO String String
handleLoginIO passwd = HandleLogin
  { handleLogin
  , handleAccessDenied
  }
 where
  handleLogin = do
    passwd' <- putStrLn "Provide a password:" >> getLine
    if passwd' == passwd
      then return $ Right handleAccess
      else return $ Left $ handleLoginIO passwd

  handleAccess = AccessHandler (pure "Hello saylor!") $
    \s -> do
      putStrLn ("secret: " ++ s)
      return LogoutHandler

  handleAccessDenied = putStrLn "AccessDenied"

-- pure @'HandleLogin'@ useful for testing @'accessSecret'@
handleLoginPure
  :: NonEmpty String -- ^ passwords to try (cyclicly, ad infinitum)
  -> String          -- ^ authtoken
  -> String          -- ^ secret
  -> HandleLogin Identity String String
handleLoginPure passwds passwd secret = HandleLogin
  { handleLogin = handleLogin passwds
  , handleAccessDenied = pure ()
  }
 where
  handleLogin (passwd' :| rest) = 
    if passwd' == passwd
      then return $ Right handleAccess
      else case rest of 
        []  -> return $ Left $ handleLoginPure passwds passwd secret
        _   -> return $ Left $ handleLoginPure (NE.fromList rest) passwd secret

  handleAccess = AccessHandler (pure secret) $ \_ -> return LogoutHandler

-- | Abstract access function
--
accessSecret
  :: forall m a . Monad m
  => Natural
  -- ^ how many times one can try to login; this could be implemented inside
  -- @'HandleLogin'@ (with a small modifications) but this way we are able to
  -- test it with a pure @'HandleLogin'@ (see @'handleLoginPure'@).
  -> HandleLogin m String a
  -> FreeLifting m (Cat (Tr a)) (State a 'LoggedOutType) (State a 'LoggedOutType)
accessSecret 0 HandleLogin{handleAccessDenied}         = lift $ handleAccessDenied $> id
accessSecret n HandleLogin{handleLogin} = lift $ do
  st <- handleLogin
  case st of
    -- login success
    Right accessHandler -> return $ handle accessHandler Nothing . login SLoggedIn
    -- login failure
    Left handler'       -> return $ accessSecret (pred n) handler'
 where
  handle :: HandleAccess m a -> Maybe a -> FreeLifting m (Cat (Tr a)) (State a 'LoggedInType) (State a 'LoggedOutType)
  handle LogoutHandler ma = logout ma
  handle (AccessHandler accessHandler dataHandler) _ = lift $ do
    a <- accessHandler
    accessHandler' <- dataHandler a
    return $ handle accessHandler' (Just a)

-- | Get data following the protocol defined by the state machine.
-- 
-- Note: in GHC-8.6 we'd need @'MonadFail'@ constraint which prevents from
-- running this in @'Identity'@ monad.  To avoid this we use @'runLoggedOut'@
-- function.
getData
  :: forall m a . Monad m
  => (forall x y. Tr a x y -> Kleisli m x y)
  -> Natural
  -> HandleLogin m String a
  -> m (Maybe a)
getData nat n handleLogin = case foldNatLift nat (accessSecret n handleLogin) of
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
  nat (Login SLoggedOut) = \_ -> LoggedOut Nothing
  nat (Logout ma)        = \_ -> LoggedOut ma
  nat Access             = \_ -> LoggedIn

prop_getData
  :: NonEmptyList String
  -> String
  -> String
  -> Positive Int
  -> Property
prop_getData (NonEmpty passwds) passwd secret (Positive n)= 
  let res = runIdentity $ getData natPure (fromIntegral n) (handleLoginPure (NE.fromList passwds) passwd secret)
  in if elem passwd (take n passwds)
    then res === Just secret
    else res === Nothing

-- | A trivial program, which extracts a trivial secret.
main :: IO ()
main = do
  putStrLn ""
  quickCheck prop_getData
  putStrLn ""
  void $ getData natPure 3 (handleLoginIO "password")
