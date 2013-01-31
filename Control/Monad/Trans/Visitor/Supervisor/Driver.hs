-- Language extensions {{{
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Supervisor.Driver where

-- Imports {{{
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid (Monoid)
import Data.Serialize (Serialize)
import Options.Applicative (InfoMod,Parser)

import Control.Monad.Trans.Visitor (Visitor,VisitorIO,VisitorT)
import Control.Monad.Trans.Visitor.Checkpoint (VisitorProgress)
import Control.Monad.Trans.Visitor.Supervisor.RequestQueue (RequestQueueMonad,RequestQueueMonadResult)
-- }}}

-- Types {{{

data TerminationReason result = -- {{{
    Aborted (VisitorProgress result)
  | Completed result
  | Failure String
  deriving (Eq,Show)
-- }}}

data Driver result_monad configuration result =  -- {{{
    ∀ manager_monad.
    ( RequestQueueMonad (manager_monad result)
    , RequestQueueMonadResult (manager_monad result) ~ result
    ) ⇒
    Driver
    {   driverRunVisitor ::
            ( Monoid result
            , Serialize result
            , MonadIO result_monad
            ) ⇒
            Parser configuration →
            (∀ α. InfoMod α) →
            (configuration → IO (Maybe (VisitorProgress result))) →
            (configuration → TerminationReason result → IO ()) →
            (configuration → Visitor result) →
            (configuration → manager_monad result ()) →
            result_monad ()
    ,   driverRunVisitorIO ::
            ( Monoid result
            , Serialize result
            , MonadIO result_monad
            ) ⇒
            Parser configuration →
            (∀ α. InfoMod α) →
            (configuration → IO (Maybe (VisitorProgress result))) →
            (configuration → TerminationReason result → IO ()) →
            (configuration → VisitorIO result) →
            (configuration → manager_monad result ()) →
            result_monad ()
    ,   driverRunVisitorT ::
            ( Monoid result
            , Serialize result
            , MonadIO result_monad
            , Functor m
            , MonadIO m
            ) ⇒
            (∀ α. m α → IO α) →
            Parser configuration →
            (∀ α. InfoMod α) →
            (configuration → IO (Maybe (VisitorProgress result))) →
            (configuration → TerminationReason result → IO ()) →
            (configuration → VisitorT m result) →
            (configuration → manager_monad result ()) →
            result_monad ()
    } -- }}}

-- }}}
