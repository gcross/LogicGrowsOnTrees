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

data Driver result_monad result =  -- {{{
    ∀ manager_monad.
    ( RequestQueueMonad (manager_monad result)
    , RequestQueueMonadResult (manager_monad result) ~ result
    ) ⇒
    Driver
    {   runVisitor ::
            ( Monoid result
            , Serialize result
            , MonadIO result_monad
            ) ⇒
            IO (Maybe (VisitorProgress result)) →
            (TerminationReason result → IO ()) →
            Visitor result →
            manager_monad result () →
            result_monad ()
    ,   runVisitorIO ::
            ( Monoid result
            , Serialize result
            , MonadIO result_monad
            ) ⇒
            IO (Maybe (VisitorProgress result)) →
            (TerminationReason result → IO ()) →
            VisitorIO result →
            manager_monad result () →
            result_monad ()
    ,   runVisitorT ::
            ( Monoid result
            , Serialize result
            , MonadIO result_monad
            , Functor m
            , MonadIO m
            ) ⇒
            (∀ α. m α → IO α) →
            IO (Maybe (VisitorProgress result)) →
            (TerminationReason result → IO ()) →
            VisitorT m result →
            manager_monad result () →
            result_monad ()
    } -- }}}

-- }}}
