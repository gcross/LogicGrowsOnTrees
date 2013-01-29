-- Language extensions {{{
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
-- }}}

module Control.Monad.Trans.Visitor.Supervisor.Driver where

-- Imports {{{
import Control.Monad.IO.Class (MonadIO)
import Data.Monoid (Monoid)

import Control.Monad.Trans.Visitor (Visitor,VisitorIO,VisitorT)
import Control.Monad.Trans.Visitor.Checkpoint (VisitorProgress)
import Control.Monad.Trans.Visitor.Supervisor.RequestQueue (RequestQueueMonad)
-- }}}

-- Types {{{

data TerminationReason result = -- {{{
    Aborted (VisitorProgress result)
  | Completed result
  | Failure String
  deriving (Eq,Show)
-- }}}

data Driver result result_constraint result_monad manager_monad = Driver -- {{{
    {   runVisitor ::
            (Monoid result, MonadIO result_monad, RequestQueueMonad manager_monad, result_constraint) ⇒
            IO (Maybe (VisitorProgress result)) →
            (TerminationReason result → IO ()) →
            Visitor result →
            manager_monad () →
            result_monad ()
    ,   runVisitorIO ::
            (Monoid result, MonadIO result_monad, result_constraint) ⇒
            IO (Maybe (VisitorProgress result)) →
            (TerminationReason result → IO ()) →
            VisitorIO result →
            manager_monad () →
            result_monad ()
    ,   runVisitorT ::
            (Monoid result, MonadIO result_monad,  Functor m, MonadIO m, result_constraint) ⇒
            IO (Maybe (VisitorProgress result)) →
            (∀ α. m α → IO α) →
            (TerminationReason result → IO ()) →
            VisitorT m result →
            manager_monad () →
            result_monad ()
    } -- }}}

-- }}}
