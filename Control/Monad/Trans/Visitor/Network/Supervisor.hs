-- Language extensions {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
-- }}}

module Control.Monad.Trans.Visitor.Network.Supervisor where

-- Imports {{{
import Data.Accessor ((^.),(^=),(^:))
import Data.Accessor.Monad.MTL.State (get)
import Data.Accessor.Template (deriveAccessors)
import Control.Applicative ((<$>))
import Control.Exception (Exception,assert)
import Control.Monad (when)
import Control.Monad.CatchIO (MonadCatchIO,throw)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (ContT)
import Control.Monad.Trans.RWS.Strict (RWST,asks)

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq,ViewL(..),(|>),viewl)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Typeable (Typeable)

import Control.Monad.Trans.Visitor.Checkpoint
import Control.Monad.Trans.Visitor.Workload
-- }}}

-- Exceptions {{{

data SupervisorError worker_id = -- {{{
    WorkerAlreadyKnown worker_id
  | WorkerNotKnown worker_id
  deriving (Eq,Show,Typeable)

instance (Eq worker_id, Show worker_id, Typeable worker_id) ⇒ Exception (SupervisorError worker_id)
-- }}}

-- }}}

-- Classes {{{

class (Eq worker_id, Ord worker_id, Show worker_id, Typeable worker_id) ⇒ WorkerId worker_id where {}

-- }}}

-- Types {{{

data VisitorNetworkSupervisorActions result worker_id m = -- {{{
    VisitorNetworkSupervisorActions
    {   broadcast_workload_steal_action :: [worker_id] → m ()
    ,   send_workload_action :: VisitorWorkload → worker_id → m ()
    ,   shutdown_action :: worker_id → m ()
    ,   snapshot_network_status_action :: VisitorStatusUpdate result → m ()
    ,   respond_to_worker_already_added_action :: worker_id → m ()
    }
-- }}}

data VisitorNetworkSupervisorState worker_id = -- {{{
    VisitorNetworkSupervisorState
    {   waiting_workers_or_available_workloads_ :: !(Either (Seq worker_id) (Set VisitorWorkload))
    ,   known_workers_ :: !(Set worker_id)
    ,   active_workers_ :: !(Map worker_id VisitorWorkload)
    ,   workers_pending_workload_steal_ :: !(Set worker_id)
    ,   workers_pending_status_update_ :: !(Set worker_id)
    }
$( deriveAccessors ''VisitorNetworkSupervisorState )
-- }}}

data VisitorNetworkResult result worker_id = VisitorNetworkResult result [worker_id]

newtype VisitorNetworkSupervisorMonad result worker_id m a = -- {{{
    VisitorNetworkSupervisorMonad {
      unwrapVisitorNetworkSupervisorMonad ::
        (ContT
            (VisitorNetworkResult result worker_id)
            (RWST
                (VisitorNetworkSupervisorActions result worker_id m)
                (VisitorStatusUpdate result)
                (VisitorNetworkSupervisorState worker_id)
                m
            )
            a
        )
    }
-- }}}

-- }}}

-- Functions {{{

validateWorkerKnown :: -- {{{
    (Monoid result, WorkerId worker_id, Functor m, MonadCatchIO m) ⇒
    worker_id →
    VisitorNetworkSupervisorMonad result worker_id m ()
validateWorkerKnown worker_id = VisitorNetworkSupervisorMonad . lift $
    Set.notMember worker_id <$> (get known_workers)
    >>=
    flip when (throw $ WorkerNotKnown worker_id)
-- }}}

-- }}}
