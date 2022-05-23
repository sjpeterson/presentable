{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module
-- Copyright
-- License
--
-- Application monad for Presentable

module Presentable.App.Monad where

import Control.Monad.Reader ( ReaderT )
import Presentable.App.Env ( AppEnv )

newtype AppT m a = AppT
    { runApp :: ReaderT AppEnv m a
    } deriving
    ( Functor
    , Applicative
    , Monad
    )

type AppM = AppT IO
