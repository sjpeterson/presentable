{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Presentable.App.Monad where

import Control.Monad.Reader ( ReaderT )
import Presentable.App.Env ( AppEnv )

-- | App monad type
newtype AppT m a = AppT
    { runApp :: ReaderT AppEnv m a
    } deriving
    ( Functor
    , Applicative
    , Monad
    )

-- | An alias for AppT IO
type AppM = AppT IO
