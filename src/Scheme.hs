module Scheme
    ( module Scheme.Data
    , evalInput
    ) where

import           Control.Monad (liftM)

import           Scheme.Data
import           Scheme.Eval
import           Scheme.Parser

evalInput :: String -> ThrowsError String
evalInput input = liftM show $ readExpr input >>= eval
