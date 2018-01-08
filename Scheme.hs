module Scheme
    ( module Scheme.Internal  -- TODO rename to Data
    , evalInput
    ) where

import Control.Monad (liftM)
import Scheme.Eval
import Scheme.Internal
import Scheme.Parser

evalInput :: String -> ThrowsError String
evalInput input = liftM show $ readExpr input >>= eval
