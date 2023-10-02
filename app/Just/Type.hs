{-# LANGUAGE RecordWildCards #-}

module Just.Type where

import Control.Lens (itoList)
import Data.Foldable (foldl')
import Variables (Vars)

type Cmd = String

data Just = JustCommand
    { justfile :: FilePath
    , variables :: Vars
    , command :: Cmd
    }
    deriving (Eq, Ord, Show)

renderJust :: Just -> String
renderJust JustCommand{..} =
    "just "
        <> "-f "
        <> justfile
        <> " "
        <> variables' variables
        <> " "
        <> command

variables' :: Vars -> String
variables' = foldl' f "" . itoList
  where
    f sets (k, v) = sets <> " --set " <> k <> " '" <> v <> "'"
