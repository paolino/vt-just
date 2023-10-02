{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Fix (MonadFix)
import Data.Maybe (fromMaybe)
import Graphics.Vty qualified as V
import Just (justWidget)
import Options (Options (..), parseOptions)
import Reflex
    ( Reflex (Event)
    , fforMaybe
    , leftmost
    )
import Reflex.Vty
    ( HasDisplayRegion
    , HasFocus
    , HasFocusReader
    , HasImageWriter
    , HasInput (input)
    , HasLayout
    , HasTheme
    , MonadHold
    , fixed
    , flex
    , initManager_
    , link
    , mainWidget
    , tile
    )

withCtrlC
    :: ( Monad m
       , HasInput t m
       , Reflex t
       , MonadFix m
       , HasTheme t m
       , MonadHold t m
       , HasFocus t m
       , HasLayout t m
       , HasImageWriter t m
       , HasFocusReader t m
       , HasDisplayRegion t m
       )
    => m ()
    -> m (Event t ())
withCtrlC f = do
    inp <- tile flex $ f >> input
    q <- tile (fixed 3) $ link "Quit"
    let exit = leftmost [Left <$> inp, Right <$> q]
    return $ fforMaybe exit $ \case
        Left (V.EvKey (V.KChar 'c') [V.MCtrl]) -> Just ()
        Right _ -> Just ()
        _ -> Nothing

main :: IO ()
main = do
    Options{..} <- parseOptions

    mainWidget $ initManager_ $ withCtrlC $ do
        justWidget
            do fromMaybe "vtjust.fifo" fifo
            do fromMaybe "justfile" justfile
