{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Just where

import Command (runCommand, runCommandSED)
import Control.Lens (itoList)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (ord)
import Data.Functor (($>))
import Data.Kind (Type)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Time (getCurrentTime)
import Graphics.Vty (Attr (attrForeColor), MaybeDefault (..))
import Graphics.Vty.Attributes.Color (Color, green, red, white)
import Graphics.Vty.Input.Events qualified as V
import ListOfListeners (listOfListeners)
import Reflex
    ( Adjustable
    , MonadHold (holdDyn)
    , PerformEvent (Performable, performEvent)
    , PostBuild
    , Reflex (Dynamic)
    , TriggerEvent
    , attachPromptlyDyn
    , ffor
    , fforMaybe
    , getPostBuild
    , leftmost
    , tickLossy
    )
import Reflex.Vty
    ( HasDisplayRegion
    , HasFocus (makeFocus, requestFocus)
    , HasFocusReader
    , HasImageWriter
    , HasInput
    , HasLayout
    , HasTheme
    , Refocus (..)
    , boxTitle
    , col
    , def
    , fixed
    , flex
    , grout
    , input
    , localTheme
    , row
    , scrollableText
    , singleBoxStyle
    , tile
    )

getJustIO :: IO [(String, String)]
getJustIO = do
    (_, just, _) <- runCommand "just --summary"
    pure $ catMaybes $ renderCmdTree $ foldr insert (leaf False) $ words just

data CmdTree = CmdTree {core :: Bool, unCmdTree :: Map.Map String CmdTree}
    deriving (Eq, Ord, Show)

leaf :: Bool -> CmdTree
leaf x = CmdTree x mempty

insert :: String -> CmdTree -> CmdTree
insert x (CmdTree y t) = case break (== '-') x of
    (xs, '-' : ys) -> case Map.lookup xs t of
        Nothing -> CmdTree y $ Map.insert xs (insert ys (leaf False)) t
        Just t' -> CmdTree y $ Map.insert xs (insert ys t') t
    (xs, _) -> case Map.lookup xs t of
        Nothing -> CmdTree y $ Map.insert xs (leaf True) t
        Just t' -> CmdTree y $ Map.insert xs (t'{core = True}) t

compose :: Bool -> [Char] -> Maybe ([Char], [Char]) -> Maybe ([Char], [Char])
compose _ x Nothing = Just (x, x)
compose False x (Just (y, y')) =
    Just
        (replicate (length x) '.' ++ " " ++ y, x ++ "-" ++ y')
compose True x (Just (y, y')) =
    Just
        (x ++ " " ++ y, x ++ "-" ++ y')

renderCmdTree :: CmdTree -> [Maybe (String, String)]
renderCmdTree (CmdTree y t) = h $ do
    (k, v) <- Map.toList t
    zipWith ($) (compose True k : repeat (compose False k)) $ renderCmdTree v
  where
    h = if y then (Nothing :) else id

type Cmd = String

getJust
    :: ( Reflex t
       , MonadFix m
       , MonadHold t m
       , MonadIO (Performable m)
       , MonadIO m
       , PerformEvent t m
       , PostBuild t m
       , TriggerEvent t m
       )
    => m (Dynamic t [(String, Cmd)])
getJust = do
    tick <- tickLossy 0.1 =<< liftIO getCurrentTime
    listings <- performEvent
        $ ffor tick
        $ \_ -> liftIO getJustIO
    holdDyn [] listings

justWidget
    :: forall m (t :: Type)
     . ( HasDisplayRegion t m
       , HasImageWriter t m
       , HasTheme t m
       , HasFocusReader t m
       , HasInput t m
       , HasLayout t m
       , MonadFix m
       , MonadHold t m
       , MonadIO m
       , MonadIO (Performable m)
       , PerformEvent t m
       , PostBuild t m
       , TriggerEvent t m
       , HasFocus t m
       , Adjustable t m
       )
    => m ()
justWidget =
    color green
        $ row
        $ do
            rec o <- fmap (Map.fromList . zip [0 ..] . addLetters) <$> getJust
                ev <- tile (fixed 50) $ do
                    ev' <- col $ listOfListeners $ fmap fst <$> o
                    pb <- getPostBuild
                    focus <- makeFocus
                    requestFocus $ pb $> Refocus_Id focus
                    i <- input
                    pure $ leftmost [Left <$> ev', Right <$> i]
                let click = fforMaybe (attachPromptlyDyn o ev)
                        $ \(resolveInt, lre) -> case lre of
                            Left clicked -> listToMaybe $ do
                                (_k, v) <-
                                    itoList
                                        $ Map.intersectionWith
                                            (,)
                                            resolveInt
                                            clicked
                                case v of
                                    (c, V.EvMouseDown _ _ V.BLeft []) -> [c]
                                    _ -> []
                            Right (V.EvKey (V.KChar c) []) ->
                                Map.lookup (ord c - ord 'a') resolveInt
                            _ -> Nothing

                (out, err) <- runCommandSED $ ("just " <>) . snd <$> click
                void
                    $ grout flex
                    $ do
                        void
                            $ grout flex
                            $ color white
                            $ scrollableText
                                def
                                out

                        grout flex $ col $ do
                            grout flex
                                $ color red
                                $ scrollableText
                                    def
                                    err
            pure ()

addLetters :: [(String, Cmd)] -> [(String, [Char])]
addLetters = zipWith (\x (y, c) -> (x : ": " <> y, c)) ['a' ..]

color :: (HasTheme t m) => Color -> m a -> m a
color g = localTheme (fmap \x -> x{attrForeColor = SetTo g})