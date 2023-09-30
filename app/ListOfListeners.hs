{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ListOfListeners
    ( listOfListeners
    )
where

import Control.Monad.Fix (MonadFix)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes (defAttr)
import Reflex
    ( Adjustable
    , MonadHold
    , PostBuild
    , Reflex (Dynamic, Event, current)
    , ffor
    , listViewWithKey
    )
import Reflex.Vty
    ( HasDisplayRegion
    , HasFocus
    , HasFocusReader (focus)
    , HasImageWriter
    , HasInput
    , HasLayout
    , HasTheme
    , RichTextConfig (RichTextConfig)
    , VtyEvent
    , fixed
    , input
    , richText
    , tile
    )

listOfListeners
    :: ( Adjustable t m
       , PostBuild t m
       , MonadFix m
       , MonadHold t m
       , HasInput t m
       , HasFocus t m
       , HasLayout t m
       , HasImageWriter t m
       , HasDisplayRegion t m
       , HasFocusReader t m
       , HasTheme t m
       )
    => Dynamic t (Map Int String)
    -> m (Event (t :: Type) (Map Int VtyEvent))
listOfListeners buttons = listViewWithKey buttons $ \_k e -> tile (fixed 1) $ do
    f <- focus
    let attr = ffor f $ \case
            True -> defAttr `V.withStyle` V.reverseVideo
            False -> defAttr
    richText (RichTextConfig $ current attr) (T.pack <$> current e)
    input
