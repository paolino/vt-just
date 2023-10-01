{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Variables where

import Command (runCommand)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Zipper qualified as Z (value)
import Data.Time (getCurrentTime)
import Data.Void (Void)
import Reflex
    ( Adjustable
    , MonadHold
    , MonadSample (sample)
    , PerformEvent (Performable, performEvent)
    , PostBuild
    , Reflex (Dynamic, Event, constant, current)
    , TriggerEvent
    , attach
    , fforMaybe
    , foldDyn
    , leftmost
    , listViewWithKey
    , tickLossy
    )
import Reflex.Vty
    ( HasDisplayRegion
    , HasFocusReader
    , HasImageWriter
    , HasInput
    , HasTheme
    , TextInput (_textInput_userInput)
    , TextInputConfig (_textInputConfig_initialValue)
    , def
    , flex
    , row
    , text
    , textInput, HasFocus (makeFocus), tile
    )
import Reflex.Vty.Widget.Layout (HasLayout, fixed, grout)
import Text.Megaparsec
    ( ParseErrorBundle
    , Parsec
    , anySingleBut
    , between
    , errorBundlePretty
    , many
    , parse
    , satisfy
    , takeWhileP
    , (<|>)
    )
import Text.Megaparsec.Char (newline)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Monad (void)

data Var = Var {varName :: String, varValue :: String}
    deriving (Show, Eq)

type Parser = Parsec Void String

parseVar :: Parser Var
parseVar = do
    name <- parseName
    _ <- L.lexeme C.space ":="
    val <- quoted parseVal
    pure $ Var name val

parseName :: Parser String
parseName =
    L.lexeme C.space
        $ many (C.alphaNumChar <|> satisfy (`elem` ("_-" :: String)))

parseVal :: Parser String
parseVal = L.lexeme C.space $ many $ anySingleBut '\"'

-- | Parse the rest of a line, without the newline character.
line :: Parser String
line = takeWhileP (Just "character") (/= '\n')

symbol :: String -> Parser String
symbol = L.symbol C.space

quoted :: Parser a -> Parser a
quoted = between (symbol "\"") (symbol "\"")

ignoreNewLines :: Parser a -> Parser a
ignoreNewLines p = p <* many newline

parseVars :: String -> Either (ParseErrorBundle String Void) [Var]
parseVars = parse (many $ ignoreNewLines parseVar) ""

varsIO :: IO [Var]
varsIO = do
    (_, output, _) <- runCommand "just --evaluate"
    case parseVars output of
        Left err -> do
            putStrLn $ errorBundlePretty err
            pure []
        Right x -> pure x

type Vars = Map String String

data VarsState = VarsState
    { varsConfig :: Vars
    , varsRewrite :: Vars
    }
    deriving (Show, Eq)

data VarsChanges = ChangeConfig Vars | ChangeRewrite RewriteChanges

type RewriteChanges = Map String ChangeVar

changeVarsState :: VarsChanges -> VarsState -> VarsState
changeVarsState (ChangeConfig x) s =
    s{varsConfig = x}
changeVarsState (ChangeRewrite x) s = foldl' update s $ Map.toList x

update :: VarsState -> (String, ChangeVar) -> VarsState
update s@VarsState{..} (k, v) = case v of
    Delete -> s{varsRewrite = Map.delete k varsRewrite}
    Add v' -> s{varsRewrite = Map.insert k v' varsRewrite}

bootVarsState :: VarsState
bootVarsState = VarsState mempty mempty

applyVarsState :: VarsState -> Vars
applyVarsState VarsState{..} = varsRewrite <> varsConfig

mkVars :: [Var] -> Vars
mkVars = Map.fromList . map (\(Var k v) -> (k, v))

varsState
    :: ( PostBuild t m
       , PerformEvent t m
       , TriggerEvent t m
       , MonadIO m
       , MonadIO (Performable m)
       , MonadHold t m
       , MonadFix m
       )
    => Event t RewriteChanges
    -> m (Dynamic t Vars)
varsState changes = do
    tick <- tickLossy 0.2 =<< liftIO getCurrentTime
    vars <- performEvent $ liftIO varsIO <$ tick
    rec state <-
            foldDyn changeVarsState bootVarsState
                $ leftmost
                    [ ChangeConfig <$> filteredChangeConfig
                    , ChangeRewrite <$> changes
                    ]
        let filteredChangeConfig = fforMaybe (attach (current state) vars)
                $ \(s, vs) ->
                    let
                        s' = mkVars vs
                     in
                        if s' == varsConfig s then Nothing else Just s'
    pure $ applyVarsState <$> state

data ChangeVar = Delete | Add String

renderVars
    :: ( Applicative m
       , Adjustable t m
       , PostBuild t m
       , MonadHold t m
       , MonadFix m
       , HasDisplayRegion t m
       , HasImageWriter t m
       , HasTheme t m
       , HasLayout t m
       , HasInput t m
       , HasFocusReader t m
       , HasFocus t m
       )
    => Dynamic (t :: Type) Vars
    -> m (Event t (Map String ChangeVar))
renderVars vars = do
    listViewWithKey vars $ \name value -> do
        grout (fixed 1) $ row $ do
            grout flex $ text $ constant $ T.pack name
            grout (fixed 2) $ text $ constant $ T.pack "= "

            initial <- sample $ current value
            t <-
                tile flex
                    $ do
                        void makeFocus
                        textInput
                            $ def
                                { _textInputConfig_initialValue = fromString initial
                                }

            pure
                $ fforMaybe (attach (current value) $ _textInput_userInput t)
                $ \(old, new) ->
                    if fromString old == new
                        then Nothing
                        else
                            Just
                                $ Add
                                $ T.unpack
                                $ Z.value new

varsWidget
    :: ( Applicative m
       , Adjustable t m
       , PostBuild t m
       , MonadHold t m
       , MonadFix m
       , HasDisplayRegion t m
       , HasImageWriter t m
       , HasTheme t m
       , PerformEvent t m
       , TriggerEvent t m
       , MonadIO m
       , MonadIO (Performable m)
       , HasLayout t m
       , HasInput t m
       , HasFocusReader t m, HasFocus t m
       )
    => m (Dynamic t Vars)
varsWidget = do
    rec vars <- varsState locals
        locals <- renderVars vars
    pure vars