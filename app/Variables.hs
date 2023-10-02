{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Variables where

import Command (runCommand)
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix, fix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (foldl')
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Text.Zipper (TextZipper)
import Data.Text.Zipper qualified as Z (value)
import Data.Time (getCurrentTime)
import Data.Void (Void)
import Reflex
    ( Adjustable
    , MonadHold
    , PerformEvent (Performable, performEvent)
    , PostBuild
    , Reflex (Dynamic, Event, constant, current)
    , TriggerEvent
    , attach
    , attachWith
    , ffor
    , fforMaybe
    , foldDyn
    , getPostBuild
    , leftmost
    , listViewWithKey
    , newTriggerEvent
    , performEvent_
    , sample
    , tickLossy
    )
import Reflex.Vty
    ( HasDisplayRegion
    , HasFocus
    , HasFocusReader
    , HasImageWriter
    , HasInput
    , HasTheme
    , TextInput (_textInput_userInput)
    , TextInputConfig (_textInputConfig_initialValue, _textInputConfig_modify)
    , def
    , flex
    , row
    , text
    , textInput
    , tile
    )
import Reflex.Vty.Widget.Layout (HasLayout, fixed, grout)
import System.IO (IOMode (..), openFile)
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
    -> Event t RewriteChanges
    -> m (Event t (Map String ChangeVar))
renderVars vars updates = do
    listViewWithKey vars $ \name value -> do
        grout (fixed 1) $ row $ do
            grout (fixed 20) $ text $ constant $ T.pack name
            grout (fixed 2) $ text $ constant $ T.pack "= "
            v <- sample $ current value
            t <-
                tile flex
                    $ textInput
                    $ def
                        { _textInputConfig_initialValue = fromString v
                        , _textInputConfig_modify =
                            interpretRewrite name <$> updates
                        }

            pure
                $ fforMaybe (attach (current value) $ _textInput_userInput t)
                $ \(old, new) -> diff old $ T.unpack $ Z.value new

interpretRewrite :: String -> RewriteChanges -> TextZipper -> TextZipper
interpretRewrite n m = case Map.lookup n m of
    Nothing -> id
    Just Delete -> error "interpretRewrite: delete not implemented"
    Just (Add x) -> const $ fromString x

diff :: String -> String -> Maybe ChangeVar
diff old new =
    if fromString old == new
        then Nothing
        else Just $ Add new

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
       , HasFocusReader t m
       , HasFocus t m
       )
    => FilePath
    -> m (Dynamic t Vars)
varsWidget fifoPath = do
    fifo <- varsFifoSource fifoPath
    rec let fifoUpdates = attachWith fifoToChanges (current vars) fifo
        vars <- varsState $ leftmost [locals, fifoUpdates]
        locals <- renderVars vars fifoUpdates
    pure vars

fifoToChanges :: Vars -> [Var] -> RewriteChanges
fifoToChanges vars fifos = Map.fromList $ do
    Var k v <- fifos
    case Map.lookup k vars of
        Nothing -> [(k, Add v)]
        Just v' -> [(k, Add v) | v /= v']

varsFifoSource
    :: ( Monad m
       , PostBuild t m
       , TriggerEvent t m
       , PerformEvent t m
       , MonadIO (Performable m)
       )
    => FilePath
    -> m (Event t [Var])
varsFifoSource fifoPath = do
    pb <- getPostBuild
    (e, t) <- newTriggerEvent
    performEvent_ $ ffor pb $ \_ -> void $ liftIO $ forkIO $ do
        fifo <- openFile fifoPath ReadWriteMode
        fix $ \loop -> do
            l <- T.hGetLine fifo
            case parseVars $ T.unpack l of
                Left _ -> loop
                Right vars -> t vars
            loop
    pure e
