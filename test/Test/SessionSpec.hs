{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Test.SessionSpec where

import Data.Aeson as A (encode)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Network.HTTP.Types (urlEncode)
import Skeletest
import Web.Hyperbole
import Web.Hyperbole.Data.Cookie as Cookie
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Effect.Page
import Web.Hyperbole.Effect.Session (sessionCookie)


-- import Skeletest.Predicate qualified as P

data Woot = Woot Text
  deriving (Generic, Show, ToJSON, FromJSON)
instance Session Woot where
  cookiePath = Just $ Path True ["somepage"]


spec :: Spec
spec = do
  describe "Session" $ do
    it "should encode cookie" $ do
      let woot = Woot "hello"
      toCookie woot `shouldBe` CookieValue (cs $ A.encode woot)

  describe "sessionCookie" $ do
    it "should create cookie" $ do
      let woot = Woot "hello"
      cook <- runSessionPage $ sessionCookie woot
      cook `shouldBe` Cookie (sessionKey @Woot) (Path True ["somepage"]) (Just (toCookie woot))

  describe "render" $ do
    it "should parse cookies" $ do
      Cookie.parse (path "/") [("Woot", "Woot")] `shouldBe` Right (Cookie.fromList [Cookie "Woot" (path "/") (Just (CookieValue "Woot"))])

    it "should render cookie with root path" $ do
      let cookie = Cookie "Woot" (path "/") (Just (CookieValue "Woot"))
      Cookie.render cookie `shouldBe` "Woot=Woot; SameSite=None; secure; path=/"

    it "should render complex cookie with included path" $ do
      let woot = Woot "hello world"
      cookie <- runSessionPage $ sessionCookie woot
      Cookie.render cookie `shouldBe` "Woot=" <> urlEncode True (cs $ A.encode woot) <> "; SameSite=None; secure; path=/somepage"

  describe "Session class" $ do
    it "should encode class" $ do
      let prefs = Preferences "hello" Warning
      cook <- runSessionPage $ sessionCookie prefs
      let cooks = Cookie.insert cook mempty
      Cookie.lookup (sessionKey @Preferences) cooks `shouldBe` Just (CookieValue $ cs $ A.encode prefs)

    it "should decode class" $ do
      let prefs = Preferences "hello" Warning
      cook <- runSessionPage $ sessionCookie prefs
      let cooks = Cookie.insert cook mempty
      Just val <- pure $ Cookie.lookup (sessionKey @Preferences) cooks
      parseCookie val `shouldBe` Right prefs
 where
  runPage :: Eff (Page : es) a -> Eff es a
  runPage = interpret $ \_ -> \case
    GetPageInfo -> pure $ PageInfo (Host "none") (Path True [])
    InterruptWith i -> error $ show i
    PutClient _ -> pure ()
    GetClient -> pure $ Client mempty mempty

  runSessionPage = runEff . runPage


data Preferences = Preferences
  { message :: Text
  , color :: AppColor
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, Session)
instance Default Preferences where
  def = Preferences "_" White


data AppColor
  = White
  | Light
  | GrayLight
  | GrayDark
  | Dark
  | DarkHighlight
  | Success
  | Danger
  | Warning
  | Primary
  | PrimaryLight
  | Secondary
  | SecondaryLight
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
