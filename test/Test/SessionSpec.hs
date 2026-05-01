{-# LANGUAGE OverloadedLists #-}

module Test.SessionSpec where

import Data.Aeson qualified as A
import Data.String.Conversions (cs)
import Data.Text (Text)
import Network.HTTP.Types (urlEncode)
import Skeletest
import Web.Hyperbole
import Web.Hyperbole.Data.Cookie as Cookie
import Web.Hyperbole.Data.URI
import Web.Hyperbole.Effect.Session (sessionCookie)


-- import Skeletest.Predicate qualified as P

data Woot = Woot Text
  deriving (Generic, Show, ToJSON, FromJSON)
instance Session Woot where
  cookiePath = Just $ Path ["somepage"]


data InsecureSession = InsecureSession Text
  deriving (Generic, Show, ToJSON, FromJSON)
instance Session InsecureSession where
  cookieSecure = False


spec :: Spec
spec = do
  describe "Session" $ do
    it "should encode cookie" $ do
      let woot = Woot "hello"
      toCookie woot `shouldBe` CookieValue (cs $ A.encode woot)

  describe "sessionCookie" $ do
    it "should create cookie" $ do
      let woot = Woot "hello"
      sessionCookie woot `shouldBe` Cookie (sessionKey @Woot) (cookiePath @Woot) (Just (toCookie woot)) (cookieSecure @Woot)

  describe "render" $ do
    it "should parse cookies" $ do
      Cookie.parse [("Woot", "Woot")] `shouldBe` Right (Cookie.fromList [Cookie "Woot" Nothing (Just (CookieValue "Woot")) True])

    it "should render cookie with root path" $ do
      let cookie = Cookie "Woot" Nothing (Just (CookieValue "Woot")) True
      Cookie.render [] cookie `shouldBe` "Woot=Woot; SameSite=None; secure; path=/"

    it "should render non-secure cookie" $ do
      let cookie = Cookie "Woot" Nothing (Just (CookieValue "Woot")) False
      Cookie.render [] cookie `shouldBe` "Woot=Woot; SameSite=Lax; path=/"

    it "should render complex cookie with included path" $ do
      let woot = Woot "hello world"
      let cookie = sessionCookie woot
      Cookie.render [] cookie `shouldBe` "Woot=" <> urlEncode True (cs $ A.encode woot) <> "; SameSite=None; secure; path=/somepage"

  describe "Session class" $ do
    it "should encode class" $ do
      let prefs = Preferences "hello" Warning
      let cooks = Cookie.insert (sessionCookie prefs) mempty
      Cookie.lookup (sessionKey @Preferences) cooks `shouldBe` Just (CookieValue $ cs $ A.encode prefs)

    it "should decode class" $ do
      let prefs = Preferences "hello" Warning
      let cooks = Cookie.insert (sessionCookie prefs) mempty
      Just val <- pure $ Cookie.lookup (sessionKey @Preferences) cooks
      parseCookie val `shouldBe` Right prefs

    it "should create non-secure cookie when cookieSecure is False" $ do
      let insecure = InsecureSession "test"
      let cookie = sessionCookie insecure
      cookie.secure `shouldBe` False


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
