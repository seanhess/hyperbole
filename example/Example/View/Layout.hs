{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Example.View.Layout where

import Data.Text (Text)
import Example.Colors (AppColor (..))
import Web.Hyperbole
import Data.String.Conversions (cs)
import Text.Casing (toWords, fromHumps)
import Example.Style qualified as Style
import Example.AppRoute


exampleLayout :: AppRoute -> View c () -> View c ()
exampleLayout rt pageView =
  layout id $ do
    row grow $ do
      sidebar
      col (pad 20 . gap 20 . grow) $ do
        link sourceUrl Style.link "View Source"
        row (bg White) $ do
          pageView
          space
        pageDescription rt
 where
  sourceUrl = "https://github.com/seanhess/hyperbole/blob/latest/example/" <> routeSource rt


  sidebar = do
    nav (bg Dark . color White) $ do
      col id $ do
        link "https://github.com/seanhess/hyperbole" (bold . fontSize 24 . pad 20) "HYPERBOLE"
        exampleMenu rt


  routeSource :: AppRoute -> Url
  routeSource = \case
    Simple -> "Example/Simple.hs"
    Contacts ContactsAll -> "Example/Contacts.hs"
    Contacts (Contact _) -> "Example/Contact.hs"
    Counter -> "Example/Counter.hs"
    Transitions -> "Example/Transitions.hs"
    Forms -> "Example/Forms.hs"
    Sessions -> "Example/Sessions.hs"
    LazyLoading -> "Example/LazyLoading.hs"
    Concurrent -> "Example/Concurrent.hs"
    Redirects -> "Example/Redirects.hs"
    Requests -> "Example/Requests.hs"
    LiveSearch -> "Example/LiveSearch.hs"
    Errors -> "Example/Errors.hs"
    RedirectNow -> "Main.hs"
    Query -> "Main.hs"
    Hello _ -> "Main.hs"
    Main -> "Main.hs"



exampleMenu :: AppRoute -> View c ()
exampleMenu current = do
    example Simple
    example Counter
    example Transitions
    example Forms
    example Sessions
    example Requests
    example Redirects
    example RedirectNow
    example LazyLoading
    example Concurrent
    example LiveSearch
    example (Contacts ContactsAll)
    example Errors
    -- link "/query?key=value" lnk "Query Params"
  where
    example rt =
      route rt (menuItem . selected rt) (text $ routeTitle rt)
    menuItem =
      pad (XY 20 10) . color White . hover (bg DarkHighlight)
    selected rt =
      if rt == current then bg DarkHighlight else id

routeTitle :: AppRoute -> Text
routeTitle (Hello _) = "Hello World"
routeTitle (Contacts ContactsAll) = "Contacts (Advanced)"
routeTitle r = cs $ toWords $ fromHumps $ show r


pageDescription :: AppRoute -> View c ()
pageDescription = \case
  Simple -> el_ "Create HyperViews that can update independently"
  _ -> none

