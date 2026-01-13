{-# LANGUAGE AllowAmbiguousTypes #-}

module Example.View.Menu where

import App.Route
import Control.Monad (when)
import App.Docs.Page
import Example.Colors (AppColor (..), cyan)
import Web.Atomic.CSS
import Web.Hyperbole

menu :: forall sections c. (PageAnchor sections) => AppRoute -> View c ()
menu current = do
  col ~ color White $ do
    docLink Intro
    docLink Basics
    docLink Hyperviews
    docLink Concurrency
    docLink ViewFunctions
    docLink SideEffects
    docLink State
    docLink CSS
    docLink HyperboleEffect
    docLink (Forms FormSimple)
    docLink (Data DataLists)
    case current of
      Data _ -> do
        subLink (Data SortableTable)
        subLink (Data Autocomplete)
        subLink (Data Filter)
        subLink (Data LoadMore)
      _ -> none
    docLink Interactivity
    docLink (Examples OtherExamples)
    -- case current of
    --   Examples _ ->
    --     completeExamples
    --   (Contacts _) ->
    --     completeExamples
    --   _ -> none
 where
  -- completeExamples = do
  --   subLink (Examples Tags)
  --   subLink (Contacts ContactsAll)
  --   subLink (Examples OAuth2)
  --   subLink (Examples Todos)
  --   subLink (Examples TodosCSS)

  -- isExamples =
  --   case current of
  --     Examples _ -> True
  --     _ -> False

  sub = pad (TRBL 5 10 5 40) . fontSize 14

  menuItem :: (Styleable h) => CSS h -> CSS h
  menuItem =
    pad (XY 20 10) . hover (bg DarkHighlight)

  docLink rt = docLink' (rt == current) rt

  docLink' isSelected rt = do
    let highlight = if isSelected then bg DarkHighlight . border (L 4) . pad (L 16) . color cyan else id
    route rt ~ highlight . menuItem $
      text $
        routeTitle
          rt
    when (rt == current) $ do
      mapM_ anchorLink (subnav @sections)

  subLink rt = do
    let isSelected = rt == current
    let highlight = if isSelected then bg DarkHighlight . color cyan else id -- border (L 4) . pad (L 16) . color cyan else id
    route rt ~ highlight . sub . menuItem $
      text $
        routeTitle rt

  anchorLink :: (PageAnchor a) => a -> View c ()
  anchorLink a = do
    tag "a" ~ sub . menuItem @ att "href" ("#" <> pageAnchor a) $ do
      text $ navEntry a
