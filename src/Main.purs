module Main where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import UI.AppM (runAppM)
import UI.Components.Router as Router
import UI.Store as Store

main :: Effect Unit
main = HA.runHalogenAff do
  let store = { route: Store.Home }
  rootComponent <- runAppM store Router.component
  body <- HA.awaitBody
  runUI rootComponent unit body
