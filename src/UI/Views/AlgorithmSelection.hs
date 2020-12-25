{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-
  Implements a view that displays a list with Algorithms to choose from.
  The base for all this code was taken from one of the example programs for Brick:
  https://github.com/jtdaugherty/brick/blob/master/programs/ListDemo.hs
-}
module UI.Views.AlgorithmSelection (start) where

import Service.DataTypes

-- Packages used by the Bruck TUI
import Lens.Micro ((^.), (&), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void, forever)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as VC

import Brick.Main
import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center as C
import Brick.Widgets.Border.Style
import qualified Brick.Widgets.Border as B
import qualified Brick.AttrMap
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import Brick.Util (on, fg)

import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vec

{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}
{-  listDrawElement display algorithm
    Creates a list-item widget, displaying the name of an algorithm.
    RETURNS: a widget displaying the name of algorithm
-}
listDrawElement :: Bool -> Algorithm -> Widget ()

{-  drawUI st
    Dispays the entire UI
    RETURNS: widget displaying every widget on the screen
-}
drawUI :: L.List () Algorithm -> [Widget ()]

{-  appEvent st e
    Directs the app events to the current views event handler.
    RETURNS: the new state based on st
-}
appEvent :: L.List () Algorithm -> BrickEvent () e -> EventM () (Next (L.List () Algorithm))

{-  initialState algorithms
    Creates the list contents.
    RETURNS: a list with the same items as algorithms
-}
initialState :: [Algorithm] -> L.List () Algorithm

{-  globalDefault
    The default values for text colors and backgrounds
-}
globalDefault :: V.Attr

{-  theMap
    Defines the different colors that can be applied to Widgets
    RETURNS: custom colors
-}
theMap :: AttrMap

{-  theApp
    Creates the Brick Application, defining the appEvent-handler, etc.
    RETURNS: the app instance used by Brick to run the application
-}
theApp :: App (L.List () Algorithm) e ()

{-  start
    Displays the list view which allows the user to select an algorithm.
    RETURNS: the selected algorithm
-}
start :: [Algorithm] -> Algorithm -> IO (Algorithm)


{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}
listDrawElement _ (Algorithm _ name _) = C.hCenter $ str name

drawList st = B.borderWithLabel (str "Select Algorithm")
    $ hLimit 30
    $ vLimit 15
    $ padTopBottom 1
    $ padLeftRight 2
    $ L.renderList listDrawElement True st

drawDescription Nothing = str ""
drawDescription (Just ((_, Algorithm _ name description))) = hLimit 120 $ padTop (Pad 2) $ str $ description

drawUI st = [ C.center $ (C.hCenter $ drawList st) <=> (C.hCenter $ drawDescription (L.listSelectedElement st)) ]

appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
        VtyEvent (V.EvKey V.KEnter []) -> halt st
        VtyEvent (ev) -> continue =<< (L.handleListEventVi L.handleListEvent ev st)

initialState algorithms = L.list () (Vec.fromList algorithms) 1

globalDefault = fg V.white

theMap = attrMap V.defAttr
    [
      (L.listAttr, fg V.white),
      (L.listSelectedAttr, V.black `on` (VC.rgbColor 255 168 1))
    ]

theApp =
    App {
        appDraw = drawUI,
        appChooseCursor = showFirstCursor,
        appHandleEvent = appEvent,
        appStartEvent = return,
        appAttrMap = const theMap
    }

start algorithms selected = do
  st' <- defaultMain theApp (L.listMoveToElement selected (initialState algorithms))
  let selectedItem = L.listSelectedElement st'
  case selectedItem of
    (Just (_, algorithm)) -> return (algorithm)
    Nothing -> return (head $ Vec.toList $ L.listElements st')

