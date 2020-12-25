module UI.Widgets (drawKeyBinding ) where


{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}

{-  drawKeyBinding label key
    Displays all key bindings, F: Select stock, S: Start trading, B: Backtesting, L: Live, Q: Quit 
-}
drawKeyBinding :: String -> String -> Widget ()

{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}

drawKeyBinding label key = (str $ label <> " ") <+> (withAttr "keybinding" $ str $ " " <> key <> " ")

