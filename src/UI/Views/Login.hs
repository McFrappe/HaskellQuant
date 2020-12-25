{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-
  Imlements a login screen that requires you to enter valid credentials to access the application.
  The base for all this code was taken from one of the example programs for Brick:
  https://github.com/jtdaugherty/brick/blob/master/programs/FormDemo.hs
-}
module UI.Views.Login (start) where

import Service.DataManager
import Service.DataTypes as Types
import Control.Concurrent

import System.IO
import qualified Data.Text as T
import Lens.Micro ((^.), (&), (.~), (%~))
import Lens.Micro.TH (makeLenses)
import Control.Monad.IO.Class (liftIO)

import Brick
import Brick.Forms
  ( Form
  , newForm
  , formState
  , formFocus
  , setFieldValid
  , renderForm
  , handleFormEvent
  , invalidFields
  , allFieldsValid
  , focusedFormInputAttr
  , invalidFormInputAttr
  , checkboxField
  , radioField
  , editShowableField
  , editTextField
  , editPasswordField
  , (@@=)
  )
import Brick.Focus (focusGetCurrent, focusRingCursor)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes.Color as VC

{-
  Represents the different fields in the form.
  Each constructor represents a single field.
-}
data Name = NameField
          | PasswordField
          deriving (Eq, Ord, Show)

{-
  Represents the current values of the form fields.
  _name is used for username and _pwd for password.
-}
data UserInfo = UserInfo {
    _name :: T.Text,
    _pwd  :: T.Text
} deriving (Show)


makeLenses ''UserInfo
{---------------------------------------------------------------
                          INTERFACE
----------------------------------------------------------------}
{-  drawLogo
    Creates a widget displaying "Haskell Quant" in Ascii-art
    RETURNS: a widget displaying "Haskell Quant" in Ascii-art
-}
drawLogo :: Widget Name

{-  drawLabel s w
    Displays a widget with a label in front of it
    RETURNS: a widget displaying w labeled with s
-}
drawLabel :: String -> Widget Name -> Widget Name

{-  drawUI st
    Dispays the entire UI
    RETURNS: widget displaying every widget on the screen
-}
drawUI :: Form UserInfo e Name -> [Widget Name]

{-  isValidLogin username password
    Validates a username and password entered by the user.
    RETURNS: if username and password is valid
-}
isValidLogin :: String -> String -> IO Bool

{-  submitLogin st
    Handles a form submit and tries to validate the entered username and password.
    RETURNS: (if the validation succeeded, the new form state)
-}
submitLogin :: Form UserInfo e Name -> EventM Name (Bool, (Form UserInfo e Name))

{-  makeForm st
    Creates the form state
    RETURNS: the intial form state based on st
-}
makeForm :: UserInfo -> Form UserInfo e Name

{-  appEvent st e
    Directs the app events to the current views event handler.
    RETURNS: the new state based on st
-}
appEvent :: Form UserInfo e Name -> BrickEvent Name e -> EventM Name (Next (Form UserInfo e Name))

{-  initialState
    Creates the initial (empty) values of the text fields
    RETURNS: the initial state
-}
initialState :: UserInfo

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
theApp :: App (Form UserInfo e Name) e Name

{- getUserLogin list
   Creates a list with the userdata that is stored in a credentials file.
   RETURNS: list containing elements of username and password in a tuple.
   EXAMPE: getUserLogin (Just [User {username = "fadde", password = "test"}]) == [("fadde", "test")]
-}
getUserLogin :: Maybe [User] -> [(String, String)]

{- checklogin username password dataList
   Checks if input of username and password is in the database.
   RETURNS: True or False wether username and password is in dataList or not.
-}
checkLogin :: String -> String -> [(String, String)] -> Bool

{-  start
    Displays the login view which allows the user to enter a username and password.
    RETURNS: if the login was successful
-}
start :: IO (Bool)

{---------------------------------------------------------------
                        IMPLEMENTATION
----------------------------------------------------------------}
drawLogo = withAttr "logo" (
          str "    __  __           __        ____                              __ "       <=>
          str "   / / / /___ ______/ /_____  / / /     ____ ___  ______ _____  / /_"       <=>
          str "  / /_/ / __ `/ ___/ //_/ _ \\/ / /_____/ __ `/ / / / __ `/ __ \\/ __/"     <=>
          str " / __  / /_/ (__  ) ,< /  __/ / /_____/ /_/ / /_/ / /_/ / / / / /_  "       <=>
          str "/_/ /_/\\__,_/____/_/|_|\\___/_/_/      \\__, /\\__,_/\\__,_/_/ /_/\\__/  " <=>
          str "                                       /_/                          ")

drawLabel s w = padBottom (Pad 1)
    $ vLimit 1
    $ hLimit 60
    $ str s <+> fill ' ' <+> w

drawUI f = [
    (C.center
    $ padBottom (Pad 2) drawLogo
    <=> (B.borderWithLabel (str "Login")
    $ padTop (Pad 1)
    $ padLeftRight 2
    $ hLimit 60
    $ renderForm f))
    ]

-- Check if the username and password is a valid login
isValidLogin username password = do
  userData <- getUserData
  let userDataInList = getUserLogin userData
  return $ checkLogin username password userDataInList

-- Converts the data from credentials.json and makes it easier to work with.
getUserLogin Nothing = []
getUserLogin (Just []) = []
getUserLogin (Just (x:xs)) = (username x, password x) : getUserLogin (Just xs)

-- controls if the login that the user has inputted is a part from the credentials data stored in a file
checkLogin username password userData
  | elem (username, password) userData = True
  | otherwise = False

submitLogin s = do
  validation <- liftIO $ isValidLogin (T.unpack ((formState s)^.name)) (T.unpack ((formState s)^.pwd))
  let newState = setFieldValid validation NameField (setFieldValid validation PasswordField s)
  return (validation, newState)

makeForm st = newForm [
    drawLabel "Name: " @@= editTextField name NameField (Just 1),
    drawLabel "Password: " @@= editPasswordField pwd PasswordField
    ] st

appEvent s e =
  case e of
    VtyEvent (V.EvResize {})                  -> continue s
    VtyEvent (V.EvKey(V.KChar 'q') [V.MCtrl]) -> halt $ setFieldValid False NameField (setFieldValid False PasswordField s)
    VtyEvent (V.EvKey V.KEnter [])            -> do
      (validation, newState) <- submitLogin s
      case (validation) of True  -> halt newState
                           False -> continue newState
    _ -> do
        s' <- handleFormEvent e s
        continue s'

initialState = UserInfo {
    _name = "",
    _pwd = ""
}

globalDefault = fg V.white

theMap = attrMap globalDefault
  [
    (E.editAttr, fg V.white),
    (E.editFocusedAttr, fg V.white),
    (invalidFormInputAttr, fg $ VC.rgbColor 252 60 63),
    (focusedFormInputAttr, V.black `on` V.yellow),
    ("logo", fg $ VC.rgbColor 11 232 129)
  ]

theApp =
    App {
      appDraw = drawUI,
      appHandleEvent = appEvent,
      appChooseCursor = focusRingCursor formFocus,
      appStartEvent = return,
      appAttrMap = const theMap
    }

start = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

    initialVty <- buildVty
    f' <- customMain initialVty buildVty Nothing theApp (makeForm initialState)

    -- If all the fields have been validated successfully, we want to run the trader
    case (allFieldsValid f') of
      True -> return True
      False -> return False


