{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Main where

import           Data.Int
import qualified Data.Text as Text
import           Data.Text (Text)

import           Data.GI.Base
import qualified GI.Gtk as Gtk

tooltipMarkup :: Text
tooltipMarkup = 
  "As per the <span foreground=\"blue\">gi-base documentation (https://hackage.haskell.org/package/haskell-gi-base-0.21.0/docs/Data-GI-Base.html#t:AttrOp)</span> there are several ways to assign to the object properties for convenience:\n\
  \\n\
  \<b>Normal value assignment </b>\n\
  \Bare constants or return values from full function applications can be assigned to the properties by <tt>(:=)</tt> operator. This operator can be used in <tt>new</tt> object construction.\n\
  \\n\
  \<b>Assignment of the values encapsulated by IO monad</b>\n\
  \<tt>(:=>)</tt> operator can assign the values wrapped in <tt>IO</tt>. This operator can also be used in <tt>new</tt> object construction.\n\
  \\n\
  \<b>Function application</b>\n\
  \Functions can be used to calculate property values using <tt>(:~)</tt> operator. This function is provided by the current value of the property in its parameter (be aware that if the return type of <tt>get</tt> function for the property is <tt>Maybe</tt> then the value is wrapped in <tt>Maybe</tt>) and return value is assigned as the new value. This operator <i>can not</i> be used in <tt>new</tt> object construction.\n\
  \\n\
  \<b>Monadic function application</b>\n\
  \Results of functions returning <tt>IO</tt> wrapped values can be assigned by <tt>(:~>)</tt> operator. The current value of the property is passed in sole parameter of the function (note the type may be <tt>Maybe a</tt> depending on the get result for the same property). This operator <i>can not</i> be used in <tt>new</tt> object construction."
    
appWinWidth :: Int32
appWinWidth = 500

goldenRatio :: Double
goldenRatio = (1.0 + sqrt 5.0) / 2.0

appActivate :: Gtk.Application -> IO ()
appActivate app = do
  -- we can set attributes of GTK objects by various ways.
  -- when constructing
                                        -- You can assign normal values
  appWin <- new Gtk.ApplicationWindow [ #application := app
                                        -- as well as IO wrapped values
                                      , #title :=> getTextValue "Enter title of the window"
                                      ]
  tv <- new Gtk.TextView [ #editable := False
                         , #wrapMode := Gtk.WrapModeWordChar
                         ]
  buf <- tv `get` #buffer
  iter <- #getStartIter buf
  #insertMarkup buf iter tooltipMarkup $ toEnum $ Text.length tooltipMarkup
  #add appWin tv
  let scWidth = 640 :: Double
  -- after construction you can...
                 -- assign normal values
  appWin `set` [ #opacity := 0.50
                 -- assign return values of pure functions
               , #defaultWidth :~ \_ -> round scWidth
               -- additionally you can
                 -- assign IO wrapped values and ...
               , #tooltipMarkup :=> getTextValue "Enter window tooltip text (enter Pango markup if you dare!)"
                 -- return values of monadic functions
               , #defaultHeight :~> (\_ -> return . round $ (scWidth / goldenRatio))
               ]
  #showAll appWin
  return ()

getTextValue ::Text -> IO Text
getTextValue prompt = do
  win <- new Gtk.Dialog [#title := prompt]
  box <- #getContentArea win
  entry <- newEntry
  #packStart box entry True False 0
  let respOk = (toEnum $ fromEnum Gtk.ResponseTypeOk)::Int32
  #addButton win "OK" $ fromInteger $ toInteger $ respOk
  #setDefaultResponse win respOk
  #showAll win
  result <- loopUntilOk win entry
  #destroy win
  return result

  where 
    newEntry :: IO Gtk.Entry
    newEntry = do
      result <- new Gtk.Entry [#placeholderText := prompt] 
      set result [#widthChars := toEnum $ Text.length prompt]
      return result

    loopUntilOk :: Gtk.Dialog -> Gtk.Entry -> IO Text
    loopUntilOk win entry = do
      response <- #run win >>= return . toEnum . fromEnum
      if response == Gtk.ResponseTypeOk
        then do
          isValid <- testValid entry
          if isValid
            then getResult entry
            else loopUntilOk win entry
        else loopUntilOk win entry

    getResult :: Gtk.Entry -> IO Text
    getResult = ( `get` #text) 

    testValid :: Gtk.Entry -> IO Bool
    testValid entry = return . ( > 0) . Text.length =<< getResult entry

main :: IO ()
main = do
  app <- new Gtk.Application [#applicationId := "haskell-gi.examples.attribute-usage"]
  on app #activate $ appActivate app

  #run app Nothing
  return ()
