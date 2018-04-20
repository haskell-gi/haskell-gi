module GIHelper(dialogRun, dialogAddButton) where 

import qualified GI.Cairo   
import qualified GI.Gtk as GTK
import qualified Data.Text as Text 

dialogRun :: GTK.IsDialog a => a -> IO GTK.ResponseType 
dialogRun dialog = toEnum . fromIntegral <$> GTK.dialogRun dialog 

dialogAddButton :: GTK.IsDialog a => a -> String -> GTK.ResponseType -> IO GTK.Widget
dialogAddButton dialog text response = 
  GTK.dialogAddButton dialog (Text.pack text) (fromIntegral $ fromEnum response)
