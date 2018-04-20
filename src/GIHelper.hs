module GIHelper(renderWithContext, dialogRun, dialogAddButton) where 

import qualified Data.GI.Base as GI 
import qualified GI.Cairo   
import qualified GI.Gtk as GTK

import qualified Graphics.Rendering.Cairo as Cairo 
import Graphics.Rendering.Cairo.Internal(runRender, Cairo(..))
import Control.Monad.Reader(runReaderT) 
import qualified Data.Text as Text 
import Foreign.Ptr (castPtr)  

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: (GI.Cairo.Context -> Cairo.Render Bool) -> GI.Cairo.Context -> IO Bool
renderWithContext renderFn context = GI.withManagedPtr context $ \p ->
  runReaderT (runRender (renderFn context)) (Cairo (castPtr p))  

dialogRun :: GTK.IsDialog a => a -> IO GTK.ResponseType 
dialogRun dialog = toEnum . fromIntegral <$> GTK.dialogRun dialog 

dialogAddButton :: GTK.IsDialog a => a -> String -> GTK.ResponseType -> IO GTK.Widget
dialogAddButton dialog text response = 
  GTK.dialogAddButton dialog (Text.pack text) (fromIntegral $ fromEnum response)
