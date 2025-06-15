{-# Language GHC2021 #-}

{-# Language LambdaCase #-}
{-# Language OverloadedRecordDot #-}
{-# Language OverloadedStrings #-}
{-# Language OverloadedLabels #-}
{-# Language ImplicitParams #-}
{-# Language NumericUnderscores #-}
{-# Language DuplicateRecordFields #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, when)
import GHC.Generics (Generic)

import qualified Data.Text as T
import Data.Void (Void, absurd)
import Text.Read (readMaybe)

import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk

import Declarative.Gtk

import Data.Functor ((<&>))

data ForecastStatus = Refreshing
                    | OK
  deriving (Eq)

data Weather = Cloudy | CloudyAndRainy
  deriving (Show, Eq)

data Forecast = Forecast {
  temp :: Float -- in Kelvin
  , status :: ForecastStatus
  , weather :: Weather
  } deriving (Generic)

data Msg = RefreshForecast

-- We should pass the model explicitly as a DynVal, distinguish
-- between direct (single parent) and indirect (potentially multiple
-- parents, collected through functions) DynVal values in the
-- functor/applicative instance (otherwise the HasField instance is
-- not correct) and (perhaps?) make View a newtype/GADT.

weatherView :: View Forecast Msg Gtk.Widget
weatherView forecast emit = widget DeclarativeBox [
  #children :=
    [widget Gtk.Image [#iconName :<~
                       forecast.weather <&> \case
                          Cloudy -> "weather-overcast-symbolic"
                          CloudyAndRainy -> "weather-showers-symbolic",
                       #iconSize := Gtk.IconSizeLarge],
     widget Gtk.Label [#label :<~ format <$> forecast],
     widget Gtk.Label [#label := " : "],
     widget Gtk.Button [#label :<~ forecast.status <&> \case
                           Refreshing -> "Refreshing, please wait..."
                           OK -> "Refresh forecast",
                        #sensitive :<~ (/= Refreshing) <$> forecast.status,
                        On #clicked (emit RefreshForecast)]
    ]]
  where
    format :: Forecast -> T.Text
    format fc = "Current temp is " <> tshow fc.temp
                <> " K, the forecast for tomorrow is "
                <> tshow fc.weather <> "."
      where tshow :: Show a => a -> T.Text
            tshow = T.pack . show

weatherHandler :: Handler Msg Forecast
weatherHandler RefreshForecast forecast = withCurrent forecast.status $ \curStatus ->
  when (curStatus /= Refreshing) $ do
    forecast.status <~ Refreshing
    void $ forkIO $ gather forecast $ do
      threadDelay 500_000

      rand <- GLib.randNew
      currentTemp <- rand.intRange 100 300
      forecast.temp <~ fromIntegral currentTemp

      modify forecast.weather $ \case
        Cloudy -> CloudyAndRainy
        CloudyAndRainy -> Cloudy

      forecast.status <~ OK

celsiusToKelvin, fahrenheitToKelvin, kelvinToCelsius, kelvinToFahrenheit, fahrenheitToCelsius, celsiusToFahrenheit
  :: Float -> Float
celsiusToKelvin c = c + 273.15
kelvinToCelsius k = k - 273.15
fahrenheitToKelvin f = (f - 32) * 5.0/9 + 273.15
fahrenheitToCelsius f = (f - 32) * 5.0/9
kelvinToFahrenheit k = (k - 273.15) * 9/5.0 + 32
celsiusToFahrenheit c = c * 9/5.0 + 32

data CalcMsg = SetFromCelsius T.Text
             | SetFromFahrenheit T.Text

data CalcData = CalcData {
  celsius :: T.Text,
  fahrenheit :: T.Text
  } deriving (Generic)

calcHandler :: Handler CalcMsg CalcData
calcHandler (SetFromCelsius c) cdata = do
  cdata.celsius <~ c
  cdata.fahrenheit <~ case readMaybe (T.unpack c) of
                        Just cf -> T.pack . show . celsiusToFahrenheit $ cf
                        Nothing -> ""
calcHandler (SetFromFahrenheit f) cdata = do
  cdata.fahrenheit <~ f
  cdata.celsius <~ case readMaybe (T.unpack f) of
                     Just cf -> T.pack . show . fahrenheitToCelsius $ cf
                     Nothing -> ""

-- The "has-focus" property of GtkEntry is always False in gtk4, see
-- https://blog.gtk.org/2019/03/15/entries-in-gtk-4/
--
-- The following checks whether the "delegate", the actual widget that
-- can have focus, does have focus.
hasFocus :: Gtk.Entry -> IO Bool
hasFocus entry = do
  Just del <- entry.getDelegate
  del.hasFocus

calcView :: View CalcData CalcMsg Gtk.Widget
calcView cdata emit = do
  widget DeclarativeBox [
    #children := [
        widget Gtk.Entry [
            #placeholderText := "Temp in Celsius",
            #text :<~ cdata.celsius,
            -- We connect to modifications of the ‘text’ property,
            -- rather than the changed signal, due to
            -- https://gitlab.gnome.org/GNOME/gtk/-/issues/7077
            --
            -- (this makes no difference in practice since we check
            -- the focus below, but it avoids invoking the signal
            -- handler twice)
            On (PropertySet #text) $ \c -> do
                -- Check whether we have focus (so the user is
                -- interacting directly with us), and only emit the
                -- message then. We do this to break the cycle, since
                -- gtk emits the "changed" signal also when the text
                -- property is changed programmatically, and numerical
                -- instabilities can easily induce loops.
                focused <- hasFocus ?self
                when focused $ do
                  putStrLn $ "Celsius changed: " <> show c
                  emit (SetFromCelsius c)
            ],
        widget Gtk.Label [#label := "C = "],
        widget Gtk.Entry [
            #placeholderText := "Temp in Fahrenheit",
            #text :<~ cdata.fahrenheit,
            On (PropertySet #text) $ \f -> do
                focused <- hasFocus ?self
                when focused $ do
                  putStrLn $ "Fahrenheit changed: " <> show f
                  emit (SetFromFahrenheit f)
            ],
        widget Gtk.Label [#label := "F"]
      ]
    ]

data ReverseData = ReverseData {
  text :: T.Text
  } deriving (Generic)

data ReverseMsg = SetFromOriginal T.Text
                | SetFromReversed T.Text

reverseHandler :: Handler ReverseMsg ReverseData
reverseHandler (SetFromOriginal o) rdata = rdata.text <~ o
reverseHandler (SetFromReversed r) rdata = rdata.text <~ T.reverse r

reverseView :: View ReverseData ReverseMsg Gtk.Widget
reverseView rdata emit = do
  widget DeclarativeBox [
    #children := [
        widget Gtk.Entry [
            #placeholderText := "Text to reverse",
            Bind #text #text
            ],
        widget Gtk.Entry [
            #placeholderText := "Reversed text",
            #text :<~ T.reverse <$> rdata.text,
            On (PropertySet #text) $ emit . SetFromReversed
            ]
        ]
    ]

data DuplicateData = DuplicateData {
  duplicated :: DuplicatedText
  } deriving (Generic)

newtype DuplicatedText = DuplicatedText {text :: T.Text}
  deriving (Generic)

-- An example of two properties bound to the same field in the model.
duplicateView :: View DuplicateData Void Gtk.Widget
duplicateView _ddata _emit = do
  widget DeclarativeBox [
    #children := [
        widget Gtk.Entry [
            #placeholderText := "Text to duplicate",
            Bind #text #"duplicated.text"
            ],
        widget Gtk.Label [
            Bind #label #"duplicated.text"
            ]
        ]
    ]

activate :: Gtk.Application -> IO ()
activate app = do
  let initialForecast = Forecast {temp = 283.15, status = OK,
                                  weather = CloudyAndRainy}
      initialCalcData = CalcData {celsius = "", fahrenheit = ""}
      initialReverseData = ReverseData {text = ""}
      initialDuplicateData = DuplicateData {duplicated = DuplicatedText ""}
  window <- new Gtk.ApplicationWindow [
    #application := app,
    #title := "Hi there",
    #child :=> new DeclarativeBox [
        #orientation := Gtk.OrientationVertical,
        #children := [
            mvh initialForecast weatherView weatherHandler,
            mvh initialCalcData calcView calcHandler,
            mvh initialReverseData reverseView reverseHandler,
            mvh initialDuplicateData duplicateView absurd
            ]
        ]
    ]
  window.show

main :: IO ()
main = do
  _ <- Gtk.init

  app <- new Gtk.Application [#applicationId := "declarative.example",
                              On #activate (activate ?self)]

  void $ app.run Nothing

