module Editor.Signals where

import Signal
import String
import Signal.Time exposing (settledAfter)
import Time

requestPalette :
  { address : Signal.Address String
  , signal : Signal String
  }
requestPalette = Signal.mailbox ""


requestPaletteFilter : Signal String
requestPaletteFilter =
  Signal.filter (String.isEmpty >> not) "" requestPalette.signal
  |> settledAfter (300 * Time.millisecond)
