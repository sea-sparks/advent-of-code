module DayTwo where

import Prelude hiding (length)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow ((***))
import Data.Monoid
import Data.Text hiding (minimum, length, foldr, map) 
import Text.Read (readEither)
import Control.Monad.Except (Except, throwError)

data Box = Box { length :: Integer
               , width  :: Integer
               , height :: Integer}

data Measurements = Measurements { paper :: Integer, ribbon :: Integer }

instance Monoid Measurements where
    mempty = Measurements 0 0
    Measurements a b `mappend` Measurements c d = Measurements (a + c) (b + d)

instance Show Measurements where
    show (Measurements p r) = "Paper: " ++ show p ++ "\nRibbon: " ++ show r

-- Compute the amount of paper and ribbon required for one box
oneBox :: Box -> Measurements
oneBox b =
    let lw = length b * width b in
    let lh = length b * height b in
    let hw = height b * width b in
    let p1 = 2 * (length b + width b) in
    let p2 = 2 * (length b + height b) in
    let p3 = 2 * (height b + width b) in
    let vol = length b * width b * height b in
    Measurements (2 * (lw + lh + hw) + minimum [lw, lh, hw])
                 (minimum [p1, p2, p3] + vol)

allBoxen :: [Box] -> Measurements
allBoxen = mconcat . map oneBox

parseBox :: Text -> Either String Box 
parseBox t = do
    let dims = split (=='x') t 
    case dims of
        [l, w, h] -> Box <$> readEither (unpack l)
                         <*> readEither (unpack w)
                         <*> readEither (unpack h) 
        _ -> throwError$ "Error on string " ++ unpack t ++ ": must give exactly three dimensions"