{- LANGUAGE TemplateHaskell #-}

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe
import Data.List
import Control.Lens

data Loc =
     Albania
   | Ankara
   | Apulia
   | Armenia
   | Belgium
   | Berlin
   | Bohemia
   | Brest
   | Budapest
   | Bulgaria
   | Burgundy
   | Clyde
   | Constantinople
   | Denmark
   | Edinburgh
   | Finland
   | Galicia
   | Gascony
   | Greece
   | Holland
   | Kiel
   | Liverpool
   | Livonia
   | London
   | Marseilles
   | Moscow
   | Munich
   | Naples
   | NorthAfrica
   | Norway
   | Paris
   | Picardy
   | Piedmont
   | Portugal
   | Prussia
   | Rome
   | Ruhr
   | Rumania
   | Serbia
   | Sevastopol
   | Silesia
   | Smyrna
   | Spain
   | StPetersburg
   | Sweden
   | Switzerland
   | Syria
   | Trieste
   | Tunis
   | Tuscany
   | Tyrolia
   | Ukraine
   | Venice
   | Vienna
   | Wales
   | Warsaw
   | Yorkshire
   | AdriaticSea
   | AegeanSea
   | BalticSea
   | BarentsSea
   | BlackSea
   | EasternMediterranean
   | EnglishChannel
   | GulfOfLyon
   | GulfOfBothnia
   | HelgolandBight
   | IonianSea
   | IrishSea
   | MidAtlanticOcean
   | NorthAtlanticOcean
   | NorthSea
   | NorwegianSea
   | Skagerrak
   | TyrrhenianSea
   | WesternMediterranean deriving (Eq, Ord, Show, Enum, Bounded)

waterProvinces = [
   AdriaticSea,
   AegeanSea,
   BalticSea,
   BarentsSea,
   BlackSea,
   EasternMediterranean,
   EnglishChannel,
   GulfOfLyon,
   GulfOfBothnia,
   HelgolandBight,
   IonianSea,
   IrishSea,
   MidAtlanticOcean,
   NorthAtlanticOcean,
   NorthSea,
   NorwegianSea,
   Skagerrak,
   TyrrhenianSea,
   WesternMediterranean
   ]

coastalProvinces = [
   Wales
   ]

adjMap = Map.fromList [
   (Wales, [London, Yorkshire, Liverpool, EnglishChannel]),
   (Switzerland, [])
   ]

data UnitType =
     Army
   | Fleet deriving (Eq, Show, Ord)

data Power =
     Austria
   | England
   | France
   | Germany
   | Italy
   | Russia
   | Turkey deriving (Eq, Show, Ord)


data Season =
     Spring
   | Summer
   | Fall
   | Winter deriving (Eq, Show)

data Unit = Unit {
   _power    :: Power,
   _utype    :: UnitType
} deriving (Eq, Show, Ord)

data Occ = Occ {
   _present   :: Maybe Unit,
   _displaced :: Maybe Unit
} deriving (Show)

data GameState = GameState {
   _date   :: (Int, Season),
   _board  :: Map Loc Occ,
   _depots :: [(Loc, Maybe Power)]
} deriving (Show)

makeLenses ''GameState
makeLenses ''Unit
makeLenses ''Occ

data Order =
     Hold    Unit
   | Move    Unit Loc
   | Support Unit Order
   | Convoy  Unit Loc Loc deriving (Eq, Show)

type Result = (Order, Either String ())

--getUnits b x = view (board . ix x) $ b0
--
--addUnit :: GameState -> Loc -> Unit -> GameState
--addUnit i l u = over (board . ix l . present) $ i
--
--removeUnit :: GameState -> Loc -> Unit -> GameState
--removeUnit i l u = over (board . ix l) (delete u) $ i
--
--resolveOrders :: GameState -> [Order] -> [Result]
--resolveOrders = undefined

--changeStatus :: GameState -> Loc -> Unit -> Status -> GameState

newGame = GameState (1901, Spring) initialUnits initialDepots
   where
      initialUnits = Map.fromList [(x, Occ Nothing Nothing) | x <- [minBound :: Loc ..]]
      initialDepots = [
         (Vienna, Just Austria), (Budapest, Just Austria), (Trieste, Just Austria)]

--advanceGame :: GameState -> Result -> GameState

--advanceGame :: GameState -> Order -> GameState
--advanceGame s o (Move u l) =


--evaluateOrders :: GameState -> [Orders] -> Result -> Result



--filterOrders :: ([Order], [Order]) -> [Order]
--filterOrders (xs, []) = xs
--filterOrders (xs, y:ys) = case (y) of
--   Hold _ -> filterOrders (y:xs, ys)
--   Move u t -> case (adjacent (location u) t) of
--      True -> filterOrders (y:xs, ys)
--      False -> filterOrders (xs, ys)
--
--legalOrder :: Board -> [Order] -> Order -> Bool
--legalOrder b as a = case (a) of
--   Hold u -> unitExists u
--   Move u t -> unitExists u && (adjacent (location u) t) && case (utype u) of
--      Army -> t `notElem` waterProvinces
--      Fleet -> t `elem` waterProvinces || t `elem` coastalProvinces
--   where
--      unitExists u = u `elem` b
--
--adjacent f t = fromMaybe False $ do
--   l <- M.lookup f adjMap
--   return (t `elem` l)
--
--
--
