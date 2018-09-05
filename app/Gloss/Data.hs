{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Gloss.Data
 ( module Gloss.Data
 , module Control.Lens
 ) where

import           Data.Char (toLower)
import           Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq (..))
import           Control.Lens
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import           System.Exit (exitSuccess)

data WindowSize = WindowSize
  { _winX :: !Int
  , _winY :: !Int
  }
  deriving (Show)
makeLenses ''WindowSize

data PointerPosition = PointerPosition
  { _posX :: !Float
  , _posY :: !Float
  }
  deriving (Show)
makeLenses ''PointerPosition

type PressedKeys = Seq Gloss.Key

data GlossState = GlossState
  { _stPosMaybe :: !(Maybe PointerPosition)
  , _stMods :: !Gloss.Modifiers
  , _stPressed :: !PressedKeys
  , _stWin :: !WindowSize
  }
  deriving (Show)
makeLenses ''GlossState

stPos :: Lens' GlossState PointerPosition
stPos = lens (\st -> fromMaybe (PointerPosition 0 0) $ st ^. stPosMaybe)
             (\st pos -> st & stPosMaybe ?~ pos)

(|>!) :: Seq a -> a -> Seq a
els |>! (!el) = els Seq.|> el

data KeyboardButton =
    CharButton
    { _kbChar :: !Char
    }
  | SpecialKey
    { _kbKey :: !Gloss.SpecialKey
    }
  deriving (Show)
makeLenses ''KeyboardButton

data Event =
    MouseClick
    { _mcButton :: !Gloss.MouseButton
    , _mcState :: !Gloss.KeyState
    }
  | KeyPress
    { _kpButton :: !KeyboardButton
    , _kpState :: !Gloss.KeyState
    }
  | PointerMove
    { _pmPos :: !PointerPosition
    }
  | Resize
    { _rsSize :: !WindowSize
    }
  deriving Show
makeLenses ''Event

data SimulationState world = SimulationState
  { _ssWorld :: !world
  , _ssState :: !GlossState
  , _ssInited :: !Bool
  }
  deriving Show
makeLenses ''SimulationState

play
  :: Gloss.Display
  -> Gloss.Color
  -> Int
  -> world
  -> (world -> Gloss.Picture)
  -> (Event -> GlossState -> world -> world)
  -> (Float -> GlossState -> world -> world)
  -> IO ()
play display color fps world render handle update =
  playIO display color fps world render' handle' update'
  where
    render' = return .  render
    handle' evt gs w = do
      case evt of
        KeyPress (SpecialKey Gloss.KeyEsc) Gloss.Down -> exitSuccess
        _                                             -> return ()
      return $ handle evt gs w
    update' tm gs = return . update tm gs

playIO
  :: Gloss.Display
  -> Gloss.Color
  -> Int
  -> world
  -> (world -> IO Gloss.Picture)
  -> (Event -> GlossState -> world -> IO world)
  -> (Float -> GlossState -> world -> IO world)
  -> IO ()
playIO display color fps world render handle update =
  Gloss.playIO display color fps (SimulationState world (gsDefault 0 0) False)
                                 (render . (^. ssWorld)) handle' update'
  where
    gsDefault width height = GlossState Nothing
                                        (Gloss.Modifiers Gloss.Down Gloss.Down Gloss.Down)
                                        mempty
                                        (WindowSize width height)
    handle' evt ss
      | ss ^. ssInited = case evt of
          Gloss.EventKey keY keySt mods (x, y) ->
            let key = case keY of
                  Gloss.Char chr -> Gloss.Char $ toLower chr
                  _              -> keY
                gs = ss ^. ssState & (stMods .~ mods)
                                   . (stPos .~ PointerPosition x y)
                                   . (stPressed %~ addMb . Seq.filter (/= key))
                addMb pressed = case keySt of
                  Gloss.Down -> pressed |>! key
                  Gloss.Up -> pressed
                ev = case key of
                  Gloss.Char chr -> KeyPress (CharButton chr) keySt
                  Gloss.SpecialKey key' -> KeyPress (SpecialKey key') keySt
                  Gloss.MouseButton btn -> MouseClick btn keySt
            in do
              w <- handle ev gs $ ss ^. ssWorld
              return $ ss & ssState .~ gs
                          & ssWorld .~ w
          Gloss.EventMotion (x, y) ->
            let pos = PointerPosition x y
                gs = ss ^. ssState & stPos .~ pos
            in do
              w <- handle (PointerMove pos) gs $ ss ^. ssWorld
              return $ ss & ssState .~ gs
                          & ssWorld .~  w
          Gloss.EventResize (width, height) ->
            let size = WindowSize width height
                gs = ss ^. ssState & stWin .~ size
            in do
              w <- handle (Resize size) gs $ ss ^. ssWorld
              return $ ss & ssState .~ gs
                          & ssWorld .~ w
      | otherwise      = case evt of
          Gloss.EventResize (width, height) ->
            let gs = gsDefault width height
            in do
              w <- handle (Resize $ WindowSize width height) gs $ ss ^. ssWorld
              return $ ss & ssState .~ gs
                          & ssWorld .~ w
                          & ssInited .~ True
          _ -> error "Impossible"
    update' secs ss
      | ss ^. ssInited = do
          w <- update secs (ss ^. ssState) $ ss ^. ssWorld
          return $ ss & ssWorld .~ w
      | otherwise      = return ss
