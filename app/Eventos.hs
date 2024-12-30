module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventKey (SpecialKey (KeyDown)) Down _ _) it 
    | estadoIT it == Menu = it {estadoIT = Jogando} 
    | otherwise = it 
reageEventos _ it = it 
