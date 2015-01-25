module JoinListBufEditor where

import Buffer
import JoinListBuffer
import Editor
import Data.Monoid
import JoinList
import Sized
import Scrabble

bufferString  :: String -> EditorJoinList
bufferString = fromString

main = runEditor editor $ (bufferString . unlines) [ "This buffer is for notes you don't want to save, and for" , "evaluation of steam valve coefficients." , "To load a different file, type the character L followed" , "by the name of the file." ]
