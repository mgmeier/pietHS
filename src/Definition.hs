
module  Definition
        ( PietInstruction(..)
        , Direction(..)
        , PietVM(..)
        , Code(..)
        , showInstruction
        , decodeToken
        , pietColorScheme
        , tokenBlock
        , tokenPassThru
        , module ReExport
        ) where

import  Data.Vector.Unboxed (Vector)
import  Control.Monad       as ReExport (when, (>=>))
import  Data.Word           as ReExport (Word8)
import  Data.Char           as ReExport (toLower, ord, chr)
import  Data.Maybe          as ReExport
import  Data.Bits


data PietInstruction
    = Nop
    | Push
    | Pop
    | Add
    | Subtract
    | Multiply
    | Divide
    | Mod
    | Pointer
    | Not
    | Greater
    | Switch
    | Roll
    | Duplicate
    | In        Bool
    | Out       Bool
    deriving Show

data Direction
    = Up
    | Left
    | Down
    | Right
    deriving Enum

data PietVM = PietVM
    { code      :: Code
    , running   :: Bool
    , dp        :: Direction
    , cc        :: Direction
    , pos       :: (Int, Int)
    , stack     :: [Int]
    , trace     :: PietInstruction -> Int -> IO ()
    }

-- data type hides the underlying token container behind well-behaved and meaningful accessor functions
data Code = Code
    { getBlock  :: (Int, Int) -> [(Int, Int)]
    , getToken  :: (Int, Int) -> Word8
    }


-- pretty printing, sort of
showInstruction :: PietInstruction -> Int -> String
showInstruction instr val = case instr of
    Push        -> "push " ++ show val
    In isInt    -> "in " ++ if isInt then showInt else showChar
    Out isInt   -> "out " ++ if isInt then showInt else showChar
    i           -> map toLower $ show i
  where
    showChar    = "char '" ++ [chr val] ++ "'"
    showInt     = "int " ++ show val



data Hue = Red | Yellow | Green | Cyan | Blue | Magenta deriving Enum

data Lightness = Light | Normal | Dark deriving Enum

tokenBlock, tokenPassThru :: Word8
tokenBlock      = 64
tokenPassThru   = 128

encodeToken :: Lightness -> Hue -> Word8
encodeToken l h = fromIntegral $ (fromEnum l `shiftL` 4) .|. fromEnum h

decodeToken :: Word8 -> Word8 -> PietInstruction
decodeToken prev next =
    (opcodeMatrix !! dHue) !! dLightness
  where
    hue c       = fromIntegral $ c .&. 15
    lightness c = fromIntegral $ c `shiftR` 4
    dHue        = (hue next - hue prev) `mod` 6
    dLightness  = (lightness next - lightness prev) `mod` 3
    opcodeMatrix =
        [ [Nop,         Push,       Pop     ]
        , [Divide,      Mod,        Not     ]
        , [Greater,     Pointer,    Switch  ]
        , [Duplicate,   Roll,       In False]
        , [In True,     Out False,  Out True]
        ]

pietColorScheme :: [((Word8, Word8, Word8), Word8)]
pietColorScheme =
    [ ((0x00, 0x00, 0x00), tokenBlock)                                          -- blocking color; black
    , ((0xff, 0xc0, 0xc0), encodeToken Light Red)
    , ((0xff, 0xff, 0xc0), encodeToken Light Yellow)
    , ((0xc0, 0xff, 0xc0), encodeToken Light Green)
    , ((0xc0, 0xff, 0xff), encodeToken Light Cyan)
    , ((0xc0, 0xc0, 0xff), encodeToken Light Blue)
    , ((0xff, 0xc0, 0xff), encodeToken Light Magenta)
    , ((0xff, 0x00, 0x00), encodeToken Normal Red)
    , ((0xff, 0xff, 0x00), encodeToken Normal Yellow)
    , ((0x00, 0xff, 0x00), encodeToken Normal Green)
    , ((0x00, 0xff, 0xff), encodeToken Normal Cyan)
    , ((0x00, 0x00, 0xff), encodeToken Normal Blue)
    , ((0xff, 0x00, 0xff), encodeToken Normal Magenta)
    , ((0xc0, 0x00, 0x00), encodeToken Dark Red)
    , ((0xc0, 0xc0, 0x00), encodeToken Dark Yellow)
    , ((0x00, 0xc0, 0x00), encodeToken Dark Green)
    , ((0x00, 0xc0, 0xc0), encodeToken Dark Cyan)
    , ((0x00, 0x00, 0xc0), encodeToken Dark Blue)
    , ((0xc0, 0x00, 0xc0), encodeToken Dark Magenta)
    , ((0xff, 0xff, 0xff), tokenPassThru)                                       -- pass-thru color; white
    ]
