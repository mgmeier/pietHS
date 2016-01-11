{-# LANGUAGE RecordWildCards, LambdaCase, MultiWayIf #-}

module  PietVM
        ( newPietVM
        , stepVM
        ) where

import  Definition

import  Prelude             hiding (Either(..))
import  Text.Read           (readMaybe)


newPietVM :: Code -> PietVM
newPietVM c = PietVM
    { code      = c
    , running   = True
    , cc        = Left
    , dp        = Right
    , pos       = (0, 0)
    , stack     = []
    , trace     = \_ _ -> return ()
    }


move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) = \case
    Up      -> (x, y-1)
    Left    -> (x-1, y)
    Down    -> (x, y+1)
    Right   -> (x+1, y)


roll :: [Int] -> [Int]
roll (steps:depth:xs)
    | depth == 0 || steps' == 0  = xs
    | depth <= length xs =
        let
            (xs', rest) = splitAt depth xs
        in xs' ++ rest                                                          -- TODO perform roll
  where
    steps' = steps `mod` depth
roll xs = xs


findInstruction :: PietVM -> (Maybe (PietInstruction, Int), PietVM)
findInstruction vm @ PietVM {code = Code {..}, pos = currentPosition} =
    (instr, vm')
  where
    block   = getBlock currentPosition
    prevCol = getToken currentPosition
    nextCol = getToken pos

    (passedWhite, vm' @ PietVM {..}) = findBlock 1 False vm
    instr = if not running || passedWhite
        then Nothing
        else Just (decodeToken prevCol nextCol, length block)

findBlock :: Int -> Bool -> PietVM -> (Bool, PietVM)
findBlock tries passedWhite vm @ PietVM {code = Code {..}, pos = pos, dp = dp}
    | tries > 8 = (passedWhite, vm {running = False})
    | otherwise = let tok = getToken pos' in if
        | tok == tokenBlock -> undefined                                        -- TODO
        | tok == tokenPassThru -> findBlock 0 True vm {pos = pos'}
        | otherwise -> undefined                                                -- TODO
  where
    block = getBlock pos
    pos' = move pos dp
    findEdge = (0, 0)                                                           -- TODO


stepVM :: PietVM -> IO PietVM
stepVM vm_ = flip (maybe (return vm)) mInstr $ \(instr, value) -> case instr of
    In isInt    -> in_ isInt >>= maybe
                    (return vm)
                    (\x -> trace instr x >> return vm {stack = x:stack})
    Out isInt   -> case stack of
                    x:xs    -> trace instr x >> out isInt x >> return vm {stack = xs}
                    _       -> return vm
    _           -> trace instr value >> return (case instr of
        Add         -> bivalent (\x y -> Just $ y + x)
        Subtract    -> bivalent (\x y -> Just $ y - x)
        Multiply    -> bivalent (\x y -> Just $ y * x)
        Greater     -> bivalent (\x y -> Just $ if y > x then 1 else 0)
        Divide      -> bivalent (\x y -> if x == 0 then Nothing else Just $ y `div` x)
        Mod         -> bivalent (\x y -> if x == 0 then Nothing else Just $ y `mod` x)

        Pointer     -> monovalent (\h t -> vm {stack = t, dp = toEnum $ (h + fromEnum dp) `mod` 4})
        Duplicate   -> monovalent (\h _ -> vm {stack = h : stack})
        Pop         -> monovalent (\_ t -> vm {stack = t})
        Switch      -> monovalent (\h t -> vm {stack = t, cc = if (even . abs) h then cc else flipDir cc})
        Not         -> monovalent (\h t -> vm {stack = intNot h : t})

        Roll        -> vm {stack = roll stack}
        Push        -> vm {stack = value : stack}

        _           -> vm
        )
  where
    (mInstr, vm @ PietVM {..}) = findInstruction vm_

    -- operation with one argument; applied only unless stack is empty
    monovalent op = case stack of
        x:xs    -> op x xs
        _       -> vm

    -- operation with two arguments and the possibility of failure
    bivalent op = case stack of
        x:y:xs  -> vm {stack = maybe stack (:xs) (x `op` y)}
        _       -> vm



flipDir :: Direction -> Direction
flipDir Left    = Right
flipDir Right   = Left
flipDir Up      = Down
flipDir Down    = Up

intNot :: Int -> Int
intNot 0 = 1
intNot _ = 0

out :: Bool -> Int -> IO ()
out True    = putStr . show
out False   = putChar . chr

in_ :: Bool -> IO (Maybe Int)
in_ True = do
    putStr "Enter an integer: "
    readMaybe <$> readLn
in_ False = Just . ord <$> getChar
