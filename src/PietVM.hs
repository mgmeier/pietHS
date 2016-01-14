{-# LANGUAGE RecordWildCards, LambdaCase, MultiWayIf #-}

module  PietVM
        ( newPietVM
        , stepVM
        , roll
        ) where

import  Definition

import  qualified GHC.Exts  as Ext (Down(..))
import  Prelude             hiding (Either(..))
import  Data.Ord            (comparing)
import  Data.List           (minimumBy)
import  Data.Monoid         ((<>))
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



findEdge2 :: Direction -> Direction -> [(Int, Int)] -> (Int, Int)
findEdge2 dp_ cc_ =
    minimumBy (dpOrdering <> ccOrdering)
  where
    dpOrdering  = compPred dp_
    ccOrdering  = compPred (ccDirection cc_ dp_)

    -- get a comparison predicate on coordinates given some Direction
    compPred :: Direction -> (Int, Int) -> (Int, Int) -> Ordering
    compPred Up     = comparing snd
    compPred Left   = comparing fst
    compPred Down   = comparing (Ext.Down . snd)
    compPred Right  = comparing (Ext.Down . fst)

    -- given cc and dp, get combined direction for finding code block edge
    ccDirection :: Direction -> Direction -> Direction
    ccDirection Left    = pointer (-1)
    ccDirection _       = pointer 1


findInstruction :: PietVM -> (Maybe (PietInstruction, Int), PietVM)
findInstruction vm @ PietVM {code = Code {..}, pos = currentPosition} =
    (instr, vm')
  where
    prevCol = getToken currentPosition
    nextCol = getToken pos
    (passedWhite, vm' @ PietVM {..}) = findBlock 1 False vm
    instr = if not running || passedWhite
        then Nothing
        else Just (decodeToken prevCol nextCol, length $ getBlock currentPosition)

findBlock :: Int -> Bool -> PietVM -> (Bool, PietVM)
findBlock tries passedWhite vm @ PietVM {code = Code {..}, pos = pos, dp = dp, cc = cc}
    | tries > 8 = (passedWhite, vm {running = False})
    | otherwise = let tok = getToken pos' in if
        | tok == tokenBlock -> undefined                                        -- TODO
        | tok == tokenPassThru -> findBlock 0 True vm {pos = pos'}
        | otherwise -> undefined                                                -- TODO
  where
    block = getBlock pos
    pos' = move (findEdge2 dp cc block) dp


stepVM :: PietVM -> IO PietVM
stepVM vm_ = flip (maybe (return vm)) mInstr $ \(instr, value) -> case instr of
    In isInt    -> in_ isInt >>= maybe
                    (return vm)
                    (\x -> trace instr x >> return vm {stack = x : stack})
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

        Pointer     -> monovalent (\h t -> vm {stack = t, dp = pointer h dp})
        Duplicate   -> monovalent (\h _ -> vm {stack = h : stack})
        Pop         -> monovalent (\_ t -> vm {stack = t})
        Switch      -> monovalent (\h t -> vm {stack = t, cc = if (even . abs) h then cc else ccFlip cc})
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
        x:y:xs  -> vm {stack = maybe xs (: xs) (x `op` y)}
        _       -> vm


-- primops for the VM

-- intNot primop for not instruction
intNot :: Int -> Int
intNot 0 = 1
intNot _ = 0

-- roll primop
roll :: [Int] -> [Int]
roll (steps:depth:xs)
    | depth < 2 || steps' == 0  = xs
    | depth <= length xs =
        let (xs', rest) = splitAt depth xs
        in (take depth . drop steps' . cycle) xs' ++ rest
  where
    steps' = steps `mod` depth
roll xs = xs

-- pointer primop
pointer :: Int -> Direction -> Direction
pointer 0 dir = dir
pointer steps dir = toEnum $ (steps + fromEnum dir) `mod` 4

-- flip primop for switch instruction
ccFlip :: Direction -> Direction
ccFlip Left = Right
ccFlip _    = Left

-- out primop
out :: Bool -> Int -> IO ()
out True    = putStr . show
out False   = putChar . chr

-- in primop
in_ :: Bool -> IO (Maybe Int)
in_ True = do
    putStr "Enter an integer: "
    readMaybe <$> readLn
in_ False = Just . ord <$> getChar
