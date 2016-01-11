{-# LANGUAGE LambdaCase, FlexibleContexts, TypeFamilies #-}

-- cf. http://www.dangermouse.net/esoteric/piet.html

import  PietVM
import  Definition                      hiding (Direction(..))

import  Codec.Picture
import  Codec.Picture.Types             (convertPixel)

import  Data.Vector.Unboxed             ((!?))
import  qualified Data.Vector.Unboxed   as VU
import  qualified Data.Vector.Storable  as VS

import  Control.DeepSeq                 (deepseq)
import  System.Environment              (getArgs)
import  System.IO
import  Data.IORef


-- TODO map different, more stylish color schemes to standard piet
-- TODO variable codel size
-- TODO try switching appearance and/or semantics of b/w


-- image loader action
pietLoader :: FilePath -> IO (Maybe Code)
pietLoader = readImage >=> \case
    Left err                -> putStrLn err >> return Nothing
    Right (ImageRGB8 img)   -> return $ process 3 img
    Right (ImageRGBA8 img)  -> return $ process 4 img
    Right (ImageCMYK8 img)  -> return $ process 3 (pixelMap convertPixel img :: Image PixelRGB8)
    -- TODO add more conversions if necessary

    _ -> putStrLn "unhandled pixel format in source image" >> return Nothing
  where
    process pxSize Image {imageWidth = w, imageData = vec} =
        deepseq code $ Just Code
            { getBlock = _getBlock
            , getToken = fromMaybe tokenBlock . (code !?) . toOffset
            }
      where
        toOffset (x, y) = y * w + x
        -- fromOffset      = flip quotRem w
        neighbs (x, y)  = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

        code :: VU.Vector Word8
        code = VU.generate (VS.length vec `div` pxSize) $ \ix ->
            let getPx ofs = vec `VS.unsafeIndex` (ofs + ix * pxSize)
            in fromMaybe tokenPassThru $ lookup (getPx 0, getPx 1, getPx 2) pietColorScheme

        _getBlock ix_ =
            maybe [] (\col -> gather col [] [ix_]) (code !? toOffset ix_)
         where
            gather _ res [] = res
            gather col res (ix:ixs) = case code !? toOffset ix of
                Just tok | tok == col && ix `notElem` res ->
                     gather col (ix:res) (ixs ++ neighbs ix)
                _ -> gather col res ixs


-- a non-stop run of a VM until termination
runVM :: PietVM -> IO ()
runVM vm = when (running vm) $ stepVM vm >>= runVM

-- given a runner action, print a protocol of all instructions executed during the VM run
withTracer :: (PietVM -> IO ()) -> PietVM -> IO ()
withTracer runner vm = do
    ioRef <- newIORef []
    runner vm {trace = \a b -> modifyIORef' ioRef ((a, b):)}
    putStrLn "\n--> execution trace  <--"
    readIORef ioRef >>= mapM_ (putStrLn . uncurry showInstruction) . reverse

main :: IO ()
main = getArgs >>= \case
    [] -> putStrLn "specify program"
    xs -> pietLoader (last xs) >>= \case
        Nothing -> return ()
        Just c  -> do
            hSetBuffering stdout NoBuffering
            let vm = newPietVM c
            if "-t" `elem` xs then withTracer runVM vm else runVM vm
