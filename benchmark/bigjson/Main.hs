{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Control.Monad
import Criterion.Main
import Criterion.Main.Options (describe)
import Data
import Data.Aeson
import qualified Data.ByteString as B (readFile, writeFile)
import Data.Maybe (fromJust)
import Data.Packed
import Data.Packed.Instances (caseList)
import qualified Data.Packed.Reader as R
import GHC.IO.Exception (ExitCode (ExitFailure))
import Options.Applicative (execParser)
import System.Directory
import System.Exit (exitWith)

main :: IO ()
main = do
    let jsonFile = "./areaFull.json"
    let packedFile = "./areaFull.pckd"
    ogJsonExists <- doesPathExist jsonFile
    packedFileExists <- doesPathExist packedFile
    unless ogJsonExists $ do
        putStrLn "Could not find Original JSON"
        putStrLn "Download and unzip a dump of the 'area' table here (also, make it a list):"
        putStrLn "https://data.metabrainz.org/pub/musicbrainz/data/json-dumps/20241130-001001/"
        exitWith (ExitFailure 1)
    unless packedFileExists $ do
        -- l <- readFile jsonFile
        -- writeFile "./test.json" "["
        -- forM_ (lines l) $ \line -> appendFile "./test.json" (line ++ ",\n")
        -- appendFile "./test.json" "]"
        eareas <- eitherDecodeFileStrict jsonFile :: IO (Either String [MBArea])
        case eareas of
            Left err -> putStrLn err >> exitWith (ExitFailure 1)
            Right areas -> B.writeFile packedFile (fromPacked (pack areas))
    -- let bs = fromPacked (pack areas)
    -- _ <- print . name . Prelude.head . fst =<< runReader reader (unsafeToPacked bs)
    -- B.writeFile packedFile bs
    -- rbs <- B.readFile packedFile
    -- print (compare bs rbs)
    -- print (compare (B.length bs) (B.length rbs))
    -- _ <- print . name . Prelude.head . fst =<< runReader reader (unsafeToPacked bs)
    -- return ()
    putStrLn "Ready to start"
    mode <- execParser (describe defaultConfig)
    packed <- B.readFile packedFile
    json <- B.readFile jsonFile
    runMode
        mode
        [ bench "packed" $
            nfAppIO
                (\p -> runReader getTotalAreaNameLengthPacked (unsafeToPacked p))
                packed
        , bench "json" $
            nf
                (getTotalAreaNameLength . fromJust . decodeStrict)
                json
        ]

getTotalAreaNameLengthPacked :: PackedReader '[[MBArea]] '[] Int
getTotalAreaNameLengthPacked =
    caseList
        (R.return 0)
        ( R.do
            !n <-
                caseMBArea
                    ( R.do
                        !n <- readerWithFieldSize
                        skipWithFieldSize
                        skipWithFieldSize
                        skipWithFieldSize
                        skipWithFieldSize
                        skipWithFieldSize
                        skipWithFieldSize
                        skipWithFieldSize
                        R.return n
                    )
            r <- getTotalAreaNameLengthPacked
            R.return (length n + r)
        )

getTotalAreaNameLength :: [MBArea] -> Int
getTotalAreaNameLength [] = 0
getTotalAreaNameLength (area : areas) =
    length (name area) + getTotalAreaNameLength areas
