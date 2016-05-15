{-# LANGUAGE OverloadedStrings #-}

module Baselang
  (hours)
  where

import           Control.Lens                ((&), (.~), (^.))
import           Control.Monad.Trans.Class   (lift)
import           Control.Monad.Trans.Maybe   (MaybeT (..))
import           Data.ByteString.Char8       (pack)
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as LBS
import           Data.Maybe                  (fromMaybe)
import           Network.Wreq                (Options, Response, defaults,
                                              getWith, header, responseBody)
import           System.Environment          (lookupEnv)
import           Text.HandsomeSoup           (css, (!))
import           Text.XML.HXT.Core           (readString, runX, withParseHTML,
                                              yes, (>>>))
import           Control.Concurrent.Async    (mapConcurrently)

type Teacher = Int
type Token = String
type Hours = [String]

fromMaybeT :: Monad m => a -> MaybeT m a -> m a
fromMaybeT x = fmap (fromMaybe x) . runMaybeT

base :: String
base = "https://web.baselang.com/classes/calendar/"

lookupToken :: MaybeT IO Token
lookupToken = MaybeT (lookupEnv "BASELANG_TOKEN")

options :: Token -> Options
options token = defaults
  & header "Cookie" .~ [pack(show token)]

mkUrl :: Int -> String
mkUrl = (base++) . show

mkGet :: Teacher -> (Token -> IO (Response ByteString))
mkGet teacher token = getWith (options token) (mkUrl teacher)

readBody :: Response ByteString -> Maybe String
readBody r =  case LBS.length $ r ^. responseBody of
               0 -> Nothing
               _ -> Just $ show $ r ^.responseBody

parseHours :: String -> IO Hours
parseHours s = runX $ readString [withParseHTML yes] s
  >>> css "td.classtime" ! "data-datetime"

getHours :: Teacher -> IO Hours
getHours teacher = fromMaybeT [] $ lookupToken
  >>= lift . mkGet teacher
  >>= MaybeT . return . readBody
  >>= lift . parseHours

teachers :: [Teacher]
teachers = [21,23,24,25,26,46,47,48,50,51,52,53,54,170,181,184,190,196,197,198,199,322]

hours :: IO Hours
hours = concat <$> mapConcurrently getHours teachers
