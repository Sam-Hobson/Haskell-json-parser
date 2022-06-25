module Main where

import JSON
import Parser
import System.Environment ( getArgs )

-- | Prompt user for a JSON file, parse it and pretty print the result.
-- For example, it sould print new lines after each '[',']','{','}' or ',' 
-- and '[' or '{' should increase the indent level while ']' or '}' should decrease the indent level.
-- You can try it out with the JSON examples in <code>share/</code>.
--
-- /Tip:/ use @getArgs@, @readJsonFile@ and @putStrLn@
--
-- You can run this function in GHCi by calling:
-- > :main "input.json"
wrapIn :: String -> IO String -> String -> IO String
wrapIn s1 ios s2 = (\x -> s1 ++ x ++ s2) <$> ios 

printJson :: Int -> JsonValue -> IO String
printJson i (JsonString s) = pure $ replicate i '\t' ++ show s ++ "\n"
printJson i (JsonRational s) = pure $ replicate i '\t' ++ show s ++ "\n"
printJson i JsonTrue = pure $ replicate i '\t' ++ "True\n"
printJson i JsonFalse = pure $ replicate i '\t' ++ "False\n"
printJson i JsonNull = pure $ replicate i '\t' ++ "Null\n"
printJson i (JsonArray a) = wrapIn "[\n" (mconcat (printJson (i + 1) <$> a)) "]\n"
printJson i (JsonObject o) = wrapIn "{\n" (mconcat $ printJson (i + 1) . snd <$> o) "}\n"

main :: IO ()
main = do
    x <- head <$> getArgs
    let jsonVals = getMem <$> readJsonValue x
    (jsonVals >>= printJson 0) >>= putStr
