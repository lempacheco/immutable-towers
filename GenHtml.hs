module Main where

import qualified System.FilePath.Glob as Glob

htmlHeader = unlines
    ["<!DOCTYPE html>"
    ,"<html>"
    ,"<head>"
    ,"<script src=\"polyfill.min.js?features=default,Array.prototype.find,Number.isFinite,Number.isInteger,console,console.log,document.head,performance.now\"></script>"
    ,"<title>CodeWorld</title>"
    ,"<link rel=\"stylesheet\" href=\"codeworld.css\">"
    
    ,"<script type=\"text/javascript\" src=\"ghcjs-debugmode.js\"></script>"
    ,"<script type=\"text/javascript\" src=\"cw-demo.js\"></script>"
    ,"<script language=\"javascript\" src=\"rts.js\"></script>"
    ,"<script language=\"javascript\" src=\"lib.js\"></script>"
    ,"<script language=\"javascript\" src=\"out.js\"></script>"
    ,"</head>"
    ,"<body style=\"text-align: center\">"
      
    ,"<canvas id=\"screen\" class=\"fullscreen\"></canvas>"
    ]

htmlFooter = unlines
    ["</body>"
  
    ,"<script type=\"text/javascript\">"
    ,"function addMessage(err, str) {"
    ,"  console.log(str);"
    ,"}"
    ,"</script>"
  
    ,"<script language=\"javascript\" src=\"runmain.js\" defer></script>"
    ,"</html>"
    ]

loadImages :: FilePath -> IO [(String,String)]
loadImages fp = do
    fs <- Glob.glob fp
    return $ map (\x -> (x,x)) fs

addImages :: FilePath -> IO String
addImages fp = do
    imgs <- loadImages fp
    return $ unlines $ map addImage imgs

addImage :: (String,String) -> String
addImage (iden,src) = "<img style=\"visibility: hidden;\" id=" ++ show iden ++ " src=" ++ show src ++ ">"

main :: IO ()
main = do
    imgs <- addImages "resources/**/*.bmp"
    writeFile "app/Main.jsexe/run.html" $ unlines [htmlHeader,imgs,htmlFooter]


    
