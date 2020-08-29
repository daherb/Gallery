import System.Directory
import System.FilePath
import Data.Char
import System.Process
import System.Environment

thumb_width = 200
thumb_height = 100

pictures_per_row = 5

image_types = ["jpg","png"]
video_types = ["mp4"]

types = image_types ++ video_types

thumbs_dir = "thumbnails"
html_file = "index.html"

play_file = do
  t <- getTemporaryDirectory
  return $ t </> "play_icon.png"

listFiles :: IO [(FilePath,String)]
listFiles =
  do
    fs <- listDirectory "."
    -- The _:fext drops the dot before the extension
    return [ (f,tail fext) | f <- fs, let (_,fext) = splitExtensions (map toLower f), (not . null) fext, (tail fext) `elem` types]

createThumbnailsDir :: IO ()
createThumbnailsDir =
  createDirectoryIfMissing False thumbs_dir

createThumbnails :: [(FilePath,String)] -> IO [FilePath]
createThumbnails files = do
  createThumbnailsDir
  sequence [ createThumb fext fpath | (fpath, fext) <- files]
  where
    createThumb :: String -> FilePath -> IO FilePath
    createThumb ext
      | ext `elem` image_types = createImageThumb
      | ext `elem` video_types = createVideoThumb
      | otherwise = (\f -> error $ "How did the file " ++ f ++ " with extension " ++ ext ++ " get into the list?")
    createImageThumb :: FilePath -> IO FilePath
    createImageThumb file =
      do
        let nfile = thumbs_dir </> (file <.> ".jpg")
        system $ "convert " ++ file ++ " -resize " ++ show thumb_width ++ "x" ++ show thumb_height ++ "\\> "++ nfile
        return nfile
    createVideoThumb :: FilePath -> IO FilePath
    createVideoThumb file =
      do
        icon <- play_file
        let nfile = thumbs_dir </> (file <.> ".jpg")
        system $ "convert " ++ file ++ "[1] -resize " ++ show thumb_width ++ "x" ++ show thumb_height ++ "\\> "++ icon ++ " -gravity center -composite " ++ nfile
        return nfile

decodeIcon :: IO ()
decodeIcon = do
  icon <- play_file
  system $ "printf " ++ play_icon ++ " | base64 -d > " ++ icon
  return ()
  
generateHTML :: String -> [(FilePath,FilePath)] -> IO ()
generateHTML title files =
  writeFile html_file $ unlines htmlCode
  where
    htmlCode =
      ["<html>","<head>","<link rel=\"stylesheet\" type=\"text/css\" href=\"gallery.css\">","</head>","<body>","<h1>" ++ title ++ "</h1>","<table id = \"gallery\">"] ++
      ["<tr>" ++ unlines ["<td class=\"thumb\"> <a href=\""++ pic ++ "\" target=\"_blank\"> <img src=\""++ tn ++ "\" /> </a> </td>" | (pic,tn) <- row] ++ "</tr>" | row <- splitList pictures_per_row files] ++
      ["</table>","</body>","</html>"]
    
splitList :: Int -> [a] -> [[a]]
splitList _ [] = []
splitList count l = take count l:splitList count (drop count l)

main =
  do
    args <- getArgs
    if length args == 0 then usage
      else
      do
        fs <- listFiles
        ts <- createThumbnails fs
        decodeIcon
        generateHTML (args !! 0) $ zip (map fst fs) ts

usage :: IO ()
usage = do
  name <- getProgName
  putStrLn $ name ++ " title : Generates a new thumbnail gallery with the given title"
  
play_icon :: String
play_icon =
  "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"
