#!/usr/bin/env stack
{- stack script
 --resolver lts-20.3
 --install-ghc
 --package "containers text"
-}

import Control.Monad
import Data.Function
import qualified Data.List as List
import Data.Maybe
import qualified Data.Text as Text
import System.Environment

main = do
    (inputPath:_) <- getArgs
    contents <- readFile inputPath
    let initialTS = TerminalState { currentDir = [], 
                                  fileSystem = Folder{ folderName="/", childEntries=[] } }
        diskSpace = 70_000_000
        requiredSpace = 30_000_000

    print (contents & 
        parseContent & 
        foldl applyCommand initialTS & 
        fileSystem & 
        getAnswer diskSpace requiredSpace)

data Path = Root | Parent | Child String deriving Show
type FileSize = Int
type FileName = String
data Node = FileEntry { name :: FileName, size :: FileSize } | 
    DirEntry { name :: String } 
    deriving Show
data Command = CD { to :: Path } | LS { children :: [Node] } deriving Show

parsePath :: String -> Path
parsePath text = case text of ".." -> Parent
                              "/" -> Root
                              otherwise -> Child text

parseNode :: String -> Node
parseNode text
    | List.isPrefixOf "dir " text = DirEntry { name = List.stripPrefix "dir " text & fromJust }
    | otherwise = FileEntry { name = name, size = read size :: Int }
        where (size, _:name) = break (==' ') text

parseCD :: String -> Command
parseCD text = text & 
    List.stripPrefix "cd " & 
    fromJust &
    takeWhile (/='\n') &
    parsePath & 
    (\path -> CD { to = path })

parseLS :: String -> Command
parseLS text = text & 
    List.stripPrefix "ls\n" & 
    fromJust & 
    lines &
    map parseNode & 
    (\nodes -> LS { children = nodes })

parseCommand :: String -> Command
parseCommand text
    | List.isPrefixOf "cd" text = parseCD text
    | List.isPrefixOf "ls" text = parseLS text
    | otherwise = error ("Unknown command: " ++ text)

parseContent :: String -> [Command]
parseContent content = content &
    Text.pack &
    Text.splitOn (Text.pack "$ ") &
    map Text.unpack &
    dropWhile (=="") &
    map parseCommand

data FileTree = File { fileName :: FileName, fileSize :: FileSize } | 
    Folder { folderName :: FileName, childEntries :: [FileTree] } 
    deriving Show
data TerminalState = TerminalState { currentDir :: [FileName], 
                                   fileSystem :: FileTree } 
                                   deriving Show

applyCommand :: TerminalState -> Command -> TerminalState
applyCommand ts command = case command of 
    CD to -> TerminalState { currentDir = newPath to, fileSystem = fileSystem ts }
    LS children -> TerminalState { currentDir = currentDir ts, fileSystem = newFS children }
    where
    newPath :: Path -> [FileName]
    newPath to = changeCurrentPath (currentDir ts) to
    newFS :: [Node] -> FileTree
    newFS children = children & 
        map lsNodeToTree & 
        foldl (updateNode (currentDir ts)) (fileSystem ts) 
    

lsNodeToTree :: Node -> FileTree
lsNodeToTree node = case node of
    FileEntry name size -> File { fileName = name, fileSize = size }
    DirEntry name -> Folder { folderName = name, childEntries = [] }

changeCurrentPath :: [FileName] -> Path -> [FileName]
changeCurrentPath currentDir path = case path of 
    Root -> []
    Parent -> init currentDir
    Child p -> currentDir ++ [p]
 
findSubtree :: FileTree -> [FileName] -> Maybe FileTree
findSubtree tree filePath = foldM getSubtree tree filePath

getSubtree :: FileTree -> FileName -> Maybe FileTree
getSubtree tree childName = case tree of
    Folder _ children -> List.find (matchesName childName) children
    File _ _ -> Nothing

getName :: FileTree -> FileName
getName tree = case tree of
    Folder name _ -> name
    File name _ -> name

matchesName :: FileName -> FileTree -> Bool
matchesName name tree = getName tree == name

isSame :: FileTree -> FileTree -> Bool
isSame tree1 tree2 = getName tree1 == getName tree2

updateChild :: FileTree -> FileTree -> FileTree
updateChild currentNode newChild = Folder {folderName = folderName currentNode,
                                          childEntries = updatedChildren}
    where 
    updateMatching newChild child = if isSame child newChild then newChild else child
    children = childEntries currentNode
    updatedChildren = case List.find (isSame newChild) children of
        Just child -> map (updateMatching newChild) children
        Nothing -> newChild:children


updateNode :: [FileName] -> FileTree -> FileTree -> FileTree
updateNode [] root newNode = updateChild root newNode
updateNode (childName:path) root newNode = updateChild root (updateNode path (fromJust directChild) newNode)
    where directChild = getSubtree root childName

calcSize :: FileTree -> Int
calcSize tree = case tree of
    File _ size -> size
    Folder _ children -> sum (map calcSize children)

folders :: FileTree -> [FileTree]
folders tree = case tree of 
    File _ _ -> []
    Folder _ children -> tree:(concat (map folders children))

getAnswer :: Int -> Int -> FileTree -> Int
getAnswer total required tree = folderSizes & List.sort & dropWhile (<sizeToDelete) & head
    where
    folderSizes@(rootSize:_) = tree & folders & map calcSize
    sizeToDelete = required - (total - rootSize)
