{-# LANGUAGE Arrows #-}

import Data.List
import System.IO

data TextEditor = TextEditor [Char] [Char] [Char] [Char] deriving(Show)


start::IO()
start = do
    putStrLn "\n--- Hello, Welcome to the Text Editor---\n---      Enter First Sentence         ---\n"
    a <- getLine
    putStrLn "---      Enter Second Sentence        ---\n"
    c <- getLine
    let b = "|"
    let d = " "
    putStrLn(a ++ b ++ c)
    
    let mcl = showOutput(moveCursorLeft(TextEditor a b c d))
    let mcr = showOutput(moveCursorRight(TextEditor a b c d))
    let dcl = showOutput(deleteCharLeft(TextEditor a b c d))
    let dcr = showOutput(deleteCharRight(TextEditor a b c d))
    let ms = showOutput(moveStart(TextEditor a b c d))
    let me = showOutput(moveEnd(TextEditor a b c d))
    let mcwr = showOutput(moveCursorWordRight(TextEditor a b c d))
    let mcwl = showOutput(moveCursorWordLeft(TextEditor a b c d))
    let swr = showOutput(selectWordRight(TextEditor a b c d))
    let swl = showOutput(selectWordLeft(TextEditor a b c d))
    let scr = showOutput(selectCharRight(TextEditor a b c d))
    let scl = showOutput(selectCharLeft(TextEditor a b c d))
    
    
    let menu  = do
            putStrLn "---  Please enter a function to use   ---\n"
            putStrLn "1 - Add words\n2 - Move Cursor Left\n3 - Move Cursor Right\n4 - Delete Char Left\n5 - Delete Char Right\n6 - Move Cursor to the start\n7 - Move Cursor to the end\n8 - Move Cursor Word Left\n9 - Move Cursor Word Right\n10 - Select Char Left\n11 - Select Char Right\n12 - Select Word Left\n13 - Select Word Right\n14 - Count Characters\n15 - Save To File\n16 - Open File\nq - quit\nr - restart\n"
    
            i <- getLine
            if i == "1" then do
                        putStrLn "Enter a string"
                        inputChar <- getLine
                        let addC = showOutput(addChar(TextEditor a b c d)inputChar)
                        putStrLn(addC)
                        menu
            else if i == "2" then do
                        putStrLn(mcl)
                        menu    
            else if i == "3" then do
                        putStrLn (mcr) 
                        menu
            else if i == "4" then do
                        putStrLn(dcl) 
                        menu
            else if i == "5" then do
                        putStrLn(dcr) 
                        menu
            else if i == "6" then do
                        putStrLn(ms) 
                        menu
            else if i == "7" then do 
                        putStrLn(me) 
                        menu
            else if i == "8" then do  
                        putStrLn(mcwl) 
                        menu
            else if i == "9" then do 
                        putStrLn(mcwr) 
                        menu
            else if i == "10" then do
                        putStrLn "Press enter to copy selected char"
                        let g = copy(selectCharLeft(TextEditor a b c d))
                        putStrLn(showCopyRight(g))
                        putStrLn "Would you like to paste\n1 - Paste after cursor\n2 - Paste before cursor\n0 - quit\nr - restart"
                        choice <- getLine
                        case choice of 
                            "1" -> do
                                let g = copy(selectCharLeft(TextEditor a b c d))
                                let showPaste = showOutput(pasteAfter g)
                                putStrLn (showPaste)
                            "2" -> do
                                let g = copy(selectCharLeft(TextEditor a b c d))
                                let showPaste = showOutput(pasteBefore g)
                                putStrLn (showPaste)
                            "0" -> return()
                        menu
            else if i == "11" then do
                        putStrLn "Press enter to copy selected char"
                        let g = copy(selectCharRight(TextEditor a b c d))
                        putStrLn(showCopyRight(g))
                        putStrLn "Would you like to paste\n1 - Paste after cursor\n2 - Paste before cursor\n0 - quit\n0 - restart"
                        choice <- getLine
                        case choice of 
                            "1" -> do
                                let g = copy(selectCharRight(TextEditor a b c d))
                                let showPaste = showOutput(pasteAfter g)
                                putStrLn (showPaste)
                            "2" -> do
                                let g = copy(selectCharRight(TextEditor a b c d))
                                let showPaste = showOutput(pasteBefore g)
                                putStrLn (showPaste)
                            "0" -> return()
                        menu
            else if i == "12" then do
                        putStrLn "Press enter to copy selected word"
                        let g = copy(selectWordLeft(TextEditor a b c d))
                        putStrLn(showCopyLeft(g))
                        putStrLn "Would you like to paste\n1 - Paste after cursor\n2 - Paste before cursor\n0 - quit\n0 - restart"
                        choice <- getLine
                        case choice of 
                            "1" -> do
                                let g = copy(selectWordLeft(TextEditor a b c d))
                                let showPaste = showOutput(pasteAfterWL g)
                                putStrLn (showPaste)
                            "2" -> do
                                let g = copy(selectWordLeft(TextEditor a b c d))
                                let showPaste = showOutput(pasteBeforeWL g)
                                putStrLn (showPaste)
                            "0" -> return()
                        menu
            else if i == "13" then do
                        putStrLn "Press enter to copy selected word"
                        let g = copy(selectWordRight(TextEditor a b c d))
                        putStrLn(showCopyRight(g))
                        putStrLn "Would you like to paste\n1 - Paste after cursor\n2 - Paste before cursor\n0 - quit\n0 - restart"
                        choice <- getLine
                        case choice of 
                            "1" -> do
                                let g = copy(selectWordRight(TextEditor a b c d))
                                let showPaste = showOutput(pasteAfter g)
                                putStrLn (showPaste)
                            "2" -> do
                                let g = copy(selectWordRight(TextEditor a b c d))
                                let showPaste = showOutput(pasteBefore g)
                                putStrLn (showPaste)
                            "0" -> return()
                        menu
            else if i == "14" then do 
                        putStrLn "Press enter to copy selected word"
                        let g = copy(selectAll(TextEditor a b c d))
                        putStrLn(showCopyRight(g))
                        putStrLn "Would you like to paste\n1 - Paste after cursor\n2 - Paste before cursor\n0 - quit\n0 - restart"
                        choice <- getLine
                        case choice of 
                            "1" -> do
                                let g = copy(selectAll(TextEditor a b c d))
                                let showPaste = showOutput(pasteAfter g)
                                putStrLn (showPaste)
                            "2" -> do
                                let g = copy(selectAll(TextEditor a b c d))
                                let showPaste = showOutput(pasteBefore g)
                                putStrLn (showPaste)
                            "0" -> return()
                        menu
            else if i == "14" then do 
                        let g = countChar(showOutput(TextEditor a b c d))
                        putStrLn "\nWord Count: "
                        print(g)
                        menu
            else if i == "15" then do
                        putStrLn "save input text"
                        save(TextEditor a b c d)  
                        menu
            else if i == "16" then do 
                        putStrLn "Opening File..."
                        let filePath = "Test.txt"
                        contents <- readFile filePath 
                        putStrLn(contents)
                        menu
            else if i == "r" then 
                        start 
            else if i == "q"
                then return()
                else do
                    putStrLn "Please enter a correct value\n"
                    menu
    menu

--shows text editor output - 
showOutput :: TextEditor -> String
showOutput (TextEditor a b c d) = a ++ b ++ c ++ d

-- add chars -- 
addChar :: TextEditor -> String -> TextEditor
addChar (TextEditor a b c d) inputChar = TextEditor(a)(inputChar ++ b)(c)(d)

-- cutting functions --
cutRight:: TextEditor -> TextEditor
cutRight (TextEditor a b c d) = TextEditor(a)(b)("")(c)

cutLeft :: TextEditor -> TextEditor
cutLeft (TextEditor a b c d) = TextEditor("")(b)(c)(a)

-- delete character left or right --
deleteCharLeft :: TextEditor -> TextEditor 
deleteCharLeft (TextEditor a b c d) = TextEditor(reverse(tail(reverse(head[a]))))(b)(c)(d)

deleteCharRight :: TextEditor -> TextEditor
deleteCharRight (TextEditor a b c d) = TextEditor(a)(b)(tail c)(d)

-- move cursor left or right - 
moveCursorLeft :: TextEditor -> TextEditor
moveCursorLeft (TextEditor a b c d) = TextEditor(reverse(tail(reverse(head[a]))))(b)(take 1 (reverse(head[a]))++c)(d)

moveCursorRight :: TextEditor -> TextEditor
moveCursorRight (TextEditor a b c d) = TextEditor (a++ [head c])(b)(tail c)(d)

-- move cursor to the start or end of the line --
moveStart :: TextEditor -> TextEditor
moveStart (TextEditor a b c d) = TextEditor (b)(a)(c)(d)

moveEnd :: TextEditor -> TextEditor
moveEnd (TextEditor a b c d) = TextEditor (a)(c)(b)(d)

-- move cursor word left or right --
moveCursorWordLeft :: TextEditor -> TextEditor
moveCursorWordLeft (TextEditor a b c d)
    | a==[] = TextEditor([])([])(c)(d)
    | head(reverse a) == ' ' = TextEditor(reverse(tail(reverse a)))([])(head[reverse a])(d)
    | otherwise = moveCursorWordLeft(TextEditor(reverse(tail(reverse a)))([])(head[reverse a])(d))


moveCursorWordRight :: TextEditor -> TextEditor
moveCursorWordRight (TextEditor a b c d)
    | c==[] = TextEditor(a)(b)([])(d)
    | head(c) == ' ' = TextEditor(a)(b)(tail c)(d)
    | otherwise = moveCursorWordRight(moveCursorRight(TextEditor a b c d))

-- select word left or right --
selectWordLeft :: TextEditor -> TextEditor
selectWordLeft (TextEditor a b c d)
    | c == [] = TextEditor(a)(b)([])([])
    | head(reverse a) == ' ' = TextEditor(a)(reverse(b++([head(reverse a)])))(c)([])
    | otherwise = selectWordLeft(TextEditor(reverse(tail(reverse(a))))(b++[head(reverse a)])(c)([]))

selectWordRight :: TextEditor -> TextEditor
selectWordRight (TextEditor a b c d)
    | c == [] = TextEditor(a)(b)([])(d)
    | head(c) == ' ' = TextEditor(a)(b++[head (c)])(tail c)(d)
    | otherwise = selectWordRight(TextEditor(a)(b++[head (c)])(tail c)(d))
    
-- select char left or right --
selectCharLeft :: TextEditor -> TextEditor
selectCharLeft (TextEditor a b c d) = TextEditor(reverse(tail(reverse(head[a]))))(b ++take 1(reverse(head[a])))(c)(d)

selectCharRight :: TextEditor -> TextEditor
selectCharRight (TextEditor a b c d) = TextEditor(a)(b++[head c])(tail c)(d)

selectAll :: TextEditor -> TextEditor
selectAll (TextEditor a b c d) = TextEditor([])(b++(a++c))([])(d)

-- copy word left or right -- 
copy :: TextEditor -> TextEditor
copy (TextEditor a b c d) = TextEditor a b c b

-- show the copied word --
showCopyRight :: TextEditor -> String
showCopyRight (TextEditor a b c d) = "Selected Word: " ++ tail d

showCopyLeft :: TextEditor -> String
showCopyLeft (TextEditor a b c d) = "Selected Word: " ++ reverse(tail(reverse d))

-- paste the word before or after cursor --
pasteAfter :: TextEditor -> TextEditor
pasteAfter (TextEditor a b c d) = TextEditor (a)(b++ (tail d))(c)([])

pasteBefore :: TextEditor -> TextEditor
pasteBefore (TextEditor a b c d) = TextEditor (a)((tail d) ++ b)(c)([])

pasteAfterWL :: TextEditor -> TextEditor
pasteAfterWL (TextEditor a b c d) = TextEditor (a)(b++ (reverse(tail(reverse d))))(c)([])

pasteBeforeWL :: TextEditor -> TextEditor
pasteBeforeWL (TextEditor a b c d) = TextEditor (a)((reverse(tail(reverse d))) ++ b)(c)([])

-- save text editor to a txt file --
save :: TextEditor -> IO()
save(TextEditor a b c d) = writeFile "Test.txt" (showOutput(TextEditor(a)(b)(c)(d)))

countChar :: String -> Int
countChar str = length str


    
    

   

