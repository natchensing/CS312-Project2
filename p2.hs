putTodo :: (Int, String) -> IO ()
putTodo (n, todo) = putStrLn (show n ++ ": " ++ todo)

prompt :: [String] -> IO ()
prompt todos = do
    putStrLn ""
    putStrLn "Current TODO list:"
    mapM_ putTodo (zip [0..] todos)
    command <- getLine
    interpret command todos

interpret :: String -> [String] -> IO ()
interpret ('+':' ':todo) todos = prompt (todo:todos)
interpret ('-':' ':num ) todos =
    case delete (read num) todos of
        Nothing -> do
            putStrLn "No TODO entry matches the given number"
            prompt todos
        Just todos' -> prompt todos'
interpret  "q"           todos = return ()
interpret  command       todos = do
    putStrLn ("Invalid command: `" ++ command ++ "`")
    prompt todos

delete :: Int -> [a] -> Maybe [a]
delete 0 (_:as) = Just as
delete n (a:as) = do
    as' <- delete (n - 1) as
    return (a:as')
delete _  []    = Nothing

-- calendar part

data DayOfWeek
    = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
    deriving (Eq, Enum, Bounded)

data Month
    = January | February | March     | April   | May      | June
    | July    | August   | September | October | November | December
    deriving (Enum, Bounded, Show)

next :: (Eq a, Enum a, Bounded a) => a -> a
next x | x == maxBound = minBound
       | otherwise     = succ x

pad :: Int -> String
pad day = case show day of
    [c] -> [' ', c]
    cs  -> cs

month :: Month -> DayOfWeek -> Int -> String
month m startDay maxDay = show m ++ " 2015\n" ++ week ++ spaces Sunday
  where
    week = "Su Mo Tu We Th Fr Sa\n"

    spaces currDay | startDay == currDay = days startDay 1
                   | otherwise           = "   " ++ spaces (next currDay)

    days Sunday    n | n > maxDay = "\n"
    days _         n | n > maxDay = "\n\n"
    days Saturday  n              = pad n ++ "\n" ++ days  Sunday    (succ n)
    days day       n              = pad n ++ " "  ++ days (next day) (succ n)

year = month January   Thursday  31
    ++ month February  Sunday    28
    ++ month March     Sunday    31
    ++ month April     Wednesday 30
    ++ month May       Friday    31
    ++ month June      Monday    30
    ++ month July      Wednesday 31
    ++ month August    Saturday  31
    ++ month September Tuesday   30
    ++ month October   Thursday  31
    ++ month November  Sunday    30
    ++ month December  Tuesday   31

main = do
    putStrLn "Commands:"
    putStrLn "+ <String> - Add a TODO entry"
    putStrLn "- <Int>    - Delete the numbered entry"
    putStrLn "q          - Quit"
    putStr year
    prompt []
