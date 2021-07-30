valuesGBP :: [Int]
valuesGBP = [1,2,5,10,20,50,100,200,500,1000,2000,5000]

formatFractionTo2DP :: String -> String
formatFractionTo2DP value
    | length value == 0 = "00"
    | length value == 1 = value ++ "0"
    | length value == 2 = value

formatValueGBP :: Int -> String
formatValueGBP value = do
    "£" ++ show (div value 100) ++ "." ++ formatFractionTo2DP(show (mod value 100))

getDenomNameGBP :: Int -> String
getDenomNameGBP value
    | value == 1 = "1p"
    | value == 2 = "2p"
    | value == 5 = "5p"
    | value == 10 = "10p"
    | value == 20 = "20p"
    | value == 50 = "50p"
    | value == 100 = "£1"
    | value == 200 = "£2"
    | value == 500 = "£5"
    | value == 1000 = "£10"
    | value == 2000 = "£20"
    | value == 5000 = "£50"
    | otherwise = "** UNKNOWN"

data ChangeCalcRecord  = ChangeCalcRecord {
    denomValue :: Int,
    denomCount :: Int
} deriving Show

data ChangeResults = ChangeResults {
    changeValue :: Int,
    records :: [ChangeCalcRecord]
} deriving Show

sumChange :: [ChangeCalcRecord] -> Int
sumChange [] = 0
sumChange (x:xs) = (denomValue x) * (denomCount x) + sumChange xs

calcNextChangeRecord :: Int -> ChangeResults -> ChangeResults
calcNextChangeRecord currentDenomValue lastChangeResults = do
    let newChangeCalcRecord = ChangeCalcRecord {
        denomValue = currentDenomValue, 
        denomCount = (div ((changeValue lastChangeResults) - sumChange (records lastChangeResults)) currentDenomValue)
        }
    ChangeResults { changeValue = changeValue lastChangeResults, records = newChangeCalcRecord:records lastChangeResults }

calculateChange :: Int -> [ChangeCalcRecord]
calculateChange changeValue
    | changeValue <= 0 = []
    | otherwise =
        do
            let initResults = ChangeResults { changeValue = changeValue, records = [] }
            let results = foldr calcNextChangeRecord initResults valuesGBP
            reverse (filter (\x -> (denomCount x) > 0) (records results))

formatChangeRecords :: [ChangeCalcRecord] -> String
formatChangeRecords [] = ""
formatChangeRecords (x:xs) = do
    show x
    formatChangeRecords xs

changeRecordString :: ChangeCalcRecord -> String
changeRecordString record = show (denomCount record) ++ " x " ++ getDenomNameGBP (denomValue record)

formatResults :: [ChangeCalcRecord] -> [String]
formatResults records = do
    map changeRecordString $ records

printElements :: [String] -> IO()
printElements [] = putStr ""
printElements (x:xs) = do
    putStrLn $ x
    printElements xs

printHeader :: Int -> Int -> IO()
printHeader productCost amountPresented = do
    putStrLn $ "Product cost = " ++ (formatValueGBP productCost) ++ ", amount presented = " ++ (formatValueGBP amountPresented)
    putStrLn ""

printFooter :: Int -> IO()
printFooter changeValue = do
    putStrLn ""
    putStrLn ""
    putStrLn $ "Total change = " ++ (formatValueGBP changeValue)

main = do
    let productCost = 3410
    let valuePresented = 10000
    let changeValue = valuePresented - productCost
    let changeRecords = calculateChange changeValue
    let formattedChangeResults = formatResults changeRecords
    printHeader productCost valuePresented
    printElements formattedChangeResults
    printFooter changeValue
