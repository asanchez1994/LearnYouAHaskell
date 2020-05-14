-- record syntax 
data Person = Person { firstName :: String
                    ,  lastName :: String
                    ,  age :: Int
                    ,  height :: Float
                    ,  phoneNumber :: String
                    ,  flavor :: String
                    }  deriving (Show)

johnson = Person "Johnson" "Boons" 27 3.0 "12123098" "Almond"


