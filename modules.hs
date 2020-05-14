import Data.List
import Data.Char
import qualified Data.Map as Map 
import qualified Data.Set as Set

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub 

-- concat: flattens but only removes one level of nesting

-- transpose: switch columns and rows in 2d matrix(list of lists)

-- intercalate: insert something between elements in a list of lists and return a flattened result

-- intersperse: intersperse something between elements in a list 

-- foldl' and foldl1': non-lazy versions of fold; useful in case of stack overflow from folding on a long list

-- concatMap: map a function to a list and then flatten result

-- and: takes in a list and returns true iff all elemets of list evaluate to true 

-- or: same as and but returns true if any elements of list evalute to true otherwise false

-- any and all: take predicate and return a bool if any/all elements of list satisfy the predicate condition 

-- iterate: take a starting value and repeatedly apply function to it; return infinite list of values generated from repeated function application

numDigits x
    | x > 0     = 1 + numDigits y 
    | otherwise = 0
    where y = (x `div` 10)

-- splitAt: take a number and splits a list at that index; return a tuple containing the two pieces of the split

-- takeWhile: take elements from list until element does not satisfy predicate argument of tW function

-- dropWhile: opposite of above

-- span: like takeWhile but returns a pair of list; first list is what takeWhile would return; second list is what takeWhile would leave out

-- break: breaks list at first point predicate is true; first element of second returned list is first element that satisfies the condition 

-- inits: recursively apply init to a list and return a list of the result
-- tails: as above, but from the tail of the list

-- group: return a list of lists containing adjacent elements of a list of equal 
-- group [1,1,1,2,2,3]
-- [[1,1,1],[2,2],[3]

-- partition: split into two lists based on satisfying predicate; n.b. this is different from span and break which start and beginning and stop once predicate is triggered. Partition picks out all elements of list and the sorts into separate lists

-- find: return first element of list that satisfied predicate; return value is wrapped in a Maybe value 

-- elemIndex: value wrapped in Maybe: Just index of element looked up or Nothing 

-- lines: take a string and returns a list of the lines split on the newline character

-- words and unwords: split words of string into list (splitting on space)

-- nub: returns list with duplicates removed


-- "\\": set difference; removes all elements in left hand list that match those in right hand list

-- union\intersection: same as in sets

-- insert: insert element into sorted list maintaining sortedness 

-- "by and `on`:
-- sortBy (compare `on` length) ""list of liists""

encode :: Int -> String -> String
encode offset msg = 
    let ords = map ord msg
        shifted = map (+ offset) ords
    in  map chr shifted 

decode shift msg = encode (negate shift) msg 
-- Dictionary: list of tuples

phonebook = [("Andrew", 18374928340), ("Sophie", 192834734)]

findKey :: Eq a => a -> [(a, c)] -> c
findKey key dictionary = snd . head . filter (\(k,v) ->  k == key) $ dictionary

findKeyIm key = foldr (\(k,v) acc -> if k ==key then Just v else acc) Nothing

-- map: implement key/ value pairs that are stored in a tree; keys must be orderable and look up is faster

-- fromList (in Data.Map): transforms list of key-value pairs into map implemented in a tree

-- Data.set: ordered lists with unique pairs 
