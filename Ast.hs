module Ast (Ast (Node, Leaf), mergeFunction) where
import Data.Maybe (isJust)


data Ast = Node String String [Ast] | Leaf String String

mergeNodes :: [Ast] -> Maybe Ast
mergeNodes [] = Nothing
mergeNodes ((Node n v c):xs) = Just $ Node n v (c++help xs)
  where
    help [] = []
    help ((Node _ _ c):xs) = c ++ help xs
    help ((Leaf _ _):xs) = help xs
mergeNodes ((Leaf _ _):xs) = Nothing

mergeFunction :: [Ast] -> [Ast]
mergeFunction xs = help xs []
  where
    help :: [Ast] -> [Ast] -> [Ast]
    help [] ys     = ys
    help lis@((Node n v c):xs) ys = help (minus xs kvi) (add z ys)
        where
          kvi = keyValueIn n v lis
          z = mergeNodes kvi
          add (Just x) ys = ys ++ [x]
          add Nothing ys = ys

          minus [] ys = []
          minus xs [] = xs
          minus xs (y:ys) = minus (filter (/=y) xs) ys

    help (leaf:xs) ys = help xs (leaf:ys)

    keyValueIn :: String -> String -> [Ast] -> [Ast]
    keyValueIn k v [] = []
    keyValueIn k1 v1 (n@(Node k2 v2 c2):xs)
      | k1 == k2 && v1 == v2 = n:(keyValueIn k1 v1 xs)
      | otherwise = keyValueIn k1 v1 xs
    keyValueIn k1 v1 (l@(Leaf k2 v2):xs)
      | k1 == k2 && v1 == v2 = l:(keyValueIn k1 v1 xs)
      | otherwise = keyValueIn k1 v1 xs

instance Show Ast where
  show :: Ast -> String
  show = prettyShow

-- Pretty print function
prettyShow :: Ast -> String
prettyShow = go ""
  where
    go :: String -> Ast -> String
    go indent (Node name value children) =
      indent ++ name ++ "(\"" ++ value ++ "\")\n" ++
      concatMap (go (indent ++ "| ")) children
    go indent (Leaf name value) = indent ++ name ++ "(\"" ++ value ++ "\")\n"


instance Eq Ast where
  (==) :: Ast -> Ast -> Bool
  (==) (Node {}) (Leaf {}) = False
  (==) (Leaf {}) (Node {}) = True
  (==) (Node n1 v1 _) (Node n2 v2 _) = n1 == n2 && v1 == v2
  (==) (Leaf n1 v1) (Leaf n2 v2) = n1 == n2 && v1 == v2