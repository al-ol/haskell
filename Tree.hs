import Prelude hiding (lookup)

data BinaryTree k v = Nil | Vertex k v (BinaryTree k v) (BinaryTree k v)  deriving (Show, Eq)

lookup :: Ord k => k -> BinaryTree k v -> Maybe v

lookup k Nil = Nothing
lookup k (Vertex k1 v1 left1 rigth1) | k == k1 = Just v1
                                     | k <  k1 = lookup k left1
                                     | k >  k1 = lookup k rigth1

insert :: Ord k => k -> v -> BinaryTree k v -> BinaryTree k v
insert k v Nil = (Vertex k v Nil Nil)
insert k1 v1 (Vertex k v left rigth) | k1 == k = (Vertex k v1 left rigth)
                                     | k1 >  k = (Vertex k v left (insert k1 v1 rigth))
                                     | k1 <  k = (Vertex k v (insert k1 v1 left) rigth)


merge :: BinaryTree k v -> BinaryTree k v -> BinaryTree k v
merge Nil x = x
merge x Nil = x
merge left_tree (Vertex k1 v1 left rigth) = Vertex k1 v1 (merge left_tree left) rigth

delete :: Ord k => k -> BinaryTree k v -> BinaryTree k v
delete k Nil = Nil                                 
delete k (Vertex k1 v1 left1 rigth1) | k == k1 = merge left1 rigth1
                                     | k <  k1 = Vertex k1 v1 (delete k left1) rigth1
                                     | k >  k1 = Vertex k1 v1 left1 (delete k rigth1)
 
