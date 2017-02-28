data BSTree : (a: Type) -> Type where
     Empty : Ord a => BSTree a
     Node : Ord a => (left : BSTree a) -> (val : a) ->
                        (right : BSTree a) -> BSTree a

%name BSTree t, t1

insert : a -> BSTree a -> BSTree a
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) =
     case compare x val of
        LT => Node (insert x left) val right
        EQ => orig
        GT => Node left val (insert x right)

listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = (treeToList left) ++ (val :: treeToList right)
