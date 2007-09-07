class FooClass a b | a -> b where
    myf1 :: a -> b
    myf2 :: a -> a

instance FooClass [a] a where
    myf1 = head
    myf2 = id
