import Test.QuickCheck

main :: IO ()
main =
    do
        let g = do
            s <- (arbitrary :: Gen String) `suchThat` (not . null)
            c <- elements s
            return (s, c)
        sample g
