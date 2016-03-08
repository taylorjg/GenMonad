import Test.QuickCheck

main :: IO ()
main =
    do
        let g = (arbitrary :: Gen String) `suchThat` (not . null) >>= \s ->
            (\c -> (s, c)) `fmap` (elements s)
        sample g
