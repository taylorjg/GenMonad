import Test.QuickCheck

main :: IO ()
main =
	do
		let g = (arbitrary :: Gen String) `suchThat` (not . null) >>= \s ->
			elements s >>= \c ->
				return (s, c)
		sample g
