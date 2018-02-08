a [x, y] = [y, "3*(" ++ y ++ ")+(" ++ x ++ ")"]
b [x, y] = ["2*(" ++ y ++ ")+" ++  x, y]
c [x, y] = "(" ++ x ++ ")/(" ++ y ++ ")"
fns n q = fn n (a . b) q ++ "\n" ++ fn n (b . a) q where
	fn n f q = c $ last $ take n $ iterate f q
main = do putStrLn $ fns 8 ["m", "n"]
