import Test.QuickCheck
import Bug.Types

genIdx :: Gen (Int,Int)
genIdx = do
    s <- choose (3,4)
    i <- choose (0,s*s -1)
    return (s,i)

prop_idxFrmIdx = forAll genIdx $ \(s,i) -> (idx s . frmIdx s) i == i

main :: IO ()
main = quickCheck prop_idxFrmIdx
