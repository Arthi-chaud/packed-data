import qualified PackedTest.CaseTest as CaseTest
import qualified PackedTest.IdentityTest as IdentityTest
import qualified PackedTest.PackTest as PackTest
import qualified PackedTest.UnpackTest as UnpackTest
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    PackTest.specs
    UnpackTest.specs
    CaseTest.specs
    IdentityTest.specs
