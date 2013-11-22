
import           System.Random
import           Control.Monad.Random

die :: (RandomGen g) => Rand g Int
die = getRandomR (1, 6)

dice :: (RandomGen g) => Int -> Rand g [Int]
dice n = sequence (replicate n die)
