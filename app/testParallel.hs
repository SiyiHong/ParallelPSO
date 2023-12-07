import ParallelPSO (optimize)
import Data.Time.Clock

objectiveFunction :: [Double] -> Double
objectiveFunction vars = 
    sum [sin x * cos y * exp (abs (1 - sqrt (x^2 + y^2) / pi)) | x <- vars, y <- vars]

swarmSize :: Int
swarmSize = 1000

dimension :: Int
dimension = 2

lowerBounds :: [Double]
lowerBounds = [-100, -100]

upperBounds :: [Double]
upperBounds = [100, 100]

c1, c2, w :: Double
c1 = 1.5  -- 个体学习因子
c2 = 1.5  -- 社会学习因子
w = 0.9   -- 惯性权重

iterations :: Int
iterations = 100  -- 迭代次数

main :: IO ()
main = do
    start <- getCurrentTime
    (bestPosition, bestScore) <- optimize swarmSize dimension objectiveFunction (lowerBounds, upperBounds) c1 c2 w iterations
    end <- getCurrentTime
    let diff = diffUTCTime end start
    putStrLn $ "Best Position: " ++ show bestPosition
    putStrLn $ "Best Score: " ++ show bestScore
    putStrLn $ "Time taken: " ++ show (diff * 1000) ++ " milliseconds"