import System.Environment (getArgs)
import qualified PSO (optimize)
import qualified ParallelPSO (optimize)
import qualified ParallelPSO2 (optimize)
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
c1 = 1.5
c2 = 1.5
w = 0.9

iterations :: Int
iterations = 100

main :: IO ()
main = do
    args <- getArgs
    let psoVersion = if not (null args) then head args else "default"
    start <- getCurrentTime
    (bestPosition, bestScore) <- case psoVersion of
        "parallel1" -> ParallelPSO.optimize swarmSize dimension objectiveFunction (lowerBounds, upperBounds) c1 c2 w iterations
        "parallel2" -> ParallelPSO2.optimize swarmSize dimension objectiveFunction (lowerBounds, upperBounds) c1 c2 w iterations
        _           -> PSO.optimize swarmSize dimension objectiveFunction (lowerBounds, upperBounds) c1 c2 w iterations
    end <- getCurrentTime
    let diff = diffUTCTime end start
    putStrLn $ "Best Position: " ++ show bestPosition
    putStrLn $ "Best Score: " ++ show bestScore
    putStrLn $ "Time taken: " ++ show (diff * 1000) ++ " milliseconds"