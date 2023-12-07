module ParallelPSO (optimize) where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (minimumBy, findIndex)
import Data.Maybe (fromMaybe)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData, rnf)

data Particle = Particle {
    position :: [Double],
    velocity :: [Double],
    bestPosition :: [Double],
    bestScore :: Double
} deriving (Show)

instance NFData Particle where
    rnf (Particle position velocity bestPosition bestScore) =
        rnf position `seq` rnf velocity `seq` rnf bestPosition `seq` rnf bestScore

generatePosition :: ([Double], [Double]) -> IO [Double]
generatePosition (lowerBounds, upperBounds) = 
    mapM (\(l, u) -> randomRIO (l, u)) (zip lowerBounds upperBounds)

createParticle :: [Double] -> Int -> ([Double] -> Double) -> Particle
createParticle pos dimension objectiveFunction =
    let vel = replicate dimension 0
        score = objectiveFunction pos
    in Particle pos vel pos score

initializeSwarm :: Int -> Int -> ([Double], [Double]) -> ([Double] -> Double) -> IO ([Particle], Double, [Double])
initializeSwarm swarmSize dimension bounds objectiveFunction = do
    positions <- replicateM swarmSize (generatePosition bounds)
    let swarm = parMap rdeepseq (\pos -> createParticle pos dimension objectiveFunction) positions
    let scores = map bestScore swarm
    let bestScoreValue = minimum scores
    let bestParticleIndex = fromMaybe 0 (findIndex (\p -> bestScore p == bestScoreValue) swarm)
    let bestParticle = swarm !! bestParticleIndex
    return (swarm, bestScoreValue, position bestParticle)
    
updateParticle :: Particle -> [Double] -> ([Double] -> Double) -> ([Double], [Double]) -> Double -> Double -> Double -> Double -> Double -> Particle
updateParticle particle globalBestPosition objectiveFunction bounds c1 c2 w r1' r2' =
    let r1 = r1' * c1
        r2 = r2' * c2
        inertia = map (* w) (velocity particle)
        cognitive = map (* r1) (zipWith (-) (bestPosition particle) (position particle))
        social = map (* r2) (zipWith (-) globalBestPosition (position particle))
        newVelocity = zipWith3 (\i c s -> i + c + s) inertia cognitive social
        newPositionRaw = zipWith (+) (position particle) newVelocity
        newPosition = clipToBounds newPositionRaw bounds
        newScore = objectiveFunction newPosition
        (newBestPosition, newBestScore) = if newScore < bestScore particle
                                          then (newPosition, newScore)
                                          else (bestPosition particle, bestScore particle)
    in particle { position = newPosition, velocity = newVelocity, bestPosition = newBestPosition, bestScore = newBestScore }

clipToBounds :: [Double] -> ([Double], [Double]) -> [Double]
clipToBounds position (lowerBounds, upperBounds) = 
    zipWith3 (\p l u -> max l (min u p)) position lowerBounds upperBounds

optimize :: Int -> Int -> ([Double] -> Double) -> ([Double], [Double]) -> Double -> Double -> Double -> Int -> IO ([Double], Double)
optimize swarmSize dimension objectiveFunction bounds c1 c2 w iterations = do
    (swarm, initialBestScore, initialBestPosition) <- initializeSwarm swarmSize dimension bounds objectiveFunction
    iterateSwarm swarm initialBestScore initialBestPosition iterations
    where
        iterateSwarm :: [Particle] -> Double -> [Double] -> Int -> IO ([Double], Double)
        iterateSwarm swarm globalBestScore globalBestPosition 0 = return (globalBestPosition, globalBestScore)
        iterateSwarm swarm globalBestScore globalBestPosition n = do
            randomValues <- replicateM (2 * length swarm) (randomRIO (0.0, 1.0))
            let randomPairs = pairUp randomValues
            let updatedSwarm = parMap rdeepseq (\(p, (r1, r2)) -> updateParticle p globalBestPosition objectiveFunction bounds c1 c2 w r1 r2) (zip swarm randomPairs)
            let newBestParticle = minimumBy (\p1 p2 -> compare (bestScore p1) (bestScore p2)) updatedSwarm
            let newBestScore = bestScore newBestParticle
            let newBestPosition = bestPosition newBestParticle
            iterateSwarm updatedSwarm newBestScore newBestPosition (n - 1)
        pairUp :: [a] -> [(a, a)]
        pairUp (x:y:xs) = (x, y) : pairUp xs
        pairUp _ = []