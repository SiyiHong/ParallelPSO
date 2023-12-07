module PSO (optimize) where

import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (minimumBy, findIndex)
import Data.Maybe (fromMaybe)

data Particle = Particle {
    position :: [Double],
    velocity :: [Double],
    bestPosition :: [Double],
    bestScore :: Double
} deriving (Show)

initializeParticle :: Int -> ([Double], [Double]) -> ([Double] -> Double) -> IO Particle
initializeParticle dimension (lowerBounds, upperBounds) objectiveFunction = do
    pos <- mapM (\(l, u) -> randomRIO (l, u)) (zip lowerBounds upperBounds)
    let vel = replicate dimension 0
    let score = objectiveFunction pos
    return $ Particle pos vel pos score

initializeSwarm :: Int -> Int -> ([Double], [Double]) -> ([Double] -> Double) -> IO ([Particle], Double, [Double])
initializeSwarm swarmSize dimension bounds objectiveFunction = do
    swarm <- replicateM swarmSize (initializeParticle dimension bounds objectiveFunction)
    let scores = map bestScore swarm
    let bestScoreValue = minimum scores
    let bestParticleIndex = fromMaybe 0 (findIndex (\p -> bestScore p == bestScoreValue) swarm)
    let bestParticle = swarm !! bestParticleIndex
    return (swarm, bestScoreValue, position bestParticle)
    
updateParticle :: Particle -> [Double] -> ([Double] -> Double) -> ([Double], [Double]) -> Double -> Double -> Double -> IO Particle
updateParticle particle globalBestPosition objectiveFunction bounds c1 c2 w = do
    r1' <- randomRIO (0.0, 1.0)
    r2' <- randomRIO (0.0, 1.0)
    let r1 = r1' * c1
    let r2 = r2' * c2
    let inertia = map (* w) (velocity particle)
    let cognitive = map (* r1) (zipWith (-) (bestPosition particle) (position particle))
    let social = map (* r2) (zipWith (-) globalBestPosition (position particle))
    let newVelocity = zipWith3 (\i c s -> i + c + s) inertia cognitive social
    let newPositionRaw = zipWith (+) (position particle) newVelocity
    let newPosition = clipToBounds newPositionRaw bounds
    let newScore = objectiveFunction newPosition
    let (newBestPosition, newBestScore) = if newScore < bestScore particle
                                          then (newPosition, newScore)
                                          else (bestPosition particle, bestScore particle)
    return particle { position = newPosition, velocity = newVelocity, bestPosition = newBestPosition, bestScore = newBestScore }

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
            updatedSwarm <- mapM (\p -> updateParticle p globalBestPosition objectiveFunction bounds c1 c2 w) swarm
            let newBestParticle = minimumBy (\p1 p2 -> compare (bestScore p1) (bestScore p2)) updatedSwarm
            let newBestScore = bestScore newBestParticle
            let newBestPosition = bestPosition newBestParticle
            iterateSwarm updatedSwarm newBestScore newBestPosition (n - 1)