module Main where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as B
import Data.Foldable
import Data.List
import Numeric (pi)
import System.IO
import System.Process
import Text.Printf

type Pulse = Float

type Seconds = Float

type Samples = Float

type Hz = Float

type Semitone = Float

type Beats = Float

type SoundFunction = Float -> Float

type Point = (Float, Float)

type Phase = Float

type Wave = (Hz, Phase)

-- freq :: Hz -> Seconds -> [Pulse]
-- freq hz duration =
--   map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output
--   where
--     step = (hz * 2 * pi) / sampleRate
--
--     attack :: [Pulse]
--     attack = map (min 1.0) [0.0, 0.001 ..]
--
--     release :: [Pulse]
--     release = reverse $ take (length output) attack
--
--     output :: [Pulse]
--     output = map (sin . (* step)) [0.0 .. sampleRate * duration]

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0

outputFilePath :: FilePath
outputFilePath = "output.bin"

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
semitoneToFrequency :: Semitone -> Hz
semitoneToFrequency n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

phaseToMatchPreviousSinWave :: Wave -> Float -> Hz -> Phase
phaseToMatchPreviousSinWave (prevFreq, prevPhase) x freq = prevFreq * 2 * pi * x + prevPhase - (freq * 2 * pi * x)

nextPulse :: Wave -> Float -> Hz -> (Pulse, Wave, Float)
nextPulse wave x freq = (sin (freq * 2 * pi * (x - step) + phase), (freq, phase), x + step)
  where
    step = 1 / sampleRate
    phase = phaseToMatchPreviousSinWave wave x freq

toPulses :: [Semitone] -> Wave -> Float -> [Pulse]
toPulses [] wave prevX = []
toPulses (semitone : semitones) prevWave prevX = pulse : toPulses semitones wave nextX
  where
    (pulse, wave, nextX) = nextPulse prevWave prevX (semitoneToFrequency semitone)

functionAsPulses :: SoundFunction -> Float -> Float -> [Pulse]
functionAsPulses soundFunction from duration = toPulses semitones (pitchStandard, 0.0) 0.0
  where
    semitones = map (soundFunction . (/ sampleRate)) [0.0 .. sampleRate * duration]

constant :: Float -> SoundFunction
constant value x = value

linear :: Float -> Float -> SoundFunction
linear sloap yIntercept x = sloap * x + yIntercept

parabel :: Float -> Float -> SoundFunction
parabel koefficient yIntercept x= koefficient * x * x + yIntercept


volume :: Float
volume = 0.2

soundFunction :: SoundFunction
soundFunction = constant (-10)

duration :: Float
duration = 10.0

start :: Float
start = 0.0

saveAsCsv :: FilePath -> IO ()
saveAsCsv filePath = do
  let contents = unlines $ zipWith (++) (map (++ ",") xs) pulses
  writeFile filePath contents
  where pulses = map show $ functionAsPulses soundFunction start duration
        xs     = map (show . (/ sampleRate)) [0.0 .. fromIntegral (length pulses)]

save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ BB.toLazyByteString $ foldMap (BB.floatLE . (volume *)) (functionAsPulses soundFunction start duration)

play :: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = save outputFilePath
