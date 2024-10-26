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

type SoundFunction = Float -> Float

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

semitonesAsPulsesHelper :: [Semitone] -> Wave -> Float -> [Pulse]
semitonesAsPulsesHelper [] wave prevX = []
semitonesAsPulsesHelper (semitone : semitones) prevWave prevX = pulse : semitonesAsPulsesHelper semitones wave nextX
  where
    (pulse, wave, nextX) = nextPulse prevWave prevX (semitoneToFrequency semitone)

semitonesAsPulses :: [Semitone] -> [Pulse]
semitonesAsPulses semitones = semitonesAsPulsesHelper semitones (pitchStandard, 0.0) 0.0

functionAsSemitones :: SoundFunction -> Float -> Seconds -> [Semitone]
functionAsSemitones soundFunction from duration = map (soundFunction . (+ from) . (/ sampleRate)) [ 0.0 .. sampleRate * duration]

constant :: Float -> SoundFunction
constant value x = value

linear :: Float -> Float -> SoundFunction
linear sloap offset x = sloap * x + offset

parabel :: Float -> Float -> SoundFunction
parabel koefficient offset x = koefficient * x * x + offset

sinus :: Float -> Float -> Float -> SoundFunction
sinus amplitude freq offset x = amplitude * sin (freq * 2 * pi * x) + offset

volume :: Float
volume = 0.2

repeatList :: Int -> [a] -> [a]
repeatList repetitions list = concatMap (const list) [1 .. repetitions]

composition :: [Pulse]
composition =  zipWith (+) (repeatList 12 (semitonesAsPulses parabelSemitones)) (zipWith (+) (semitonesAsPulses sinusSemitones2) $ semitonesAsPulses sinusSemitones)
  where parabelSemitones = functionAsSemitones (parabel 20 (-8)) (-0.5) 1
        sinusSemitones = functionAsSemitones (sinus 6 0.1 0) 0 6
        sinusSemitones2 = functionAsSemitones (sinus 4 0.3 0) 0 6

saveAsCsv :: FilePath -> IO ()
saveAsCsv filePath = do
  let contents = unlines $ zipWith (++) (map (++ ",") xs) pulses
  writeFile filePath contents
  where pulses = map show composition
        xs     = map (show . (/ sampleRate)) [0.0 .. fromIntegral (length pulses)]

save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ BB.toLazyByteString $ foldMap (BB.floatLE . (volume *)) composition

play :: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = save outputFilePath
