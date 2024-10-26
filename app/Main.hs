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

attackFactors :: [Pulse] -> [Float]
attackFactors pulses = zipWith (*) (map (const 1) pulses) (map (min 1.0) [0.0, 0.001 ..])

withAttack :: [Pulse] -> [Pulse]
withAttack pulses = zipWith (*) pulses $ attackFactors pulses

withRelease :: [Pulse] -> [Pulse]
withRelease pulses = zipWith (*) pulses $ reverse $ attackFactors pulses

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
semitonesAsPulses semitones = withRelease $ withAttack $ semitonesAsPulsesHelper semitones (pitchStandard, 0.0) 0.0

functionAsSemitones :: SoundFunction -> Float -> Seconds -> [Semitone]
functionAsSemitones soundFunction from duration = map (soundFunction . (+ from) . (/ sampleRate)) [0.0 .. sampleRate * duration]

constant :: Float -> SoundFunction
constant value x = value

linear :: Float -> Float -> SoundFunction
linear sloap offset x = sloap * x + offset

parabel :: Float -> Float -> Float -> SoundFunction
parabel verticalScaling horizontalScaling offset x = verticalScaling * (x * horizontalScaling) * (x * horizontalScaling) + offset

sinus :: Float -> Float -> Float -> SoundFunction
sinus amplitude freq offset x = amplitude * sin (freq * 2 * pi * x) + offset

volume :: Float
volume = 0.2

repeatList :: Int -> [a] -> [a]
repeatList repetitions list = concatMap (const list) [1 .. repetitions]

composition :: [Pulse]
composition = zipWith (+) (repeatList 12 (semitonesAsPulses parabelSemitones)) (zipWith (+) (semitonesAsPulses sinusSemitones2) $ semitonesAsPulses sinusSemitones)
  where
    parabelSemitones = functionAsSemitones (parabel 20 (-8) 1) (-0.5) 1
    sinusSemitones = functionAsSemitones (sinus 6 0.1 0) 0 6
    sinusSemitones2 = functionAsSemitones (sinus 4 0.3 0) 0 6

composition2 :: [Pulse]
composition2 = repeatList 2 (semitonesAsPulses (parabelSemitones ++ map (const 0) [0 .. sampleRate / 4])) ++
               repeatList 2 (semitonesAsPulses (parabelSemitones2 ++ map (const 0) [0 .. sampleRate / 16])) ++
               repeatList 2 (semitonesAsPulses constSemitones ++ map (const 0) [0 .. sampleRate / 16]) ++
               repeatList 2 (semitonesAsPulses (parabelSemitones ++ map (const 0) [0 .. sampleRate / 4]))
  where
    parabelSemitones = functionAsSemitones (parabel 5.0 5.0 (-3.0)) (-0.1) 0.4
    parabelSemitones2 = functionAsSemitones (parabel 5.0 5.0 2.0) (-0.2) 0.4
    constSemitones = functionAsSemitones (const 0) 0 0.3

saveAsCsv :: FilePath -> IO ()
saveAsCsv filePath = do
  let contents = unlines $ zipWith (++) (map (++ ",") xs) pulses
  writeFile filePath contents
  where
    pulses = map show composition
    xs = map (show . (/ sampleRate)) [0.0 .. fromIntegral (length pulses)]

clamp :: Float -> Float
clamp value = if value > 1.0 then 1.0 else max value (-1.0)

save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ BB.toLazyByteString $ foldMap (BB.floatLE . (volume *) . clamp) composition2

play :: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = save outputFilePath
