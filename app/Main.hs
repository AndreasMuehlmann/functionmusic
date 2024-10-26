module Main where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as B
import Data.Foldable
import Data.List
import Numeric (pi)
import System.Process
import Text.Printf
import System.IO

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

outputFilePath :: FilePath
outputFilePath = "output.bin"

volume :: Float
volume = 0.2

sampleRate :: Samples
sampleRate = 100.0

pitchStandard :: Hz
pitchStandard = 440.0

bpm :: Beats
bpm = 120.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
semitoneToFrequency :: Semitone -> Hz
semitoneToFrequency n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: Semitone -> Beats -> [Pulse]
note n beats = freq (semitoneToFrequency n) (beats * beatDuration)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
  map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output
  where
    step = (hz * 2 * pi) / sampleRate

    attack :: [Pulse]
    attack = map (min 1.0) [0.0, 0.001 ..]

    release :: [Pulse]
    release = reverse $ take (length output) attack

    output :: [Pulse]
    output = map (sin . (* step)) [0.0 .. sampleRate * duration]

wave :: [Pulse]
wave =
  concat
    [ note 0 0.25,
      note 0 0.25,
      note 0 0.25,
      note 0 0.25,
      note 0 0.5,
      note 0 0.25,
      note 0 0.25,
      note 0 0.25,
      note 0 0.25,
      note 0 0.25,
      note 0 0.25,
      note 0 0.5,
      note 5 0.25,
      note 5 0.25,
      note 5 0.25,
      note 5 0.25,
      note 5 0.25,
      note 5 0.25,
      note 5 0.5,
      note 3 0.25,
      note 3 0.25,
      note 3 0.25,
      note 3 0.25,
      note 3 0.25,
      note 3 0.25,
      note 3 0.5,
      note (-2) 0.5,
      note 0 0.25,
      note 0 0.25,
      note 0 0.25,
      note 0 0.25,
      note 0 0.5
    ]
phaseToMatchPreviousSinWave :: Wave -> Float -> Hz -> Phase
phaseToMatchPreviousSinWave (prevFreq, prevPhase) x freq = prevFreq * 2 * pi * x + prevPhase - (freq * 2 * pi * x)

sampleRateFunctions :: Float
sampleRateFunctions = sampleRate

nextPulse :: Wave -> Float -> Hz -> (Pulse, Wave)
nextPulse wave x freq = (sin (freq * 2 * pi * nextX + phase), (freq, phase))
  where
    nextX = x + 1 / sampleRateFunctions
    phase = phaseToMatchPreviousSinWave wave x freq

toPulses :: [Semitone] -> Wave -> Float -> [Pulse]
toPulses [] wave prevX = []
toPulses (semitone : semitones) prevWave prevX = pulse : toPulses semitones wave (prevX + 1 / sampleRateFunctions)
  where (pulse, wave) = nextPulse prevWave prevX (semitoneToFrequency semitone)

functionAsSemitones :: SoundFunction -> Float -> Float -> [Pulse]
functionAsSemitones soundFunction from duration = toPulses semitones (pitchStandard, 0.0) 0.0
  where
    semitones = map (soundFunction . (/ sampleRate)) [0.0 .. sampleRate * duration]

save :: FilePath -> IO ()
save filePath = B.writeFile filePath $ BB.toLazyByteString $ foldMap BB.floatLE $ functionAsSemitones parabel 0 0.1

constant :: Float -> SoundFunction
constant value x = value

parabel :: SoundFunction
parabel x = x * x - 100


saveRawFloats :: FilePath -> IO ()
saveRawFloats filePath = do
    let contents = unlines $ map show $ functionAsSemitones parabel 0 5
    writeFile filePath contents

play :: IO ()
play = do
  save outputFilePath
  _ <- runCommand $ printf "ffplay -autoexit -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

main :: IO ()
main = save outputFilePath
