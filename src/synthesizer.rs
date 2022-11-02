use std::time::Duration;
use rodio::OutputStream;
use rodio::Sink;
use rodio::source::SineWave;
use rodio::source::Source;
use std::thread::sleep;


pub struct WaveSpec {
    pub frequency: f32,
    pub amplitude: f32, 
    pub duration: Duration,
}


pub struct Synthesizer {
    list_wave_specs: Vec<Vec<WaveSpec>>,
}


impl Synthesizer {
    pub fn new() -> Self {
        return Self {
            list_wave_specs: Vec::new(),
        } 
    }

    pub fn add_wave_specs(&mut self, wave_specs: Vec<WaveSpec>) {
        self.list_wave_specs.push(wave_specs);
    }

    pub fn play(&mut self, duration_secs: Duration) {
        let (_stream, stream_handle) = OutputStream::try_default().unwrap();

        if self.list_wave_specs.len() < 1 {
            return;
        }
        let sink1 = Sink::try_new(&stream_handle).unwrap();
        for wave_spec in &self.list_wave_specs[0] {
            let source = SineWave::new(wave_spec.frequency).take_duration(wave_spec.duration).amplify(wave_spec.amplitude);
            sink1.append(source);
        }

        if self.list_wave_specs.len() < 2 {
            sleep(duration_secs);
            return;
        }
        let sink2 = Sink::try_new(&stream_handle).unwrap();
        for wave_spec in &self.list_wave_specs[1] {
            let source = SineWave::new(wave_spec.frequency).take_duration(wave_spec.duration).amplify(wave_spec.amplitude);
            sink2.append(source);
        }

        if self.list_wave_specs.len() < 3 {
            sleep(duration_secs);
            return;
        }
        let sink3 = Sink::try_new(&stream_handle).unwrap();
        for wave_spec in &self.list_wave_specs[2] {
            let source = SineWave::new(wave_spec.frequency).take_duration(wave_spec.duration).amplify(wave_spec.amplitude);
            sink3.append(source);
        }
        if self.list_wave_specs.len() < 4 {
            sleep(duration_secs);
            return;
        }
        let sink4 = Sink::try_new(&stream_handle).unwrap();
        for wave_spec in &self.list_wave_specs[3] {
            let source = SineWave::new(wave_spec.frequency).take_duration(wave_spec.duration).amplify(wave_spec.amplitude);
            sink4.append(source);
        }
        if self.list_wave_specs.len() < 5 {
            sleep(duration_secs);
            return;
        }
        let sink5 = Sink::try_new(&stream_handle).unwrap();
        for wave_spec in &self.list_wave_specs[4] {
            let source = SineWave::new(wave_spec.frequency).take_duration(wave_spec.duration).amplify(wave_spec.amplitude);
            sink5.append(source);
        }
        if self.list_wave_specs.len() < 6 {
            sleep(duration_secs);
            return;
        }
        let sink6 = Sink::try_new(&stream_handle).unwrap();
        for wave_spec in &self.list_wave_specs[5] {
            let source = SineWave::new(wave_spec.frequency).take_duration(wave_spec.duration).amplify(wave_spec.amplitude);
            sink6.append(source);
        }
        if self.list_wave_specs.len() < 7 {
            sleep(duration_secs);
            return;
        }
        let sink7 = Sink::try_new(&stream_handle).unwrap();
        for wave_spec in &self.list_wave_specs[6] {
            let source = SineWave::new(wave_spec.frequency).take_duration(wave_spec.duration).amplify(wave_spec.amplitude);
            sink7.append(source);
        }
        if self.list_wave_specs.len() < 8 {
            sleep(duration_secs);
            return;
        }
        let sink8 = Sink::try_new(&stream_handle).unwrap();
        for wave_spec in &self.list_wave_specs[7] {
            let source = SineWave::new(wave_spec.frequency).take_duration(wave_spec.duration).amplify(wave_spec.amplitude);
            sink8.append(source);
        }
        sleep(duration_secs);
    }
}
