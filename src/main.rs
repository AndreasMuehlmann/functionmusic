use std::time::Duration;

mod synthesizer;
use synthesizer::Synthesizer;
use synthesizer::WaveSpec;


fn main() {
    let mut synthesizer: Synthesizer = Synthesizer::new();
    let duration = Duration::from_secs_f32(1.0);

    let wave_specs: Vec<WaveSpec> = get_wave_specs(get_samples(-40.0, 40.0, 1.25, &quadratic), get_samples(-50.0, 50.0, 1.25, &konstant), Duration::from_secs_f32(0.5));
    synthesizer.add_wave_specs(wave_specs);

    let wave_specs: Vec<WaveSpec> = get_wave_specs(get_samples(-500.0, 500.0, 0.5, &sinus), get_samples(-500.0 * 10.0, 500.0 * 10.0, 0.5, &cosinus), duration);
    synthesizer.add_wave_specs(wave_specs);

    let wave_specs: Vec<WaveSpec> = get_wave_specs(get_samples(-25000.0, 25000.0, 5.0, &tangens), get_samples(-2500.0 * 10.0, 2500.0 * 10.0, 0.5, &konstant), Duration::from_secs_f32(0.2));
    synthesizer.add_wave_specs(wave_specs);

    // let wave_specs: Vec<WaveSpec> = get_wave_specs(get_samples(-500.0, 500.0, 0.5, &konstant_700), get_samples(-500.0 * 10.0, 500.0 * 10.0, 0.5, &konstant), duration);
    // synthesizer.add_wave_specs(wave_specs);

    synthesizer.play(Duration::from_secs_f32(45.0));
}

fn get_wave_specs(frequencies: Vec<f32>, amplitudes: Vec<f32>, duration: Duration) -> Vec<WaveSpec> {
    let mut wave_specs: Vec<WaveSpec> = Vec::new();
    for i in 0..frequencies.len() {
        let wave_spec = WaveSpec {
            frequency: frequencies[i],
            amplitude: amplitudes[i],
            duration,
        };
        wave_specs.push(wave_spec);
    }
    return wave_specs;
}

fn get_samples(min: f32, max: f32, step_size: f32, function: &dyn Fn(f32) -> f32) -> Vec<f32> {
    let mut results: Vec<f32> = Vec::new();
    for i in 0..((max - min).abs() as i32) {
        let x: f32 = min + i as f32 * step_size;
        let result: f32 = function(x);
        results.push(result);
    }
    return results;
}

fn konstant_700(_x: f32) -> f32 {
    return 700.0;
}

fn konstant(_x: f32) -> f32 {
    return 0.4;
}

fn linear(x: f32) -> f32 {
    return 500.0 + 10.0 * x;
}

fn quadratic(x: f32) -> f32 {
    return x.powf(2.0) + 200.0;
}

fn cosinus(x: f32) -> f32 {
    return 0.2 + 0.4 * x.cos();
}

fn sinus(x: f32) -> f32 {
    return 600.0 + 500.0 * x.sin();
}

fn tangens(x: f32) -> f32 {
    return 500.0 + 500.0 * x.tan();
}
