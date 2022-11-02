# functionmusic
Authors: Andreas Mühlmann, Brandon Wolf

GitHub repository: "https://github.com/AndreasMuehlmann/functionmusic"

A project in which we draw a function in a coordinate system, where the x - axis is the time since start 'till end, 
while the y - axis stands for the frequence. So, the higher the value that comes out for y in the function, the 
higher is the frequencey for the played tone at second x. The function is defined in code and only can changed there.


## Example
![grafik](https://user-images.githubusercontent.com/100072844/197040723-cb331334-062d-47a7-951b-46931a1d3ce7.png)


## Quickstart
Clone the git repository with
"git clone https://github.com/AndreasMuehlmann/functionmusic.git".

If git is not installed download the
repository from the website via the zip-archive (click on the green button).

Run the executable suited for your OS.
When your on Linux run: ".\target\release\fraktalgen"
When your on Windows run: ".\target\release\fraktalgen.exe" (this doesn't exist right now)

or just run an executable with: ./target/release/functionmusic


## Playing with the source code
The project is written in rust so you need to install the rust compiler.

To do that visit this website: "https://www.rust-lang.org/learn/get-started"

If that is done, run "cargo run", to compile and run the program.


## Goals
- More than 1 function playable at same time and/ or behind each other
- A sign, possible is a static line, which stays at the same place to show at which moment you are by the function
  -> So that the timeline on the left always shows the current state of time
  
 ## To-Do
  
 - draw window
