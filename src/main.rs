mod program;

use program::Program;

fn main() {
    let prog = Program::compile("\
ADC #$12
LDA $12");
}
