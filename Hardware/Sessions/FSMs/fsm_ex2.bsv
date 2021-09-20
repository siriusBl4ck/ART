import StmtFSM::*;
import FIFOF::*;
(* synthesize *)
module mkTb (Empty);
    Stmt test =
    seq
        if ( cond ) seq
            // if cond is true, then execute this sequence
            $display("If seq 1"); // cycle 1
            $display("If seq 2"); // cycle 2
            $display("If seq 3"); // cycle 3
        endseq
        else seq
        // if cond is false, then execute this sequence
            action
                $display("Else seq 1");
            endaction
            action
            $display("Else seq 2");
            endaction
        endseq
////////////////////////////////////////
// noice this is different that if/else inside a action
// this takes one cycle when it executes
action
if (cond)
$display("if action 1");
else
$display("else action 1");
endaction
endseq