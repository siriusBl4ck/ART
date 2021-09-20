// Copyright 2010 Bluespec, Inc. All rights reserved.

package fsm_ex1;
import StmtFSM::*;
import FIFOF::*;

(* synthesize *)
module mkTb (Empty);
    FIFOF#(int) fifo <- mkFIFOF;

    //All things in seq happen one after another. One clock cycle per action

    Stmt test =
    seq
        action
            $display("Enq 10 at time ", $time);
            fifo.enq( 10 );
        endaction
        action
            $display("Enq 20 at time ", $time);
            fifo.enq( 20 );
        endaction
        action
            $display("Enq 30 at time ", $time);
            fifo.enq( 30 );
        endaction
        action
            $display("Enq 40 at time ", $time);
            fifo.enq( 40 );
        endaction
        // await is used to generate an implicit condition
        // await is an action which can be merged into another action
        // in this case I wanted to print a message when we were done
        // while the fifo is notEmpty, the state machine
        // sits here and no actions fire
        action
            $display("Fifo is now empty, continue...");
            await( fifo.notEmpty() == False);
        endaction
    endseq;

    Stmt rcvr =
    seq
        while(True) seq
            action
                $display("fifo popped data", fifo.first(), " at time ", $time);
                fifo.deq();
            endaction
            repeat (5) noAction;
        endseq
    endseq;

    FSM testFSM <- mkFSM(test);
    FSM rcvrFSM <- mkFSM(rcvr);

    rule startit;
        testFSM.start();
        rcvrFSM.start();
    endrule

    rule finish (testFSM.done() && !rcvrFSM.done());
        $finish;
    endrule
endmodule
endpackage