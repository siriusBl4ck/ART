package rtl_int_2;
interface Custom_IFC;
    method Action putAB (Int#(32) a, Int#(32) b);
    method Int#(32) getResult;
endinterface

import "BVI" myCustomRTL =
module mkCustom (Custom_IFC);
    default_clock clk (clk);
    default_reset reset (rst_b);
    method putAB(a, b) enable(EN);
    method res getResult();
    schedule (getResult) SB (putAB);
    schedule (getResult) CF (getResult);
    schedule (putAB) C (putAB);
endmodule

(* synthesize *)
module mkTb(Empty);
    Custom_IFC myCustom <- mkCustom();
    rule start;
        myCustom.putAB(12, 13);
    endrule
    rule finish;
        let z = myCustom.getResult();
        $display("Result: %d", z);
    endrule
endmodule
endpackage