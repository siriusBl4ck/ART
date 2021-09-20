package rtl_int;
interface Mac_IFC ;
    method Action acc (Int#(16) a, Int#(16) b);
    method Action reset_acc (Int#(16) value);
    method Int#(16) read_y;
endinterface

import "BVI" mymac =
module mkMac (Mac_IFC);
    default_clock clk (clk);
    default_reset reset (rst_b);
    method acc(a, b) enable(EN);
    method reset_acc(clear_value) enable(clear);
    method out read_y();
    schedule (read_y) SB (reset_acc, acc);
    schedule (acc) C (reset_acc);
endmodule

(* synthesize *)
module mkTb(Empty);
    Mac_IFC myMac1 <- mkMac();
endmodule
endpackage