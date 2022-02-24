package fpTest;

import fpu_common::*;
import fpu_div_sqrt::*;

(* synthesize *)
module fpTest_tb(Empty);
    Ifc_sqrt_fp shakti_sqrt <- mk_divider_sp_instance(8, 23);
    
    rule send;
        Float a = 1.0;
        Float b = 0.1;
        shakti_sqrt.send(a, Rnd_Zero);
        //Float c = a / b;
        $display(c);
        $finish();
    endrule

    rule recieve;
        Float c = shakti_sqrt.receive();
    endrule
endmodule

endpackage