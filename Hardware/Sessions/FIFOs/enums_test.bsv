package tree;

import FIFOF::*;
(*synthesize*)
module mod(Reg#(int));
	Reg#(Maybe#(int)) val <- mkReg(tagged Invalid);
	Reg#(int) i <- mkReg(0);

	FIFOF#(int) f1 <- mkFIFOF;

	rule increaser((i>0) && (i<5));
		val <= tagged Valid(val.Valid + 1);
		i <= i+1;
	endrule

	rule stopper(i==5);
		val <= tagged Invalid;
		i <= 0;
	//	$finish;
	endrule		

	rule readval(i==0); //Implicit condition that fifo has to have atleast one value
		val <= tagged Valid(f1.first());
		f1.deq();
		i <= i+1;
		$display($time, " value: %d %d",f1.first(), f1.notEmpty);
	endrule

	method Action _write(int inp); //Implicit condition to check if full
		f1.enq(inp);
		//$display("hi", $time);
	endmethod

	method int _read if(isValid(val));
		return val.Valid;
	endmethod
endmodule

(*synthesize*)
module mkTb(Empty);
	Reg#(int) mymod <- mod;

	Reg#(int) i <- mkReg(0);

	rule passer(i < 10);
		$display("Passing: %d",10+2*i, $time);
		mymod <= 10+2*i;
		i <= i+1;
	endrule

	rule display;
		let z = mymod;
		$display($time, "XD %d", z);
	endrule
endmodule

endpackage


