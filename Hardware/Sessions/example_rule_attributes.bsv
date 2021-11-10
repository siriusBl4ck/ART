package example_rule_attributes;

// IfcCounter with read method
interface IfcCounter#(type t);
method t readCounter;
endinterface
// Definition of CounterType

typedef Bit#(16) CounterType;
// Module counter using IfcCounter interface. It never contains 0.
//(* synthesize, reset_prefix = "reset_b", clock_prefix = "counter_clk", always_ready = "readCounter",always_enabled= "readCounter" *)
module counter (IfcCounter#(CounterType));
	// Reg counter gets reset to 1 asynchronously with the RST signal
	Reg#(CounterType) counter <- mkRegA(1);

	//The descending_urgency attribute will indicate the scheduling
	//order for the indicated rules.
	(* descending_urgency = "updateCounter, resetCounter" *) //When conflicting update is given preference
	//(* execution_order = "updateCounter, resetCounter" *) //Here if both the rules are being executed in the same cycle then update is executed first. Here they are never executed in the same cycle so this command has no effect

	(*descending_urgency = "updateCounter, resetCounter2" *)

	// Next rule resets the counter to 1 when it reaches its limit.
	rule resetCounter (counter == 1);
	action
	$display("reset");
	counter <= 1;
	endaction
	endrule


	// Next rule updates the counter.
	rule updateCounter;
	action
	$display("update");
	counter <= counter + 1;
	endaction
	endrule

	rule resetCounter2;
	$display("reset2");
	endrule

	// Method to output the counter’s value
	method CounterType readCounter;
	return counter;
	endmethod
endmodule

(*synthesize*)
module tb(Empty);  //For descending_urgency and execution_order
	IfcCounter#(CounterType) mymod <- counter;
	Reg#(CounterType) a <- mkReg(0);
	Reg#(CounterType) b <- mkReg(0);
	Reg#(int) i <- mkReg(0);

	rule beginn (i<10);
		let z = mymod.readCounter;
		$display("Counter value", z);
		i <= i+1;
	endrule

	rule endd(i==10);
		$finish;
	endrule
endmodule

(*synthesize*)
module tb1(Empty); //For mutually_exclusive and conflict_free
	//IfcCounter#(CounterType) mymod <- counter;
	Reg#(int) a <- mkReg(0);
	Reg#(int) b <- mkReg(0);
	Reg#(int) i <- mkReg(0);
	Reg#(int) a1 <- mkReg(0);

(*mutually_exclusive = "beginn1, beginn2"*)
//(*conflict_free = "beginn1, beginn2"*)
	rule beginn1 (b==0);
		$display("hi1", $time);
		a1 <= a1+1;
		b <= b+2;
	endrule
	
	rule beginn2 (i==0);
		$display("hi2", $time);
		a <= a+1;
		i <= i+1;
		b <= 0;
	endrule
	rule endd(b==2);
		$finish;
	endrule
endmodule
endpackage
