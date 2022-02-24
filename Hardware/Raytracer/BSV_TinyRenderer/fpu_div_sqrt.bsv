////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2011  Bluespec, Inc.   ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//see LICENSE.iitm
////////////////////////////////////////////////////////////////////////////////
/*
---------------------------------------------------------------------------------------------------

Author: Neel Gala, Shalender Kumar, Sujay Pandit, Lokhesh Kumar
Email id: neelgala@gmail.com, cs18m050@smail.iitm.ac.in, contact.sujaypandit@gmail.com, lokhesh.kumar@gmail.com
--------------------------------------------------------------------------------------------------
*/
////////////////////////////////////////////////////////////////////////////////
//  Filename      : fpu_div_sqrt.bsv
////////////////////////////////////////////////////////////////////////////////
package fpu_div_sqrt;
import fpu_common    ::*;
import Vector            ::*;
import Real              ::*;
import BUtils            ::*;
import DefaultValue      ::*;
import FShow             ::*;
import GetPut            ::*;
import ClientServer      ::*;
import FIFO              ::*;
import FixedPoint        ::*;
import DReg              ::*;
  `include "Logger.bsv"
///////////////////////////////////////////////////////////////



////////////////////////////////////////////////////////////////////////////////
/// Square Root
////////////////////////////////////////////////////////////////////////////////
interface Ifc_sqrt_fp#(numeric type e, numeric type m);
   method Action send(Tuple2#(FloatingPoint#(e,m), RoundMode) operands);
//   method Tuple3#(Bit#(1),FloatingPoint#(e,m),Exception) receive(); 
	method ReturnType#(e,m) receive(); 
endinterface
module mk_sqrt_fp(Ifc_sqrt_fp#(e,m))
provisos(
   Add#(a__, 2, TMul#(TAdd#(TDiv#(m, 2), 3), 2)),
   Log#(TAdd#(1, TMul#(TAdd#(TDiv#(m, 2), 3), 2)),TLog#(TAdd#(TMul#(TAdd#(TDiv#(m, 2), 3), 2), 1))),
   Add#(1, b__, TMul#(TAdd#(TDiv#(m, 2), 3), 2)),
   Add#(m, c__, TMul#(TAdd#(TDiv#(m, 2), 3), 2)),
   Add#(d__, TLog#(TAdd#(1, TMul#(TAdd#(TDiv#(m, 2), 3), 2))), TAdd#(e, 1)),
   Add#(e__, TMul#(TAdd#(TDiv#(m, 2), 3), 2), TMul#(2, TMul#(TAdd#(TDiv#(m,
    2), 3), 2))),
   Log#(TAdd#(1, TMul#(2, TMul#(TAdd#(TDiv#(m, 2), 3), 2))),
    TLog#(TAdd#(TMul#(2, TMul#(TAdd#(TDiv#(m, 2), 3), 2)), 1))),
   Add#(f__, 2, TMul#(2, TMul#(TAdd#(TDiv#(m, 2), 3), 2))),
   Add#(g__, TAdd#(TMul#(TAdd#(TDiv#(m, 2), 3), 2), 1), TMul#(2,
    TMul#(TAdd#(TDiv#(m, 2), 3), 2))),
   Add#(m, h__, TAdd#(TMul#(TAdd#(TDiv#(m, 2), 3), 2), 1)),
   Add#(i__, TLog#(TAdd#(1, TAdd#(TMul#(TAdd#(TDiv#(m, 2), 3), 2), 1))),
    TAdd#(e, 1)),
   Add#(j__, TLog#(TAdd#(TMul#(TAdd#(TDiv#(m, 2), 3), 2), 1)), e),
   Add#(k__, TLog#(TAdd#(TMul#(TAdd#(TDiv#(m, 2), 3), 2), 1)), TAdd#(e, 2)),
   Add#(l__, TAdd#(TMul#(TAdd#(TDiv#(m, 2), 3), 2), 1),
    TMul#(TMul#(TAdd#(TDiv#(m, 2), 3), 2), 2)),
   Log#(TAdd#(1, TMul#(TMul#(TAdd#(TDiv#(m, 2), 3), 2), 2)),
    TLog#(TAdd#(TMul#(TMul#(TAdd#(TDiv#(m, 2), 3), 2), 2), 1))),
   Add#(m__, 2, TMul#(TMul#(TAdd#(TDiv#(m, 2), 3), 2), 2)),



	Div#(m,2,mh),
	Add#(mh,3,mh1),
	Mul#(mh1,2,nsfd),

	Mul#(nsfd,2,nsfd1),
	Add#(n__, TLog#(TAdd#(1, nsfd)), TAdd#(e, 2)),
	Add#(o__, TAdd#(nsfd, 1), TMul#(nsfd, 2)),
	Add#(m, p__, TAdd#(nsfd, 1)),
	Add#(q__, TLog#(TAdd#(1, TAdd#(nsfd, 1))), TAdd#(e, 1)),


	Add#(r__, TAdd#(nsfd, 1), nsfd1)
//	Bits#(UInt#(m), m1)
//	Add#(n__, TAdd#(nsfd, 1), TMul#(nsfd, 2)),
//	Add#(o__, TLog#(TAdd#(1, TAdd#(nsfd, 1))), TAdd#(e, 1)),
//	Add#(m1, p__, TAdd#(nsfd, 1)),
//	Add#(q__, TLog#(TAdd#(1, nsfd)), TAdd#(e, 2))
);
   Reg#(Tuple3#(Bit#(1),FloatingPoint#(e,m),Exception)) rg_result <- mkDReg(tuple3(unpack(0),unpack(0),unpack(0)));
   Reg#(Tuple2#(UInt#(nsfd1),Bool)) rg_result1 <- mkReg(tuple2(unpack(0),unpack(0)));
   Reg#(Tuple2#(FloatingPoint#(e,m), RoundMode)) rg_operands <- mkReg(tuple2(unpack(0),unpack(0)));

   Reg#(Bit#(nsfd1)) rg_s <- mkReg(0);
   Reg#(Bit#(nsfd1)) rg_r <- mkReg(0);
   Reg#(Bit#(nsfd1)) rg_b <- mkReg(0);
   Reg#(Bit#(nsfd1)) rg_sum <- mkReg(0);
   Reg#(Bit#(nsfd1)) rg_sum1<- mkReg(0);
   Reg#(Maybe#(Bit#(nsfd1))) rg_res <- mkReg(tagged Invalid);
   Reg#(Bit#(nsfd1)) rg_opsfd <- mkReg(0);

   Reg#(Bit#(2)) rg_en <- mkReg(0);
   Reg#(Bool) rg_en1 <- mkReg(False);
   Reg#(UInt#(TLog#(nsfd1)))rg_count <- mkReg(0);
   Reg#(UInt#(TLog#(TAdd#(1, nsfd1)))) rg_shift <- mkReg(0);
   Reg#(Bool) rg_valid <-mkDReg(False);

	Reg#(Maybe#(FloatingPoint#(e,m))) rg_final_result <- mkReg(tagged Invalid);
	Reg#(FloatingPoint#(e,m)) rg_final_out <- mkReg(unpack(0));
	Reg#(Exception) rg_final_exc <- mkReg(defaultValue);
	Reg#(Bool) rg_cmp <- mkReg(False);


//*************************************************** rules ********************************************************************************

rule rl_initial1(rg_en == 2);
//	`logLevel( tb, 0, $format("Initial1 rule %d",rg_count))
	rg_en <= 3;
	Bit#(nsfd1) b=zExtendLSB(2'b01);
	Bit#(nsfd1) s = pack(rg_opsfd);

	let s0 = countZerosMSB(s);
	let b0 = countZerosMSB(b);
	if (s0 > 0) 
	begin
		let shift = (s0 - b0);
		if ((shift & 1) == 1)
		shift = shift + 1;
		b = b >> shift;
	end

	rg_r <= 0;
	rg_b <= b;
	rg_s <= s;
endrule:rl_initial1


rule rl_loop0(rg_en == 3 && rg_count <= fromInteger(valueOf(nsfd1)) && !rg_en1);
//	`logLevel( tb, 0, $format("Loop0 rule %d",rg_count))
	rg_en1 <= True;

	let sum = rg_r + rg_b;
	rg_sum <= sum;

	rg_cmp <= (rg_s >= sum);
endrule:rl_loop0

rule rl_loop1(rg_en == 3 && rg_count <= fromInteger(valueOf(nsfd1)) && !rg_en1);
//	`logLevel( tb, 0, $format("Loop1 rule %d",rg_count))
	let r = rg_r;
	r = r >> 1;
	let sum = r + rg_b;
	rg_sum1 <= sum;
endrule:rl_loop1

rule rl_loop(rg_en == 3 && rg_count <= fromInteger(valueOf(nsfd1)) && rg_en1);
	rg_en1 <= False;
//	`logLevel( tb, 0, $format("Loop rule %d",rg_count))
  rg_count <= rg_count + 2;
  Maybe#(Bit#(nsfd1)) res = tagged Invalid;
  Bit#(nsfd1) s;
  Bit#(nsfd1) r;
  Bit#(nsfd1) b=0;

		r = rg_r;
		s = rg_s;
		b = rg_b;
		res = rg_res;

//		if (b == 0) 
//		begin
			res = tagged Valid r;
//		end
//		else 
		if(b!=0)
		begin
//			let sum = r + b;
			let sum = rg_sum;
//			if (s >= sum) 
			r = r >> 1;
			if(rg_cmp)
			begin
				s = s - sum;
//				r = (r >> 1) + b;
//				r = r + b;
				r = rg_sum1;
			end
/*			else 
			begin
				r = r >> 1;
			end*/
			b = b >> 2;
		end
		rg_b <= b;
		rg_r <= r;
		rg_s <= s;
		rg_res <= res;		
endrule :rl_loop

	



rule rl_initial(rg_en == 1);
//	`logLevel( tb, 0, $format("Initial rule %d",rg_count))
	rg_en <= 2;
      	match {.op, .rmode} = rg_operands;
	Exception exc = defaultValue;
      	Maybe#(FloatingPoint#(e,m)) result = tagged Invalid;
	let out = op;
	Bit#(nsfd) sfd = zExtendLSB({1'b0, getHiddenBit(op), op.sfd});
      
      if (isSNaN(op)) begin
	      exc.invalid_op = True;
	      result = tagged Valid nanQuiet(op);
      end
      else if (isQNaN(op) || isZero(op) || (isInfinity(op) && (op.sign == False))) begin
	      result = tagged Valid op;
      end
      else if (op.sign) begin
	      exc.invalid_op = True;
	      result = tagged Valid qnan();
      end
      else begin
	      Int#(TAdd#(e,2)) exp = isSubNormal(op) ? fromInteger(minexp(op)) : signExtend(unpack(unbias(out)));

	      let zeros = countZerosMSB(sfd);
	      sfd = sfd << (zeros - 1);
	      exp = exp - zeroExtend(unpack(pack(zeros)));

	      out.exp = truncate(pack(exp >> 1) + fromInteger(bias(out)) + 1);

	      if ((exp & 1) == 0) begin
	        sfd = sfd >> 1;
	      end
      end
      
      UInt#(TMul#(nsfd,2)) s=0;
      Bit#(TMul#(nsfd,2)) opsfd = zExtendLSB(sfd);
      Bool inexact = False;
      
	if (result matches tagged Invalid)
	begin
//		`logLevel( tb, 0, $format("ifpart"))

		rg_opsfd <= opsfd;
	end
	rg_final_result <= result;
	rg_final_out <= out;
	rg_final_exc <= exc;

endrule:rl_initial
//----------------------------------------------------------------------------------------------------------------------------------------------------------
rule rl_final(rg_count > fromInteger(valueOf(nsfd1)));
//	`logLevel( tb, 0, $format("Final rule %d",rg_count))
	rg_count <= 0;
	rg_en <= 0;

      	Bit#(TAdd#(nsfd,1)) sfd1 = ?;
	
	let result = rg_final_result;
	let out = rg_final_out;
	let exc = rg_final_exc;
	let rmode = tpl_2(rg_operands);

	if (result matches tagged Invalid)
	begin
		let {s, inexact} = tuple2((fromMaybe(0,rg_res)),(rg_s != 0));

		sfd1 = truncate(pack(s));
		if (inexact) 
		begin
		        sfd1[0] = 1;
		end
	end
     
      	Bit#(2) guard = ?;
      	if (result matches tagged Invalid) begin
	      match {.out_, .guard_, .exc_} = normalize(out, sfd1);
	      out = out_;
	      guard = guard_;
	      exc = exc_;
      	end
     
      	if (result matches tagged Valid .x) 
	begin
	      out = x;
      	end
      	else begin
	      match {.out_, .exc_} = round(rmode, out, guard);
	      out = out_;
	      exc = exc | exc_;
      	end
	rg_result <= tuple3(1'b1, canonicalize(out), exc);


endrule:rl_final
//******************************************************************************************************************************************

	method Action send(Tuple2#(FloatingPoint#(e,m), RoundMode) operands);
//		`logLevel( tb, 0, $format("Send method %d",rg_count))
		rg_en <= 1;
		rg_count <= 0;
		rg_operands <= operands;
	endmethod

	method ReturnType#(e,m) receive(); 
	let x = ReturnType{valid:tpl_1(rg_result),value:tpl_2(rg_result),ex:tpl_3(rg_result)};
	return x;
   endmethod
endmodule

module mk_sqrt_sp_instance(Ifc_sqrt_fp#(8,23));
   let ifc();
   mk_sqrt_fp _temp(ifc);
   return (ifc);
endmodule
module mk_sqrt_dp_instance(Ifc_sqrt_fp#(11,52));
   let ifc();
   mk_sqrt_fp _temp(ifc);
   return (ifc);
endmodule


/*
module tb_squareroot();
   Ifc_sqrt_fp#(8,23) ifc <- mk_sqrt_fp();
   Ifc_sqrt_fp#(11,52) ifc1 <- mk_sqrt_fp();
   Reg#(int) cycle <- mkReg(0);
   rule count_cycle;
   cycle <= cycle + 1;
   if(cycle>120)
   begin
      $finish(0);
   end
   endrule

/*  rule rl_send_1 (cycle==0);
	Bit#(32) a = 'h00EFFDFF;
        FloatingPoint#(8,23) op2 = FloatingPoint {
           sign:       False,
           exp:        a[30:23],
           sfd:        a[22:0]
        };

     RoundMode op4 = Rnd_Nearest_Even;
     ifc.send(tuple2(op2,op4));
   endrule   
  rule receive;
//     match {.valid,.out,.exc} = ifc.receive();
	let x = ifc.receive();
	if(x.valid == 'b1)
     $display("%d : Valid %h Value %h Exc %h",cycle,x.valid,x.value,x.ex);
  endrule



  rule rl_send_2 (cycle==0);
	Bit#(64) a = 'hC34352AF1FFDF779;
        FloatingPoint#(11,52) op2 = FloatingPoint {
           sign:       unpack(a[63]),
           exp:        a[62:52],
           sfd:        a[51:0]
        };

     RoundMode op4 = Rnd_Nearest_Even;
     ifc1.send(tuple2(op2,op4));
   endrule   
  rule receive1;
//     match {.valid,.out,.exc} = ifc.receive();
	let x = ifc1.receive();
	if(x.valid == 'b1)
     $display("%d : Valid %h Value %h Exc %h",cycle,x.valid,x.value,x.ex);
  endrule



endmodule*/

////////////////////////////////////////////////////////////////////////////////
/// Divide
////////////////////////////////////////////////////////////////////////////////
interface Ifc_divider_fp#(numeric type e, numeric type m);
   method Action send(Tuple3#(FloatingPoint#(e,m),FloatingPoint#(e,m), RoundMode) operands);
	method ReturnType#(e,m) receive(); 
endinterface
module mk_divider_fp(Ifc_divider_fp#(e,m))
provisos(
   Add#(a__, TLog#(TAdd#(1, TAdd#(TAdd#(m, 5), 1))), TAdd#(e, 1)),
   Add#(b__, TLog#(TAdd#(1, TAdd#(m, 1))), TAdd#(TAdd#(e, 1), 1)),


	Add#(e,1,ebits),
	Add#(ebits,1,ebits1),
	Add#(m,1,m1bits),
	Add#(m,5,dbits),
	Add#(dbits,1,dbits1),
	Add#(dbits,dbits,nbits),
      // per bsc request
	Mul#(2, dbits, nbits),
//	Add#(1, nbits, TAdd#(dbits, a__))
//     	Add#(m, b__, dbits1),
//     	Add#(c__, TLog#(TAdd#(1, dbits1)), TAdd#(e, 1)),
//     	Add#(d__, e, ebits1),
//     	Add#(e__, TLog#(TAdd#(1, m1bits)), ebits1),
//     	Add#(2, f__, dbits),
//     	Add#(2, g__, dbits1)

      // per request of bsc
//	Add#(b__, dbits, nbits)
//     	Add#(1, nbits, TAdd#(dbits, a__))



	Add#(c__, TLog#(TAdd#(1, m1bits)), ebits1),
	Add#(d__, e, ebits1),
	Add#(m, e__, dbits1),
	Add#(f__, TLog#(TAdd#(1, dbits1)), TAdd#(e, 1)),
	Add#(2, g__, dbits1),
	Add#(1, nbits, TAdd#(dbits, h__)),
	Add#(i__, TAdd#(1, dbits), dbits1)

);



   Reg#(Tuple3#(Bit#(1),FloatingPoint#(e,m),Exception)) rg_result <- mkDReg(tuple3(unpack(0),unpack(0),unpack(0)));
   Reg#(Tuple3#(FloatingPoint#(e,m), FloatingPoint#(e,m), RoundMode)) rg_operands <- mkReg(tuple3(unpack(0),unpack(0),unpack(0)));
   Reg#(UInt#(TLog#(TAdd#(dbits,1)))) rg_count <-mkReg(0);
   Reg#(Int#(TAdd#(1,dbits))) rg_q<- mkReg(0);
   Reg#(Int#(TAdd#(1,dbits))) rg_d <- mkReg(0);
   Reg#(Int#(TAdd#(2,nbits))) rg_r <-mkReg(0);
   Reg#(Int#(TAdd#(2,nbits))) rg_bigd <- mkReg(0);
   Reg#(Bit#(2)) rg_en <- mkReg(0);


	Reg#(Maybe#(FloatingPoint#(e,m))) rg_final_out <- mkReg(tagged Invalid);
	Reg#(Exception) rg_final_exc <- mkReg(defaultValue);
	Reg#(FloatingPoint#(e,m)) rg_final_result <- mkReg(defaultValue);
	Reg#(Bit#(e)) rg_final_shift <- mkReg(0);
	Reg#(Bit#(2)) rg_en1 <- mkReg(0);
	Reg#(Bool) rg_en2 <- mkReg(False);
	

	Reg#(Bit#(m1bits)) rg_sfdA <- mkReg(0);
	Reg#(Bit#(m1bits)) rg_sfdB <- mkReg(0);
	Reg#(Int#(ebits1)) rg_newexp <- mkReg(0);

	Reg#(Bit#(nbits))  rg_opA <- mkReg(0);
	Reg#(Bit#(dbits))  rg_opB <- mkReg(0);

	Reg#(UInt#(TLog#(TAdd#(1, m1bits)))) rg_zerosA <- mkReg(0);
	Reg#(UInt#(TLog#(TAdd#(1, m1bits)))) rg_zerosB <- mkReg(0);

	Reg#(Bit#(dbits1)) rg_final_rsfd <- mkReg(0);


//******************************************************* Rules **************************************************************************
rule rl_initial0(rg_en1 == 1);
	rg_en1 <= 2;
      	match {.in1, .in2, .rmode} = rg_operands;
      	Bit#(m1bits) sfdA = {getHiddenBit(in1), in1.sfd};
      	Bit#(m1bits) sfdB = {getHiddenBit(in2), in2.sfd};


     	let zerosA = countZerosMSB(sfdA);
     	let zerosB = countZerosMSB(sfdB);

      	sfdA = sfdA << zerosA;
      	sfdB = sfdB << zerosB;

	rg_zerosA <= zerosA;
	rg_zerosB <= zerosB;


	rg_sfdA <= sfdA;
	rg_sfdB <= sfdB;
	
endrule:rl_initial0



rule rl_initial1(rg_en1 == 2);
//	`logLevel( tb, 0, $format("Initial0 rule , %d",rg_count))
	rg_en1 <= 3;
      	match {.in1, .in2, .rmode} = rg_operands;
//      	Bit#(m1bits) sfdA = {getHiddenBit(in1), in1.sfd};
//      	Bit#(m1bits) sfdB = {getHiddenBit(in2), in2.sfd};

//     		let zerosA = countZerosMSB(sfdA);
//      	sfdA = sfdA << zerosA;

//      	let zerosB = countZerosMSB(sfdB);
//      	sfdB = sfdB << zerosB;

	let zerosA = rg_zerosA;
	let zerosB = rg_zerosB;
	let sfdA = rg_sfdA;
	let sfdB = rg_sfdB;
      Int#(ebits1) exp1 = isSubNormal(in1) ? fromInteger(minexp(in1)) : signExtend(unpack(unbias(in1)));
      Int#(ebits1) exp2 = isSubNormal(in2) ? fromInteger(minexp(in2)) : signExtend(unpack(unbias(in2)));
      Int#(ebits1) newexp = (exp1 - zeroExtend(unpack(pack(zerosA)))) - (exp2 - zeroExtend(unpack(pack(zerosB))));
	

	Bit#(nbits) opA = zExtendLSB({ 1'b0, sfdA });
      	Bit#(dbits) opB = zExtend({ sfdB, 4'b0000 });
//	rg_sfdA <= sfdA;
//	rg_sfdB <= sfdB;
	rg_opA <= opA;
	rg_opB <= opB;
	rg_newexp <= newexp;
	
endrule:rl_initial1

//function Tuple2#(FloatingPoint#(e,m),Exception) fn_divider_fp(Tuple3#(FloatingPoint#(e,m), FloatingPoint#(e,m), RoundMode) inputs)
rule rl_initial(rg_en == 1 && rg_en1 == 3);
//	`logLevel( tb, 0, $format("Initial rule , %d",rg_count))
//	rg_en1 <= 0;
	rg_en <= 2;

      match {.in1, .in2, .rmode} = rg_operands;
      Maybe#(FloatingPoint#(e,m)) out = tagged Invalid;
      Exception exc = defaultValue;
      FloatingPoint#(e,m) result = defaultValue;
//      Bit#(m1bits) sfdA = {getHiddenBit(in1), in1.sfd};
//      Bit#(m1bits) sfdB = {getHiddenBit(in2), in2.sfd};
      Bit#(e) shift = 0;

//      let zerosA = countZerosMSB(sfdA);
//      sfdA = sfdA << zerosA;

//      let zerosB = countZerosMSB(sfdB);
//      sfdB = sfdB << zerosB;

      // calculate the new exponent
//      Int#(ebits1) exp1 = isSubNormal(in1) ? fromInteger(minexp(in1)) : signExtend(unpack(unbias(in1)));
//      Int#(ebits1) exp2 = isSubNormal(in2) ? fromInteger(minexp(in2)) : signExtend(unpack(unbias(in2)));
//      Int#(ebits1) newexp = (exp1 - zeroExtend(unpack(pack(zerosA)))) - (exp2 - zeroExtend(unpack(pack(zerosB))));


	let sfdA = rg_sfdA;
	let sfdB = rg_sfdB;
	let newexp = rg_newexp;	

//      Bit#(nbits) opA = zExtendLSB({ 1'b0, sfdA });
//      Bit#(dbits) opB = zExtend({ sfdB, 4'b0000 });
	let opA = rg_opA;
	let opB = rg_opB;

      // calculate the sign
      result.sign = in1.sign != in2.sign;

      	if (isSNaN(in1)) 
	begin
	 	out = tagged Valid nanQuiet(in1);
	 	exc.invalid_op = True;
      	end
      	else if (isSNaN(in2)) 
	begin
	 	out = tagged Valid nanQuiet(in2);
	 	exc.invalid_op = True;
      	end
      	else if (isQNaN(in1)) 
	begin
	 	out = tagged Valid in1;
      	end
      	else if (isQNaN(in2)) 
	begin
	 	out = tagged Valid in2;
      	end
      	else if ((isInfinity(in1) && isInfinity(in2)) || (isZero(in1) && isZero(in2))) 
	begin
	 	out = tagged Valid qnan();
	 	exc.invalid_op = True;
      	end
      	else if (isZero(in2) && !isInfinity(in1)) 
	begin
	 	out = tagged Valid infinity(result.sign);
	 	exc.divide_0 = True;
      	end
      	else if (isInfinity(in1)) 
	begin
	 	out = tagged Valid infinity(result.sign);
      	end
      	else if (isZero(in1) || isInfinity(in2)) 
	begin
	 	out = tagged Valid zero(result.sign);
      	end
      	else if (newexp > fromInteger(maxexp(in1)+1)) 
	begin
	 	result.exp = maxBound - 1;
	 	result.sfd = maxBound;
	
	 	exc.overflow = True;
	 	exc.inexact = True;

	 	let y = round(rmode, result, '1);
	 	out = tagged Valid tpl_1(y);
	 	exc = exc | tpl_2(y);
      	end
      	else if (newexp < (fromInteger(minexp_subnormal(in1))-2)) 
	begin
		result.exp = 0;
	 	result.sfd = 0;

	 	exc.underflow = True;
	 	exc.inexact = True;

	 	let y = round(rmode, result, 'b01);
	 	out = tagged Valid tpl_1(y);
	 	exc = exc | tpl_2(y);
      	end
      	else if (newexp < fromInteger(minexp(result))) 
	begin
	 	result.exp = 0;
	 	shift = cExtend(fromInteger(minexp(result)) - newexp);
      	end
      	else 
      	begin
	 	result.exp = cExtend(newexp + fromInteger(bias(result)));
      	end
     

	if (out matches tagged Invalid) 
	begin
//		match {.q,.p} = mk_NonPipelinedDiv(tuple2(unpack(opA),unpack(opB)));
//-----------------------------------------------------------------------------------------
		Int#(TAdd#(2,nbits)) temp_r = cExtend(opA);
		rg_r <= temp_r;

		Int#(TAdd#(1,dbits)) temp_d = cExtend(opB);
		rg_d<= temp_d;
		Int#(TAdd#(2,nbits)) temp_bigd = cExtendLSB(temp_d);
		rg_bigd <= temp_bigd;

//-----------------------------------------------------------------------------------------
	end
	rg_final_out <= out;
	rg_final_exc <= exc;
	rg_final_result <= result;
	rg_final_shift <= shift;
endrule:rl_initial


//function Tuple2#(UInt#(n),UInt#(n)) mk_NonPipelinedDiv (Tuple2#(UInt#(m),UInt#(n)) x)
rule rl_loop(rg_en == 2 && rg_count <= fromInteger(valueOf(dbits)));
//	`logLevel( tb, 0, $format("Loop rule %d",rg_count))
	rg_count <= rg_count + 1;

/*	Int#(TAdd#(1,dbits)) d = cExtend(tpl_2(x));
	Int#(TAdd#(2,nbits)) r = cExtend(tpl_1(x));
	Int#(TAdd#(1,dbits)) q = 0;
	UInt#(TLog#(TAdd#(dbits,1))) rg_index;

	Int#(TAdd#(2,nbits)) bigd = cExtendLSB(d);*/

//	for(rg_index = 0; rg_index <= fromInteger(valueOf(dbits)); rg_index = rg_index + 1) 
//	begin
	let r = rg_r;
	let q = rg_q;
	let bigd = rg_bigd;
		if (r >= 0) 
		begin
		       q = (q << 1) | 1;
		       r = (r << 1) - bigd;
		end
		else 
		begin
		       q = (q << 1);
		       r = (r << 1) + bigd;
		end
	rg_q <= q;
	rg_r <= r;
//	end
/*	q = q + (-(~q));
	if (r < 0) 
	begin
    		q = q - 1;
    		r = r + cExtendLSB(d);
	end*/
//	 UInt#(TAdd#(1,dbits)) qq = unpack(pack(q));
//	 UInt#(TAdd#(1,dbits)) rr = cExtendLSB(r);
//	 rg_int_outputs <= tuple2(truncate(qq),truncate(rr));
//	 return(tuple2(truncate(qq),truncate(rr)));
endrule:rl_loop
//endfunction


rule rl_final1(!rg_en2 && rg_count > fromInteger(valueOf(dbits)));
//	`logLevel( tb, 0, $format("Fianl rule , %d",rg_count))
//	rg_count <= 0;
//	rg_en <= 0;
	rg_en2 <= True;
      	Bit#(dbits1) rsfd = ?;
	let q1 = rg_q;
	let r1 = rg_r;
	let d1 = rg_d;

	let out = rg_final_out;
	let exc = rg_final_exc;
	let result = rg_final_result;
	let shift = rg_final_shift;

	let rmode = tpl_3(rg_operands);


	if (out matches tagged Invalid) 
	begin
		q1 = q1 + (-(~q1));
		if (r1 < 0) 
		begin
	    		q1 = q1 - 1;
	    		r1 = r1 + cExtendLSB(d1);
		end


		UInt#(TAdd#(1,dbits)) qq = unpack(pack(q1));
		UInt#(TAdd#(1,dbits)) rr = cExtendLSB(r1);
		let q = qq;
		let p = rr;

		if (shift < fromInteger(valueOf(dbits1))) 
		begin
			UInt#(dbits1) qdbits1 = extend(q);
			Bit#(1) sfdlsb = |(pack(qdbits1 << (fromInteger(valueOf(dbits1)) - shift)));
			rsfd = cExtend(q >> shift);
			rsfd[0] = rsfd[0] | sfdlsb;
		end
		else 
		begin
			Bit#(1) sfdlsb = |(pack(q));
			rsfd = 0;
			rsfd[0] = sfdlsb;
		end
		if (p != 0) 
		begin
			rsfd[0] = 1;
		end
	end

	rg_final_result <= result;
	rg_final_rsfd <= rsfd;
	rg_final_exc <= exc;
	rg_final_out <= out;
endrule:rl_final1


rule rl_final2(rg_en2 && rg_count > fromInteger(valueOf(dbits)));
	rg_en2 <= False;
	rg_count <= 0;
	rg_en <= 0;



	let result = rg_final_result;
	let exc = rg_final_exc;
	let rmode = tpl_3(rg_operands);
	let out = rg_final_out;
	let rsfd = rg_final_rsfd;


      	Bit#(2) guard = ?;
	if (result.exp == maxBound) 
	begin
//		`logLevel( tb, 0, $format("13"))
		if (truncateLSB(rsfd) == 2'b00) 
		begin
			rsfd = rsfd << 1;
			result.exp = result.exp - 1;
		end
		else 
		begin
			result.exp = maxBound - 1;
			result.sfd = maxBound;

			exc.overflow = True;
			exc.inexact = True;

			let y = round(rmode, result, '1);
			out = tagged Valid tpl_1(y);
			exc = exc | tpl_2(y);
		end
	end

	if (out matches tagged Invalid) 
	begin
//		`logLevel( tb, 0, $format("14"))
		match {.out_, .guard_, .exc_} = normalize(result, rsfd);
		result = out_;
		guard = guard_;
		exc = exc | exc_;
	end

	if (out matches tagged Valid .x)
	begin
//		`logLevel( tb, 0, $format("15"))
		result = x;
	end
	else 
	begin
//		`logLevel( tb, 0, $format("16"))
		match {.out_, .exc_} = round(rmode,result,guard);
		result = out_;
		exc = exc | exc_;
	end
	if(rg_count > fromInteger(valueOf(dbits)))
		rg_result <= tuple3(1,canonicalize(result),exc);
//      return tuple2(canonicalize(result),exc);
endrule:rl_final2
//endfunction
//********************************************************************************************************************************************************
	method Action send(Tuple3#(FloatingPoint#(e,m),FloatingPoint#(e,m), RoundMode) operands);
//	`logLevel( tb, 0, $format("Send method %d",rg_count))
		rg_en <= 1;
		rg_operands <= operands;
		rg_en1 <= 1;
//		$display("send method");
//		match {.lv_out,.lv_exc} = fn_divider_fp(operands);
//		rg_result <= tuple3(1'b1,lv_out,lv_exc);
/*		match {.in1, .in2, .rmode} = operands;
		Bit#(m1bits) sfdA = {getHiddenBit(in1), in1.sfd};
		Bit#(m1bits) sfdB = {getHiddenBit(in2), in2.sfd};
		Bit#(e) shift = 0;

		let zerosA = countZerosMSB(sfdA);
      		sfdA = sfdA << zerosA;

      		let zerosB = countZerosMSB(sfdB);
      		sfdB = sfdB << zerosB;

		Bit#(nbits) opA = zExtendLSB({ 1'b0, sfdA });
		Bit#(dbits) opB = zExtend({ sfdB, 4'b0000 });
	Int#(TAdd#(1,dbits)) d = cExtend(opB);
	rg_d <= d;
	Int#(TAdd#(2,nbits)) r = cExtend(opA);
	Int#(TAdd#(1,dbits)) q = 0;
	Int#(TAdd#(2,nbits)) bigd = cExtendLSB(d);
	rg_q <= q;
	rg_r <= r;
	rg_bigd <= bigd;
	rg_count <= 0;*/
//	`logLevel( tb, 0, $format("inside send method r: %h, d: %h ",r,d))
	endmethod
//   method Tuple3#(Bit#(1),FloatingPoint#(e,m),Exception) receive();
	method ReturnType#(e,m) receive(); 
//		$display("receive method");
		let x = ReturnType{valid:tpl_1(rg_result),value:tpl_2(rg_result),ex:tpl_3(rg_result)};
		return x;
//      return rg_result;
	endmethod
endmodule
module mk_divider_sp_instance(Ifc_divider_fp#(8,23));
   let ifc();
   mk_divider_fp _temp(ifc);
   return (ifc);
endmodule
module mk_divider_dp_instance(Ifc_divider_fp#(11,52));
   let ifc();
   mk_divider_fp _temp(ifc);
   return (ifc);
endmodule

/*
module tb_divider();
   Ifc_divider_fp#(8,23) ifc <- mk_divider_fp();
   Ifc_divider_fp#(11,52) ifc1 <- mk_divider_fp();
   Reg#(int) cycle <- mkReg(0);
   rule count_cycle;
   cycle <= cycle + 1;
   if(cycle>100)
   begin
      $finish(0);
   end
   endrule
	Bit#(32) a = 'h40F00000;
	Bit#(32) b = 'h3FA00000;
  rule rl_send_1 (cycle==0);
        FloatingPoint#(8,23) op2 = FloatingPoint {
           sign:       False,
           exp:        b[30:23],
           sfd:        b[22:0]
        };
  
        FloatingPoint#(8,23) op1 = FloatingPoint {
           sign:       False,
           exp:        a[30:23],
           sfd:        a[22:0]
           };
     RoundMode op4 = Rnd_Nearest_Even;
//     ifc.send(tuple3(op1,op2,op4));
   endrule   
  rule receive;
//     match {.valid,.out, .exc} = ifc.receive();
//	let x = ifc.receive();
//     $display("%d : Valid %h Value %h Exc %h",cycle,x.valid,x.value,x.ex);
  endrule
//*********************************************************
	Bit#(64) a1 = 'h40F00000;
	Bit#(64) b1 = 'h3FA00000;
  rule rl_send_2 (cycle==0);
        FloatingPoint#(11,52) op2 = FloatingPoint {
           sign:       False,
           exp:        b1[62:52],
           sfd:        b1[51:0]
        };
  
        FloatingPoint#(11,52) op1 = FloatingPoint {
           sign:       False,
           exp:        a1[62:52],
           sfd:        a1[51:0]
           };
     RoundMode op4 = Rnd_Nearest_Even;
     ifc1.send(tuple3(op1,op2,op4));
   endrule   
  rule receive1;
//     match {.valid,.out, .exc} = ifc.receive();
	let x = ifc1.receive();
//     $display("%d : Valid %h Value %h Exc %h",cycle,x.valid,x.value,x.ex);
  endrule
//*********************************************************
endmodule*/
endpackage
