////////////////////////////////////////////////////////////////////////////////
// Copyright (c) 2011  Bluespec, Inc.   ALL RIGHTS RESERVED.
// $Revision$
// $Date$
////////////////////////////////////////////////////////////////////////////////
//see LICENSE.iitm
////////////////////////////////////////////////////////////////////////////////
/*
---------------------------------------------------------------------------------------------------

Author: Neel Gala, Sujay Pandit, Shalender Kumar, Lokhesh Kumar
Email id: neelgala@gmail.com, contact.sujaypandit@gmail.com, cs18m050@smail.iitm.ac.in, lokhesh.kumar@gmail.com
--------------------------------------------------------------------------------------------------
*/
package fpu_common;

// Notes :
// increasing exp = right shift sfd
// decreasing exp = left shift sfd

////////////////////////////////////////////////////////////////////////////////
/// Imports
////////////////////////////////////////////////////////////////////////////////
import Real              ::*;
import Vector            ::*;
import BUtils            ::*;
import DefaultValue      ::*;
import FShow             ::*;
import GetPut            ::*;
import ClientServer      ::*;
import FIFO              ::*;
import FixedPoint        ::*;
////////////////////////////////////////////////////////////////////////////////
/// Types
////////////////////////////////////////////////////////////////////////////////
typedef struct{
		Bit#(width) final_result;					// the final result for the operation
		Bit#(5) fflags; 					// indicates if any exception is generated.
	}Floating_output#(numeric type width) deriving(Bits, Eq);
typedef struct {
   Bool        sign;
   Bit#(e)     exp;
   Bit#(m)     sfd;
} FloatingPoint#(numeric type e, numeric type m) deriving (Bits);


typedef struct {
   Bit#(1) 	valid;
   FloatingPoint#(e,m)	value;
   Exception  	ex;
} ReturnType#(numeric type e, numeric type m) deriving (Bits,FShow);

typedef struct {
   Bit#(1) 	valid;
   Bit#(width)  value;
   Exception  	ex;
} ReturnTypeInt#(numeric type width) deriving (Bits);


instance DefaultValue#( FloatingPoint#(e,m) );
   defaultValue = FloatingPoint {
      sign:       False,
      exp:        0,
      sfd:        0
      };
endinstance

typedef enum {
   LT,
   EQ,
   GT,
   UO,
   SN
	 } Disorder deriving (Bits, Eq, Bounded);
typedef struct {
		Maybe#(FloatingPoint#(e,m)) res;
		Exception exc;
		RoundMode rmode;
		} CommonState#(numeric type e, numeric type m) deriving (Bits, Eq);
////////////////////////////////////////////////////////////////////////////////
/// Eq
////////////////////////////////////////////////////////////////////////////////
instance FShow#( FloatingPoint#(e,m) );
   function Fmt fshow( FloatingPoint#(e,m) value );
      return $format("<Float %s%x.%x>", value.sign ? "-" : "+", value.exp, value.sfd);
   endfunction
endinstance

instance Bounded#(FloatingPoint#(e,m));
   minBound = FloatingPoint {
      sign: True,
      exp: ('1 - 1),
      sfd: '1
      };
   maxBound = FloatingPoint {
      sign: False,
      exp: ('1 - 1),
      sfd: '1
      };
endinstance

typedef enum {
     Rnd_Nearest_Even		//rnear_even 	Round to nearest/even. 
   , Rnd_Zero			//rminMag 	Round to minimum magnitude (toward zero).
   , Rnd_Minus_Inf		//rmin 		Round to minimum (down).
   , Rnd_Plus_Inf		//rmax 		Round to maximum (up).
   , Rnd_Nearest_Away_Zero	//rnear_maxMag 	Round to nearest/maximum magnitude (nearest/away).
} RoundMode deriving (Bits, Eq);

instance DefaultValue#(RoundMode);
   defaultValue = Rnd_Nearest_Even;
endinstance

instance FShow#( RoundMode );
   function Fmt fshow( RoundMode value );
      case(value)
	 Rnd_Nearest_Even:      return $format("<Round Mode: Nearest Even>");
	 Rnd_Nearest_Away_Zero: return $format("<Round Mode: Nearest Away From Zero>");
	 Rnd_Plus_Inf:          return $format("<Round Mode: +Infinity>");
	 Rnd_Minus_Inf:         return $format("<Round Mode: -Infinity>");
	 Rnd_Zero:              return $format("<Round Mode: Zero>");
      endcase
   endfunction
endinstance

typedef struct {
   Bool invalid_op;
   Bool divide_0;
   Bool overflow;
   Bool underflow;
   Bool inexact;
} Exception deriving (Bits, Eq);

instance DefaultValue#(Exception);
   defaultValue = unpack(0);
endinstance

instance Bitwise#(Exception);
   function Exception \& (Exception x1, Exception x2);
      return unpack(pack(x1) & pack(x2));
   endfunction
   function Exception \| (Exception x1, Exception x2);
      return unpack(pack(x1) | pack(x2));
   endfunction
   function Exception \^ (Exception x1, Exception x2);
      return unpack(pack(x1) ^ pack(x2));
   endfunction
   function Exception \~^ (Exception x1, Exception x2);
      return unpack(pack(x1) ~^ pack(x2));
   endfunction
   function Exception \^~ (Exception x1, Exception x2);
      return unpack(pack(x1) ^~ pack(x2));
   endfunction
   function Exception invert (Exception x1);
      return unpack(~pack(x1));
   endfunction
   function Exception \<< (Exception x1, ix x2)
     provisos(PrimShiftIndex#(ix,dx));
      return error("Bitwise left shift not supported for type " + quote("Exception"));
   endfunction
   function Exception \>> (Exception x1, ix x2)
     provisos(PrimShiftIndex#(ix,dx));
      return error("Bitwise right shift not supported for type " + quote("Exception"));
   endfunction
   function Bit#(1) msb (Exception x);
      return error("Bitwise msb() not supported for type " + quote("Exception"));
   endfunction
   function Bit#(1) lsb (Exception x);
      return error("Bitwise lsb() not supported for type " + quote("Exception"));
   endfunction
endinstance

instance FShow#( Exception );
   function Fmt fshow( Exception value );
      Fmt f = $format("<Exception: ");
      if (value.invalid_op)
	 f = f + $format("Invalid_Op ");
      if (value.divide_0)
	 f = f + $format("Divide_0 ");
      if (value.overflow)
	 f = f + $format("Overflow ");
      if (value.underflow)
	 f = f + $format("Underflow ");
      if (value.inexact)
	 f = f + $format("Inexact ");
      f = f + $format(">");
      return f;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Float Formats
////////////////////////////////////////////////////////////////////////////////
typedef FloatingPoint#(5,10)  Half;
typedef FloatingPoint#(8,23)  Float;
typedef FloatingPoint#(11,32) SingleExtended;
typedef FloatingPoint#(11,52) Double;
typedef FloatingPoint#(15,64) DoubleExtended;

////////////////////////////////////////////////////////////////////////////////
/// Functions
////////////////////////////////////////////////////////////////////////////////
// Zero extend a quantity by padding on the LSB side.
function Bit#(m) zExtendLSB(Bit#(n) value)
   provisos( Add#(n,m,k) );
   Bit#(k) out = { value, 0 };
   return out[valueof(k)-1:valueof(n)];
endfunction

// Zero extend and change type by padding on the LSB side (of bits instance)
function a cExtendLSB(b value)
   provisos( Bits#(a,sa), Bits#(b,sb) );
   let out = unpack(zExtendLSB(pack(value)));
   return out;
endfunction

// Returns the 1-based index (or 0 if not found) of the first 1
// from the MSB down.
function Integer findIndexOneMSB_( Bit#(s) din );
   Vector#(s, Bit#(1)) v = unpack(din);
   Integer result = 0;
   for(Integer i = 0; i < valueof(s); i = i + 1) begin
      if (v[i] == 1) result = i + 1;
   end
   return result;
endfunction

function UInt#(l) findIndexOneMSB( Bit#(s) din )
   provisos( Add#(_, 1, s), Log#(s, logs), Add#(logs,1,l));
   Vector#(s, Bit#(1)) v = unpack(reverseBits(din));
   if (findElem(1'b1, v) matches tagged Valid .ridx) begin
      return fromInteger(valueOf(s)) - cExtend(ridx);
   end
   else begin
      return 0;
   end
endfunction

// Returns the 1-based index (or 0 if not found) of the first 1
// from the LSB up.
function Integer findIndexOneLSB_( Bit#(s) din );
   Vector#(s, Bit#(1)) v = unpack(din);
   Integer result = 0;
   Bool done = False;
   for(Integer i = 0; i < valueof(s); i = i + 1) begin
      if (v[i] == 1)  done = True;
      else if (!done) result = i + 1;
   end
   return (done) ? result : 0;
endfunction

function UInt#(l) findIndexOneLSB( Bit#(s) din )
   provisos( Add#(_, 1, s), Log#(s, logs), Add#(logs,1,l));
   Vector#(s, Bit#(1)) v = unpack(din);
   if (findElem(1'b1, v) matches tagged Valid .ridx) begin
      return cExtend(ridx);
   end
   else begin
      return 0;
   end
endfunction


function FloatingPoint#(e,m) infinity(Bool sign);
   return FloatingPoint {
      sign:     sign,
      exp:      maxBound,
      sfd:      0
      };
endfunction

function Bool isInfinity( FloatingPoint#(e,m) din );
   return ((&din.exp == 1) && (din.sfd == 0));
endfunction

function FloatingPoint#(e,m) qnan();
   return FloatingPoint {
      sign:     False,
      exp:      '1,
      sfd:      reverseBits('b1)
      };
endfunction

function Bool isQNaN( FloatingPoint#(e,m) din );
   return ((&din.exp == 1) && (msb(din.sfd) == 1));
endfunction

function FloatingPoint#(e,m) snan();
   return FloatingPoint {
      sign:     False,
      exp:      '1,
      sfd:      reverseBits('b10)
      };
endfunction

function Bool isSNaN( FloatingPoint#(e,m) din );
   return ((&din.exp == 1) && (|din.sfd == 1) && (msb(din.sfd) == 0));
endfunction

function FloatingPoint#(e,m) nanQuiet(FloatingPoint#(e,m) din);
   din.sfd = din.sfd | reverseBits('b1);
   return din;
endfunction

function FloatingPoint#(e,m) zero(Bool sign);
   return FloatingPoint {
      sign:     sign,
      exp:      0,
      sfd:      0
      };
endfunction

function FloatingPoint#(e,m) one(Bool sign);
   FloatingPoint#(e,m) dummy = ?;
   return FloatingPoint {
      sign:     sign,
      exp:      fromInteger(bias(dummy)),
      sfd:      0
      };
endfunction

function FloatingPoint#(e,m) canonicalize (FloatingPoint#(e,m) in);
  if (in.exp == '1 && in.sfd != 0)
    return FloatingPoint{sign:False, exp:'1, sfd:1<<(valueof(m)-1)};
  else
    return in;
endfunction

function Bool isZero( FloatingPoint#(e,m) din );
   return isSubNormal(din) && (din.sfd == 0);
endfunction

function Bool isOne( FloatingPoint#(e,m) din );
   return (din.sfd == 0) && (din.exp == fromInteger(bias(din)));
endfunction

function Bool isNegativeZero( FloatingPoint#(e,m) din );
   return isZero(din) && (din.sign);
endfunction

function Bool isNaN( FloatingPoint#(e,m) din );
   return isNaNOrInfinity(din) && !isInfinity(din);
endfunction

function Exception ex_flag(FloatingPoint#(e,m) op);
	Bool a0 = isSNaN(op) || isQNaN(op);
	Bool a3 = (op.exp == '0) && (op.sfd == '0);
	Bool a1 = (&op.exp == 1 && |op.sfd == 0);
	Bool a4 = (op.exp == '0 && op.sfd != '0);
	Bool a2 = False;
	let e = Exception{invalid_op: a4, divide_0: a3,  overflow: a2, underflow: a1, inexact: a0};
	return e;
endfunction

function Bool isNaNOrInfinity( FloatingPoint#(e,m) din );
   return (din.exp == '1);
endfunction

function Bool isSubNormal( FloatingPoint#(e,m) din );
   return (din.exp == 0);
endfunction

function Integer bias( FloatingPoint#(e,m) din );
   return (2 ** (valueof(e)-1)) - 1;
endfunction

function Bit#(e) unbias( FloatingPoint#(e,m) din );
   return (din.exp - fromInteger(bias(din)));
endfunction

function Bit#(1) getHiddenBit( FloatingPoint#(e,m) din );
   return (isSubNormal(din)) ? 0 : 1;
endfunction

function Integer minexp( FloatingPoint#(e,m) din );
  return 1-bias(din);
endfunction

function Integer minexp_subnormal( FloatingPoint#(e,m) din );
   return minexp(din)-valueof(m);
endfunction

function Integer maxexp( FloatingPoint#(e,m) din );
   return bias(din);
endfunction

function FloatingPoint#(e,m) rightshift( FloatingPoint#(e,m) din, Bit#(e) amt )
   provisos(  Add#(m, 4, m4)
	    , Add#(m4, m, x)
	    );
   Bit#(x) sfd = cExtendLSB({ getHiddenBit(din), din.sfd });
   Bit#(1) hidden;
   Bit#(m) s;
   Bit#(m) rest;
   { hidden, s, rest } = cExtendLSB(sfd >> amt);
   din.sfd    = s;
   return din;
endfunction

function Tuple2#(FloatingPoint#(e,m),Exception) round( RoundMode rmode, FloatingPoint#(e,m) din, Bit#(2) guard )
   provisos(  Add#(m, 1, m1)
	    , Add#(m, 2, m2)
	    );

   FloatingPoint#(e,m) out = defaultValue;
   Exception exc = defaultValue;

   if (isNaNOrInfinity(din)) begin
      out = din;
   end
   else begin
      let din_inc = din;

      Bit#(TAdd#(m,2)) sfd = unpack({1'b0, getHiddenBit(din), din.sfd}) + 1;

      if (msb(sfd) == 1) begin
	 if (din.exp == fromInteger(maxexp(din) + bias(out))) begin
	    din_inc = infinity(din_inc.sign);
	 end
	 else begin
	    din_inc.exp = din_inc.exp + 1;
	    din_inc.sfd = truncate(sfd >> 1);
	 end
      end
      else if ((din.exp == 0) && (truncateLSB(sfd) == 2'b01)) begin
	 din_inc.exp = 1;
	 din_inc.sfd = truncate(sfd);
      end
      else begin
	 din_inc.sfd = truncate(sfd);
      end

      if (guard != 0) begin
	 exc.inexact = True;
      end

      case(rmode)
	 Rnd_Nearest_Even:
	 begin
	    case (guard)
	       'b00: out = din;
	       'b01: out = din;
	       'b10: out = (lsb(din.sfd) == 1) ? din_inc : din;
	       'b11: out = din_inc;
	    endcase
	 end

	 Rnd_Nearest_Away_Zero:
	 begin
	    case (guard)
	       'b00: out = din;
//	       'b01: out = din_inc;
	       'b01: out = din;
	       'b10: out = din_inc;
	       'b11: out = din_inc;
	    endcase
	 end

	 Rnd_Plus_Inf:
	 begin
	    if (guard == 0)
	       out = din;
	    else if (din.sign)
	       out = din;
	    else
	       out = din_inc;
	 end

	 Rnd_Minus_Inf:
	 begin
	    if (guard == 0)
	       out = din;
	    else if (din.sign)
	       out = din_inc;
	    else
	       out = din;
	 end

	 Rnd_Zero:
	 begin
	    out = din;
	 end
      endcase
   end

   if (isInfinity(out)) begin
      exc.overflow = True;
   end

   return tuple2(out,exc);
endfunction


function Tuple3#(FloatingPoint#(e,m),Bit#(2),Exception) normalize( FloatingPoint#(e,m) din, Bit#(x) sfdin )
   provisos(
      Add#(1, a__, x),
      Add#(m, b__, x),
      // per request of bsc
      Add#(c__, TLog#(TAdd#(1, x)), TAdd#(e, 1))
      );

   FloatingPoint#(e,m) out = din;
   Bit#(2) guard = 0;
   Exception exc = defaultValue;
   Int#(TAdd#(e,1)) exp = isSubNormal(out) ? fromInteger(minexp(out)) : signExtend(unpack(unbias(out)));
   let zeros = countZerosMSB(sfdin);

   if ((zeros == 0) && (exp == fromInteger(maxexp(out)))) begin
      out.exp = maxBound - 1;
      out.sfd = maxBound;
      guard = '1;
      exc.overflow = True;
      exc.inexact = True;
   end
   else begin
      if (zeros == 0) begin
	 // carry, no sfd adjust necessary

	 if (out.exp == 0)
	    out.exp = 2;
	 else
	    out.exp = out.exp + 1;

	 // carry bit
	 sfdin = sfdin << 1;
      end
      else if (zeros == 1) begin
	 // already normalized

	 if (out.exp == 0)
	    out.exp = 1;

	 // carry, hidden bits
	 sfdin = sfdin << 2;
      end
      else if (zeros == fromInteger(valueOf(x))) begin
	 // exactly zero
	 out.exp = 0;
      end
      else begin
	 // try to normalize
	 Int#(TAdd#(e,1)) shift = zeroExtend(unpack(pack(zeros - 1)));
	 Int#(TAdd#(e,1)) maxshift = exp - fromInteger(minexp(out));

`ifdef denormal_support
if (shift > maxshift) begin
	    // result will be subnormal

	    sfdin = sfdin << maxshift;
	    out.exp = 0;
	 end
	 else begin
	    // result will be normal

	    sfdin = sfdin << shift;
	    out.exp = out.exp - truncate(pack(shift));
	 end

 	 // carry, hidden bits
	 sfdin = sfdin << 2;
      end
`else
         if (shift <= maxshift) begin
	    // result will be normal

	    sfdin = sfdin << shift;
       out.exp = out.exp - truncate(pack(shift));
       end
	    sfdin = sfdin << 2;
	 

 	 // carry, hidden bits
	
      end
`endif
      
      out.sfd = unpack(truncateLSB(sfdin));
      sfdin = sfdin << fromInteger(valueOf(m));

      guard[1] = unpack(truncateLSB(sfdin));
      sfdin = sfdin << 1;

      guard[0] = |sfdin;
   end

   if ((out.exp == 0) && (guard != 0))
      exc.underflow = True;

   return tuple3(out,guard,exc);
endfunction

function Tuple2#(FloatingPoint#(e,m),Exception) fract(FloatingPoint#(e,m) din)
   provisos(
      // per request of bsc
      Add#(a__, TLog#(TAdd#(1, TAdd#(1, TAdd#(TAdd#(m, 1), 1)))), TAdd#(e, 1))
      );
   FloatingPoint#(e,m) res = din;
   Exception exc = defaultValue;

   // this routine extracts the fractional portion of a floating point number, i.e.
   //  123.456 would return 0.456.

   // if the value is not a number, provide not a number result
   if (isNaN(din))
      res = din;
   else if (isInfinity(din)) // if the number is infinity, signal NaN
      res = qnan();
   else if (din.exp < fromInteger(bias(din))) // 0 <= quantity < 1
      res = din;
   else begin // all other cases
      Bit#(TAdd#(m,1)) m = { 1'b1, din.sfd };
      m = m << (din.exp - fromInteger(bias(din)) + 1);
      res.exp = fromInteger(bias(din)) - 1;
      let x = normalize(res, { 1'b0, m, 1'b0 });
      res = tpl_1(x);
      exc = tpl_3(x);
      let y = round(defaultValue, res, tpl_2(x));
      res = tpl_1(y);
      exc = tpl_2(y);
   end
   return tuple2(res,exc);
endfunction

////////////////////////////////////////////////////////////////////////////////
/// Real type conversion
////////////////////////////////////////////////////////////////////////////////
instance RealLiteral#( FloatingPoint#(e,m) );
   function FloatingPoint#(e,m) fromReal( Real n );
      FloatingPoint#(e,m) out = defaultValue;
      Bit#(m) sfdm = 0; Bit#(2) rnd = 0; Bit#(53) rest = 0;

      let {s,ma,ex} = decodeReal(n);

      Bit#(53) sfd = s ? fromInteger(ma) : fromInteger(-ma);
      let msbindex = findIndexOneMSB_(sfd);
      let exp      = ex + msbindex - 1;

      if (msbindex == 0) begin
	 out.sign   = !s;
	 out.exp    = 0;
	 out.sfd    = 0;
      end
      else if (exp > maxexp(out)) begin
      	 out = error("Specified Real '" + realToString(n) + "' caused overflow and cannot be represented by the given type 'FloatingPoint#(" + integerToString(valueof(e)) + "," + integerToString(valueof(m)) + ")'.", out);
      end
      else if (exp < minexp_subnormal(out)) begin
      	 out = error("Specified Real '" + realToString(n) + "' caused underflow and cannot be represented by the given type 'FloatingPoint#(" + integerToString(valueof(e)) + "," + integerToString(valueof(m)) + ")'.", out);
      end
      else if (exp < minexp(out)) begin
	 out.sign       = !s;
	 out.exp        = 0;
	 { sfdm, rnd, rest } = unpack(cExtendLSB(sfd) >> (minexp(out) - exp - 1));
	 out.sfd        = sfdm;
      end
      else begin
      	 out.sign       = !s;
      	 Bit#(e) x      = fromInteger(exp) + fromInteger(bias(out));
      	 out.exp        = unpack(x);
	 { sfdm, rnd, rest } = unpack(cExtendLSB(sfd) << (53 - (msbindex - 1)));
	 out.sfd        = sfdm;
      end

      return out;
   endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Literal
////////////////////////////////////////////////////////////////////////////////
instance Literal#( FloatingPoint#(e,m) );
   function FloatingPoint#(e,m) fromInteger( Integer n );
      FloatingPoint#(e,m) out = defaultValue;
      Bool issue_warning = False;
      String warning_msg = "";

      let maxsfd = 2 ** (valueof(m)+1);

      out.sign = n < 0;
      Integer x = (out.sign) ? -n : n;
      Integer exp = 0;

      if (n != 0) begin
	 // determine the initial exponent
	 while(mod(x,2) == 0 ) begin
	    exp = exp + 1;
	    x   = x / 2;
	 end

	 // determineif we have to represent too many bits -- if so, truncate
	 // perhaps warn about the loss of precision
	 if (x > maxsfd) begin
	    while(x > maxsfd) begin
	       exp = exp + 1;
	       x   = x / 2;
	    end

	    Integer s = x * (2 ** exp);
	    s = (out.sign) ? -s : s;

	    warning_msg = "Converting from Literal '" + integerToString(n) + "' to type 'FloatingPoint#(" + integerToString(valueof(e)) + "," + integerToString(valueof(m)) + ")' exceeds the precision offered.  Replacing with " + integerToString(s) + ".";
	    issue_warning = True;
	 end

	 // move the significand into a field with hidden bit explicit.
	 Bit#(TAdd#(m,1)) sx = fromInteger(x);

	 // If the hidden bit is indeed set, we are done.  Convert to float.
	 if (msb(sx) == 1) begin
	    out.exp  = fromInteger(exp + bias(out) + valueof(m));
	    out.sfd  = cExtend(sx);
	 end
	 else begin
	    Bit#(m) mval = cExtend(sx);
	    let msbindex = findIndexOneMSB_(mval);
	    out.exp      = fromInteger(exp + bias(out) + msbindex - 1);
	    out.sfd      = mval << (valueof(m) - (msbindex - 1));
	 end
      end

      return (issue_warning) ? warning(warning_msg,out) : out;
   endfunction

   function Bool inLiteralRange(FloatingPoint#(e,m) a, Integer n);
      return False;
   endfunction
endinstance

instance Ord#(FloatingPoint#(e,m));
   function Bool \< ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
      let c = compare(in1, in2);
      return (c == LT);
   endfunction

   function Bool \<= ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
      let c = compare(in1, in2);
      return (c == LT) || (c == EQ);
   endfunction

   function Bool \> ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
      let c = compare(in1, in2);
      return (c == GT);
   endfunction

   function Bool \>= ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
      let c = compare(in1, in2);
      return (c == GT) || (c == EQ);
   endfunction
//**************************************************************************************************
/*   function FloatingPoint#(e,m) min( FloatingPoint#(e,m) x, FloatingPoint#(e,m) y );
      if (isSNaN(x)) return nanQuiet(x);
      else if (isSNaN(y)) return nanQuiet(y);
      else if (isQNaN(x)) return x;
      else if (isQNaN(y)) return y;
      else begin
	 let signLT = (x.sign && !y.sign);
	 let signEQ = x.sign == y.sign;
	 let expLT  = x.exp < y.exp;
	 let expEQ  = x.exp == y.exp;
	 let manLT  = x.sfd < y.sfd;

	 if (signLT || (signEQ && expLT) || (signEQ && expEQ && manLT)) return x;
	 else return y;
      end
   endfunction*/

function FloatingPoint#(e,m) min( FloatingPoint#(e,m) x, FloatingPoint#(e,m) y );
	let lv_m = valueof(m);
	Bit#(m) z = 0;
	z[lv_m-1] = 1;
	if((isSNaN(x) || isQNaN(x)) && (isSNaN(y) || isQNaN(y))) return FloatingPoint{sign:False,exp:'1,sfd:z};
      else if (isSNaN(x)) return y;
      else if (isSNaN(y)) return x;
      else if (isQNaN(x)) return y;
      else if (isQNaN(y)) return x;
      else begin
	 let signLT = (x.sign && !y.sign);	
	 let signEQ = x.sign == y.sign;		
	 let expLT  = x.exp < y.exp;		
	 let expGT  = x.exp > y.exp;	
	 let expEQ  = x.exp == y.exp;		
	 let manLT  = x.sfd < y.sfd;	
	 let manGT  = x.sfd > y.sfd;	
	 let signXpos  = !x.sign;
	 let signXneg  = x.sign;

	 if (signLT || (signEQ && expLT && signXpos) || (signEQ && expEQ && manLT && signXpos)) return x;
	 else if ((signEQ && expGT && signXneg) || (signEQ && expEQ && manGT && signXneg)) return x;
	 else return y;
      end
endfunction
//*******************************************************************************************************************


//*******************************************************************************************************************
/*
   function FloatingPoint#(e,m) max( FloatingPoint#(e,m) x, FloatingPoint#(e,m) y );
      if (isSNaN(x)) return nanQuiet(x);
      else if (isSNaN(y)) return nanQuiet(y);
      else if (isQNaN(x)) return x;
      else if (isQNaN(y)) return y;
      else begin
	 let signEQ = x.sign == y.sign;
	 let signGT = (!x.sign && y.sign);
	 let expEQ  = x.exp == y.exp;
	 let expGT  = x.exp > y.exp;
	 let manGT  = x.sfd > y.sfd;

	 if (signGT || (signEQ && expGT) || (signEQ && expEQ && manGT)) return x;
	 else return y;
      end
   endfunction*/
function FloatingPoint#(e,m) max( FloatingPoint#(e,m) x, FloatingPoint#(e,m) y );
	let lv_m = valueof(m);
	Bit#(m) z = 0;
	z[lv_m-1] = 1;
	if((isSNaN(x) || isQNaN(x)) && (isSNaN(y) || isQNaN(y))) return FloatingPoint{sign:False,exp:'1,sfd:z};
      else if (isSNaN(x)) return y;
      else if (isSNaN(y)) return x;
      else if (isQNaN(x)) return y;
      else if (isQNaN(y)) return x;
/*      if (isSNaN(x)) return nanQuiet(x);
      else if (isSNaN(y)) return nanQuiet(y);
      else if (isQNaN(x)) return x;
      else if (isQNaN(y)) return y;*/
      else begin
	 let signEQ = x.sign == y.sign;
	 let signGT = (!x.sign && y.sign);
	 let expEQ  = x.exp == y.exp;
	 let expGT  = x.exp > y.exp;
	 let manGT  = x.sfd > y.sfd;
	 let signXpos  = !x.sign;
	 let signXneg  = x.sign;
	 let expLT  = x.exp < y.exp;
	 let manLT  = x.sfd < y.sfd;

	 if (signGT || (signEQ && expGT && signXpos) || (signEQ && expEQ && manGT && signXpos)) return x;
	 else if ((signEQ && expLT && signXneg) || (signEQ && expEQ && manLT && signXneg)) return x;
	 else return y;
      end
endfunction
endinstance

////////////////////////////////////////////////////////////////////////////////
/// Arith (sort of)
/// but some operations are not defined
/// relationships between operations does not always hold due to NaN
////////////////////////////////////////////////////////////////////////////////
instance Arith#(FloatingPoint#(e,m))
   provisos(
      // per request of bsc
      Add#(a__, TLog#(TAdd#(1, TAdd#(TAdd#(m, 1), TAdd#(m, 1)))), TAdd#(e, 1)),
      Add#(b__, TLog#(TAdd#(1, TAdd#(m, 4))), TAdd#(e, 1)),
      Add#(c__, TLog#(TAdd#(1, TAdd#(m, 2))), TAdd#(e, 1)),
      Add#(d__, TLog#(TAdd#(1, TAdd#(TAdd#(m, 5), 1))), TAdd#(e, 1)),
      Add#(e__, TLog#(TAdd#(1, TAdd#(m, 1))), TAdd#(TAdd#(e, 1), 1)),
      Add#(f__, TLog#(TAdd#(1, TAdd#(m, 5))), TAdd#(e, 1))
      );

	function FloatingPoint#(e,m) \+ ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
			return error("The operator " + quote("+") +
		   " is not defined for " + quote("FloatingPoint") + ".");	
   endfunction

   function FloatingPoint#(e,m) \- ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
			return error("The operator " + quote("-") +
		   " is not defined for " + quote("FloatingPoint") + ".");	
   endfunction

   function FloatingPoint#(e,m) negate (FloatingPoint#(e,m) in1 );
      in1.sign = !in1.sign;
      return in1;
   endfunction

   function FloatingPoint#(e,m) \* ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
			return error("The operator " + quote("*") +
		   " is not defined for " + quote("FloatingPoint") + ".");	
   endfunction

   function FloatingPoint#(e,m) \/ ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
			return error("The operator " + quote("/") +
		   " is not defined for " + quote("FloatingPoint") + ".");
   endfunction

   function FloatingPoint#(e,m) \% ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
      return error("The operator " + quote("%") +
		   " is not defined for " + quote("FloatingPoint") + ".");
   endfunction

   function FloatingPoint#(e,m) abs (FloatingPoint#(e,m) in1 );
      in1.sign = False;
      return in1;
   endfunction

   function FloatingPoint#(e,m) signum (FloatingPoint#(e,m) in1 );
      FloatingPoint#(e,m) out = defaultValue;
      out.sign       = in1.sign;
      out.exp        = fromInteger(bias(in1));
      return out;
   endfunction

   function FloatingPoint#(e,m) \** ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
      return error("The operator " + quote("**") +
		   " is not defined for " + quote("FloatingPoint") + ".");
   endfunction

   function FloatingPoint#(e,m) exp_e ( FloatingPoint#(e,m) in1 );
      return error("The operator " + quote("exp_e") +
		   " is not defined for " + quote("FloatingPoint") + ".");
   endfunction

   function FloatingPoint#(e,m) log ( FloatingPoint#(e,m) in1 );
      return error("The operator " + quote("log") +
                  " is not defined for " + quote("FloatingPoint") + ".");
   endfunction

   function FloatingPoint#(e,m) logb ( FloatingPoint#(e,m) in1, FloatingPoint#(e,m) in2 );
      return error("The operator " + quote("logb") +
                  " is not defined for " + quote("FloatingPoint") + ".");
   endfunction

   function FloatingPoint#(e,m) log2 ( FloatingPoint#(e,m) in1 );
      return error("The operator " + quote("log2") +
                  " is not defined for " + quote("FloatingPoint") + ".");
   endfunction

   function FloatingPoint#(e,m) log10 ( FloatingPoint#(e,m) in1 );
      return error("The operator " + quote("log10") +
                  " is not defined for " + quote("FloatingPoint") + ".");
   endfunction
endinstance


////////////////////////////////////////////////////////////////////////////////
/// FixedFloatCVT instance
////////////////////////////////////////////////////////////////////////////////

typeclass FixedFloatCVT#(type tfl, type tfx);
   function Tuple2#(tfl,Exception) vFixedToFloat(tfx fx, UInt#(ln) frac, RoundMode rmode);
   function Tuple2#(tfx,Exception) vFloatToFixed(UInt#(ln) frac, tfl fl, RoundMode rmode);
endtypeclass

instance FixedFloatCVT#(FloatingPoint#(e,m),UInt#(n))
   provisos(
      // per request of bsc
      Add#(1, a__, TMax#(n, TAdd#(m, 2))),
      Add#(m, b__, TMax#(n, TAdd#(3, m))),
      Add#(1, c__, TMax#(n, TAdd#(3, m)))
      );
   function Tuple2#(FloatingPoint#(e,m),Exception) vFixedToFloat(UInt#(n) fx, UInt#(ln) frac, RoundMode rmode);
      FloatingPoint#(e,m) out = defaultValue;
      Exception exc = defaultValue;

      Bit#(TMax#(n,TAdd#(3,m))) sfd = zExtendLSB(pack(fx));
      let shft = countZerosMSB(sfd);
      Int#(TMax#(ln,TMax#(TLog#(TAdd#(1,TMax#(n,TAdd#(3,m)))),TAdd#(1,e)))) exp = fromInteger(valueOf(n)) - zeroExtend(unpack(pack(frac))) - zeroExtend(unpack(pack(shft))) - 1;

      if (fx == 0) begin
	 out = zero(False);
      end
      else if (shft == fromInteger(valueOf(n))) begin
	 out = zero(False);
      end
      else if (exp > fromInteger(maxexp(out))) begin
	 out = infinity(False);
	 exc.overflow = True;
      end
      else if (exp < fromInteger(minexp_subnormal(out))) begin
	 out = zero(False);
	 exc.underflow = True;
      end
      else if (exp < fromInteger(minexp(out))) begin
	 Bit#(2) guard = 0;

	 out.sign = False;
	 out.exp = 0;

	 sfd = sfd << shft;

	 out.sfd = pack(truncateLSB(sfd));
	 sfd = sfd << fromInteger(valueOf(m));

	 guard[1] = truncateLSB(sfd);
	 sfd = sfd << 1;
	 guard[0] = |sfd;

	 let x = round(rmode, out, guard);
	 out = tpl_1(x);
	 exc = tpl_2(x);
      end
      else begin
	 Bit#(2) guard = 0;

	 out.sign = False;

	 Int#(TAdd#(1,e)) x = truncate(exp + fromInteger(bias(out)));
	 out.exp = truncate(pack(x));

	 sfd = sfd << shft;

	 // hidden bit
	 sfd = sfd << 1;

	 out.sfd = pack(truncateLSB(sfd));
	 sfd = sfd << fromInteger(valueOf(m));

	 guard[1] = truncateLSB(sfd);
	 sfd = sfd << 1;
	 guard[0] = |sfd;

	 let y = round(rmode, out, guard);
	 out = tpl_1(y);
	 exc = tpl_2(y);
      end

      return tuple2(out,exc);
   endfunction

   function Tuple2#(UInt#(n),Exception) vFloatToFixed(UInt#(ln) frac, FloatingPoint#(e,m) fl, RoundMode rmode);
      UInt#(n) out = 0;
      Exception exc = defaultValue;

      if (isNaN(fl)) begin
	 out = minBound;
	 exc.invalid_op = True;
      end
      else if (isInfinity(fl)) begin
	 out = maxBound;
	 exc.invalid_op = True;
      end
      else if (fl.sign) begin
	 out = minBound;
	 exc.invalid_op = True;
      end
      else if (isZero(fl)) begin
	 out = 0;
      end
      else begin
	 // todo: does this work for subnormal?

	 UInt#(TAdd#(n,TAdd#(m,2))) sfd = unpack(zExtendLSB({getHiddenBit(fl), fl.sfd}));
	 Int#(TAdd#(TAdd#(TAdd#(e,1),TLog#(m)),ln)) shft = -signExtend(unpack(unbias(fl))) + fromInteger(valueOf(n)) - 1 - zeroExtend(unpack(pack(frac)));

	 if (shft < 0) begin
	    out = maxBound;
	    exc.invalid_op = True;  // overflow signals invalid op
	 end
	 else if (shft > fromInteger(valueOf(n))) begin
	    out = 0;
	    exc.inexact = True;
	 end
	 else begin
	    Bit#(2) guard = 0;

	    sfd = sfd >> shft;

	    out = unpack(truncateLSB(pack(sfd)));
	    sfd = sfd << fromInteger(valueOf(n));

	    guard[1] = truncateLSB(pack(sfd));
	    sfd = sfd << 1;
	    guard[0] = |pack(sfd);

	    if (|guard == 1) begin
	       exc.inexact = True;
	    end

	    Bool inc = False;

	    case (rmode)
	       Rnd_Nearest_Even:
	       case (guard)
		  'b10: inc = (lsb(out) == 'b1);
		  'b11: inc = True;
	       endcase
	       Rnd_Plus_Inf: inc = (guard != 0);
	       Rnd_Minus_Inf: inc = False;
	       Rnd_Zero: inc = False;
	    endcase

	    if (inc) begin
	       if (out == maxBound) begin
		  exc.invalid_op = True;  // overflow signals invalid op
	       end
	       else begin
		  out = out + 1;
	       end
	    end
	 end
      end

      return tuple2(out,exc);
   endfunction
endinstance

instance FixedFloatCVT#(FloatingPoint#(e,m),Int#(n))
   provisos(
      // per request of bsc
      Add#(1, a__, n),
      Add#(1, b__, TMax#(n, TAdd#(3, m))),
      Add#(m, c__, TMax#(n, TAdd#(3, m)))
      );
   function Tuple2#(FloatingPoint#(e,m),Exception) vFixedToFloat(Int#(n) fx, UInt#(ln) frac, RoundMode rmode);
      FloatingPoint#(e,m) out = defaultValue;
      Exception exc = defaultValue;

      Bool sign = unpack(msb(pack(fx)));
      Bit#(TMax#(n,TAdd#(3,m))) sfd = zExtendLSB(pack(sign ? -fx : fx));
      let shft = countZerosMSB(sfd);
      Int#(TMax#(ln,TMax#(TLog#(TAdd#(1,TMax#(n,TAdd#(3,m)))),TAdd#(1,e)))) exp = fromInteger(valueOf(n)) - zeroExtend(unpack(pack(frac))) - zeroExtend(unpack(pack(shft))) - 1;

      if (fx == 0) begin
	 out = zero(False);
      end
      else if (shft == fromInteger(valueOf(n))) begin
	 out = zero(sign);
      end
      else if (exp > fromInteger(maxexp(out))) begin
	 out = infinity(sign);
	 exc.overflow = True;
      end
      else if (exp < fromInteger(minexp_subnormal(out))) begin
	 out = zero(sign);
	 exc.underflow = True;
      end
      else if (exp < fromInteger(minexp(out))) begin
	 Bit#(2) guard = 0;

	 out.sign = sign;
	 out.exp = 0;

	 sfd = sfd << shft;

	 out.sfd = pack(truncateLSB(sfd));
	 sfd = sfd << fromInteger(valueOf(m));

	 guard[1] = truncateLSB(sfd);
	 sfd = sfd << 1;
	 guard[0] = |sfd;

	 let x = round(rmode, out, guard);
	 out = tpl_1(x);
	 exc = tpl_2(x);
      end
      else begin
	 Bit#(2) guard = 0;

	 out.sign = sign;

	 Int#(TAdd#(1,e)) x = truncate(exp + fromInteger(bias(out)));
	 out.exp = truncate(pack(x));

	 sfd = sfd << shft;

	 // hidden bit
	 sfd = sfd << 1;

	 out.sfd = pack(truncateLSB(sfd));
	 sfd = sfd << fromInteger(valueOf(m));

	 guard[1] = truncateLSB(sfd);
	 sfd = sfd << 1;
	 guard[0] = |sfd;

	 let y = round(rmode, out, guard);
	 out = tpl_1(y);
	 exc = tpl_2(y);
      end

      return tuple2(out,exc);
   endfunction

   function Tuple2#(Int#(n),Exception) vFloatToFixed(UInt#(ln) frac, FloatingPoint#(e,m) fl, RoundMode rmode);
      Int#(n) out = 0;
      Exception exc = defaultValue;

      if (isNaN(fl)) begin
	 out = minBound;
	 exc.invalid_op = True;
      end
      else if (isInfinity(fl)) begin
	 out = fl.sign ? minBound : maxBound;
	 exc.invalid_op = True;
      end
      else if (isZero(fl)) begin
	 out = 0;
      end
      else begin
	 // todo: does this work for subnormal?

	 // needs to be large so bits aren't lost before rounding
	 Int#(TAdd#(n,TAdd#(m,2))) sfd = fl.sign ? -unpack(zExtendLSB({1'b0, getHiddenBit(fl), fl.sfd})) : unpack(zExtendLSB({1'b0, getHiddenBit(fl), fl.sfd}));
	 Int#(TAdd#(TAdd#(TAdd#(e,1),TLog#(m)),ln)) shft = -signExtend(unpack(unbias(fl))) + fromInteger(valueOf(n)) - 2 - zeroExtend(unpack(pack(frac)));

	 if (shft == -1) begin
	    Bit#(TAdd#(n,1)) out1;
	    Bit#(2) guard;

	    out1 = truncateLSB(pack(sfd));
	    sfd = sfd << fromInteger(valueOf(n) + 1);

	    guard[1] = truncateLSB(pack(sfd));
	    sfd = sfd << 1;
	    guard[0] = |pack(sfd);

	    Bool inc = False;

	    case (rmode)
	       Rnd_Nearest_Even:
	       case (guard)
		  'b10: inc = (lsb(out1) == 'b1);
		  'b11: inc = True;
	       endcase
	       Rnd_Plus_Inf: inc = (guard != 0);
	       Rnd_Minus_Inf: inc = False;
	       Rnd_Zero: inc = (msb(out1) == 'b1) ? (guard != 0) : False;
	    endcase

	    if (inc) begin
	       out1 = out1 + 1;
	    end

	    if (truncateLSB(pack(out1)) == 2'b11) begin
	       out = unpack(truncate(out1));

	       if (|guard == 1) begin
		  exc.inexact = True;
	       end
	    end
	    else begin
	       out = fl.sign ? minBound : maxBound;
	       exc.invalid_op = True;  // overflow signals invalid op
	    end
	 end
	 else if (shft < 0) begin
	    out = fl.sign ? minBound : maxBound;
	    exc.invalid_op = True;  // overflow signals invalid op
	 end
	 else if (shft > fromInteger(valueOf(n))) begin
	    out = 0;
	    exc.inexact = True;
	 end
	 else begin
	    Bit#(2) guard = 0;

	    sfd = sfd >> shft;

	    out = unpack(truncateLSB(pack(sfd)));
	    sfd = sfd << fromInteger(valueOf(n));

	    guard[1] = truncateLSB(pack(sfd));
	    sfd = sfd << 1;
	    guard[0] = |pack(sfd);

	    if (|guard == 1) begin
	       exc.inexact = True;
	    end

	    Bool inc = False;

	    case (rmode)
	       Rnd_Nearest_Even:
	       case (guard)
		  'b10: inc = (lsb(out) == 'b1);
		  'b11: inc = True;
	       endcase
	       Rnd_Plus_Inf: inc = (guard != 0);
	       Rnd_Minus_Inf: inc = False;
	       Rnd_Zero: inc = (msb(out) == 'b1) ? (guard != 0) : False;
	    endcase

	    if (inc) begin
	       if (out == maxBound) begin
		  exc.invalid_op = True;  // overflow signals invalid op
	       end
	       else begin
		  out = out + 1;
	       end
	    end

	 end
      end

      return tuple2(out,exc);
   endfunction
endinstance

// Note: this is not well tested
instance FixedFloatCVT#(FloatingPoint#(e,m), FixedPoint#(isize,fsize))
   provisos(
      Add#(isize, fsize, n),
      // per request of bsc
      Add#(1, a__, n),
      Add#(1, b__, TMax#(n, TAdd#(3, m))),
      Add#(m, c__, TMax#(n, TAdd#(3, m)))
      );
   function Tuple2#(FloatingPoint#(e,m),Exception) vFixedToFloat(FixedPoint#(isize,fsize) fx, UInt#(ln) frac, RoundMode rmode);
      FloatingPoint#(e,m) out = defaultValue;
      Exception exc = defaultValue;

      Bit#(TMax#(n,TAdd#(3,m))) sfd = zExtendLSB({fx.i,fx.f});
      let shft = countZerosMSB(sfd);
      Int#(TMax#(ln,TMax#(TLog#(TAdd#(1,TMax#(n,TAdd#(3,m)))),TAdd#(1,e)))) exp = fromInteger(valueOf(n)) - zeroExtend(unpack(pack(frac))) - zeroExtend(unpack(pack(shft))) - 1;

      if (sfd == 0) begin
	 out = zero(False);
      end
      else if (shft == fromInteger(valueOf(n))) begin
	 out = zero(False);
      end
      else if (exp > fromInteger(maxexp(out))) begin
	 out = infinity(False);
	 exc.overflow = True;
      end
      else if (exp < fromInteger(minexp_subnormal(out))) begin
	 out = zero(False);
	 exc.underflow = True;
      end
      else if (exp < fromInteger(minexp(out))) begin
	 Bit#(2) guard = 0;

	 out.sign = False;
	 out.exp = 0;

	 sfd = sfd << shft;

	 out.sfd = pack(truncateLSB(sfd));
	 sfd = sfd << fromInteger(valueOf(m));

	 guard[1] = truncateLSB(sfd);
	 sfd = sfd << 1;
	 guard[0] = |sfd;

	 let x = round(rmode, out, guard);
	 out = tpl_1(x);
	 exc = tpl_2(x);
      end
      else begin
	 Bit#(2) guard = 0;

	 out.sign = False;

	 Int#(TAdd#(1,e)) x = truncate(exp + fromInteger(bias(out)));
	 out.exp = truncate(pack(x));

	 sfd = sfd << shft;

	 // hidden bit
	 sfd = sfd << 1;

	 out.sfd = pack(truncateLSB(sfd));
	 sfd = sfd << fromInteger(valueOf(m));

	 guard[1] = truncateLSB(sfd);
	 sfd = sfd << 1;
	 guard[0] = |sfd;

	 let y = round(rmode, out, guard);
	 out = tpl_1(y);
	 exc = tpl_2(y);
      end

      return tuple2(out,exc);
   endfunction

   function Tuple2#(FixedPoint#(isize,fsize),Exception) vFloatToFixed(UInt#(ln) frac, FloatingPoint#(e,m) fl, RoundMode rmode);
      Int#(n) out = 0;
      Exception exc = defaultValue;

      if (isNaN(fl)) begin
	 out = minBound;
	 exc.invalid_op = True;
      end
      else if (isInfinity(fl)) begin
	 out = fl.sign ? minBound : maxBound;
	 exc.invalid_op = True;
      end
      else if (isZero(fl)) begin
	 out = 0;
      end
      else begin
	 // todo: does this work for subnormal?

	 // needs to be large so bits aren't lost before rounding
	 Int#(TAdd#(n,TAdd#(m,2))) sfd = fl.sign ? -unpack(zExtendLSB({1'b0, getHiddenBit(fl), fl.sfd})) : unpack(zExtendLSB({1'b0, getHiddenBit(fl), fl.sfd}));
	 Int#(TAdd#(TAdd#(TAdd#(e,1),TLog#(m)),ln)) shft = -signExtend(unpack(unbias(fl))) + fromInteger(valueOf(n)) - 2 - zeroExtend(unpack(pack(frac)));

	 if (shft == -1) begin
	    Bit#(TAdd#(n,1)) out1;
	    Bit#(2) guard;

	    out1 = truncateLSB(pack(sfd));
	    sfd = sfd << fromInteger(valueOf(n) + 1);

	    guard[1] = truncateLSB(pack(sfd));
	    sfd = sfd << 1;
	    guard[0] = |pack(sfd);

	    Bool inc = False;

	    case (rmode)
	       Rnd_Nearest_Even:
	       case (guard)
		  'b10: inc = (lsb(out1) == 'b1);
		  'b11: inc = True;
	       endcase
	       Rnd_Plus_Inf: inc = (guard != 0);
	       Rnd_Minus_Inf: inc = False;
	       Rnd_Zero: inc = (msb(out1) == 'b1) ? (guard != 0) : False;
	    endcase

	    if (inc) begin
	       out1 = out1 + 1;
	    end

	    if (truncateLSB(pack(out1)) == 2'b11) begin
	       out = unpack(truncate(out1));

	       if (|guard == 1) begin
		  exc.inexact = True;
	       end
	    end
	    else begin
	       out = fl.sign ? minBound : maxBound;
	       exc.invalid_op = True;  // overflow signals invalid op
	    end
	 end
	 else if (shft < 0) begin
	    out = fl.sign ? minBound : maxBound;
	    exc.invalid_op = True;  // overflow signals invalid op
	 end
	 else if (shft > fromInteger(valueOf(n))) begin
	    out = 0;
	    exc.inexact = True;
	 end
	 else begin
	    Bit#(2) guard = 0;

	    sfd = sfd >> shft;

	    out = unpack(truncateLSB(pack(sfd)));
	    sfd = sfd << fromInteger(valueOf(n));

	    guard[1] = truncateLSB(pack(sfd));
	    sfd = sfd << 1;
	    guard[0] = |pack(sfd);

	    if (|guard == 1) begin
	       exc.inexact = True;
	    end

	    Bool inc = False;

	    case (rmode)
	       Rnd_Nearest_Even:
	       case (guard)
		  'b10: inc = (lsb(out) == 'b1);
		  'b11: inc = True;
	       endcase
	       Rnd_Plus_Inf: inc = (guard != 0);
	       Rnd_Minus_Inf: inc = False;
	       Rnd_Zero: inc = (msb(out) == 'b1) ? (guard != 0) : False;
	    endcase

	    if (inc) begin
	       if (out == maxBound) begin
		  exc.invalid_op = True;  // overflow signals invalid op
	       end
	       else begin
		  out = out + 1;
	       end
	    end

	 end
      end

      FixedPoint#(isize,fsize) fx;
      fx.i = truncateLSB(pack(out));
      fx.f = truncate(pack(out));

      return tuple2(fx,exc);
   endfunction
endinstance


endpackage
