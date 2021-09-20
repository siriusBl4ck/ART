import FIFO::*;

typedef Bit#(24) DataT;

interface BlockIFC;
    method Action push1 (DataT a);
    method Action push2 (DataT a);
    method ActionValue#(DataT) get();
endinterface

module mkBlock1(BlockIFC);
    Integer fifo_depth = 16;

    FIFO#(DataT) inbound1 <- mkSizedFIFO(fifo_depth);
    FIFO#(DataT) inbound2 <- mkSizedFIFO(fifo_depth);
    FIFO#(DataT) outbound <- mkSizedFIFO(fifo_depth);

    rule enq1;
        DataT in_data = inbound1.first;
        DataT out_data = in_data;
        outbound.enq(out_data);
        inbound1.deq;
    endrule

    rule enq2;
        DataT in_data = inbound1.first;
        DataT out_data = in_data;
        outbound.enq(out_data);
        inbound2.deq;
    endrule

    method Action push1(DataT a);
        inbound1.enq(a);