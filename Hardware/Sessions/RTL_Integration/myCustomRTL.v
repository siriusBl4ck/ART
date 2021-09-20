module myCustomRTL(
    input [31:0] a,
    input [31:0] b,
    input EN, clk, rst_b,
    output /*reg*/ [31:0] res
);
    assign res = a + b;
    /*
    always @(posedge clk or negedge rst_b) begin
        if (!rst_b) res <= 0;
        else res <= a + b;
    end
    */
endmodule
