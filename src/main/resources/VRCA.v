module VRCA 
    #(
        parameter width = 32
    ) (
        input  [width-1:0] a,
        input  [width-1:0] b,
        input              cin,
        output [width-1:0] s,
        output             cout
    );

    wire [width-1:0] zero;
    wire [width  :0] carry;
    wire [width  :0] sum;

    assign zero  = 0;
    assign carry = {zero, cin};
    assign sum   = a + b + carry;

    assign s    = sum[width-1:0];
    assign cout = sum[width];
endmodule
