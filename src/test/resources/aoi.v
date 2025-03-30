// Verilog code for AND-OR-INVERT gate
module aoi (input A, B, C, D, output F);
  assign F = ~((A & B) | (C & D));
endmodule
