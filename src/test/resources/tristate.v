// Verilog test design with a 2-driver tri-state bus and an inout port.
// The tri-state buffers are instantiated directly: Yosys's `opt` rejects two
// behavioral drivers on one net, while `$_TBUF_` cells are its native tri-state
// primitive (several TBUFs driving one net is a normal shared bus).
module tristate(a, b, sel_a, sel_b, bus, y);
  input a;
  input b;
  input sel_a;
  input sel_b;
  inout bus;
  output y;
  $_TBUF_ t1(.A(a), .E(sel_a), .Y(bus));
  $_TBUF_ t2(.A(b), .E(sel_b), .Y(bus));
  assign y = bus;
endmodule
