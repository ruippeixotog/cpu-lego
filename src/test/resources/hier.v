// Small hierarchical design for naming tests: a submodule instance with a visible
// internal net, to verify submodule instances become named subcomponents and
// visible netnames become named ports/buses in the Index.
module submod(in_a, in_b, out_y);
  input in_a;
  input in_b;
  output out_y;
  wire internal_net;
  wire internal_net2;
  assign internal_net = in_a & in_b;
  assign internal_net2 = in_a | in_b;
  assign out_y = internal_net ^ internal_net2;
endmodule

module topmod(a, b, y);
  input a;
  input b;
  output y;
  wire mid;
  submod u1(.in_a(a), .in_b(b), .out_y(mid));
  assign y = mid;
endmodule
