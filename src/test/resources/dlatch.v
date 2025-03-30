module d_latch (  input d,
                  input en,
                  input rstn,
                  output reg q);

   always @ (en or rstn or d) begin
      if (!rstn) begin
         q <= 0;
      end else begin
         if (en) begin
            q <= d;
		 end
	  end
   end
endmodule
