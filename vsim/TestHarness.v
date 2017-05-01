// See LICENSE for license details.

`timescale 1 ns / 1 ps

`ifndef VERILATOR
// Simple placeholder
module TestHarness_tb #(
`ifndef AHBTEST
  parameter AHB_TEST = 0,
`else
  parameter AHB_TEST = 1,
`endif
`ifndef VERB
  parameter VERBOSE = 0,
`else
  parameter VERBOSE = 1,
`endif
`ifndef XLEN
  parameter XLen = 64
`else
  parameter XLen = `XLEN
`endif
);
testbench #(.AHB_TEST(AHB_TEST), .VERBOSE(VERBOSE), .XLen(XLen)) tb ();
endmodule

module TestHarness #(
`ifndef AHBTEST
  parameter AHB_TEST = 0,
`else
  parameter AHB_TEST = 1,
`endif
`ifndef VERB
  parameter VERBOSE = 0,
`else
  parameter VERBOSE = 1,
`endif
`ifndef XLEN
  parameter XLen = 64
`else
  parameter XLen = `XLEN
`endif
);
testbench #(.AHB_TEST(AHB_TEST), .VERBOSE(VERBOSE), .XLen(XLen)) tb ();
endmodule


module testbench #(
  parameter AHB_TEST = 0,
  parameter VERBOSE = 0,
  parameter XLen = 32
);
  reg clk = 1;
  reg resetn = 0;
  wire trap;

  always #5 clk = ~clk;

  // Reset initial
  initial begin
    repeat (100) @(posedge clk);
    resetn <= 1;
  end

  // Writting to vcd dump, also the TIMEOUT message
  initial begin
    if ($test$plusargs("vcd")) begin
      $dumpfile("testbench.vcd");
      $dumpvars(0, testbench);
    end
    repeat (1000000) @(posedge clk);
    $display("TIMEOUT");
    $finish;
  end

  wire trace_valid;
  wire [35:0] trace_data;
  integer trace_file;

  // Trace info writting
  initial begin
    if ($test$plusargs("trace")) begin
      trace_file = $fopen("testbench.trace", "w");
      repeat (10) @(posedge clk);
      while (!trap) begin
        @(posedge clk);
        if (trace_valid)
          $fwrite(trace_file, "%x\n", trace_data);
      end
      $fclose(trace_file);
      $display("Finished writing testbench.trace.");
    end
  end

  zscale_wrapper #(
    .AHB_TEST (AHB_TEST),
    .VERBOSE  (VERBOSE),
    .XLen (XLen)
  ) top (
    .clk(clk),
    .resetn(resetn),
    .trap(trap),
    .trace_valid(trace_valid),
    .trace_data(trace_data)
  );
endmodule
`endif

module zscale_wrapper #(
  parameter AHB_TEST = 0,
  parameter VERBOSE = 0,
  parameter XLen = 32
) (
  input clk,
  input resetn,
  output trap,
  output trace_valid,
  output [35:0] trace_data
);
  wire tests_passed;
  reg [31:0] irq;

  always @* begin
    irq = 0;
    // TODO: Another way to trigger interrupts
    //irq[4] = &uut.picorv32_core.count_cycle[12:0];
    //irq[5] = &uut.picorv32_core.count_cycle[15:0];
  end
  
  // Memory <> Zscale
  wire [1:0]  io_mem_imem_htrans;
  wire        io_mem_imem_hmastlock;
  wire [XLen-1:0] io_mem_imem_haddr;
  wire        io_mem_imem_hwrite;
  wire [2:0]  io_mem_imem_hburst;
  wire [2:0]  io_mem_imem_hsize;
  wire [3:0]  io_mem_imem_hprot;
  wire [XLen-1:0] io_mem_imem_hwdata;
  wire [XLen-1:0] io_mem_imem_hrdata;
  wire        io_mem_imem_hready;
  wire        io_mem_imem_hresp;
  wire [1:0]  io_mem_dmem_htrans;
  wire        io_mem_dmem_hmastlock;
  wire [XLen-1:0] io_mem_dmem_haddr;
  wire        io_mem_dmem_hwrite;
  wire [2:0]  io_mem_dmem_hburst;
  wire [2:0]  io_mem_dmem_hsize;
  wire [3:0]  io_mem_dmem_hprot;
  wire [XLen-1:0] io_mem_dmem_hwdata;
  wire [XLen-1:0] io_mem_dmem_hrdata;
  wire        io_mem_dmem_hready;
  wire        io_mem_dmem_hresp;

  ahb_memory #(
    .AHB_TEST (AHB_TEST),
    .VERBOSE  (VERBOSE),
    .XLen (XLen)
  ) mem (
    .clk                    (clk                    ),
    .reset                  (resetn                 ),
    
    .io_mem_imem_htrans     (io_mem_imem_htrans     ),
    .io_mem_imem_hmastlock  (io_mem_imem_hmastlock  ),
    .io_mem_imem_haddr      (io_mem_imem_haddr      ),
    .io_mem_imem_hwrite     (io_mem_imem_hwrite     ),
    .io_mem_imem_hburst     (io_mem_imem_hburst     ),
    .io_mem_imem_hsize      (io_mem_imem_hsize      ),
    .io_mem_imem_hprot      (io_mem_imem_hprot      ),
    .io_mem_imem_hwdata     (io_mem_imem_hwdata     ),
    .io_mem_imem_hrdata     (io_mem_imem_hrdata     ),
    .io_mem_imem_hready     (io_mem_imem_hready     ),
    .io_mem_imem_hresp      (io_mem_imem_hresp      ),
    
    .io_mem_dmem_htrans     (io_mem_dmem_htrans     ),
    .io_mem_dmem_hmastlock  (io_mem_dmem_hmastlock  ),
    .io_mem_dmem_haddr      (io_mem_dmem_haddr      ),
    .io_mem_dmem_hwrite     (io_mem_dmem_hwrite     ),
    .io_mem_dmem_hburst     (io_mem_dmem_hburst     ),
    .io_mem_dmem_hsize      (io_mem_dmem_hsize      ),
    .io_mem_dmem_hprot      (io_mem_dmem_hprot      ),
    .io_mem_dmem_hwdata     (io_mem_dmem_hwdata     ),
    .io_mem_dmem_hrdata     (io_mem_dmem_hrdata     ),
    .io_mem_dmem_hready     (io_mem_dmem_hready     ),
    .io_mem_dmem_hresp      (io_mem_dmem_hresp      ),
    
    .tests_passed           (tests_passed           )
  );

  ExampleTopZscale uut(
    .clock                  (clk                    ),
    .reset                  (~resetn                ),
    
    .io_mem_imem_htrans     (io_mem_imem_htrans     ),
    .io_mem_imem_hmastlock  (io_mem_imem_hmastlock  ),
    .io_mem_imem_haddr      (io_mem_imem_haddr      ),
    .io_mem_imem_hwrite     (io_mem_imem_hwrite     ),
    .io_mem_imem_hburst     (io_mem_imem_hburst     ),
    .io_mem_imem_hsize      (io_mem_imem_hsize      ),
    .io_mem_imem_hprot      (io_mem_imem_hprot      ),
    .io_mem_imem_hwdata     (io_mem_imem_hwdata     ),
    .io_mem_imem_hrdata     (io_mem_imem_hrdata     ),
    .io_mem_imem_hready     (io_mem_imem_hready     ),
    .io_mem_imem_hresp      (io_mem_imem_hresp      ),
    
    .io_mem_dmem_htrans     (io_mem_dmem_htrans     ),
    .io_mem_dmem_hmastlock  (io_mem_dmem_hmastlock  ),
    .io_mem_dmem_haddr      (io_mem_dmem_haddr      ),
    .io_mem_dmem_hwrite     (io_mem_dmem_hwrite     ),
    .io_mem_dmem_hburst     (io_mem_dmem_hburst     ),
    .io_mem_dmem_hsize      (io_mem_dmem_hsize      ),
    .io_mem_dmem_hprot      (io_mem_dmem_hprot      ),
    .io_mem_dmem_hwdata     (io_mem_dmem_hwdata     ),
    .io_mem_dmem_hrdata     (io_mem_dmem_hrdata     ),
    .io_mem_dmem_hready     (io_mem_dmem_hready     ),
    .io_mem_dmem_hresp      (io_mem_dmem_hresp      ),
    
    // TODO: (Maybe) Not implemented
    //.io_trap                (trap                   ),
    //.io_irq                 (irq                    ),
    //.io_trace_valid         (trace_valid            ),
    //.io_trace_data          (trace_data             ),
    
    .io_success             (/*NOTHING*/            )
  );
  
  // TODO: (Maybe) Not implemented workarounds
  assign trap        = 1'b0;
  assign trace_valid = 1'b0;
  assign trace_data  = 0;

  reg [1023:0] firmware_file;
  integer k;
  
  initial begin
    if(XLen != 64 && XLen != 32) begin
      $display("UNSUPPORTED XLen");
      $finish;
    end
    if (!$value$plusargs("firmware=%s", firmware_file))
      firmware_file = "firmware.hex";
    $readmemh(firmware_file, mem.memory);
    // If xlen is 64, need to transform memory
    if(XLen == 64) begin
      for(k = 0; k < (64*1024/4/2); k=k+1) begin
        mem.memory[k][31:0] = mem.memory[k*2][31:0];
        mem.memory[k][63:32] = mem.memory[k*2+1][31:0];
      end
    end
  end

  integer cycle_counter;
  always @(posedge clk) begin
    cycle_counter <= resetn ? cycle_counter + 1 : 0;
    if (tests_passed) begin
      $display("ALL TESTS PASSED.");
      $finish;
    end
    if (resetn && trap) begin
`ifndef VERILATOR
      repeat (10) @(posedge clk);
`endif
      $display("TRAP after %1d clock cycles", cycle_counter);
      if (tests_passed) begin
        $display("ALL TESTS PASSED.");
        $finish;
      end else begin
        $display("ERROR!");
        if ($test$plusargs("noerror"))
          $finish;
        $stop;
      end
    end
  end
endmodule

module ahb_memory #(
  parameter AHB_TEST = 0,
  parameter VERBOSE = 0,
  parameter XLen = 32
) (
  input               clk,
  input               reset,
  input  [1:0]        io_mem_imem_htrans,
  input               io_mem_imem_hmastlock,
  input  [XLen-1:0]       io_mem_imem_haddr,
  input               io_mem_imem_hwrite,
  input  [2:0]        io_mem_imem_hburst,
  input  [2:0]        io_mem_imem_hsize,
  input  [3:0]        io_mem_imem_hprot,
  input  [XLen-1:0]       io_mem_imem_hwdata,
  output reg [XLen-1:0]   io_mem_imem_hrdata,
  output reg          io_mem_imem_hready,
  output reg          io_mem_imem_hresp,
  
  input  [1:0]        io_mem_dmem_htrans,
  input               io_mem_dmem_hmastlock,
  input  [XLen-1:0]       io_mem_dmem_haddr,
  input               io_mem_dmem_hwrite,
  input  [2:0]        io_mem_dmem_hburst,
  input  [2:0]        io_mem_dmem_hsize,
  input  [3:0]        io_mem_dmem_hprot,
  input  [XLen-1:0]       io_mem_dmem_hwdata,
  output reg [XLen-1:0]   io_mem_dmem_hrdata,
  output reg          io_mem_dmem_hready,
  output reg          io_mem_dmem_hresp,

  output reg tests_passed
);
  reg [XLen-1:0]   memory [0:64*1024/4-1] /* verilator public */;
  reg verbose;
  initial verbose = $test$plusargs("verbose") || VERBOSE;

  reg ahb_test;
  initial ahb_test = $test$plusargs("ahb_test") || AHB_TEST;

  initial tests_passed = 0;

  reg [63:0] xorshift64_state = 64'd88172645463325252;

  task xorshift64_next;
    begin
      // see page 4 of Marsaglia, George (July 2003). "Xorshift RNGs". Journal of Statistical Software 8 (14).
      xorshift64_state = xorshift64_state ^ (xorshift64_state << 13);
      xorshift64_state = xorshift64_state ^ (xorshift64_state >>  7);
      xorshift64_state = xorshift64_state ^ (xorshift64_state << 17);
    end
  endtask

  reg [2:0] fast_ahb_transaction = ~0;
  reg [4:0] async_ahb_transaction = ~0;
  reg [4:0] delay_ahb_transaction = 0;

  always @(posedge clk) begin
    if (ahb_test) begin
        xorshift64_next;
        {fast_ahb_transaction, async_ahb_transaction, delay_ahb_transaction} <= xorshift64_state;
    end
  end

  reg latched_i_r_en = 0;
  reg latched_i_w_en = 0;
  reg [XLen-1:0]   latched_i_raddr;
  reg [XLen-1:0]   latched_i_waddr;
  reg [XLen-1:0]   latched_i_wdata;
  reg [XLen/8-1:0] latched_i_wstrb;
  reg              latched_i_rinsn;
  
  reg latched_d_r_en = 0;
  reg latched_d_w_en = 0;
  reg [XLen-1:0]   latched_d_raddr;
  reg [XLen-1:0]   latched_d_waddr;
  reg [XLen-1:0]   latched_d_wdata;
  reg [XLen/8-1:0] latched_d_wstrb;
  reg              latched_d_rinsn;
  
  // Task for handling hready
  task handle_ahb_request; 
  input  [2:0]        size;
  input  [XLen-1:0]   haddr;
  input  [XLen-1:0]   hwdata;
  input               hinsn;
  input               hwrite;
  output              hready;
  output [XLen-1:0]   raddr;
  output [XLen-1:0]   waddr;
  output [XLen-1:0]   wdata;
  output [XLen/8-1:0] wstrb;
  output              rinsn;
  output              r_en;
  output              w_en;
  begin
    raddr = haddr;
    waddr = haddr;
    wdata = hwdata;
    rinsn = hinsn;
    r_en = ~hwrite;
    w_en =  hwrite;
    hready = 0;
    case (size)
      3'd0:     wstrb = {{(XLen/8-1){1'b0}}, {1{1'b1}}} << (XLen == 64? waddr[2:0] : waddr[1:0]);
      3'd1:     wstrb = {{(XLen/8-2){1'b0}}, {2{1'b1}}} << (XLen == 64? waddr[2:0] : waddr[1:0]);
      3'd2:     wstrb = {{(XLen/8-4){1'b0}}, {4{1'b1}}} << (XLen == 64? waddr[2:0] : waddr[1:0]);
      default:  wstrb = {(XLen/8){1'b1}}              << (XLen == 64? waddr[2:0] : waddr[1:0]);
    endcase
  end endtask
  
  // Task for handling readings
  task handle_ahb_read; 
  input  [XLen-1:0]   raddr;
  input               rinsn;
  output              hready;
  output              r_en;
  output [XLen-1:0]   rdata;
  begin
    if (raddr < 64*1024) begin
      if(XLen == 64) begin
        rdata = memory[raddr >> 3];
      end else begin
        rdata = memory[raddr >> 2];
      end
      if (verbose)
        $display("RD: ADDR=%x DATA=%x%s",raddr, rdata, rinsn ? " INSN" : "");
      hready = 1;
      r_en = 0;
    end else begin
      $display("OUT-OF-BOUNDS MEMORY READ FROM %x", raddr);
      $finish;
    end
  end endtask

  // Task for handling writtings
  task handle_ahb_write; 
  input  [XLen-1:0]   waddr;
  input  [XLen-1:0]   wdata;
  input  [XLen/8-1:0] wstrb;
  output              hready;
  output              w_en;
  reg [XLen-1:0] wrdata;
  begin
    if (waddr < 64*1024) begin
      if(XLen == 64) begin
        wrdata = wdata << ((waddr & 'h7) << 3);
        if (wstrb[0]) memory[waddr >> 3][ 7: 0] <= wrdata[ 7: 0];
        if (wstrb[1]) memory[waddr >> 3][15: 8] <= wrdata[15: 8];
        if (wstrb[2]) memory[waddr >> 3][23:16] <= wrdata[23:16];
        if (wstrb[3]) memory[waddr >> 3][31:24] <= wrdata[31:24];
        if (wstrb[4]) memory[waddr >> 3][39:32] <= wrdata[39:32];
        if (wstrb[5]) memory[waddr >> 3][47:40] <= wrdata[47:40];
        if (wstrb[6]) memory[waddr >> 3][55:48] <= wrdata[55:48];
        if (wstrb[7]) memory[waddr >> 3][63:56] <= wrdata[63:56];
      end else begin
        wrdata = wdata << ((waddr & 'h3) << 3);
        if (wstrb[0]) memory[waddr >> 2][ 7: 0] <= wrdata[ 7: 0];
        if (wstrb[1]) memory[waddr >> 2][15: 8] <= wrdata[15: 8];
        if (wstrb[2]) memory[waddr >> 2][23:16] <= wrdata[23:16];
        if (wstrb[3]) memory[waddr >> 2][31:24] <= wrdata[31:24];
      end
      if (verbose)
        $display("WR: ADDR=%x DATA=%x STRB=%b", waddr, wrdata, wstrb);
    end else
    if (waddr == (XLen == 64? 64'h1000_0000:32'h1000_0000)) begin
      if (verbose) begin
        if (32 <= wdata && wdata < 128)
          $display("OUT: '%c'", wdata[7:0]);
        else
          $display("OUT: %3d", wdata);
      end else begin
        $write("%c", wdata[7:0]);
        //$display("OUT: '%c' %3d", wdata[7:0], wdata[7:0]);
`ifndef VERILATOR
        $fflush();
`endif
      end
    end else
    if (waddr == (XLen == 64? 64'h2000_0000:32'h2000_0000)) begin
      if (wdata == (XLen == 64? 64'd123456789:32'd123456789))
        tests_passed = 1;
    end else begin
      $display("OUT-OF-BOUNDS MEMORY WRITE TO %x", waddr);
      $finish;
    end
    hready = 1;
    w_en = 0;
  end endtask
  
  always @(posedge clk) begin 
  io_mem_imem_hready <= 1'b1;
  io_mem_dmem_hready <= 1'b1;
  io_mem_imem_hresp  <= 0;  // Always OK
  io_mem_dmem_hresp  <= 0;  // Always OK
  
  if(reset) begin

    // request, TODO: only works with NONSEQ
    if (io_mem_imem_htrans != 2'b00 && io_mem_imem_hready /*&& io_mem_imem_hsel*/) begin
      if(io_mem_imem_htrans != 2'b10) begin
        $display("This simulation is incompatible with other different that NONSEQ");
        $finish;
      end
      handle_ahb_request(io_mem_imem_hsize, io_mem_imem_haddr, io_mem_imem_hwdata, 1, io_mem_imem_hwrite, io_mem_imem_hready, 
        latched_i_raddr, latched_i_waddr, latched_i_wdata, latched_i_wstrb, latched_i_rinsn, latched_i_r_en, latched_i_w_en);
    end
    
    // request, TODO: only works with NONSEQ
    if (io_mem_dmem_htrans != 2'b00 && io_mem_dmem_hready /*&& io_mem_dmem_hsel*/) begin
      if(io_mem_dmem_htrans != 2'b10) begin
        $display("This simulation is incompatible with other different that NONSEQ");
        $finish;
      end
      handle_ahb_request(io_mem_dmem_hsize, io_mem_dmem_haddr, io_mem_dmem_hwdata, 0, io_mem_dmem_hwrite, io_mem_dmem_hready, 
        latched_d_raddr, latched_d_waddr, latched_d_wdata, latched_d_wstrb, latched_d_rinsn, latched_d_r_en, latched_d_w_en);
    end

    if (latched_i_r_en && !delay_ahb_transaction[0]) handle_ahb_read(latched_i_raddr, latched_i_rinsn, io_mem_imem_hready, latched_i_r_en, io_mem_imem_hrdata);
    if (latched_i_w_en && !delay_ahb_transaction[1]) handle_ahb_write(latched_i_waddr, latched_i_wdata, latched_i_wstrb, io_mem_imem_hready, latched_i_w_en);
    if (latched_d_r_en && !delay_ahb_transaction[2]) handle_ahb_read(latched_d_raddr, latched_d_rinsn, io_mem_dmem_hready, latched_d_r_en, io_mem_dmem_hrdata);
    if (latched_d_w_en && !delay_ahb_transaction[3]) handle_ahb_write(latched_d_waddr, latched_d_wdata, latched_d_wstrb, io_mem_dmem_hready, latched_d_w_en);
  end end
endmodule
