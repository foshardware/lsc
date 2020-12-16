-- Copyright 2018 - Andreas Westerwick <westerwick@pconas.de>
-- SPDX-License-Identifier: GPL-3.0-or-later

circuit picorv32:
  module picorv32_wb:
    output mem_instr: UInt<1>
    output trace_data: UInt<36>
    output trace_valid: UInt<1>
    output eoi: UInt<32>
    input irq: UInt<32>
    input pcpi_ready: UInt<1>
    input pcpi_wait: UInt<1>
    input pcpi_rd: UInt<32>
    input pcpi_wr: UInt<1>
    output pcpi_rs2: UInt<32>
    output pcpi_rs1: UInt<32>
    output pcpi_insn: UInt<32>
    output pcpi_valid: UInt<1>
    output wbm_cyc_o: UInt<1>
    input wbm_ack_i: UInt<1>
    output wbm_stb_o: UInt<1>
    output wbm_sel_o: UInt<4>
    output wbm_we_o: UInt<1>
    input wbm_dat_i: UInt<32>
    output wbm_dat_o: UInt<32>
    output wbm_adr_o: UInt<32>
    input wb_clk_i: UInt<1>
    input wb_rst_i: UInt<1>
    output trap: UInt<1>

    wire _0_state_1_0_: UInt<2>
    wire _0_wbm_cyc_o_0_0_: UInt<1>
    wire _0_wbm_stb_o_0_0_: UInt<1>
    wire _0_wbm_sel_o_3_0_: UInt<4>
    wire _0_wbm_we_o_0_0_: UInt<1>
    wire _0_wbm_dat_o_31_0_: UInt<32>
    wire _0_wbm_adr_o_31_0_: UInt<32>
    wire _0_mem_rdata_31_0_: UInt<32>
    wire _0_mem_ready_0_0_: UInt<1>
    wire _or_picorv32_v_2924_884_Y: UInt<1>
    wire _or_picorv32_v_2924_883_Y: UInt<1>
    wire _or_picorv32_v_2924_882_Y: UInt<1>
    wire _not_picorv32_v_2838_881_Y: UInt<1>
    wire we: UInt<1>
    wire state: UInt<2>
    wire resetn: UInt<1>
    wire clk: UInt<1>
    wire mem_rdata: UInt<32>
    wire mem_ready: UInt<1>
    wire mem_wstrb: UInt<4>
    wire mem_wdata: UInt<32>
    wire mem_addr: UInt<32>
    wire mem_valid: UInt<1>
    wire _or_picorv32_v_2924_884: UInt<1>
    wire _or_picorv32_v_2924_883: UInt<1>
    wire _or_picorv32_v_2924_882: UInt<1>
    wire _not_picorv32_v_2838_881: UInt<1>
    wire _0: UInt<1>
    wire _1: UInt<1>
    wire _2: UInt<1>
    _0_state_1_0_ is invalid
    _0_wbm_cyc_o_0_0_ is invalid
    _0_wbm_stb_o_0_0_ is invalid
    _0_wbm_sel_o_3_0_ is invalid
    _0_wbm_we_o_0_0_ is invalid
    _0_wbm_dat_o_31_0_ is invalid
    _0_wbm_adr_o_31_0_ is invalid
    _0_mem_rdata_31_0_ is invalid
    _0_mem_ready_0_0_ is invalid
    state is invalid
    mem_rdata is invalid
    mem_ready is invalid
    mem_wstrb is invalid
    mem_wdata is invalid
    mem_addr is invalid
    mem_valid is invalid
    mem_instr is invalid
    trace_data is invalid
    trace_valid is invalid
    eoi is invalid
    pcpi_rs2 is invalid
    pcpi_rs1 is invalid
    pcpi_insn is invalid
    pcpi_valid is invalid
    wbm_cyc_o is invalid
    wbm_stb_o is invalid
    wbm_sel_o is invalid
    wbm_we_o is invalid
    wbm_dat_o is invalid
    wbm_adr_o is invalid
    trap is invalid

    _or_picorv32_v_2924_884 <= or(_or_picorv32_v_2924_883_Y, asUInt(bits(mem_wstrb, 3, 3)))
    _or_picorv32_v_2924_883 <= or(_or_picorv32_v_2924_882_Y, asUInt(bits(mem_wstrb, 2, 2)))
    _or_picorv32_v_2924_882 <= or(bits(mem_wstrb, 0, 0), asUInt(bits(mem_wstrb, 1, 1)))
    _not_picorv32_v_2838_881 <= not(pad(wb_rst_i, 1))
    _0 <= wb_clk_i
    _1 <= _not_picorv32_v_2838_881_Y
    _2 <= _or_picorv32_v_2924_884_Y

    inst picorv32_core of picorv32
    trace_data <= picorv32_core.trace_data
    trace_valid <= picorv32_core.trace_valid
    eoi <= picorv32_core.eoi
    picorv32_core.irq <= irq
    picorv32_core.pcpi_ready <= pcpi_ready
    picorv32_core.pcpi_wait <= pcpi_wait
    picorv32_core.pcpi_rd <= pcpi_rd
    picorv32_core.pcpi_wr <= pcpi_wr
    pcpi_rs2 <= picorv32_core.pcpi_rs2
    pcpi_rs1 <= picorv32_core.pcpi_rs1
    pcpi_insn <= picorv32_core.pcpi_insn
    pcpi_valid <= picorv32_core.pcpi_valid
    picorv32_core.mem_rdata <= mem_rdata
    picorv32_core.mem_ready <= mem_ready
    mem_instr <= picorv32_core.mem_instr
    mem_wstrb <= picorv32_core.mem_wstrb
    mem_wdata <= picorv32_core.mem_wdata
    mem_addr <= picorv32_core.mem_addr
    mem_valid <= picorv32_core.mem_valid
    trap <= picorv32_core.trap
    picorv32_core.resetn <= resetn
    picorv32_core.clk <= clk
    _or_picorv32_v_2924_884_Y <= bits(_or_picorv32_v_2924_884, 0, 0)
    _or_picorv32_v_2924_883_Y <= bits(_or_picorv32_v_2924_883, 0, 0)
    _or_picorv32_v_2924_882_Y <= bits(_or_picorv32_v_2924_882, 0, 0)
    _not_picorv32_v_2838_881_Y <= bits(_not_picorv32_v_2838_881, 0, 0)
    we <= bits(_2, 0, 0)
    resetn <= bits(_1, 0, 0)
    clk <= bits(_0, 0, 0)
  module picorv32_axi_adapter:
    output mem_rdata: UInt<32>
    input mem_wstrb: UInt<4>
    input mem_wdata: UInt<32>
    input mem_addr: UInt<32>
    output mem_ready: UInt<1>
    input mem_instr: UInt<1>
    input mem_valid: UInt<1>
    input mem_axi_rdata: UInt<32>
    output mem_axi_rready: UInt<1>
    input mem_axi_rvalid: UInt<1>
    output mem_axi_arprot: UInt<3>
    output mem_axi_araddr: UInt<32>
    input mem_axi_arready: UInt<1>
    output mem_axi_arvalid: UInt<1>
    output mem_axi_bready: UInt<1>
    input mem_axi_bvalid: UInt<1>
    output mem_axi_wstrb: UInt<4>
    output mem_axi_wdata: UInt<32>
    input mem_axi_wready: UInt<1>
    output mem_axi_wvalid: UInt<1>
    output mem_axi_awprot: UInt<3>
    output mem_axi_awaddr: UInt<32>
    input mem_axi_awready: UInt<1>
    output mem_axi_awvalid: UInt<1>
    input resetn: UInt<1>
    input clk: UInt<1>

    wire _logic_or_picorv32_v_2729_880_Y: UInt<1>
    wire _logic_not_picorv32_v_2729_879_Y: UInt<1>
    wire _logic_and_picorv32_v_2727_878_Y: UInt<1>
    wire _logic_and_picorv32_v_2725_877_Y: UInt<1>
    wire _logic_and_picorv32_v_2723_876_Y: UInt<1>
    wire _logic_and_picorv32_v_2722_875_Y: UInt<1>
    wire _logic_not_picorv32_v_2719_874_Y: UInt<1>
    wire _0_xfer_done_0_0_: UInt<1>
    wire _0_ack_wvalid_0_0_: UInt<1>
    wire _0_ack_awvalid_0_0_: UInt<1>
    wire _0_ack_arvalid_0_0_: UInt<1>
    wire _logic_and_picorv32_v_2715_872_Y: UInt<1>
    wire _logic_not_picorv32_v_2715_871_Y: UInt<1>
    wire _logic_and_picorv32_v_2714_870_Y: UInt<1>
    wire _reduce_or_picorv32_v_2714_869_Y: UInt<1>
    wire _logic_or_picorv32_v_2713_868_Y: UInt<1>
    wire _logic_and_picorv32_v_2709_867_Y: UInt<1>
    wire _logic_not_picorv32_v_2709_866_Y: UInt<1>
    wire _logic_and_picorv32_v_2709_865_Y: UInt<1>
    wire _reduce_or_picorv32_v_2709_864_Y: UInt<1>
    wire _ternary_picorv32_v_2707_863_Y: UInt<3>
    wire _logic_and_picorv32_v_2705_862_Y: UInt<1>
    wire _logic_not_picorv32_v_2705_861_Y: UInt<1>
    wire _logic_and_picorv32_v_2705_860_Y: UInt<1>
    wire _logic_not_picorv32_v_2705_859_Y: UInt<1>
    wire _logic_and_picorv32_v_2701_858_Y: UInt<1>
    wire _logic_not_picorv32_v_2701_857_Y: UInt<1>
    wire _logic_and_picorv32_v_2701_856_Y: UInt<1>
    wire _reduce_or_picorv32_v_2701_855_Y: UInt<1>
    wire xfer_done: UInt<1>
    wire ack_wvalid: UInt<1>
    wire ack_arvalid: UInt<1>
    wire ack_awvalid: UInt<1>
    wire _logic_or_picorv32_v_2729_880: UInt<1>
    wire _logic_not_picorv32_v_2729_879: UInt<1>
    wire _logic_and_picorv32_v_2727_878: UInt<1>
    wire _logic_and_picorv32_v_2725_877: UInt<1>
    wire _logic_and_picorv32_v_2723_876: UInt<1>
    wire _logic_and_picorv32_v_2722_875: UInt<1>
    wire _logic_not_picorv32_v_2719_874: UInt<1>
    wire _logic_and_picorv32_v_2715_872: UInt<1>
    wire _logic_not_picorv32_v_2715_871: UInt<1>
    wire _logic_and_picorv32_v_2714_870: UInt<1>
    wire _reduce_or_picorv32_v_2714_869: UInt<1>
    wire _logic_or_picorv32_v_2713_868: UInt<1>
    wire _logic_and_picorv32_v_2709_867: UInt<1>
    wire _logic_not_picorv32_v_2709_866: UInt<1>
    wire _logic_and_picorv32_v_2709_865: UInt<1>
    wire _reduce_or_picorv32_v_2709_864: UInt<1>
    wire _ternary_picorv32_v_2707_863: UInt<3>
    wire _logic_and_picorv32_v_2705_862: UInt<1>
    wire _logic_not_picorv32_v_2705_861: UInt<1>
    wire _logic_and_picorv32_v_2705_860: UInt<1>
    wire _logic_not_picorv32_v_2705_859: UInt<1>
    wire _logic_and_picorv32_v_2701_858: UInt<1>
    wire _logic_not_picorv32_v_2701_857: UInt<1>
    wire _logic_and_picorv32_v_2701_856: UInt<1>
    wire _reduce_or_picorv32_v_2701_855: UInt<1>
    wire _34: UInt<1>
    wire _35: UInt<32>
    wire _36: UInt<3>
    wire _37: UInt<1>
    wire _38: UInt<32>
    wire _39: UInt<3>
    wire _40: UInt<1>
    wire _41: UInt<32>
    wire _42: UInt<4>
    wire _43: UInt<1>
    wire _44: UInt<1>
    wire _45: UInt<1>
    wire _46: UInt<32>
    _0_xfer_done_0_0_ is invalid
    _0_ack_wvalid_0_0_ is invalid
    _0_ack_awvalid_0_0_ is invalid
    _0_ack_arvalid_0_0_ is invalid
    xfer_done is invalid
    ack_wvalid is invalid
    ack_arvalid is invalid
    ack_awvalid is invalid

    _logic_or_picorv32_v_2729_880 <= or(neq(xfer_done, UInt(0)), asUInt(neq(_logic_not_picorv32_v_2729_879_Y, UInt(0))))
    _logic_not_picorv32_v_2729_879 <= eq(mem_valid, UInt(0))
    _logic_and_picorv32_v_2727_878 <= and(neq(mem_axi_wready, UInt(0)), asUInt(neq(mem_axi_wvalid, UInt(0))))
    _logic_and_picorv32_v_2725_877 <= and(neq(mem_axi_arready, UInt(0)), asUInt(neq(mem_axi_arvalid, UInt(0))))
    _logic_and_picorv32_v_2723_876 <= and(neq(mem_axi_awready, UInt(0)), asUInt(neq(mem_axi_awvalid, UInt(0))))
    _logic_and_picorv32_v_2722_875 <= and(neq(mem_valid, UInt(0)), asUInt(neq(mem_ready, UInt(0))))
    _logic_not_picorv32_v_2719_874 <= eq(resetn, UInt(0))
    _logic_and_picorv32_v_2715_872 <= and(neq(mem_valid, UInt(0)), asUInt(neq(_logic_not_picorv32_v_2715_871_Y, UInt(0))))
    _logic_not_picorv32_v_2715_871 <= eq(mem_wstrb, UInt(0))
    _logic_and_picorv32_v_2714_870 <= and(neq(mem_valid, UInt(0)), asUInt(neq(_reduce_or_picorv32_v_2714_869_Y, UInt(0))))
    _reduce_or_picorv32_v_2714_869 <= orr(pad(mem_wstrb, 1))
    _logic_or_picorv32_v_2713_868 <= or(neq(mem_axi_bvalid, UInt(0)), asUInt(neq(mem_axi_rvalid, UInt(0))))
    _logic_and_picorv32_v_2709_867 <= and(neq(_logic_and_picorv32_v_2709_865_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_2709_866_Y, UInt(0))))
    _logic_not_picorv32_v_2709_866 <= eq(ack_wvalid, UInt(0))
    _logic_and_picorv32_v_2709_865 <= and(neq(mem_valid, UInt(0)), asUInt(neq(_reduce_or_picorv32_v_2709_864_Y, UInt(0))))
    _reduce_or_picorv32_v_2709_864 <= orr(pad(mem_wstrb, 1))
    _ternary_picorv32_v_2707_863 <= mux(mem_instr, UInt<3>("h4"), UInt<3>("h0"))
    _logic_and_picorv32_v_2705_862 <= and(neq(_logic_and_picorv32_v_2705_860_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_2705_861_Y, UInt(0))))
    _logic_not_picorv32_v_2705_861 <= eq(ack_arvalid, UInt(0))
    _logic_and_picorv32_v_2705_860 <= and(neq(mem_valid, UInt(0)), asUInt(neq(_logic_not_picorv32_v_2705_859_Y, UInt(0))))
    _logic_not_picorv32_v_2705_859 <= eq(mem_wstrb, UInt(0))
    _logic_and_picorv32_v_2701_858 <= and(neq(_logic_and_picorv32_v_2701_856_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_2701_857_Y, UInt(0))))
    _logic_not_picorv32_v_2701_857 <= eq(ack_awvalid, UInt(0))
    _logic_and_picorv32_v_2701_856 <= and(neq(mem_valid, UInt(0)), asUInt(neq(_reduce_or_picorv32_v_2701_855_Y, UInt(0))))
    _reduce_or_picorv32_v_2701_855 <= orr(pad(mem_wstrb, 1))
    _34 <= _logic_and_picorv32_v_2701_858_Y
    _35 <= mem_addr
    _36 <= UInt<3>("h0")
    _37 <= _logic_and_picorv32_v_2705_862_Y
    _38 <= mem_addr
    _39 <= _ternary_picorv32_v_2707_863_Y
    _40 <= _logic_and_picorv32_v_2709_867_Y
    _41 <= mem_wdata
    _42 <= mem_wstrb
    _43 <= _logic_or_picorv32_v_2713_868_Y
    _44 <= _logic_and_picorv32_v_2714_870_Y
    _45 <= _logic_and_picorv32_v_2715_872_Y
    _46 <= mem_axi_rdata

    _logic_or_picorv32_v_2729_880_Y <= bits(_logic_or_picorv32_v_2729_880, 0, 0)
    _logic_not_picorv32_v_2729_879_Y <= bits(_logic_not_picorv32_v_2729_879, 0, 0)
    _logic_and_picorv32_v_2727_878_Y <= bits(_logic_and_picorv32_v_2727_878, 0, 0)
    _logic_and_picorv32_v_2725_877_Y <= bits(_logic_and_picorv32_v_2725_877, 0, 0)
    _logic_and_picorv32_v_2723_876_Y <= bits(_logic_and_picorv32_v_2723_876, 0, 0)
    _logic_and_picorv32_v_2722_875_Y <= bits(_logic_and_picorv32_v_2722_875, 0, 0)
    _logic_not_picorv32_v_2719_874_Y <= bits(_logic_not_picorv32_v_2719_874, 0, 0)
    _logic_and_picorv32_v_2715_872_Y <= bits(_logic_and_picorv32_v_2715_872, 0, 0)
    _logic_not_picorv32_v_2715_871_Y <= bits(_logic_not_picorv32_v_2715_871, 0, 0)
    _logic_and_picorv32_v_2714_870_Y <= bits(_logic_and_picorv32_v_2714_870, 0, 0)
    _reduce_or_picorv32_v_2714_869_Y <= bits(_reduce_or_picorv32_v_2714_869, 0, 0)
    _logic_or_picorv32_v_2713_868_Y <= bits(_logic_or_picorv32_v_2713_868, 0, 0)
    _logic_and_picorv32_v_2709_867_Y <= bits(_logic_and_picorv32_v_2709_867, 0, 0)
    _logic_not_picorv32_v_2709_866_Y <= bits(_logic_not_picorv32_v_2709_866, 0, 0)
    _logic_and_picorv32_v_2709_865_Y <= bits(_logic_and_picorv32_v_2709_865, 0, 0)
    _reduce_or_picorv32_v_2709_864_Y <= bits(_reduce_or_picorv32_v_2709_864, 0, 0)
    _ternary_picorv32_v_2707_863_Y <= bits(_ternary_picorv32_v_2707_863, 2, 0)
    _logic_and_picorv32_v_2705_862_Y <= bits(_logic_and_picorv32_v_2705_862, 0, 0)
    _logic_not_picorv32_v_2705_861_Y <= bits(_logic_not_picorv32_v_2705_861, 0, 0)
    _logic_and_picorv32_v_2705_860_Y <= bits(_logic_and_picorv32_v_2705_860, 0, 0)
    _logic_not_picorv32_v_2705_859_Y <= bits(_logic_not_picorv32_v_2705_859, 0, 0)
    _logic_and_picorv32_v_2701_858_Y <= bits(_logic_and_picorv32_v_2701_858, 0, 0)
    _logic_not_picorv32_v_2701_857_Y <= bits(_logic_not_picorv32_v_2701_857, 0, 0)
    _logic_and_picorv32_v_2701_856_Y <= bits(_logic_and_picorv32_v_2701_856, 0, 0)
    _reduce_or_picorv32_v_2701_855_Y <= bits(_reduce_or_picorv32_v_2701_855, 0, 0)
    mem_rdata <= bits(_46, 31, 0)
    mem_ready <= bits(_43, 0, 0)
    mem_axi_rready <= bits(_45, 0, 0)
    mem_axi_arprot <= bits(_39, 2, 0)
    mem_axi_araddr <= bits(_38, 31, 0)
    mem_axi_arvalid <= bits(_37, 0, 0)
    mem_axi_bready <= bits(_44, 0, 0)
    mem_axi_wstrb <= bits(_42, 3, 0)
    mem_axi_wdata <= bits(_41, 31, 0)
    mem_axi_wvalid <= bits(_40, 0, 0)
    mem_axi_awprot <= bits(_36, 2, 0)
    mem_axi_awaddr <= bits(_35, 31, 0)
    mem_axi_awvalid <= bits(_34, 0, 0)
  module picorv32_axi:
    output trace_data: UInt<36>
    output trace_valid: UInt<1>
    output eoi: UInt<32>
    input irq: UInt<32>
    input pcpi_ready: UInt<1>
    input pcpi_wait: UInt<1>
    input pcpi_rd: UInt<32>
    input pcpi_wr: UInt<1>
    output pcpi_rs2: UInt<32>
    output pcpi_rs1: UInt<32>
    output pcpi_insn: UInt<32>
    output pcpi_valid: UInt<1>
    input mem_axi_rdata: UInt<32>
    output mem_axi_rready: UInt<1>
    input mem_axi_rvalid: UInt<1>
    output mem_axi_arprot: UInt<3>
    output mem_axi_araddr: UInt<32>
    input mem_axi_arready: UInt<1>
    output mem_axi_arvalid: UInt<1>
    output mem_axi_bready: UInt<1>
    input mem_axi_bvalid: UInt<1>
    output mem_axi_wstrb: UInt<4>
    output mem_axi_wdata: UInt<32>
    input mem_axi_wready: UInt<1>
    output mem_axi_wvalid: UInt<1>
    output mem_axi_awprot: UInt<3>
    output mem_axi_awaddr: UInt<32>
    input mem_axi_awready: UInt<1>
    output mem_axi_awvalid: UInt<1>
    output trap: UInt<1>
    input resetn: UInt<1>
    input clk: UInt<1>

    wire mem_rdata: UInt<32>
    wire mem_ready: UInt<1>
    wire mem_instr: UInt<1>
    wire mem_wstrb: UInt<4>
    wire mem_wdata: UInt<32>
    wire mem_addr: UInt<32>
    wire mem_valid: UInt<1>
    mem_rdata is invalid
    mem_ready is invalid
    mem_instr is invalid
    mem_wstrb is invalid
    mem_wdata is invalid
    mem_addr is invalid
    mem_valid is invalid
    trace_data is invalid
    trace_valid is invalid
    eoi is invalid
    pcpi_rs2 is invalid
    pcpi_rs1 is invalid
    pcpi_insn is invalid
    pcpi_valid is invalid
    mem_axi_rready is invalid
    mem_axi_arprot is invalid
    mem_axi_araddr is invalid
    mem_axi_arvalid is invalid
    mem_axi_bready is invalid
    mem_axi_wstrb is invalid
    mem_axi_wdata is invalid
    mem_axi_wvalid is invalid
    mem_axi_awprot is invalid
    mem_axi_awaddr is invalid
    mem_axi_awvalid is invalid
    trap is invalid


    inst picorv32_core of picorv32
    trace_data <= picorv32_core.trace_data
    trace_valid <= picorv32_core.trace_valid
    eoi <= picorv32_core.eoi
    picorv32_core.irq <= irq
    picorv32_core.pcpi_ready <= pcpi_ready
    picorv32_core.pcpi_wait <= pcpi_wait
    picorv32_core.pcpi_rd <= pcpi_rd
    picorv32_core.pcpi_wr <= pcpi_wr
    pcpi_rs2 <= picorv32_core.pcpi_rs2
    pcpi_rs1 <= picorv32_core.pcpi_rs1
    pcpi_insn <= picorv32_core.pcpi_insn
    pcpi_valid <= picorv32_core.pcpi_valid
    picorv32_core.mem_rdata <= mem_rdata
    picorv32_core.mem_ready <= mem_ready
    mem_instr <= picorv32_core.mem_instr
    mem_wstrb <= picorv32_core.mem_wstrb
    mem_wdata <= picorv32_core.mem_wdata
    mem_addr <= picorv32_core.mem_addr
    mem_valid <= picorv32_core.mem_valid
    trap <= picorv32_core.trap
    picorv32_core.resetn <= resetn
    picorv32_core.clk <= clk
    inst axi_adapter of picorv32_axi_adapter
    mem_rdata <= axi_adapter.mem_rdata
    axi_adapter.mem_wstrb <= mem_wstrb
    axi_adapter.mem_wdata <= mem_wdata
    axi_adapter.mem_addr <= mem_addr
    mem_ready <= axi_adapter.mem_ready
    axi_adapter.mem_instr <= mem_instr
    axi_adapter.mem_valid <= mem_valid
    axi_adapter.mem_axi_rdata <= mem_axi_rdata
    mem_axi_rready <= axi_adapter.mem_axi_rready
    axi_adapter.mem_axi_rvalid <= mem_axi_rvalid
    mem_axi_arprot <= axi_adapter.mem_axi_arprot
    mem_axi_araddr <= axi_adapter.mem_axi_araddr
    axi_adapter.mem_axi_arready <= mem_axi_arready
    mem_axi_arvalid <= axi_adapter.mem_axi_arvalid
    mem_axi_bready <= axi_adapter.mem_axi_bready
    axi_adapter.mem_axi_bvalid <= mem_axi_bvalid
    mem_axi_wstrb <= axi_adapter.mem_axi_wstrb
    mem_axi_wdata <= axi_adapter.mem_axi_wdata
    axi_adapter.mem_axi_wready <= mem_axi_wready
    mem_axi_wvalid <= axi_adapter.mem_axi_wvalid
    mem_axi_awprot <= axi_adapter.mem_axi_awprot
    mem_axi_awaddr <= axi_adapter.mem_axi_awaddr
    axi_adapter.mem_axi_awready <= mem_axi_awready
    mem_axi_awvalid <= axi_adapter.mem_axi_awvalid
    axi_adapter.resetn <= resetn
    axi_adapter.clk <= clk
  module picorv32_pcpi_div:
    output pcpi_ready: UInt<1>
    output pcpi_wait: UInt<1>
    output pcpi_rd: UInt<32>
    output pcpi_wr: UInt<1>
    input pcpi_rs2: UInt<32>
    input pcpi_rs1: UInt<32>
    input pcpi_insn: UInt<32>
    input pcpi_valid: UInt<1>
    input resetn: UInt<1>
    input clk: UInt<1>

    wire _shr_picorv32_v_2434_854_Y: UInt<32>
    wire _shr_picorv32_v_2430_853_Y: UInt<63>
    wire _or_picorv32_v_2428_852_Y: UInt<32>
    wire _sub_picorv32_v_2427_851_Y: UInt<63>
    wire _le_picorv32_v_2426_850_Y: UInt<1>
    wire _ternary_picorv32_v_2423_849_Y: UInt<32>
    wire _neg_picorv32_v_2423_848_Y: UInt<32>
    wire _ternary_picorv32_v_2421_847_Y: UInt<32>
    wire _neg_picorv32_v_2421_846_Y: UInt<32>
    wire _logic_or_picorv32_v_2420_845_Y: UInt<1>
    wire _logic_and_picorv32_v_2408_844_Y: UInt<1>
    wire _logic_not_picorv32_v_2408_843_Y: UInt<1>
    wire _logic_or_picorv32_v_2404_842_Y: UInt<1>
    wire _logic_and_picorv32_v_2404_841_Y: UInt<1>
    wire _logic_and_picorv32_v_2404_840_Y: UInt<1>
    wire _reduce_or_picorv32_v_2404_839_Y: UInt<1>
    wire _logic_and_picorv32_v_2404_838_Y: UInt<1>
    wire _ne_picorv32_v_2404_837_Y: UInt<1>
    wire _shl_picorv32_v_2403_836_Y: UInt<63>
    wire _ternary_picorv32_v_2403_835_Y: UInt<63>
    wire _extend_picorv32_v_2403_834_Y: UInt<63>
    wire _neg_picorv32_v_2403_833_Y: UInt<63>
    wire _extend_picorv32_v_2403_832_Y: UInt<63>
    wire _logic_and_picorv32_v_2403_831_Y: UInt<1>
    wire _logic_or_picorv32_v_2403_830_Y: UInt<1>
    wire _ternary_picorv32_v_2402_829_Y: UInt<32>
    wire _neg_picorv32_v_2402_828_Y: UInt<32>
    wire _logic_and_picorv32_v_2402_827_Y: UInt<1>
    wire _logic_or_picorv32_v_2402_826_Y: UInt<1>
    wire _logic_not_picorv32_v_2397_825_Y: UInt<1>
    wire _0_outsign_0_0_: UInt<1>
    wire _0_running_0_0_: UInt<1>
    wire _0_quotient_msk_31_0_: UInt<32>
    wire _0_quotient_31_0_: UInt<32>
    wire _0_divisor_62_0_: UInt<63>
    wire _0_dividend_31_0_: UInt<32>
    wire _0_pcpi_ready_0_0_: UInt<1>
    wire _0_pcpi_rd_31_0_: UInt<32>
    wire _0_pcpi_wr_0_0_: UInt<1>
    wire _logic_and_picorv32_v_2382_823_Y: UInt<1>
    wire _logic_and_picorv32_v_2381_822_Y: UInt<1>
    wire _logic_and_picorv32_v_2372_821_Y: UInt<1>
    wire _eq_picorv32_v_2372_820_Y: UInt<1>
    wire _logic_and_picorv32_v_2372_819_Y: UInt<1>
    wire _eq_picorv32_v_2372_818_Y: UInt<1>
    wire _logic_and_picorv32_v_2372_817_Y: UInt<1>
    wire _logic_not_picorv32_v_2372_816_Y: UInt<1>
    wire _logic_and_picorv32_v_2372_815_Y: UInt<1>
    wire _0_instr_remu_0_0_: UInt<1>
    wire _0_instr_rem_0_0_: UInt<1>
    wire _0_instr_divu_0_0_: UInt<1>
    wire _0_instr_div_0_0_: UInt<1>
    wire _0_pcpi_wait_q_0_0_: UInt<1>
    wire _0_pcpi_wait_0_0_: UInt<1>
    wire _logic_and_picorv32_v_2364_813_Y: UInt<1>
    wire _logic_not_picorv32_v_2364_812_Y: UInt<1>
    wire _reduce_or_picorv32_v_2361_811_Y: UInt<1>
    wire outsign: UInt<1>
    wire running: UInt<1>
    wire quotient_msk: UInt<32>
    wire quotient: UInt<32>
    wire divisor: UInt<63>
    wire dividend: UInt<32>
    wire start: UInt<1>
    wire pcpi_wait_q: UInt<1>
    wire instr_any_div_rem: UInt<1>
    wire instr_remu: UInt<1>
    wire instr_rem: UInt<1>
    wire instr_divu: UInt<1>
    wire instr_div: UInt<1>
    wire _shr_picorv32_v_2434_854: UInt<32>
    wire _shr_picorv32_v_2430_853: UInt<63>
    wire _or_picorv32_v_2428_852: UInt<32>
    wire _sub_picorv32_v_2427_851: UInt<63>
    wire _le_picorv32_v_2426_850: UInt<1>
    wire _ternary_picorv32_v_2423_849: UInt<32>
    wire _neg_picorv32_v_2423_848: UInt<32>
    wire _ternary_picorv32_v_2421_847: UInt<32>
    wire _neg_picorv32_v_2421_846: UInt<32>
    wire _logic_or_picorv32_v_2420_845: UInt<1>
    wire _logic_and_picorv32_v_2408_844: UInt<1>
    wire _logic_not_picorv32_v_2408_843: UInt<1>
    wire _logic_or_picorv32_v_2404_842: UInt<1>
    wire _logic_and_picorv32_v_2404_841: UInt<1>
    wire _logic_and_picorv32_v_2404_840: UInt<1>
    wire _reduce_or_picorv32_v_2404_839: UInt<1>
    wire _logic_and_picorv32_v_2404_838: UInt<1>
    wire _ne_picorv32_v_2404_837: UInt<1>
    wire _shl_picorv32_v_2403_836: UInt<63>
    wire _ternary_picorv32_v_2403_835: UInt<63>
    wire _neg_picorv32_v_2403_833: UInt<63>
    wire _logic_and_picorv32_v_2403_831: UInt<1>
    wire _logic_or_picorv32_v_2403_830: UInt<1>
    wire _ternary_picorv32_v_2402_829: UInt<32>
    wire _neg_picorv32_v_2402_828: UInt<32>
    wire _logic_and_picorv32_v_2402_827: UInt<1>
    wire _logic_or_picorv32_v_2402_826: UInt<1>
    wire _logic_not_picorv32_v_2397_825: UInt<1>
    wire _logic_and_picorv32_v_2382_823: UInt<1>
    wire _logic_and_picorv32_v_2381_822: UInt<1>
    wire _logic_and_picorv32_v_2372_821: UInt<1>
    wire _eq_picorv32_v_2372_820: UInt<1>
    wire _logic_and_picorv32_v_2372_819: UInt<1>
    wire _eq_picorv32_v_2372_818: UInt<1>
    wire _logic_and_picorv32_v_2372_817: UInt<1>
    wire _logic_not_picorv32_v_2372_816: UInt<1>
    wire _logic_and_picorv32_v_2372_815: UInt<1>
    wire _logic_and_picorv32_v_2364_813: UInt<1>
    wire _logic_not_picorv32_v_2364_812: UInt<1>
    wire _reduce_or_picorv32_v_2361_811: UInt<1>
    wire _81: UInt<1>
    wire _82: UInt<1>
    _extend_picorv32_v_2403_834_Y is invalid
    _extend_picorv32_v_2403_832_Y is invalid
    _0_outsign_0_0_ is invalid
    _0_running_0_0_ is invalid
    _0_quotient_msk_31_0_ is invalid
    _0_quotient_31_0_ is invalid
    _0_divisor_62_0_ is invalid
    _0_dividend_31_0_ is invalid
    _0_pcpi_ready_0_0_ is invalid
    _0_pcpi_rd_31_0_ is invalid
    _0_pcpi_wr_0_0_ is invalid
    _0_instr_remu_0_0_ is invalid
    _0_instr_rem_0_0_ is invalid
    _0_instr_divu_0_0_ is invalid
    _0_instr_div_0_0_ is invalid
    _0_pcpi_wait_q_0_0_ is invalid
    _0_pcpi_wait_0_0_ is invalid
    outsign is invalid
    running is invalid
    quotient_msk is invalid
    quotient is invalid
    divisor is invalid
    dividend is invalid
    pcpi_wait_q is invalid
    instr_remu is invalid
    instr_rem is invalid
    instr_divu is invalid
    instr_div is invalid
    pcpi_ready is invalid
    pcpi_wait is invalid
    pcpi_rd is invalid
    pcpi_wr is invalid

    _shr_picorv32_v_2434_854 <= shr(quotient_msk, UInt<32>("h00000001"))
    _shr_picorv32_v_2430_853 <= shr(divisor, UInt<32>("h00000001"))
    _or_picorv32_v_2428_852 <= or(quotient, asUInt(quotient_msk))
    _sub_picorv32_v_2427_851 <= asUInt(sub(dividend, asUInt(divisor)))
    _le_picorv32_v_2426_850 <= leq(divisor, asUInt(dividend))
    _ternary_picorv32_v_2423_849 <= mux(outsign, _neg_picorv32_v_2423_848_Y, dividend)
    _neg_picorv32_v_2423_848 <= neg(pad(dividend, 32))
    _ternary_picorv32_v_2421_847 <= mux(outsign, _neg_picorv32_v_2421_846_Y, quotient)
    _neg_picorv32_v_2421_846 <= neg(pad(quotient, 32))
    _logic_or_picorv32_v_2420_845 <= or(neq(instr_div, UInt(0)), asUInt(neq(instr_divu, UInt(0))))
    _logic_and_picorv32_v_2408_844 <= and(neq(_logic_not_picorv32_v_2408_843_Y, UInt(0)), asUInt(neq(running, UInt(0))))
    _logic_not_picorv32_v_2408_843 <= eq(quotient_msk, UInt(0))
    _logic_or_picorv32_v_2404_842 <= or(neq(_logic_and_picorv32_v_2404_840_Y, UInt(0)), asUInt(neq(_logic_and_picorv32_v_2404_841_Y, UInt(0))))
    _logic_and_picorv32_v_2404_841 <= and(neq(instr_rem, UInt(0)), asUInt(neq(bits(pcpi_rs1, 31, 31), UInt(0))))
    _logic_and_picorv32_v_2404_840 <= and(neq(_logic_and_picorv32_v_2404_838_Y, UInt(0)), asUInt(neq(_reduce_or_picorv32_v_2404_839_Y, UInt(0))))
    _reduce_or_picorv32_v_2404_839 <= orr(pad(pcpi_rs2, 1))
    _logic_and_picorv32_v_2404_838 <= and(neq(instr_div, UInt(0)), asUInt(neq(_ne_picorv32_v_2404_837_Y, UInt(0))))
    _ne_picorv32_v_2404_837 <= neq(bits(pcpi_rs1, 31, 31), asUInt(bits(pcpi_rs2, 31, 31)))
    _shl_picorv32_v_2403_836 <= bits(shl(_ternary_picorv32_v_2403_835_Y, UInt<32>("h0000001f")), 62, 0)
    _ternary_picorv32_v_2403_835 <= mux(_logic_and_picorv32_v_2403_831_Y, _neg_picorv32_v_2403_833_Y, _extend_picorv32_v_2403_834_Y)
    _neg_picorv32_v_2403_833 <= neg(pad(_extend_picorv32_v_2403_832_Y, 63))
    _logic_and_picorv32_v_2403_831 <= and(neq(_logic_or_picorv32_v_2403_830_Y, UInt(0)), asUInt(neq(bits(pcpi_rs2, 31, 31), UInt(0))))
    _logic_or_picorv32_v_2403_830 <= or(neq(instr_div, UInt(0)), asUInt(neq(instr_rem, UInt(0))))
    _ternary_picorv32_v_2402_829 <= mux(_logic_and_picorv32_v_2402_827_Y, _neg_picorv32_v_2402_828_Y, pcpi_rs1)
    _neg_picorv32_v_2402_828 <= neg(pad(pcpi_rs1, 32))
    _logic_and_picorv32_v_2402_827 <= and(neq(_logic_or_picorv32_v_2402_826_Y, UInt(0)), asUInt(neq(bits(pcpi_rs1, 31, 31), UInt(0))))
    _logic_or_picorv32_v_2402_826 <= or(neq(instr_div, UInt(0)), asUInt(neq(instr_rem, UInt(0))))
    _logic_not_picorv32_v_2397_825 <= eq(resetn, UInt(0))
    _logic_and_picorv32_v_2382_823 <= and(neq(pcpi_wait, UInt(0)), asUInt(neq(resetn, UInt(0))))
    _logic_and_picorv32_v_2381_822 <= and(neq(instr_any_div_rem, UInt(0)), asUInt(neq(resetn, UInt(0))))
    _logic_and_picorv32_v_2372_821 <= and(neq(_logic_and_picorv32_v_2372_819_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_2372_820_Y, UInt(0))))
    _eq_picorv32_v_2372_820 <= eq(bits(pcpi_insn, 31, 25), asUInt(UInt<7>("h01")))
    _logic_and_picorv32_v_2372_819 <= and(neq(_logic_and_picorv32_v_2372_817_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_2372_818_Y, UInt(0))))
    _eq_picorv32_v_2372_818 <= eq(bits(pcpi_insn, 6, 0), asUInt(UInt<7>("h33")))
    _logic_and_picorv32_v_2372_817 <= and(neq(_logic_and_picorv32_v_2372_815_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_2372_816_Y, UInt(0))))
    _logic_not_picorv32_v_2372_816 <= eq(pcpi_ready, UInt(0))
    _logic_and_picorv32_v_2372_815 <= and(neq(resetn, UInt(0)), asUInt(neq(pcpi_valid, UInt(0))))
    _logic_and_picorv32_v_2364_813 <= and(neq(pcpi_wait, UInt(0)), asUInt(neq(_logic_not_picorv32_v_2364_812_Y, UInt(0))))
    _logic_not_picorv32_v_2364_812 <= eq(pcpi_wait_q, UInt(0))
    _reduce_or_picorv32_v_2361_811 <= orr(pad(cat(instr_div, cat(instr_divu, cat(instr_rem, instr_remu))), 1))
    _81 <= _reduce_or_picorv32_v_2361_811_Y
    _82 <= _logic_and_picorv32_v_2364_813_Y

    _shr_picorv32_v_2434_854_Y <= bits(_shr_picorv32_v_2434_854, 31, 0)
    _shr_picorv32_v_2430_853_Y <= bits(_shr_picorv32_v_2430_853, 62, 0)
    _or_picorv32_v_2428_852_Y <= bits(_or_picorv32_v_2428_852, 31, 0)
    _sub_picorv32_v_2427_851_Y <= bits(_sub_picorv32_v_2427_851, 62, 0)
    _le_picorv32_v_2426_850_Y <= bits(_le_picorv32_v_2426_850, 0, 0)
    _ternary_picorv32_v_2423_849_Y <= bits(_ternary_picorv32_v_2423_849, 31, 0)
    _neg_picorv32_v_2423_848_Y <= bits(_neg_picorv32_v_2423_848, 31, 0)
    _ternary_picorv32_v_2421_847_Y <= bits(_ternary_picorv32_v_2421_847, 31, 0)
    _neg_picorv32_v_2421_846_Y <= bits(_neg_picorv32_v_2421_846, 31, 0)
    _logic_or_picorv32_v_2420_845_Y <= bits(_logic_or_picorv32_v_2420_845, 0, 0)
    _logic_and_picorv32_v_2408_844_Y <= bits(_logic_and_picorv32_v_2408_844, 0, 0)
    _logic_not_picorv32_v_2408_843_Y <= bits(_logic_not_picorv32_v_2408_843, 0, 0)
    _logic_or_picorv32_v_2404_842_Y <= bits(_logic_or_picorv32_v_2404_842, 0, 0)
    _logic_and_picorv32_v_2404_841_Y <= bits(_logic_and_picorv32_v_2404_841, 0, 0)
    _logic_and_picorv32_v_2404_840_Y <= bits(_logic_and_picorv32_v_2404_840, 0, 0)
    _reduce_or_picorv32_v_2404_839_Y <= bits(_reduce_or_picorv32_v_2404_839, 0, 0)
    _logic_and_picorv32_v_2404_838_Y <= bits(_logic_and_picorv32_v_2404_838, 0, 0)
    _ne_picorv32_v_2404_837_Y <= bits(_ne_picorv32_v_2404_837, 0, 0)
    _shl_picorv32_v_2403_836_Y <= bits(_shl_picorv32_v_2403_836, 62, 0)
    _ternary_picorv32_v_2403_835_Y <= bits(_ternary_picorv32_v_2403_835, 62, 0)
    _neg_picorv32_v_2403_833_Y <= bits(_neg_picorv32_v_2403_833, 62, 0)
    _logic_and_picorv32_v_2403_831_Y <= bits(_logic_and_picorv32_v_2403_831, 0, 0)
    _logic_or_picorv32_v_2403_830_Y <= bits(_logic_or_picorv32_v_2403_830, 0, 0)
    _ternary_picorv32_v_2402_829_Y <= bits(_ternary_picorv32_v_2402_829, 31, 0)
    _neg_picorv32_v_2402_828_Y <= bits(_neg_picorv32_v_2402_828, 31, 0)
    _logic_and_picorv32_v_2402_827_Y <= bits(_logic_and_picorv32_v_2402_827, 0, 0)
    _logic_or_picorv32_v_2402_826_Y <= bits(_logic_or_picorv32_v_2402_826, 0, 0)
    _logic_not_picorv32_v_2397_825_Y <= bits(_logic_not_picorv32_v_2397_825, 0, 0)
    _logic_and_picorv32_v_2382_823_Y <= bits(_logic_and_picorv32_v_2382_823, 0, 0)
    _logic_and_picorv32_v_2381_822_Y <= bits(_logic_and_picorv32_v_2381_822, 0, 0)
    _logic_and_picorv32_v_2372_821_Y <= bits(_logic_and_picorv32_v_2372_821, 0, 0)
    _eq_picorv32_v_2372_820_Y <= bits(_eq_picorv32_v_2372_820, 0, 0)
    _logic_and_picorv32_v_2372_819_Y <= bits(_logic_and_picorv32_v_2372_819, 0, 0)
    _eq_picorv32_v_2372_818_Y <= bits(_eq_picorv32_v_2372_818, 0, 0)
    _logic_and_picorv32_v_2372_817_Y <= bits(_logic_and_picorv32_v_2372_817, 0, 0)
    _logic_not_picorv32_v_2372_816_Y <= bits(_logic_not_picorv32_v_2372_816, 0, 0)
    _logic_and_picorv32_v_2372_815_Y <= bits(_logic_and_picorv32_v_2372_815, 0, 0)
    _logic_and_picorv32_v_2364_813_Y <= bits(_logic_and_picorv32_v_2364_813, 0, 0)
    _logic_not_picorv32_v_2364_812_Y <= bits(_logic_not_picorv32_v_2364_812, 0, 0)
    _reduce_or_picorv32_v_2361_811_Y <= bits(_reduce_or_picorv32_v_2361_811, 0, 0)
    start <= bits(_82, 0, 0)
    instr_any_div_rem <= bits(_81, 0, 0)
  module picorv32_pcpi_fast_mul:
    output pcpi_ready: UInt<1>
    output pcpi_wait: UInt<1>
    output pcpi_rd: UInt<32>
    output pcpi_wr: UInt<1>
    input pcpi_rs2: UInt<32>
    input pcpi_rs1: UInt<32>
    input pcpi_insn: UInt<32>
    input pcpi_valid: UInt<1>
    input resetn: UInt<1>
    input clk: UInt<1>

    wire _ternary_picorv32_v_2339_810_Y: UInt<64>
    wire _ternary_picorv32_v_2339_809_Y: UInt<64>
    wire _reduce_bool_picorv32_v_2339_808_Y: UInt<1>
    wire _shr_picorv32_v_2339_807_Y: UInt<64>
    wire _ternary_picorv32_v_2339_806_Y: UInt<64>
    wire _reduce_bool_picorv32_v_2339_805_Y: UInt<1>
    wire _logic_not_picorv32_v_2325_804_Y: UInt<1>
    wire _logic_and_picorv32_v_2307_803_Y: UInt<1>
    wire _logic_not_picorv32_v_2307_802_Y: UInt<1>
    wire _ternary_picorv32_v_2307_801_Y: UInt<4>
    wire _extend_picorv32_v_2307_800_Y: UInt<4>
    wire _reduce_bool_picorv32_v_2307_799_Y: UInt<1>
    wire _0_active_3_0_: UInt<4>
    wire _0_shift_out_0_0_: UInt<1>
    wire _0_rs2_32_0_: UInt<33>
    wire _0_rs1_32_0_: UInt<33>
    wire _logic_or_picorv32_v_2301_797_Y: UInt<1>
    wire _mul_picorv32_v_2299_796_Y: UInt<64>
    wire _ternary_picorv32_v_2299_795_Y: UInt<33>
    wire _reduce_bool_picorv32_v_2299_794_Y: UInt<1>
    wire _ternary_picorv32_v_2299_793_Y: UInt<33>
    wire _reduce_bool_picorv32_v_2299_792_Y: UInt<1>
    wire _logic_or_picorv32_v_2298_791_Y: UInt<1>
    wire _logic_or_picorv32_v_2294_790_Y: UInt<1>
    wire _0_pcpi_insn_valid_q_0_0_: UInt<1>
    wire _0_rd_q_63_0_: UInt<64>
    wire _0_rs2_q_32_0_: UInt<33>
    wire _0_rs1_q_32_0_: UInt<33>
    wire _0_rd_63_0_: UInt<64>
    wire _2_instr_mulhu_0_0_: UInt<1>
    wire _2_instr_mulhsu_0_0_: UInt<1>
    wire _2_instr_mulh_0_0_: UInt<1>
    wire _2_instr_mul_0_0_: UInt<1>
    wire _1_instr_mulhu_0_0_: UInt<1>
    wire _1_instr_mulhsu_0_0_: UInt<1>
    wire _1_instr_mulh_0_0_: UInt<1>
    wire _1_instr_mul_0_0_: UInt<1>
    wire _logic_and_picorv32_v_2282_788_Y: UInt<1>
    wire _ternary_picorv32_v_2282_787_Y: UInt<1>
    wire _reduce_bool_picorv32_v_2282_786_Y: UInt<1>
    wire _0_instr_mulhu_0_0_: UInt<1>
    wire _0_instr_mulhsu_0_0_: UInt<1>
    wire _0_instr_mulh_0_0_: UInt<1>
    wire _0_instr_mul_0_0_: UInt<1>
    wire _logic_and_picorv32_v_2273_784_Y: UInt<1>
    wire _eq_picorv32_v_2273_783_Y: UInt<1>
    wire _logic_and_picorv32_v_2273_782_Y: UInt<1>
    wire _eq_picorv32_v_2273_781_Y: UInt<1>
    wire _reduce_or_picorv32_v_2266_780_Y: UInt<1>
    wire _reduce_or_picorv32_v_2265_779_Y: UInt<1>
    wire _reduce_or_picorv32_v_2264_778_Y: UInt<1>
    wire _reduce_or_picorv32_v_2263_777_Y: UInt<1>
    wire pcpi_insn_valid_q: UInt<1>
    wire pcpi_insn_valid: UInt<1>
    wire rd_q: UInt<64>
    wire rd: UInt<64>
    wire rs2_q: UInt<33>
    wire rs1_q: UInt<33>
    wire rs2: UInt<33>
    wire rs1: UInt<33>
    wire active: UInt<4>
    wire shift_out: UInt<1>
    wire instr_rs2_signed: UInt<1>
    wire instr_rs1_signed: UInt<1>
    wire instr_any_mulh: UInt<1>
    wire instr_any_mul: UInt<1>
    wire instr_mulhu: UInt<1>
    wire instr_mulhsu: UInt<1>
    wire instr_mulh: UInt<1>
    wire instr_mul: UInt<1>
    wire _ternary_picorv32_v_2339_810: UInt<64>
    wire _ternary_picorv32_v_2339_809: UInt<64>
    wire _reduce_bool_picorv32_v_2339_808: UInt<1>
    wire _shr_picorv32_v_2339_807: UInt<64>
    wire _ternary_picorv32_v_2339_806: UInt<64>
    wire _reduce_bool_picorv32_v_2339_805: UInt<1>
    wire _logic_not_picorv32_v_2325_804: UInt<1>
    wire _logic_and_picorv32_v_2307_803: UInt<1>
    wire _logic_not_picorv32_v_2307_802: UInt<1>
    wire _ternary_picorv32_v_2307_801: UInt<4>
    wire _reduce_bool_picorv32_v_2307_799: UInt<1>
    wire _logic_or_picorv32_v_2301_797: UInt<1>
    wire _mul_picorv32_v_2299_796: UInt<64>
    wire _ternary_picorv32_v_2299_795: UInt<33>
    wire _reduce_bool_picorv32_v_2299_794: UInt<1>
    wire _ternary_picorv32_v_2299_793: UInt<33>
    wire _reduce_bool_picorv32_v_2299_792: UInt<1>
    wire _logic_or_picorv32_v_2298_791: UInt<1>
    wire _logic_or_picorv32_v_2294_790: UInt<1>
    wire _logic_and_picorv32_v_2282_788: UInt<1>
    wire _ternary_picorv32_v_2282_787: UInt<1>
    wire _reduce_bool_picorv32_v_2282_786: UInt<1>
    wire _logic_and_picorv32_v_2273_784: UInt<1>
    wire _eq_picorv32_v_2273_783: UInt<1>
    wire _logic_and_picorv32_v_2273_782: UInt<1>
    wire _eq_picorv32_v_2273_781: UInt<1>
    wire _reduce_or_picorv32_v_2266_780: UInt<1>
    wire _reduce_or_picorv32_v_2265_779: UInt<1>
    wire _reduce_or_picorv32_v_2264_778: UInt<1>
    wire _reduce_or_picorv32_v_2263_777: UInt<1>
    wire _115: UInt<1>
    wire _116: UInt<1>
    wire _117: UInt<1>
    wire _118: UInt<1>
    wire _119: UInt<1>
    wire _120: UInt<1>
    wire _121: UInt<1>
    wire _122: UInt<1>
    wire _123: UInt<32>
    _extend_picorv32_v_2307_800_Y is invalid
    _0_active_3_0_ is invalid
    _0_shift_out_0_0_ is invalid
    _0_rs2_32_0_ is invalid
    _0_rs1_32_0_ is invalid
    _0_pcpi_insn_valid_q_0_0_ is invalid
    _0_rd_q_63_0_ is invalid
    _0_rs2_q_32_0_ is invalid
    _0_rs1_q_32_0_ is invalid
    _0_rd_63_0_ is invalid
    _2_instr_mulhu_0_0_ is invalid
    _2_instr_mulhsu_0_0_ is invalid
    _2_instr_mulh_0_0_ is invalid
    _2_instr_mul_0_0_ is invalid
    _1_instr_mulhu_0_0_ is invalid
    _1_instr_mulhsu_0_0_ is invalid
    _1_instr_mulh_0_0_ is invalid
    _1_instr_mul_0_0_ is invalid
    _0_instr_mulhu_0_0_ is invalid
    _0_instr_mulhsu_0_0_ is invalid
    _0_instr_mulh_0_0_ is invalid
    _0_instr_mul_0_0_ is invalid
    pcpi_insn_valid_q is invalid
    rd_q is invalid
    rd is invalid
    rs2_q is invalid
    rs1_q is invalid
    rs2 is invalid
    rs1 is invalid
    active is invalid
    shift_out is invalid
    instr_mulhu is invalid
    instr_mulhsu is invalid
    instr_mulh is invalid
    instr_mul is invalid

    _ternary_picorv32_v_2339_810 <= mux(shift_out, _shr_picorv32_v_2339_807_Y, _ternary_picorv32_v_2339_809_Y)
    _ternary_picorv32_v_2339_809 <= mux(_reduce_bool_picorv32_v_2339_808_Y, rd_q, rd)
    _reduce_bool_picorv32_v_2339_808 <= asUInt(neq(asSInt(UInt<32>("h00000000")), SInt<32>(0)))
    _shr_picorv32_v_2339_807 <= shr(_ternary_picorv32_v_2339_806_Y, UInt<32>("h00000020"))
    _ternary_picorv32_v_2339_806 <= mux(_reduce_bool_picorv32_v_2339_805_Y, rd_q, rd)
    _reduce_bool_picorv32_v_2339_805 <= asUInt(neq(asSInt(UInt<32>("h00000000")), SInt<32>(0)))
    _logic_not_picorv32_v_2325_804 <= eq(resetn, UInt(0))
    _logic_and_picorv32_v_2307_803 <= and(neq(instr_any_mul, UInt(0)), asUInt(neq(_logic_not_picorv32_v_2307_802_Y, UInt(0))))
    _logic_not_picorv32_v_2307_802 <= eq(_ternary_picorv32_v_2307_801_Y, UInt(0))
    _ternary_picorv32_v_2307_801 <= mux(_reduce_bool_picorv32_v_2307_799_Y, active, _extend_picorv32_v_2307_800_Y)
    _reduce_bool_picorv32_v_2307_799 <= asUInt(neq(asSInt(UInt<32>("h00000000")), SInt<32>(0)))
    _logic_or_picorv32_v_2301_797 <= or(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(bits(active, 2, 2), UInt(0))))
    _mul_picorv32_v_2299_796 <= asUInt(mul(asSInt(cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_793_Y, 32, 32), _ternary_picorv32_v_2299_793_Y)))))))))))))))))))))))))))))))), asSInt(cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), cat(bits(_ternary_picorv32_v_2299_795_Y, 32, 32), _ternary_picorv32_v_2299_795_Y))))))))))))))))))))))))))))))))))
    _ternary_picorv32_v_2299_795 <= mux(_reduce_bool_picorv32_v_2299_794_Y, rs2_q, rs2)
    _reduce_bool_picorv32_v_2299_794 <= asUInt(neq(asSInt(UInt<32>("h00000000")), SInt<32>(0)))
    _ternary_picorv32_v_2299_793 <= mux(_reduce_bool_picorv32_v_2299_792_Y, rs1_q, rs1)
    _reduce_bool_picorv32_v_2299_792 <= asUInt(neq(asSInt(UInt<32>("h00000000")), SInt<32>(0)))
    _logic_or_picorv32_v_2298_791 <= or(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(bits(active, 1, 1), UInt(0))))
    _logic_or_picorv32_v_2294_790 <= or(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(bits(active, 0, 0), UInt(0))))
    _logic_and_picorv32_v_2282_788 <= and(neq(resetn, UInt(0)), asUInt(neq(_ternary_picorv32_v_2282_787_Y, UInt(0))))
    _ternary_picorv32_v_2282_787 <= mux(_reduce_bool_picorv32_v_2282_786_Y, pcpi_insn_valid_q, pcpi_insn_valid)
    _reduce_bool_picorv32_v_2282_786 <= asUInt(neq(asSInt(UInt<32>("h00000000")), SInt<32>(0)))
    _logic_and_picorv32_v_2273_784 <= and(neq(_logic_and_picorv32_v_2273_782_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_2273_783_Y, UInt(0))))
    _eq_picorv32_v_2273_783 <= eq(bits(pcpi_insn, 31, 25), asUInt(UInt<7>("h01")))
    _logic_and_picorv32_v_2273_782 <= and(neq(pcpi_valid, UInt(0)), asUInt(neq(_eq_picorv32_v_2273_781_Y, UInt(0))))
    _eq_picorv32_v_2273_781 <= eq(bits(pcpi_insn, 6, 0), asUInt(UInt<7>("h33")))
    _reduce_or_picorv32_v_2266_780 <= orr(pad(instr_mulh, 1))
    _reduce_or_picorv32_v_2265_779 <= orr(pad(cat(instr_mulh, instr_mulhsu), 1))
    _reduce_or_picorv32_v_2264_778 <= orr(pad(cat(instr_mulh, cat(instr_mulhsu, instr_mulhu)), 1))
    _reduce_or_picorv32_v_2263_777 <= orr(pad(cat(instr_mul, cat(instr_mulh, cat(instr_mulhsu, instr_mulhu))), 1))
    _115 <= _reduce_or_picorv32_v_2263_777_Y
    _116 <= _reduce_or_picorv32_v_2264_778_Y
    _117 <= _reduce_or_picorv32_v_2265_779_Y
    _118 <= _reduce_or_picorv32_v_2266_780_Y
    _119 <= _logic_and_picorv32_v_2273_784_Y
    _120 <= bits(active, 1, 1)
    _121 <= UInt<1>("h0")
    _122 <= bits(active, 1, 1)
    _123 <= bits(_ternary_picorv32_v_2339_810_Y, 31, 0)

    _ternary_picorv32_v_2339_810_Y <= bits(_ternary_picorv32_v_2339_810, 63, 0)
    _ternary_picorv32_v_2339_809_Y <= bits(_ternary_picorv32_v_2339_809, 63, 0)
    _reduce_bool_picorv32_v_2339_808_Y <= bits(_reduce_bool_picorv32_v_2339_808, 0, 0)
    _shr_picorv32_v_2339_807_Y <= bits(_shr_picorv32_v_2339_807, 63, 0)
    _ternary_picorv32_v_2339_806_Y <= bits(_ternary_picorv32_v_2339_806, 63, 0)
    _reduce_bool_picorv32_v_2339_805_Y <= bits(_reduce_bool_picorv32_v_2339_805, 0, 0)
    _logic_not_picorv32_v_2325_804_Y <= bits(_logic_not_picorv32_v_2325_804, 0, 0)
    _logic_and_picorv32_v_2307_803_Y <= bits(_logic_and_picorv32_v_2307_803, 0, 0)
    _logic_not_picorv32_v_2307_802_Y <= bits(_logic_not_picorv32_v_2307_802, 0, 0)
    _ternary_picorv32_v_2307_801_Y <= bits(_ternary_picorv32_v_2307_801, 3, 0)
    _reduce_bool_picorv32_v_2307_799_Y <= bits(_reduce_bool_picorv32_v_2307_799, 0, 0)
    _logic_or_picorv32_v_2301_797_Y <= bits(_logic_or_picorv32_v_2301_797, 0, 0)
    _mul_picorv32_v_2299_796_Y <= bits(_mul_picorv32_v_2299_796, 63, 0)
    _ternary_picorv32_v_2299_795_Y <= bits(_ternary_picorv32_v_2299_795, 32, 0)
    _reduce_bool_picorv32_v_2299_794_Y <= bits(_reduce_bool_picorv32_v_2299_794, 0, 0)
    _ternary_picorv32_v_2299_793_Y <= bits(_ternary_picorv32_v_2299_793, 32, 0)
    _reduce_bool_picorv32_v_2299_792_Y <= bits(_reduce_bool_picorv32_v_2299_792, 0, 0)
    _logic_or_picorv32_v_2298_791_Y <= bits(_logic_or_picorv32_v_2298_791, 0, 0)
    _logic_or_picorv32_v_2294_790_Y <= bits(_logic_or_picorv32_v_2294_790, 0, 0)
    _logic_and_picorv32_v_2282_788_Y <= bits(_logic_and_picorv32_v_2282_788, 0, 0)
    _ternary_picorv32_v_2282_787_Y <= bits(_ternary_picorv32_v_2282_787, 0, 0)
    _reduce_bool_picorv32_v_2282_786_Y <= bits(_reduce_bool_picorv32_v_2282_786, 0, 0)
    _logic_and_picorv32_v_2273_784_Y <= bits(_logic_and_picorv32_v_2273_784, 0, 0)
    _eq_picorv32_v_2273_783_Y <= bits(_eq_picorv32_v_2273_783, 0, 0)
    _logic_and_picorv32_v_2273_782_Y <= bits(_logic_and_picorv32_v_2273_782, 0, 0)
    _eq_picorv32_v_2273_781_Y <= bits(_eq_picorv32_v_2273_781, 0, 0)
    _reduce_or_picorv32_v_2266_780_Y <= bits(_reduce_or_picorv32_v_2266_780, 0, 0)
    _reduce_or_picorv32_v_2265_779_Y <= bits(_reduce_or_picorv32_v_2265_779, 0, 0)
    _reduce_or_picorv32_v_2264_778_Y <= bits(_reduce_or_picorv32_v_2264_778, 0, 0)
    _reduce_or_picorv32_v_2263_777_Y <= bits(_reduce_or_picorv32_v_2263_777, 0, 0)
    pcpi_insn_valid <= bits(_119, 0, 0)
    instr_rs2_signed <= bits(_118, 0, 0)
    instr_rs1_signed <= bits(_117, 0, 0)
    instr_any_mulh <= bits(_116, 0, 0)
    instr_any_mul <= bits(_115, 0, 0)
    pcpi_ready <= bits(_122, 0, 0)
    pcpi_wait <= bits(_121, 0, 0)
    pcpi_rd <= bits(_123, 31, 0)
    pcpi_wr <= bits(_120, 0, 0)
  module picorv32_pcpi_mul:
    output pcpi_ready: UInt<1>
    output pcpi_wait: UInt<1>
    output pcpi_rd: UInt<32>
    output pcpi_wr: UInt<1>
    input pcpi_rs2: UInt<32>
    input pcpi_rs1: UInt<32>
    input pcpi_insn: UInt<32>
    input pcpi_valid: UInt<1>
    input resetn: UInt<1>
    input clk: UInt<1>

    wire _ternary_picorv32_v_2241_776_Y: UInt<64>
    wire _shr_picorv32_v_2241_775_Y: UInt<64>
    wire _logic_and_picorv32_v_2238_774_Y: UInt<1>
    wire _0_pcpi_ready_0_0_: UInt<1>
    wire _0_pcpi_rd_31_0_: UInt<32>
    wire _0_pcpi_wr_0_0_: UInt<1>
    wire _sub_picorv32_v_2227_772_Y: UInt<32>
    wire _logic_not_picorv32_v_2220_771_Y: UInt<1>
    wire _ternary_picorv32_v_2219_770_Y: UInt<32>
    wire _logic_not_picorv32_v_2203_769_Y: UInt<1>
    wire _0_mul_finish_0_0_: UInt<1>
    wire _0_mul_waiting_0_0_: UInt<1>
    wire _0_mul_counter_6_0_: UInt<7>
    wire _0_rdx_63_0_: UInt<64>
    wire _0_rd_63_0_: UInt<64>
    wire _0_rs2_63_0_: UInt<64>
    wire _0_rs1_63_0_: UInt<64>
    wire _shl_picorv32_v_2197_767_Y: UInt<64>
    wire _shr_picorv32_v_2196_766_Y: UInt<64>
    wire _shl_picorv32_v_2194_765_Y: UInt<64>
    wire _add_picorv32_v_2193_764_Y: UInt<5>
    wire _add_picorv32_v_2193_763_Y: UInt<5>
    wire _add_picorv32_v_2193_762_Y: UInt<5>
    wire _add_picorv32_v_2193_761_Y: UInt<5>
    wire _add_picorv32_v_2193_760_Y: UInt<5>
    wire _add_picorv32_v_2193_759_Y: UInt<5>
    wire _add_picorv32_v_2193_758_Y: UInt<5>
    wire _add_picorv32_v_2193_757_Y: UInt<5>
    wire _add_picorv32_v_2193_756_Y: UInt<5>
    wire _add_picorv32_v_2193_755_Y: UInt<5>
    wire _add_picorv32_v_2193_754_Y: UInt<5>
    wire _add_picorv32_v_2193_753_Y: UInt<5>
    wire _add_picorv32_v_2193_752_Y: UInt<5>
    wire _add_picorv32_v_2193_751_Y: UInt<5>
    wire _add_picorv32_v_2193_750_Y: UInt<5>
    wire _add_picorv32_v_2193_749_Y: UInt<5>
    wire _add_picorv32_v_2193_748_Y: UInt<5>
    wire _add_picorv32_v_2193_747_Y: UInt<5>
    wire _add_picorv32_v_2193_746_Y: UInt<5>
    wire _add_picorv32_v_2193_745_Y: UInt<5>
    wire _add_picorv32_v_2193_744_Y: UInt<5>
    wire _add_picorv32_v_2193_743_Y: UInt<5>
    wire _add_picorv32_v_2193_742_Y: UInt<5>
    wire _add_picorv32_v_2193_741_Y: UInt<5>
    wire _add_picorv32_v_2193_740_Y: UInt<5>
    wire _add_picorv32_v_2193_739_Y: UInt<5>
    wire _add_picorv32_v_2193_738_Y: UInt<5>
    wire _add_picorv32_v_2193_737_Y: UInt<5>
    wire _add_picorv32_v_2193_736_Y: UInt<5>
    wire _add_picorv32_v_2193_735_Y: UInt<5>
    wire _add_picorv32_v_2193_734_Y: UInt<5>
    wire _add_picorv32_v_2193_733_Y: UInt<5>
    wire _1_next_rdt_63_0_: UInt<64>
    wire _1_next_rdx_63_0_: UInt<64>
    wire _1_next_rd_63_0_: UInt<64>
    wire _ternary_picorv32_v_2184_732_Y: UInt<64>
    wire _extend_picorv32_v_2184_731_Y: UInt<64>
    wire _0_next_rdt_63_0_: UInt<64>
    wire _0_next_rdx_63_0_: UInt<64>
    wire _0_next_rd_63_0_: UInt<64>
    wire _0_this_rs2_63_0_: UInt<64>
    wire _0_next_rs2_63_0_: UInt<64>
    wire _0_next_rs1_63_0_: UInt<64>
    wire _logic_and_picorv32_v_2155_729_Y: UInt<1>
    wire _eq_picorv32_v_2155_728_Y: UInt<1>
    wire _logic_and_picorv32_v_2155_727_Y: UInt<1>
    wire _eq_picorv32_v_2155_726_Y: UInt<1>
    wire _logic_and_picorv32_v_2155_725_Y: UInt<1>
    wire _0_pcpi_wait_q_0_0_: UInt<1>
    wire _0_instr_mulhu_0_0_: UInt<1>
    wire _0_instr_mulhsu_0_0_: UInt<1>
    wire _0_instr_mulh_0_0_: UInt<1>
    wire _0_instr_mul_0_0_: UInt<1>
    wire _0_pcpi_wait_0_0_: UInt<1>
    wire _logic_and_picorv32_v_2147_723_Y: UInt<1>
    wire _logic_not_picorv32_v_2147_722_Y: UInt<1>
    wire _reduce_or_picorv32_v_2144_721_Y: UInt<1>
    wire _reduce_or_picorv32_v_2143_720_Y: UInt<1>
    wire _reduce_or_picorv32_v_2142_719_Y: UInt<1>
    wire _reduce_or_picorv32_v_2141_718_Y: UInt<1>
    wire j: UInt<32>
    wire i: UInt<32>
    wire mul_finish: UInt<1>
    wire mul_waiting: UInt<1>
    wire mul_counter: UInt<7>
    wire next_rdt: UInt<64>
    wire next_rdx: UInt<64>
    wire next_rd: UInt<64>
    wire this_rs2: UInt<64>
    wire next_rs2: UInt<64>
    wire next_rs1: UInt<64>
    wire rdx: UInt<64>
    wire rd: UInt<64>
    wire rs2: UInt<64>
    wire rs1: UInt<64>
    wire mul_start: UInt<1>
    wire pcpi_wait_q: UInt<1>
    wire instr_rs2_signed: UInt<1>
    wire instr_rs1_signed: UInt<1>
    wire instr_any_mulh: UInt<1>
    wire instr_any_mul: UInt<1>
    wire instr_mulhu: UInt<1>
    wire instr_mulhsu: UInt<1>
    wire instr_mulh: UInt<1>
    wire instr_mul: UInt<1>
    wire _ternary_picorv32_v_2241_776: UInt<64>
    wire _shr_picorv32_v_2241_775: UInt<64>
    wire _logic_and_picorv32_v_2238_774: UInt<1>
    wire _sub_picorv32_v_2227_772: UInt<32>
    wire _logic_not_picorv32_v_2220_771: UInt<1>
    wire _ternary_picorv32_v_2219_770: UInt<32>
    wire _logic_not_picorv32_v_2203_769: UInt<1>
    wire _shl_picorv32_v_2197_767: UInt<64>
    wire _shr_picorv32_v_2196_766: UInt<64>
    wire _shl_picorv32_v_2194_765: UInt<64>
    wire _add_picorv32_v_2193_764: UInt<5>
    wire _add_picorv32_v_2193_763: UInt<5>
    wire _add_picorv32_v_2193_762: UInt<5>
    wire _add_picorv32_v_2193_761: UInt<5>
    wire _add_picorv32_v_2193_760: UInt<5>
    wire _add_picorv32_v_2193_759: UInt<5>
    wire _add_picorv32_v_2193_758: UInt<5>
    wire _add_picorv32_v_2193_757: UInt<5>
    wire _add_picorv32_v_2193_756: UInt<5>
    wire _add_picorv32_v_2193_755: UInt<5>
    wire _add_picorv32_v_2193_754: UInt<5>
    wire _add_picorv32_v_2193_753: UInt<5>
    wire _add_picorv32_v_2193_752: UInt<5>
    wire _add_picorv32_v_2193_751: UInt<5>
    wire _add_picorv32_v_2193_750: UInt<5>
    wire _add_picorv32_v_2193_749: UInt<5>
    wire _add_picorv32_v_2193_748: UInt<5>
    wire _add_picorv32_v_2193_747: UInt<5>
    wire _add_picorv32_v_2193_746: UInt<5>
    wire _add_picorv32_v_2193_745: UInt<5>
    wire _add_picorv32_v_2193_744: UInt<5>
    wire _add_picorv32_v_2193_743: UInt<5>
    wire _add_picorv32_v_2193_742: UInt<5>
    wire _add_picorv32_v_2193_741: UInt<5>
    wire _add_picorv32_v_2193_740: UInt<5>
    wire _add_picorv32_v_2193_739: UInt<5>
    wire _add_picorv32_v_2193_738: UInt<5>
    wire _add_picorv32_v_2193_737: UInt<5>
    wire _add_picorv32_v_2193_736: UInt<5>
    wire _add_picorv32_v_2193_735: UInt<5>
    wire _add_picorv32_v_2193_734: UInt<5>
    wire _add_picorv32_v_2193_733: UInt<5>
    wire _ternary_picorv32_v_2184_732: UInt<64>
    wire _logic_and_picorv32_v_2155_729: UInt<1>
    wire _eq_picorv32_v_2155_728: UInt<1>
    wire _logic_and_picorv32_v_2155_727: UInt<1>
    wire _eq_picorv32_v_2155_726: UInt<1>
    wire _logic_and_picorv32_v_2155_725: UInt<1>
    wire _logic_and_picorv32_v_2147_723: UInt<1>
    wire _logic_not_picorv32_v_2147_722: UInt<1>
    wire _reduce_or_picorv32_v_2144_721: UInt<1>
    wire _reduce_or_picorv32_v_2143_720: UInt<1>
    wire _reduce_or_picorv32_v_2142_719: UInt<1>
    wire _reduce_or_picorv32_v_2141_718: UInt<1>
    wire _159: UInt<1>
    wire _160: UInt<1>
    wire _161: UInt<1>
    wire _162: UInt<1>
    wire _163: UInt<1>
    _0_pcpi_ready_0_0_ is invalid
    _0_pcpi_rd_31_0_ is invalid
    _0_pcpi_wr_0_0_ is invalid
    _0_mul_finish_0_0_ is invalid
    _0_mul_waiting_0_0_ is invalid
    _0_mul_counter_6_0_ is invalid
    _0_rdx_63_0_ is invalid
    _0_rd_63_0_ is invalid
    _0_rs2_63_0_ is invalid
    _0_rs1_63_0_ is invalid
    _1_next_rdt_63_0_ is invalid
    _1_next_rdx_63_0_ is invalid
    _1_next_rd_63_0_ is invalid
    _extend_picorv32_v_2184_731_Y is invalid
    _0_next_rdt_63_0_ is invalid
    _0_next_rdx_63_0_ is invalid
    _0_next_rd_63_0_ is invalid
    _0_this_rs2_63_0_ is invalid
    _0_next_rs2_63_0_ is invalid
    _0_next_rs1_63_0_ is invalid
    _0_pcpi_wait_q_0_0_ is invalid
    _0_instr_mulhu_0_0_ is invalid
    _0_instr_mulhsu_0_0_ is invalid
    _0_instr_mulh_0_0_ is invalid
    _0_instr_mul_0_0_ is invalid
    _0_pcpi_wait_0_0_ is invalid
    j is invalid
    i is invalid
    mul_finish is invalid
    mul_waiting is invalid
    mul_counter is invalid
    next_rdt is invalid
    next_rdx is invalid
    next_rd is invalid
    this_rs2 is invalid
    next_rs2 is invalid
    next_rs1 is invalid
    rdx is invalid
    rd is invalid
    rs2 is invalid
    rs1 is invalid
    pcpi_wait_q is invalid
    instr_mulhu is invalid
    instr_mulhsu is invalid
    instr_mulh is invalid
    instr_mul is invalid
    pcpi_ready is invalid
    pcpi_wait is invalid
    pcpi_rd is invalid
    pcpi_wr is invalid

    _ternary_picorv32_v_2241_776 <= mux(instr_any_mulh, _shr_picorv32_v_2241_775_Y, rd)
    _shr_picorv32_v_2241_775 <= shr(rd, UInt<32>("h00000020"))
    _logic_and_picorv32_v_2238_774 <= and(neq(mul_finish, UInt(0)), asUInt(neq(resetn, UInt(0))))
    _sub_picorv32_v_2227_772 <= asUInt(sub(mul_counter, asUInt(UInt<32>("h00000001"))))
    _logic_not_picorv32_v_2220_771 <= eq(mul_start, UInt(0))
    _ternary_picorv32_v_2219_770 <= mux(instr_any_mulh, UInt<32>("h0000003e"), UInt<32>("h0000001e"))
    _logic_not_picorv32_v_2203_769 <= eq(resetn, UInt(0))
    _shl_picorv32_v_2197_767 <= bits(shl(rs2, UInt<32>("h00000001")), 63, 0)
    _shr_picorv32_v_2196_766 <= shr(rs1, UInt<32>("h00000001"))
    _shl_picorv32_v_2194_765 <= bits(shl(cat(bits(_add_picorv32_v_2193_764_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_762_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_760_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_758_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_756_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_754_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_752_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_750_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_748_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_746_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_744_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_742_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_740_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_738_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_736_Y, 4, 4), cat(UInt<3>("h0"), cat(bits(_add_picorv32_v_2193_734_Y, 4, 4), UInt<3>("h0")))))))))))))))))))))))))))))))), UInt<32>("h00000001")), 63, 0)
    _add_picorv32_v_2193_764 <= add(_add_picorv32_v_2193_763_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 63, 60)))
    _add_picorv32_v_2193_763 <= add(bits(rd, 63, 60), asUInt(bits(rdx, 63, 60)))
    _add_picorv32_v_2193_762 <= add(_add_picorv32_v_2193_761_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 59, 56)))
    _add_picorv32_v_2193_761 <= add(bits(rd, 59, 56), asUInt(bits(rdx, 59, 56)))
    _add_picorv32_v_2193_760 <= add(_add_picorv32_v_2193_759_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 55, 52)))
    _add_picorv32_v_2193_759 <= add(bits(rd, 55, 52), asUInt(bits(rdx, 55, 52)))
    _add_picorv32_v_2193_758 <= add(_add_picorv32_v_2193_757_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 51, 48)))
    _add_picorv32_v_2193_757 <= add(bits(rd, 51, 48), asUInt(bits(rdx, 51, 48)))
    _add_picorv32_v_2193_756 <= add(_add_picorv32_v_2193_755_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 47, 44)))
    _add_picorv32_v_2193_755 <= add(bits(rd, 47, 44), asUInt(bits(rdx, 47, 44)))
    _add_picorv32_v_2193_754 <= add(_add_picorv32_v_2193_753_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 43, 40)))
    _add_picorv32_v_2193_753 <= add(bits(rd, 43, 40), asUInt(bits(rdx, 43, 40)))
    _add_picorv32_v_2193_752 <= add(_add_picorv32_v_2193_751_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 39, 36)))
    _add_picorv32_v_2193_751 <= add(bits(rd, 39, 36), asUInt(bits(rdx, 39, 36)))
    _add_picorv32_v_2193_750 <= add(_add_picorv32_v_2193_749_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 35, 32)))
    _add_picorv32_v_2193_749 <= add(bits(rd, 35, 32), asUInt(bits(rdx, 35, 32)))
    _add_picorv32_v_2193_748 <= add(_add_picorv32_v_2193_747_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 31, 28)))
    _add_picorv32_v_2193_747 <= add(bits(rd, 31, 28), asUInt(bits(rdx, 31, 28)))
    _add_picorv32_v_2193_746 <= add(_add_picorv32_v_2193_745_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 27, 24)))
    _add_picorv32_v_2193_745 <= add(bits(rd, 27, 24), asUInt(bits(rdx, 27, 24)))
    _add_picorv32_v_2193_744 <= add(_add_picorv32_v_2193_743_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 23, 20)))
    _add_picorv32_v_2193_743 <= add(bits(rd, 23, 20), asUInt(bits(rdx, 23, 20)))
    _add_picorv32_v_2193_742 <= add(_add_picorv32_v_2193_741_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 19, 16)))
    _add_picorv32_v_2193_741 <= add(bits(rd, 19, 16), asUInt(bits(rdx, 19, 16)))
    _add_picorv32_v_2193_740 <= add(_add_picorv32_v_2193_739_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 15, 12)))
    _add_picorv32_v_2193_739 <= add(bits(rd, 15, 12), asUInt(bits(rdx, 15, 12)))
    _add_picorv32_v_2193_738 <= add(_add_picorv32_v_2193_737_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 11, 8)))
    _add_picorv32_v_2193_737 <= add(bits(rd, 11, 8), asUInt(bits(rdx, 11, 8)))
    _add_picorv32_v_2193_736 <= add(_add_picorv32_v_2193_735_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 7, 4)))
    _add_picorv32_v_2193_735 <= add(bits(rd, 7, 4), asUInt(bits(rdx, 7, 4)))
    _add_picorv32_v_2193_734 <= add(_add_picorv32_v_2193_733_Y, asUInt(bits(_ternary_picorv32_v_2184_732_Y, 3, 0)))
    _add_picorv32_v_2193_733 <= add(bits(rd, 3, 0), asUInt(bits(rdx, 3, 0)))
    _ternary_picorv32_v_2184_732 <= mux(bits(rs1, 0, 0), rs2, _extend_picorv32_v_2184_731_Y)
    _logic_and_picorv32_v_2155_729 <= and(neq(_logic_and_picorv32_v_2155_727_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_2155_728_Y, UInt(0))))
    _eq_picorv32_v_2155_728 <= eq(bits(pcpi_insn, 31, 25), asUInt(UInt<7>("h01")))
    _logic_and_picorv32_v_2155_727 <= and(neq(_logic_and_picorv32_v_2155_725_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_2155_726_Y, UInt(0))))
    _eq_picorv32_v_2155_726 <= eq(bits(pcpi_insn, 6, 0), asUInt(UInt<7>("h33")))
    _logic_and_picorv32_v_2155_725 <= and(neq(resetn, UInt(0)), asUInt(neq(pcpi_valid, UInt(0))))
    _logic_and_picorv32_v_2147_723 <= and(neq(pcpi_wait, UInt(0)), asUInt(neq(_logic_not_picorv32_v_2147_722_Y, UInt(0))))
    _logic_not_picorv32_v_2147_722 <= eq(pcpi_wait_q, UInt(0))
    _reduce_or_picorv32_v_2144_721 <= orr(pad(instr_mulh, 1))
    _reduce_or_picorv32_v_2143_720 <= orr(pad(cat(instr_mulh, instr_mulhsu), 1))
    _reduce_or_picorv32_v_2142_719 <= orr(pad(cat(instr_mulh, cat(instr_mulhsu, instr_mulhu)), 1))
    _reduce_or_picorv32_v_2141_718 <= orr(pad(cat(instr_mul, cat(instr_mulh, cat(instr_mulhsu, instr_mulhu))), 1))
    _159 <= _reduce_or_picorv32_v_2141_718_Y
    _160 <= _reduce_or_picorv32_v_2142_719_Y
    _161 <= _reduce_or_picorv32_v_2143_720_Y
    _162 <= _reduce_or_picorv32_v_2144_721_Y
    _163 <= _logic_and_picorv32_v_2147_723_Y

    _ternary_picorv32_v_2241_776_Y <= bits(_ternary_picorv32_v_2241_776, 63, 0)
    _shr_picorv32_v_2241_775_Y <= bits(_shr_picorv32_v_2241_775, 63, 0)
    _logic_and_picorv32_v_2238_774_Y <= bits(_logic_and_picorv32_v_2238_774, 0, 0)
    _sub_picorv32_v_2227_772_Y <= bits(_sub_picorv32_v_2227_772, 31, 0)
    _logic_not_picorv32_v_2220_771_Y <= bits(_logic_not_picorv32_v_2220_771, 0, 0)
    _ternary_picorv32_v_2219_770_Y <= bits(_ternary_picorv32_v_2219_770, 31, 0)
    _logic_not_picorv32_v_2203_769_Y <= bits(_logic_not_picorv32_v_2203_769, 0, 0)
    _shl_picorv32_v_2197_767_Y <= bits(_shl_picorv32_v_2197_767, 63, 0)
    _shr_picorv32_v_2196_766_Y <= bits(_shr_picorv32_v_2196_766, 63, 0)
    _shl_picorv32_v_2194_765_Y <= bits(_shl_picorv32_v_2194_765, 63, 0)
    _add_picorv32_v_2193_764_Y <= bits(_add_picorv32_v_2193_764, 4, 0)
    _add_picorv32_v_2193_763_Y <= bits(_add_picorv32_v_2193_763, 4, 0)
    _add_picorv32_v_2193_762_Y <= bits(_add_picorv32_v_2193_762, 4, 0)
    _add_picorv32_v_2193_761_Y <= bits(_add_picorv32_v_2193_761, 4, 0)
    _add_picorv32_v_2193_760_Y <= bits(_add_picorv32_v_2193_760, 4, 0)
    _add_picorv32_v_2193_759_Y <= bits(_add_picorv32_v_2193_759, 4, 0)
    _add_picorv32_v_2193_758_Y <= bits(_add_picorv32_v_2193_758, 4, 0)
    _add_picorv32_v_2193_757_Y <= bits(_add_picorv32_v_2193_757, 4, 0)
    _add_picorv32_v_2193_756_Y <= bits(_add_picorv32_v_2193_756, 4, 0)
    _add_picorv32_v_2193_755_Y <= bits(_add_picorv32_v_2193_755, 4, 0)
    _add_picorv32_v_2193_754_Y <= bits(_add_picorv32_v_2193_754, 4, 0)
    _add_picorv32_v_2193_753_Y <= bits(_add_picorv32_v_2193_753, 4, 0)
    _add_picorv32_v_2193_752_Y <= bits(_add_picorv32_v_2193_752, 4, 0)
    _add_picorv32_v_2193_751_Y <= bits(_add_picorv32_v_2193_751, 4, 0)
    _add_picorv32_v_2193_750_Y <= bits(_add_picorv32_v_2193_750, 4, 0)
    _add_picorv32_v_2193_749_Y <= bits(_add_picorv32_v_2193_749, 4, 0)
    _add_picorv32_v_2193_748_Y <= bits(_add_picorv32_v_2193_748, 4, 0)
    _add_picorv32_v_2193_747_Y <= bits(_add_picorv32_v_2193_747, 4, 0)
    _add_picorv32_v_2193_746_Y <= bits(_add_picorv32_v_2193_746, 4, 0)
    _add_picorv32_v_2193_745_Y <= bits(_add_picorv32_v_2193_745, 4, 0)
    _add_picorv32_v_2193_744_Y <= bits(_add_picorv32_v_2193_744, 4, 0)
    _add_picorv32_v_2193_743_Y <= bits(_add_picorv32_v_2193_743, 4, 0)
    _add_picorv32_v_2193_742_Y <= bits(_add_picorv32_v_2193_742, 4, 0)
    _add_picorv32_v_2193_741_Y <= bits(_add_picorv32_v_2193_741, 4, 0)
    _add_picorv32_v_2193_740_Y <= bits(_add_picorv32_v_2193_740, 4, 0)
    _add_picorv32_v_2193_739_Y <= bits(_add_picorv32_v_2193_739, 4, 0)
    _add_picorv32_v_2193_738_Y <= bits(_add_picorv32_v_2193_738, 4, 0)
    _add_picorv32_v_2193_737_Y <= bits(_add_picorv32_v_2193_737, 4, 0)
    _add_picorv32_v_2193_736_Y <= bits(_add_picorv32_v_2193_736, 4, 0)
    _add_picorv32_v_2193_735_Y <= bits(_add_picorv32_v_2193_735, 4, 0)
    _add_picorv32_v_2193_734_Y <= bits(_add_picorv32_v_2193_734, 4, 0)
    _add_picorv32_v_2193_733_Y <= bits(_add_picorv32_v_2193_733, 4, 0)
    _ternary_picorv32_v_2184_732_Y <= bits(_ternary_picorv32_v_2184_732, 63, 0)
    _logic_and_picorv32_v_2155_729_Y <= bits(_logic_and_picorv32_v_2155_729, 0, 0)
    _eq_picorv32_v_2155_728_Y <= bits(_eq_picorv32_v_2155_728, 0, 0)
    _logic_and_picorv32_v_2155_727_Y <= bits(_logic_and_picorv32_v_2155_727, 0, 0)
    _eq_picorv32_v_2155_726_Y <= bits(_eq_picorv32_v_2155_726, 0, 0)
    _logic_and_picorv32_v_2155_725_Y <= bits(_logic_and_picorv32_v_2155_725, 0, 0)
    _logic_and_picorv32_v_2147_723_Y <= bits(_logic_and_picorv32_v_2147_723, 0, 0)
    _logic_not_picorv32_v_2147_722_Y <= bits(_logic_not_picorv32_v_2147_722, 0, 0)
    _reduce_or_picorv32_v_2144_721_Y <= bits(_reduce_or_picorv32_v_2144_721, 0, 0)
    _reduce_or_picorv32_v_2143_720_Y <= bits(_reduce_or_picorv32_v_2143_720, 0, 0)
    _reduce_or_picorv32_v_2142_719_Y <= bits(_reduce_or_picorv32_v_2142_719, 0, 0)
    _reduce_or_picorv32_v_2141_718_Y <= bits(_reduce_or_picorv32_v_2141_718, 0, 0)
    mul_start <= bits(_163, 0, 0)
    instr_rs2_signed <= bits(_162, 0, 0)
    instr_rs1_signed <= bits(_161, 0, 0)
    instr_any_mulh <= bits(_160, 0, 0)
    instr_any_mul <= bits(_159, 0, 0)
  module picorv32_regs:
    output rdata2: UInt<32>
    output rdata1: UInt<32>
    input wdata: UInt<32>
    input raddr2: UInt<6>
    input raddr1: UInt<6>
    input waddr: UInt<6>
    input wen: UInt<1>
    input clk: UInt<1>

    wire _not_picorv32_v_2117_699_Y: UInt<5>
    wire _memrd__regs_picorv32_v_2117_698_DATA: UInt<32>
    wire _not_picorv32_v_2116_697_Y: UInt<5>
    wire _memrd__regs_picorv32_v_2116_696_DATA: UInt<32>
    wire _not_picorv32_v_2114_695_Y: UInt<5>
    wire _0_memwr__regs_picorv32_v_2114_690_EN_31_0__694: UInt<32>
    wire _0_memwr__regs_picorv32_v_2114_690_DATA_31_0__693: UInt<32>
    wire _0_memwr__regs_picorv32_v_2114_690_ADDR_4_0__692: UInt<5>
    wire _memwr__regs_picorv32_v_2114_690_EN: UInt<32>
    wire _memwr__regs_picorv32_v_2114_690_DATA: UInt<32>
    wire _memwr__regs_picorv32_v_2114_690_ADDR: UInt<5>
    wire _not_picorv32_v_2117_699: UInt<5>
    wire _not_picorv32_v_2116_697: UInt<5>
    wire _not_picorv32_v_2114_695: UInt<5>
    wire _214: UInt<32>
    wire _215: UInt<32>
    _memrd__regs_picorv32_v_2117_698_DATA is invalid
    _memrd__regs_picorv32_v_2116_696_DATA is invalid
    _0_memwr__regs_picorv32_v_2114_690_EN_31_0__694 is invalid
    _0_memwr__regs_picorv32_v_2114_690_DATA_31_0__693 is invalid
    _0_memwr__regs_picorv32_v_2114_690_ADDR_4_0__692 is invalid
    _memwr__regs_picorv32_v_2114_690_EN is invalid
    _memwr__regs_picorv32_v_2114_690_DATA is invalid
    _memwr__regs_picorv32_v_2114_690_ADDR is invalid

    _not_picorv32_v_2117_699 <= not(pad(bits(raddr2, 4, 0), 5))
    _not_picorv32_v_2116_697 <= not(pad(bits(raddr1, 4, 0), 5))
    _not_picorv32_v_2114_695 <= not(pad(bits(waddr, 4, 0), 5))
    _214 <= _memrd__regs_picorv32_v_2116_696_DATA
    _215 <= _memrd__regs_picorv32_v_2117_698_DATA

    _not_picorv32_v_2117_699_Y <= bits(_not_picorv32_v_2117_699, 4, 0)
    _not_picorv32_v_2116_697_Y <= bits(_not_picorv32_v_2116_697, 4, 0)
    _not_picorv32_v_2114_695_Y <= bits(_not_picorv32_v_2114_695, 4, 0)
    rdata2 <= bits(_215, 31, 0)
    rdata1 <= bits(_214, 31, 0)
  module picorv32:
    output trace_data: UInt<36>
    output trace_valid: UInt<1>
    output eoi: UInt<32>
    input irq: UInt<32>
    input pcpi_ready: UInt<1>
    input pcpi_wait: UInt<1>
    input pcpi_rd: UInt<32>
    input pcpi_wr: UInt<1>
    output pcpi_rs2: UInt<32>
    output pcpi_rs1: UInt<32>
    output pcpi_insn: UInt<32>
    output pcpi_valid: UInt<1>
    output mem_la_wstrb: UInt<4>
    output mem_la_wdata: UInt<32>
    output mem_la_addr: UInt<32>
    output mem_la_write: UInt<1>
    output mem_la_read: UInt<1>
    input mem_rdata: UInt<32>
    output mem_wstrb: UInt<4>
    output mem_wdata: UInt<32>
    output mem_addr: UInt<32>
    input mem_ready: UInt<1>
    output mem_instr: UInt<1>
    output mem_valid: UInt<1>
    output trap: UInt<1>
    input resetn: UInt<1>
    input clk: UInt<1>

    wire _sshr_picorv32_v_1223_687_Y: UInt<33>
    wire _ternary_picorv32_v_1223_686_Y: UInt<1>
    wire _logic_or_picorv32_v_1223_685_Y: UInt<1>
    wire _shl_picorv32_v_1222_684_Y: UInt<32>
    wire _lt_picorv32_v_1221_683_Y: UInt<1>
    wire _lt_picorv32_v_1220_682_Y: UInt<1>
    wire _eq_picorv32_v_1219_681_Y: UInt<1>
    wire _ternary_picorv32_v_1218_680_Y: UInt<32>
    wire _add_picorv32_v_1218_679_Y: UInt<32>
    wire _sub_picorv32_v_1218_678_Y: UInt<32>
    wire _0_alu_lts_0_0_: UInt<1>
    wire _0_alu_ltu_0_0_: UInt<1>
    wire _0_alu_eq_0_0_: UInt<1>
    wire _0_alu_shr_31_0_: UInt<32>
    wire _0_alu_shl_31_0_: UInt<32>
    wire _0_alu_add_sub_31_0_: UInt<32>
    wire _and_picorv32_v_1934_676_Y: UInt<32>
    wire _logic_or_picorv32_v_1920_675_Y: UInt<1>
    wire _logic_not_picorv32_v_1920_674_Y: UInt<1>
    wire _logic_and_picorv32_v_1916_673_Y: UInt<1>
    wire _logic_and_picorv32_v_1916_672_Y: UInt<1>
    wire _logic_not_picorv32_v_1916_671_Y: UInt<1>
    wire _logic_and_picorv32_v_1916_670_Y: UInt<1>
    wire _19_next_irq_pending_2_2_: UInt<1>
    wire _logic_and_picorv32_v_1911_669_Y: UInt<1>
    wire _logic_not_picorv32_v_1911_668_Y: UInt<1>
    wire _logic_and_picorv32_v_1911_667_Y: UInt<1>
    wire _logic_not_picorv32_v_1911_666_Y: UInt<1>
    wire _18_next_irq_pending_2_2_: UInt<1>
    wire _logic_and_picorv32_v_1909_665_Y: UInt<1>
    wire _ternary_picorv32_v_1909_664_Y: UInt<1>
    wire _reduce_or_picorv32_v_1909_663_Y: UInt<1>
    wire _logic_and_picorv32_v_1909_662_Y: UInt<1>
    wire _logic_and_picorv32_v_1909_661_Y: UInt<1>
    wire _17_next_irq_pending_2_2_: UInt<1>
    wire _logic_and_picorv32_v_1903_660_Y: UInt<1>
    wire _logic_not_picorv32_v_1903_659_Y: UInt<1>
    wire _logic_and_picorv32_v_1903_658_Y: UInt<1>
    wire _logic_not_picorv32_v_1903_657_Y: UInt<1>
    wire _16_next_irq_pending_2_2_: UInt<1>
    wire _logic_and_picorv32_v_1901_656_Y: UInt<1>
    wire _ne_picorv32_v_1901_655_Y: UInt<1>
    wire _eq_picorv32_v_1901_654_Y: UInt<1>
    wire _15_next_irq_pending_2_2_: UInt<1>
    wire _logic_and_picorv32_v_1896_653_Y: UInt<1>
    wire _logic_not_picorv32_v_1896_652_Y: UInt<1>
    wire _logic_and_picorv32_v_1896_651_Y: UInt<1>
    wire _logic_not_picorv32_v_1896_650_Y: UInt<1>
    wire _14_next_irq_pending_2_2_: UInt<1>
    wire _logic_and_picorv32_v_1894_649_Y: UInt<1>
    wire _ne_picorv32_v_1894_648_Y: UInt<1>
    wire _eq_picorv32_v_1894_647_Y: UInt<1>
    wire _13_next_irq_pending_2_2_: UInt<1>
    wire _logic_and_picorv32_v_1893_646_Y: UInt<1>
    wire _logic_or_picorv32_v_1893_645_Y: UInt<1>
    wire _logic_and_picorv32_v_1893_644_Y: UInt<1>
    wire _logic_and_picorv32_v_1878_643_Y: UInt<1>
    wire _logic_not_picorv32_v_1878_642_Y: UInt<1>
    wire _add_picorv32_v_1875_641_Y: UInt<32>
    wire _logic_or_picorv32_v_1865_640_Y: UInt<1>
    wire _logic_or_picorv32_v_1864_639_Y: UInt<1>
    wire _4_set_mem_do_rdata_0_0_: UInt<1>
    wire _logic_not_picorv32_v_1861_638_Y: UInt<1>
    wire _3_set_mem_do_rdata_0_0_: UInt<1>
    wire _logic_or_picorv32_v_1860_637_Y: UInt<1>
    wire _logic_not_picorv32_v_1860_636_Y: UInt<1>
    wire _logic_and_picorv32_v_1850_635_Y: UInt<1>
    wire _logic_not_picorv32_v_1850_634_Y: UInt<1>
    wire _add_picorv32_v_1847_633_Y: UInt<32>
    wire _4_set_mem_do_wdata_0_0_: UInt<1>
    wire _logic_not_picorv32_v_1836_632_Y: UInt<1>
    wire _3_set_mem_do_wdata_0_0_: UInt<1>
    wire _logic_or_picorv32_v_1835_631_Y: UInt<1>
    wire _logic_not_picorv32_v_1835_630_Y: UInt<1>
    wire _sub_picorv32_v_1828_629_Y: UInt<32>
    wire _sshr_picorv32_v_1826_628_Y: UInt<32>
    wire _logic_or_picorv32_v_1826_627_Y: UInt<1>
    wire _shr_picorv32_v_1825_626_Y: UInt<32>
    wire _logic_or_picorv32_v_1825_625_Y: UInt<1>
    wire _shl_picorv32_v_1824_624_Y: UInt<32>
    wire _logic_or_picorv32_v_1824_623_Y: UInt<1>
    wire _sub_picorv32_v_1820_622_Y: UInt<32>
    wire _sshr_picorv32_v_1818_621_Y: UInt<32>
    wire _logic_or_picorv32_v_1818_620_Y: UInt<1>
    wire _shr_picorv32_v_1817_619_Y: UInt<32>
    wire _logic_or_picorv32_v_1817_618_Y: UInt<1>
    wire _shl_picorv32_v_1816_617_Y: UInt<32>
    wire _logic_or_picorv32_v_1816_616_Y: UInt<1>
    wire _logic_and_picorv32_v_1813_615_Y: UInt<1>
    wire _ge_picorv32_v_1813_614_Y: UInt<1>
    wire _eq_picorv32_v_1809_613_Y: UInt<1>
    wire _5_set_mem_do_rinst_0_0_: UInt<1>
    wire _ternary_picorv32_v_1795_612_Y: UInt<1>
    wire _ternary_picorv32_v_1792_611_Y: UInt<1>
    wire _ternary_picorv32_v_1791_610_Y: UInt<1>
    wire _4_set_mem_do_rinst_0_0_: UInt<1>
    wire _logic_and_picorv32_v_1786_609_Y: UInt<1>
    wire _logic_not_picorv32_v_1786_608_Y: UInt<1>
    wire _3_set_mem_do_rinst_0_0_: UInt<1>
    wire _logic_and_picorv32_v_1785_607_Y: UInt<1>
    wire _logic_or_picorv32_v_1785_606_Y: UInt<1>
    wire _add_picorv32_v_1784_605_Y: UInt<32>
    wire _logic_and_picorv32_v_1774_604_Y: UInt<1>
    wire _logic_and_picorv32_v_1774_603_Y: UInt<1>
    wire _logic_or_picorv32_v_1773_602_Y: UInt<1>
    wire _logic_and_picorv32_v_1773_601_Y: UInt<1>
    wire _logic_and_picorv32_v_1769_600_Y: UInt<1>
    wire _12_next_irq_pending_1_1_: UInt<1>
    wire _logic_and_picorv32_v_1758_599_Y: UInt<1>
    wire _logic_not_picorv32_v_1758_598_Y: UInt<1>
    wire _logic_and_picorv32_v_1758_597_Y: UInt<1>
    wire _logic_not_picorv32_v_1758_596_Y: UInt<1>
    wire _11_next_irq_pending_1_1_: UInt<1>
    wire _logic_and_picorv32_v_1755_595_Y: UInt<1>
    wire _logic_or_picorv32_v_1755_594_Y: UInt<1>
    wire _10_next_irq_pending_1_1_: UInt<1>
    wire _logic_and_picorv32_v_1746_593_Y: UInt<1>
    wire _9_next_irq_pending_1_1_: UInt<1>
    wire _logic_and_picorv32_v_1724_592_Y: UInt<1>
    wire _logic_and_picorv32_v_1724_591_Y: UInt<1>
    wire _logic_or_picorv32_v_1723_590_Y: UInt<1>
    wire _logic_and_picorv32_v_1723_589_Y: UInt<1>
    wire _logic_and_picorv32_v_1719_588_Y: UInt<1>
    wire _ternary_picorv32_v_1695_587_Y: UInt<32>
    wire _extend_picorv32_v_1695_586_Y: UInt<32>
    wire _logic_and_picorv32_v_1695_585_Y: UInt<1>
    wire _logic_and_picorv32_v_1690_584_Y: UInt<1>
    wire _logic_and_picorv32_v_1682_583_Y: UInt<1>
    wire _logic_and_picorv32_v_1674_582_Y: UInt<1>
    wire _logic_not_picorv32_v_1674_581_Y: UInt<1>
    wire _logic_and_picorv32_v_1665_580_Y: UInt<1>
    wire _or_picorv32_v_1660_579_Y: UInt<32>
    wire _logic_and_picorv32_v_1656_578_Y: UInt<1>
    wire _ternary_picorv32_v_1651_577_Y: UInt<32>
    wire _and_picorv32_v_1651_576_Y: UInt<32>
    wire _logic_and_picorv32_v_1645_575_Y: UInt<1>
    wire _or_picorv32_v_1641_574_Y: UInt<32>
    wire _logic_and_picorv32_v_1636_573_Y: UInt<1>
    wire _logic_and_picorv32_v_1628_572_Y: UInt<1>
    wire _ternary_picorv32_v_1620_571_Y: UInt<32>
    wire _logic_and_picorv32_v_1613_570_Y: UInt<1>
    wire _logic_and_picorv32_v_1609_569_Y: UInt<1>
    wire _logic_and_picorv32_v_1604_568_Y: UInt<1>
    wire _8_next_irq_pending_1_1_: UInt<1>
    wire _logic_and_picorv32_v_1597_567_Y: UInt<1>
    wire _logic_not_picorv32_v_1597_566_Y: UInt<1>
    wire _logic_and_picorv32_v_1597_565_Y: UInt<1>
    wire _logic_not_picorv32_v_1597_564_Y: UInt<1>
    wire _7_next_irq_pending_1_1_: UInt<1>
    wire _logic_and_picorv32_v_1563_563_Y: UInt<1>
    wire _6_next_irq_pending_1_1_: UInt<1>
    wire _logic_and_picorv32_v_1551_562_Y: UInt<1>
    wire _logic_not_picorv32_v_1551_561_Y: UInt<1>
    wire _logic_not_picorv32_v_1551_560_Y: UInt<1>
    wire _add_picorv32_v_1547_559_Y: UInt<32>
    wire _add_picorv32_v_1542_558_Y: UInt<64>
    wire _add_picorv32_v_1538_557_Y: UInt<32>
    wire _ternary_picorv32_v_1538_556_Y: UInt<32>
    wire _add_picorv32_v_1530_555_Y: UInt<32>
    wire _ternary_picorv32_v_1530_554_Y: UInt<32>
    wire _reduce_bool_picorv32_v_1527_553_Y: UInt<1>
    wire _logic_and_picorv32_v_1526_552_Y: UInt<1>
    wire _logic_and_picorv32_v_1526_551_Y: UInt<1>
    wire _logic_or_picorv32_v_1526_550_Y: UInt<1>
    wire _or_picorv32_v_1522_549_Y: UInt<32>
    wire _ternary_picorv32_v_1519_548_Y: UInt<2>
    wire _ternary_picorv32_v_1519_547_Y: UInt<2>
    wire _eq_picorv32_v_1519_546_Y: UInt<1>
    wire _eq_picorv32_v_1518_545_Y: UInt<1>
    wire _logic_and_picorv32_v_1516_544_Y: UInt<1>
    wire _logic_or_picorv32_v_1516_543_Y: UInt<1>
    wire _logic_and_picorv32_v_1516_542_Y: UInt<1>
    wire _reduce_or_picorv32_v_1516_541_Y: UInt<1>
    wire _and_picorv32_v_1516_540_Y: UInt<32>
    wire _not_picorv32_v_1516_539_Y: UInt<32>
    wire _logic_and_picorv32_v_1516_538_Y: UInt<1>
    wire _logic_not_picorv32_v_1516_537_Y: UInt<1>
    wire _logic_and_picorv32_v_1516_536_Y: UInt<1>
    wire _logic_not_picorv32_v_1516_535_Y: UInt<1>
    wire _or_picorv32_v_1501_534_Y: UInt<36>
    wire _ternary_picorv32_v_1501_533_Y: UInt<32>
    wire _ternary_picorv32_v_1501_532_Y: UInt<36>
    wire _extend_picorv32_v_1501_531_Y: UInt<36>
    wire _or_picorv32_v_1499_530_Y: UInt<36>
    wire _and_picorv32_v_1499_529_Y: UInt<36>
    wire _or_picorv32_v_1499_528_Y: UInt<36>
    wire _ternary_picorv32_v_1499_527_Y: UInt<36>
    wire _extend_picorv32_v_1499_526_Y: UInt<36>
    wire _logic_and_picorv32_v_1495_525_Y: UInt<1>
    wire _and_picorv32_v_1491_524_Y: UInt<32>
    wire _and_picorv32_v_1490_523_Y: UInt<32>
    wire _not_picorv32_v_1490_522_Y: UInt<32>
    wire _logic_and_picorv32_v_1489_521_Y: UInt<1>
    wire _logic_and_picorv32_v_1484_520_Y: UInt<1>
    wire _logic_and_picorv32_v_1481_519_Y: UInt<1>
    wire _logic_not_picorv32_v_1481_518_Y: UInt<1>
    wire _ternary_picorv32_v_1478_517_Y: UInt<32>
    wire _and_picorv32_v_1478_516_Y: UInt<32>
    wire _ternary_picorv32_v_1478_515_Y: UInt<32>
    wire _5_next_irq_pending_31_0_: UInt<32>
    wire _3_current_pc_31_0_: UInt<32>
    wire _logic_and_picorv32_v_1470_514_Y: UInt<1>
    wire _logic_not_picorv32_v_1470_513_Y: UInt<1>
    wire _logic_not_picorv32_v_1470_512_Y: UInt<1>
    wire _4_next_irq_pending_31_0_: UInt<32>
    wire _2_current_pc_31_0_: UInt<32>
    wire _2_set_mem_do_wdata_0_0_: UInt<1>
    wire _2_set_mem_do_rdata_0_0_: UInt<1>
    wire _2_set_mem_do_rinst_0_0_: UInt<1>
    wire _3_next_irq_pending_31_0_: UInt<32>
    wire _1_current_pc_31_0_: UInt<32>
    wire _1_set_mem_do_wdata_0_0_: UInt<1>
    wire _1_set_mem_do_rdata_0_0_: UInt<1>
    wire _1_set_mem_do_rinst_0_0_: UInt<1>
    wire _logic_not_picorv32_v_1435_511_Y: UInt<1>
    wire _logic_and_picorv32_v_1424_510_Y: UInt<1>
    wire _sub_picorv32_v_1417_509_Y: UInt<32>
    wire _2_next_irq_pending_0_0_: UInt<1>
    wire _eq_picorv32_v_1415_508_Y: UInt<1>
    wire _sub_picorv32_v_1415_507_Y: UInt<32>
    wire _1_next_irq_pending_0_0_: UInt<1>
    wire _logic_and_picorv32_v_1414_506_Y: UInt<1>
    wire _ternary_picorv32_v_1405_505_Y: UInt<64>
    wire _extend_picorv32_v_1405_504_Y: UInt<64>
    wire _add_picorv32_v_1405_503_Y: UInt<64>
    wire _0_alu_wait_2_0_0_: UInt<1>
    wire _0_alu_wait_0_0_: UInt<1>
    wire _0_alu_out_0_q_0_0_: UInt<1>
    wire _0_alu_out_q_31_0_: UInt<32>
    wire _0_do_waitirq_0_0_: UInt<1>
    wire _0_next_irq_pending_31_0_: UInt<32>
    wire _0_pcpi_timeout_0_0_: UInt<1>
    wire _0_current_pc_31_0_: UInt<32>
    wire _0_latched_rd_4_0_: UInt<5>
    wire _0_latched_is_lb_0_0_: UInt<1>
    wire _0_latched_is_lh_0_0_: UInt<1>
    wire _0_latched_is_lu_0_0_: UInt<1>
    wire _0_latched_trace_0_0_: UInt<1>
    wire _0_latched_compr_0_0_: UInt<1>
    wire _0_latched_branch_0_0_: UInt<1>
    wire _0_latched_stalu_0_0_: UInt<1>
    wire _0_latched_store_0_0_: UInt<1>
    wire _0_set_mem_do_wdata_0_0_: UInt<1>
    wire _0_set_mem_do_rdata_0_0_: UInt<1>
    wire _0_set_mem_do_rinst_0_0_: UInt<1>
    wire _0_irq_state_1_0_: UInt<2>
    wire _0_cpu_state_7_0_: UInt<8>
    wire _0_dbg_rs2val_valid_0_0_: UInt<1>
    wire _0_dbg_rs1val_valid_0_0_: UInt<1>
    wire _0_dbg_rs2val_31_0_: UInt<32>
    wire _0_dbg_rs1val_31_0_: UInt<32>
    wire _0_decoder_pseudo_trigger_q_0_0_: UInt<1>
    wire _0_decoder_pseudo_trigger_0_0_: UInt<1>
    wire _0_decoder_trigger_q_0_0_: UInt<1>
    wire _0_decoder_trigger_0_0_: UInt<1>
    wire _0_mem_do_wdata_0_0_: UInt<1>
    wire _0_mem_do_rdata_0_0_: UInt<1>
    wire _0_mem_do_rinst_0_0_: UInt<1>
    wire _0_mem_do_prefetch_0_0_: UInt<1>
    wire _0_mem_wordsize_1_0_: UInt<2>
    wire _0_timer_31_0_: UInt<32>
    wire _0_irq_pending_31_0_: UInt<32>
    wire _0_irq_mask_31_0_: UInt<32>
    wire _0_irq_active_0_0_: UInt<1>
    wire _0_irq_delay_0_0_: UInt<1>
    wire _0_reg_sh_4_0_: UInt<5>
    wire _0_reg_out_31_0_: UInt<32>
    wire _0_reg_op2_31_0_: UInt<32>
    wire _0_reg_op1_31_0_: UInt<32>
    wire _0_reg_next_pc_31_0_: UInt<32>
    wire _0_reg_pc_31_0_: UInt<32>
    wire _0_count_instr_63_0_: UInt<64>
    wire _0_count_cycle_63_0_: UInt<64>
    wire _0_trace_data_35_0_: UInt<36>
    wire _0_trace_valid_0_0_: UInt<1>
    wire _0_eoi_31_0_: UInt<32>
    wire _0_pcpi_valid_0_0_: UInt<1>
    wire _0_trap_0_0_: UInt<1>
    wire _logic_and_picorv32_v_1372_501_Y: UInt<1>
    wire _logic_or_picorv32_v_1372_500_Y: UInt<1>
    wire _logic_not_picorv32_v_1372_499_Y: UInt<1>
    wire _and_picorv32_v_1372_498_Y: UInt<32>
    wire _not_picorv32_v_1372_497_Y: UInt<32>
    wire _logic_or_picorv32_v_1372_496_Y: UInt<1>
    wire _logic_or_picorv32_v_1372_495_Y: UInt<1>
    wire _logic_and_picorv32_v_1372_494_Y: UInt<1>
    wire _eq_picorv32_v_1372_493_Y: UInt<1>
    wire _ternary_picorv32_v_1325_492_Y: UInt<32>
    wire _reduce_bool_picorv32_v_1325_491_Y: UInt<1>
    wire _memrd__cpuregs_picorv32_v_1325_490_DATA: UInt<32>
    wire _ternary_picorv32_v_1324_489_Y: UInt<32>
    wire _reduce_bool_picorv32_v_1324_488_Y: UInt<1>
    wire _memrd__cpuregs_picorv32_v_1324_487_DATA: UInt<32>
    wire _1_cpuregs_rs2_31_0_: UInt<32>
    wire _1_cpuregs_rs1_31_0_: UInt<32>
    wire _0_decoded_rs_4_0_: UInt<5>
    wire _0_cpuregs_rs2_31_0_: UInt<32>
    wire _0_cpuregs_rs1_31_0_: UInt<32>
    wire _logic_and_picorv32_v_1316_485_Y: UInt<1>
    wire _logic_and_picorv32_v_1316_484_Y: UInt<1>
    wire _0_memwr__cpuregs_picorv32_v_1317_14_EN_31_0__483: UInt<32>
    wire _0_memwr__cpuregs_picorv32_v_1317_14_DATA_31_0__482: UInt<32>
    wire _0_memwr__cpuregs_picorv32_v_1317_14_ADDR_4_0__481: UInt<5>
    wire _and_picorv32_v_1307_479_Y: UInt<32>
    wire _not_picorv32_v_1307_478_Y: UInt<32>
    wire _logic_and_picorv32_v_1306_477_Y: UInt<1>
    wire _or_picorv32_v_1303_476_Y: UInt<32>
    wire _logic_and_picorv32_v_1302_475_Y: UInt<1>
    wire _ternary_picorv32_v_1299_474_Y: UInt<32>
    wire _logic_and_picorv32_v_1298_473_Y: UInt<1>
    wire _logic_not_picorv32_v_1298_472_Y: UInt<1>
    wire _add_picorv32_v_1295_471_Y: UInt<32>
    wire _ternary_picorv32_v_1295_470_Y: UInt<32>
    wire _2_cpuregs_wrdata_31_0_: UInt<32>
    wire _2_cpuregs_write_0_0_: UInt<1>
    wire _1_cpuregs_wrdata_31_0_: UInt<32>
    wire _1_cpuregs_write_0_0_: UInt<1>
    wire _eq_picorv32_v_1291_469_Y: UInt<1>
    wire _0_cpuregs_wrdata_31_0_: UInt<32>
    wire _0_cpuregs_write_0_0_: UInt<1>
    wire _2_clear_prefetched_high_word_0_0_: UInt<1>
    wire _logic_or_picorv32_v_1277_467_Y: UInt<1>
    wire _logic_not_picorv32_v_1277_466_Y: UInt<1>
    wire _logic_or_picorv32_v_1277_465_Y: UInt<1>
    wire _1_clear_prefetched_high_word_0_0_: UInt<1>
    wire _logic_not_picorv32_v_1275_464_Y: UInt<1>
    wire _0_clear_prefetched_high_word_0_0_: UInt<1>
    wire _0_clear_prefetched_high_word_q_0_0_: UInt<1>
    wire _logic_and_picorv32_v_1260_461_Y: UInt<1>
    wire _logic_or_picorv32_v_1260_460_Y: UInt<1>
    wire _logic_or_picorv32_v_1260_459_Y: UInt<1>
    wire _logic_or_picorv32_v_1260_458_Y: UInt<1>
    wire _logic_and_picorv32_v_1258_457_Y: UInt<1>
    wire _logic_or_picorv32_v_1258_456_Y: UInt<1>
    wire _and_picorv32_v_1257_455_Y: UInt<32>
    wire _logic_or_picorv32_v_1256_454_Y: UInt<1>
    wire _or_picorv32_v_1255_453_Y: UInt<32>
    wire _logic_or_picorv32_v_1254_452_Y: UInt<1>
    wire _xor_picorv32_v_1253_451_Y: UInt<32>
    wire _logic_or_picorv32_v_1252_450_Y: UInt<1>
    wire _1_alu_out_31_0_: UInt<32>
    wire _logic_and_picorv32_v_1241_449_Y: UInt<1>
    wire _logic_or_picorv32_v_1241_448_Y: UInt<1>
    wire _logic_not_picorv32_v_1241_447_Y: UInt<1>
    wire _logic_and_picorv32_v_1239_446_Y: UInt<1>
    wire _logic_or_picorv32_v_1239_445_Y: UInt<1>
    wire _logic_not_picorv32_v_1239_444_Y: UInt<1>
    wire _logic_not_picorv32_v_1238_443_Y: UInt<1>
    wire _logic_not_picorv32_v_1236_442_Y: UInt<1>
    wire _logic_not_picorv32_v_1234_441_Y: UInt<1>
    wire _1_alu_out_0_0_0_: UInt<1>
    wire _0_alu_out_0_0_0_: UInt<1>
    wire _0_alu_out_31_0_: UInt<32>
    wire _ternary_picorv32_v_1191_439_Y: UInt<32>
    wire _and_picorv32_v_1191_438_Y: UInt<32>
    wire _logic_and_picorv32_v_1191_437_Y: UInt<1>
    wire _8_dbg_ascii_state_127_0_: UInt<128>
    wire _eq_picorv32_v_1173_436_Y: UInt<1>
    wire _7_dbg_ascii_state_127_0_: UInt<128>
    wire _eq_picorv32_v_1172_435_Y: UInt<1>
    wire _6_dbg_ascii_state_127_0_: UInt<128>
    wire _eq_picorv32_v_1171_434_Y: UInt<1>
    wire _5_dbg_ascii_state_127_0_: UInt<128>
    wire _eq_picorv32_v_1170_433_Y: UInt<1>
    wire _4_dbg_ascii_state_127_0_: UInt<128>
    wire _eq_picorv32_v_1169_432_Y: UInt<1>
    wire _3_dbg_ascii_state_127_0_: UInt<128>
    wire _eq_picorv32_v_1168_431_Y: UInt<1>
    wire _2_dbg_ascii_state_127_0_: UInt<128>
    wire _eq_picorv32_v_1167_430_Y: UInt<1>
    wire _1_dbg_ascii_state_127_0_: UInt<128>
    wire _eq_picorv32_v_1166_429_Y: UInt<1>
    wire _0_dbg_ascii_state_127_0_: UInt<128>
    wire _logic_not_picorv32_v_1116_427_Y: UInt<1>
    wire _reduce_or_picorv32_v_1105_426_Y: UInt<1>
    wire _shl_picorv32_v_1104_425_Y: UInt<32>
    wire _reduce_or_picorv32_v_1103_424_Y: UInt<1>
    wire _logic_and_picorv32_v_1094_423_Y: UInt<1>
    wire _reduce_or_picorv32_v_1094_422_Y: UInt<1>
    wire _logic_and_picorv32_v_1091_421_Y: UInt<1>
    wire _eq_picorv32_v_1091_420_Y: UInt<1>
    wire _eq_picorv32_v_1091_419_Y: UInt<1>
    wire _logic_and_picorv32_v_1092_418_Y: UInt<1>
    wire _eq_picorv32_v_1092_417_Y: UInt<1>
    wire _eq_picorv32_v_1092_416_Y: UInt<1>
    wire _logic_and_picorv32_v_1094_415_Y: UInt<1>
    wire _eq_picorv32_v_1094_414_Y: UInt<1>
    wire _eq_picorv32_v_1093_413_Y: UInt<1>
    wire _logic_or_picorv32_v_1088_412_Y: UInt<1>
    wire _logic_and_picorv32_v_1088_411_Y: UInt<1>
    wire _reduce_or_picorv32_v_1088_410_Y: UInt<1>
    wire _eq_picorv32_v_1082_409_Y: UInt<1>
    wire _eq_picorv32_v_1083_408_Y: UInt<1>
    wire _eq_picorv32_v_1084_407_Y: UInt<1>
    wire _eq_picorv32_v_1085_406_Y: UInt<1>
    wire _eq_picorv32_v_1086_405_Y: UInt<1>
    wire _eq_picorv32_v_1088_404_Y: UInt<1>
    wire _logic_and_picorv32_v_1079_403_Y: UInt<1>
    wire _reduce_or_picorv32_v_1079_402_Y: UInt<1>
    wire _logic_and_picorv32_v_1076_401_Y: UInt<1>
    wire _eq_picorv32_v_1076_400_Y: UInt<1>
    wire _eq_picorv32_v_1076_399_Y: UInt<1>
    wire _logic_and_picorv32_v_1077_398_Y: UInt<1>
    wire _eq_picorv32_v_1077_397_Y: UInt<1>
    wire _eq_picorv32_v_1077_396_Y: UInt<1>
    wire _logic_and_picorv32_v_1079_395_Y: UInt<1>
    wire _eq_picorv32_v_1079_394_Y: UInt<1>
    wire _eq_picorv32_v_1078_393_Y: UInt<1>
    wire _logic_and_picorv32_v_1073_392_Y: UInt<1>
    wire _logic_and_picorv32_v_1073_391_Y: UInt<1>
    wire _logic_and_picorv32_v_1073_390_Y: UInt<1>
    wire _eq_picorv32_v_1073_389_Y: UInt<1>
    wire _eq_picorv32_v_1073_388_Y: UInt<1>
    wire _logic_and_picorv32_v_1072_387_Y: UInt<1>
    wire _logic_and_picorv32_v_1072_386_Y: UInt<1>
    wire _eq_picorv32_v_1072_385_Y: UInt<1>
    wire _eq_picorv32_v_1072_384_Y: UInt<1>
    wire _logic_and_picorv32_v_1071_383_Y: UInt<1>
    wire _logic_and_picorv32_v_1071_382_Y: UInt<1>
    wire _logic_and_picorv32_v_1071_381_Y: UInt<1>
    wire _eq_picorv32_v_1071_380_Y: UInt<1>
    wire _eq_picorv32_v_1071_379_Y: UInt<1>
    wire _logic_and_picorv32_v_1070_378_Y: UInt<1>
    wire _logic_and_picorv32_v_1070_377_Y: UInt<1>
    wire _logic_and_picorv32_v_1070_376_Y: UInt<1>
    wire _eq_picorv32_v_1070_375_Y: UInt<1>
    wire _eq_picorv32_v_1070_374_Y: UInt<1>
    wire _logic_or_picorv32_v_1068_373_Y: UInt<1>
    wire _logic_and_picorv32_v_1068_372_Y: UInt<1>
    wire _eq_picorv32_v_1068_371_Y: UInt<1>
    wire _logic_and_picorv32_v_1067_370_Y: UInt<1>
    wire _logic_not_picorv32_v_1067_369_Y: UInt<1>
    wire _logic_and_picorv32_v_1067_368_Y: UInt<1>
    wire _logic_not_picorv32_v_1067_367_Y: UInt<1>
    wire _eq_picorv32_v_1067_366_Y: UInt<1>
    wire _logic_and_picorv32_v_1065_365_Y: UInt<1>
    wire _logic_and_picorv32_v_1065_364_Y: UInt<1>
    wire _logic_and_picorv32_v_1065_363_Y: UInt<1>
    wire _eq_picorv32_v_1065_362_Y: UInt<1>
    wire _eq_picorv32_v_1065_361_Y: UInt<1>
    wire _logic_and_picorv32_v_1064_360_Y: UInt<1>
    wire _logic_and_picorv32_v_1064_359_Y: UInt<1>
    wire _eq_picorv32_v_1064_358_Y: UInt<1>
    wire _eq_picorv32_v_1064_357_Y: UInt<1>
    wire _logic_and_picorv32_v_1063_356_Y: UInt<1>
    wire _logic_and_picorv32_v_1063_355_Y: UInt<1>
    wire _logic_or_picorv32_v_1063_354_Y: UInt<1>
    wire _logic_and_picorv32_v_1063_353_Y: UInt<1>
    wire _eq_picorv32_v_1063_352_Y: UInt<1>
    wire _eq_picorv32_v_1063_351_Y: UInt<1>
    wire _logic_and_picorv32_v_1062_350_Y: UInt<1>
    wire _eq_picorv32_v_1062_349_Y: UInt<1>
    wire _eq_picorv32_v_1062_348_Y: UInt<1>
    wire _logic_and_picorv32_v_1061_347_Y: UInt<1>
    wire _logic_or_picorv32_v_1061_346_Y: UInt<1>
    wire _logic_and_picorv32_v_1061_345_Y: UInt<1>
    wire _eq_picorv32_v_1061_344_Y: UInt<1>
    wire _eq_picorv32_v_1061_343_Y: UInt<1>
    wire _logic_and_picorv32_v_1060_342_Y: UInt<1>
    wire _eq_picorv32_v_1060_341_Y: UInt<1>
    wire _eq_picorv32_v_1060_340_Y: UInt<1>
    wire _logic_and_picorv32_v_1058_339_Y: UInt<1>
    wire _eq_picorv32_v_1058_338_Y: UInt<1>
    wire _logic_and_picorv32_v_1058_337_Y: UInt<1>
    wire _eq_picorv32_v_1058_336_Y: UInt<1>
    wire _logic_and_picorv32_v_1057_335_Y: UInt<1>
    wire _eq_picorv32_v_1057_334_Y: UInt<1>
    wire _logic_and_picorv32_v_1057_333_Y: UInt<1>
    wire _eq_picorv32_v_1057_332_Y: UInt<1>
    wire _logic_and_picorv32_v_1056_331_Y: UInt<1>
    wire _eq_picorv32_v_1056_330_Y: UInt<1>
    wire _logic_and_picorv32_v_1056_329_Y: UInt<1>
    wire _eq_picorv32_v_1056_328_Y: UInt<1>
    wire _logic_and_picorv32_v_1055_327_Y: UInt<1>
    wire _eq_picorv32_v_1055_326_Y: UInt<1>
    wire _logic_and_picorv32_v_1055_325_Y: UInt<1>
    wire _eq_picorv32_v_1055_324_Y: UInt<1>
    wire _logic_and_picorv32_v_1054_323_Y: UInt<1>
    wire _eq_picorv32_v_1054_322_Y: UInt<1>
    wire _logic_and_picorv32_v_1054_321_Y: UInt<1>
    wire _eq_picorv32_v_1054_320_Y: UInt<1>
    wire _logic_and_picorv32_v_1053_319_Y: UInt<1>
    wire _eq_picorv32_v_1053_318_Y: UInt<1>
    wire _logic_and_picorv32_v_1053_317_Y: UInt<1>
    wire _eq_picorv32_v_1053_316_Y: UInt<1>
    wire _logic_and_picorv32_v_1052_315_Y: UInt<1>
    wire _eq_picorv32_v_1052_314_Y: UInt<1>
    wire _logic_and_picorv32_v_1052_313_Y: UInt<1>
    wire _eq_picorv32_v_1052_312_Y: UInt<1>
    wire _logic_and_picorv32_v_1051_311_Y: UInt<1>
    wire _eq_picorv32_v_1051_310_Y: UInt<1>
    wire _logic_and_picorv32_v_1051_309_Y: UInt<1>
    wire _eq_picorv32_v_1051_308_Y: UInt<1>
    wire _logic_and_picorv32_v_1050_307_Y: UInt<1>
    wire _eq_picorv32_v_1050_306_Y: UInt<1>
    wire _logic_and_picorv32_v_1050_305_Y: UInt<1>
    wire _eq_picorv32_v_1050_304_Y: UInt<1>
    wire _logic_and_picorv32_v_1049_303_Y: UInt<1>
    wire _eq_picorv32_v_1049_302_Y: UInt<1>
    wire _logic_and_picorv32_v_1049_301_Y: UInt<1>
    wire _eq_picorv32_v_1049_300_Y: UInt<1>
    wire _logic_and_picorv32_v_1047_299_Y: UInt<1>
    wire _eq_picorv32_v_1047_298_Y: UInt<1>
    wire _logic_and_picorv32_v_1047_297_Y: UInt<1>
    wire _eq_picorv32_v_1047_296_Y: UInt<1>
    wire _logic_and_picorv32_v_1046_295_Y: UInt<1>
    wire _eq_picorv32_v_1046_294_Y: UInt<1>
    wire _logic_and_picorv32_v_1046_293_Y: UInt<1>
    wire _eq_picorv32_v_1046_292_Y: UInt<1>
    wire _logic_and_picorv32_v_1045_291_Y: UInt<1>
    wire _eq_picorv32_v_1045_290_Y: UInt<1>
    wire _logic_and_picorv32_v_1045_289_Y: UInt<1>
    wire _eq_picorv32_v_1045_288_Y: UInt<1>
    wire _logic_and_picorv32_v_1043_287_Y: UInt<1>
    wire _eq_picorv32_v_1043_286_Y: UInt<1>
    wire _logic_and_picorv32_v_1042_285_Y: UInt<1>
    wire _eq_picorv32_v_1042_284_Y: UInt<1>
    wire _logic_and_picorv32_v_1041_283_Y: UInt<1>
    wire _eq_picorv32_v_1041_282_Y: UInt<1>
    wire _logic_and_picorv32_v_1040_281_Y: UInt<1>
    wire _eq_picorv32_v_1040_280_Y: UInt<1>
    wire _logic_and_picorv32_v_1039_279_Y: UInt<1>
    wire _eq_picorv32_v_1039_278_Y: UInt<1>
    wire _logic_and_picorv32_v_1038_277_Y: UInt<1>
    wire _eq_picorv32_v_1038_276_Y: UInt<1>
    wire _logic_and_picorv32_v_1036_275_Y: UInt<1>
    wire _eq_picorv32_v_1036_274_Y: UInt<1>
    wire _logic_and_picorv32_v_1035_273_Y: UInt<1>
    wire _eq_picorv32_v_1035_272_Y: UInt<1>
    wire _logic_and_picorv32_v_1034_271_Y: UInt<1>
    wire _eq_picorv32_v_1034_270_Y: UInt<1>
    wire _logic_and_picorv32_v_1032_269_Y: UInt<1>
    wire _eq_picorv32_v_1032_268_Y: UInt<1>
    wire _logic_and_picorv32_v_1031_267_Y: UInt<1>
    wire _eq_picorv32_v_1031_266_Y: UInt<1>
    wire _logic_and_picorv32_v_1030_265_Y: UInt<1>
    wire _eq_picorv32_v_1030_264_Y: UInt<1>
    wire _logic_and_picorv32_v_1029_263_Y: UInt<1>
    wire _eq_picorv32_v_1029_262_Y: UInt<1>
    wire _logic_and_picorv32_v_1028_261_Y: UInt<1>
    wire _eq_picorv32_v_1028_260_Y: UInt<1>
    wire _logic_and_picorv32_v_1026_259_Y: UInt<1>
    wire _eq_picorv32_v_1026_258_Y: UInt<1>
    wire _logic_and_picorv32_v_1025_257_Y: UInt<1>
    wire _eq_picorv32_v_1025_256_Y: UInt<1>
    wire _logic_and_picorv32_v_1024_255_Y: UInt<1>
    wire _eq_picorv32_v_1024_254_Y: UInt<1>
    wire _logic_and_picorv32_v_1023_253_Y: UInt<1>
    wire _eq_picorv32_v_1023_252_Y: UInt<1>
    wire _logic_and_picorv32_v_1022_251_Y: UInt<1>
    wire _eq_picorv32_v_1022_250_Y: UInt<1>
    wire _logic_and_picorv32_v_1021_249_Y: UInt<1>
    wire _eq_picorv32_v_1021_248_Y: UInt<1>
    wire _logic_and_picorv32_v_1018_247_Y: UInt<1>
    wire _logic_not_picorv32_v_1018_246_Y: UInt<1>
    wire _logic_and_picorv32_v_1000_245_Y: UInt<1>
    wire _ne_picorv32_v_1000_244_Y: UInt<1>
    wire _ne_picorv32_v_1000_243_Y: UInt<1>
    wire _logic_and_picorv32_v_995_242_Y: UInt<1>
    wire _eq_picorv32_v_995_241_Y: UInt<1>
    wire _logic_and_picorv32_v_995_240_Y: UInt<1>
    wire _ne_picorv32_v_995_239_Y: UInt<1>
    wire _ne_picorv32_v_995_238_Y: UInt<1>
    wire _logic_and_picorv32_v_989_237_Y: UInt<1>
    wire _ne_picorv32_v_989_236_Y: UInt<1>
    wire _eq_picorv32_v_989_235_Y: UInt<1>
    wire _logic_and_picorv32_v_984_234_Y: UInt<1>
    wire _eq_picorv32_v_984_233_Y: UInt<1>
    wire _logic_and_picorv32_v_984_232_Y: UInt<1>
    wire _ne_picorv32_v_984_231_Y: UInt<1>
    wire _eq_picorv32_v_984_230_Y: UInt<1>
    wire _reduce_bool_picorv32_v_977_229_Y: UInt<1>
    wire _logic_not_picorv32_v_969_228_Y: UInt<1>
    wire _add_picorv32_v_961_227_Y: UInt<32>
    wire _add_picorv32_v_956_226_Y: UInt<32>
    wire _add_picorv32_v_948_225_Y: UInt<32>
    wire _add_picorv32_v_947_224_Y: UInt<32>
    wire _add_picorv32_v_946_223_Y: UInt<32>
    wire _eq_picorv32_v_944_222_Y: UInt<1>
    wire _add_picorv32_v_942_221_Y: UInt<32>
    wire _add_picorv32_v_941_220_Y: UInt<32>
    wire _eq_picorv32_v_939_219_Y: UInt<1>
    wire _add_picorv32_v_936_218_Y: UInt<32>
    wire _add_picorv32_v_935_217_Y: UInt<32>
    wire _logic_and_picorv32_v_933_216_Y: UInt<1>
    wire _logic_not_picorv32_v_933_215_Y: UInt<1>
    wire _logic_not_picorv32_v_933_214_Y: UInt<1>
    wire _eq_picorv32_v_921_213_Y: UInt<1>
    wire _logic_or_picorv32_v_920_212_Y: UInt<1>
    wire _add_picorv32_v_899_211_Y: UInt<32>
    wire _add_picorv32_v_898_210_Y: UInt<32>
    wire _add_picorv32_v_894_209_Y: UInt<32>
    wire _add_picorv32_v_893_208_Y: UInt<32>
    wire _add_picorv32_v_889_207_Y: UInt<32>
    wire _reduce_or_picorv32_v_887_206_Y: UInt<1>
    wire _logic_and_picorv32_v_874_205_Y: UInt<1>
    wire _ne_picorv32_v_874_204_Y: UInt<1>
    wire _logic_and_picorv32_v_870_203_Y: UInt<1>
    wire _logic_and_picorv32_v_870_202_Y: UInt<1>
    wire _eq_picorv32_v_870_201_Y: UInt<1>
    wire _eq_picorv32_v_870_200_Y: UInt<1>
    wire _logic_and_picorv32_v_867_199_Y: UInt<1>
    wire _logic_and_picorv32_v_867_198_Y: UInt<1>
    wire _logic_and_picorv32_v_867_197_Y: UInt<1>
    wire _eq_picorv32_v_867_196_Y: UInt<1>
    wire _eq_picorv32_v_867_195_Y: UInt<1>
    wire _eq_picorv32_v_859_194_Y: UInt<1>
    wire _eq_picorv32_v_858_193_Y: UInt<1>
    wire _eq_picorv32_v_857_192_Y: UInt<1>
    wire _eq_picorv32_v_856_191_Y: UInt<1>
    wire _eq_picorv32_v_855_190_Y: UInt<1>
    wire _logic_and_picorv32_v_853_189_Y: UInt<1>
    wire _logic_and_picorv32_v_853_188_Y: UInt<1>
    wire _eq_picorv32_v_853_187_Y: UInt<1>
    wire _eq_picorv32_v_853_186_Y: UInt<1>
    wire _logic_and_picorv32_v_852_185_Y: UInt<1>
    wire _logic_and_picorv32_v_852_184_Y: UInt<1>
    wire _eq_picorv32_v_852_183_Y: UInt<1>
    wire _eq_picorv32_v_852_182_Y: UInt<1>
    wire _logic_and_picorv32_v_851_181_Y: UInt<1>
    wire _eq_picorv32_v_851_180_Y: UInt<1>
    wire _eq_picorv32_v_851_179_Y: UInt<1>
    wire _eq_picorv32_v_850_178_Y: UInt<1>
    wire _eq_picorv32_v_849_177_Y: UInt<1>
    wire _eq_picorv32_v_848_176_Y: UInt<1>
    wire _logic_and_picorv32_v_847_175_Y: UInt<1>
    wire _reduce_or_picorv32_v_845_174_Y: UInt<1>
    wire _reduce_or_picorv32_v_844_173_Y: UInt<1>
    wire _reduce_or_picorv32_v_843_172_Y: UInt<1>
    wire _reduce_or_picorv32_v_842_171_Y: UInt<1>
    wire _reduce_or_picorv32_v_841_170_Y: UInt<1>
    wire _reduce_or_picorv32_v_840_169_Y: UInt<1>
    wire _0_is_compare_0_0_: UInt<1>
    wire _0_is_alu_reg_reg_0_0_: UInt<1>
    wire _0_is_alu_reg_imm_0_0_: UInt<1>
    wire _0_is_lbu_lhu_lw_0_0_: UInt<1>
    wire _0_is_beq_bne_blt_bge_bltu_bgeu_0_0_: UInt<1>
    wire _0_is_sltiu_bltu_sltu_0_0_: UInt<1>
    wire _0_is_slti_blt_slt_0_0_: UInt<1>
    wire _0_is_lui_auipc_jal_jalr_addi_add_sub_0_0_: UInt<1>
    wire _0_is_sll_srl_sra_0_0_: UInt<1>
    wire _0_is_sb_sh_sw_0_0_: UInt<1>
    wire _0_is_jalr_addi_slti_sltiu_xori_ori_andi_0_0_: UInt<1>
    wire _0_is_slli_srli_srai_0_0_: UInt<1>
    wire _0_is_lb_lh_lw_lbu_lhu_0_0_: UInt<1>
    wire _0_is_lui_auipc_jal_0_0_: UInt<1>
    wire _0_compressed_instr_0_0_: UInt<1>
    wire _0_decoded_imm_uj_31_0_: UInt<32>
    wire _0_decoded_imm_31_0_: UInt<32>
    wire _0_decoded_rs2_4_0_: UInt<5>
    wire _0_decoded_rs1_4_0_: UInt<5>
    wire _0_decoded_rd_4_0_: UInt<5>
    wire _0_instr_timer_0_0_: UInt<1>
    wire _0_instr_waitirq_0_0_: UInt<1>
    wire _0_instr_maskirq_0_0_: UInt<1>
    wire _0_instr_retirq_0_0_: UInt<1>
    wire _0_instr_setq_0_0_: UInt<1>
    wire _0_instr_getq_0_0_: UInt<1>
    wire _0_instr_ecall_ebreak_0_0_: UInt<1>
    wire _0_instr_rdinstrh_0_0_: UInt<1>
    wire _0_instr_rdinstr_0_0_: UInt<1>
    wire _0_instr_rdcycleh_0_0_: UInt<1>
    wire _0_instr_rdcycle_0_0_: UInt<1>
    wire _0_instr_and_0_0_: UInt<1>
    wire _0_instr_or_0_0_: UInt<1>
    wire _0_instr_sra_0_0_: UInt<1>
    wire _0_instr_srl_0_0_: UInt<1>
    wire _0_instr_xor_0_0_: UInt<1>
    wire _0_instr_sltu_0_0_: UInt<1>
    wire _0_instr_slt_0_0_: UInt<1>
    wire _0_instr_sll_0_0_: UInt<1>
    wire _0_instr_sub_0_0_: UInt<1>
    wire _0_instr_add_0_0_: UInt<1>
    wire _0_instr_srai_0_0_: UInt<1>
    wire _0_instr_srli_0_0_: UInt<1>
    wire _0_instr_slli_0_0_: UInt<1>
    wire _0_instr_andi_0_0_: UInt<1>
    wire _0_instr_ori_0_0_: UInt<1>
    wire _0_instr_xori_0_0_: UInt<1>
    wire _0_instr_sltiu_0_0_: UInt<1>
    wire _0_instr_slti_0_0_: UInt<1>
    wire _0_instr_addi_0_0_: UInt<1>
    wire _0_instr_sw_0_0_: UInt<1>
    wire _0_instr_sh_0_0_: UInt<1>
    wire _0_instr_sb_0_0_: UInt<1>
    wire _0_instr_lhu_0_0_: UInt<1>
    wire _0_instr_lbu_0_0_: UInt<1>
    wire _0_instr_lw_0_0_: UInt<1>
    wire _0_instr_lh_0_0_: UInt<1>
    wire _0_instr_lb_0_0_: UInt<1>
    wire _0_instr_bgeu_0_0_: UInt<1>
    wire _0_instr_bltu_0_0_: UInt<1>
    wire _0_instr_bge_0_0_: UInt<1>
    wire _0_instr_blt_0_0_: UInt<1>
    wire _0_instr_bne_0_0_: UInt<1>
    wire _0_instr_beq_0_0_: UInt<1>
    wire _0_instr_jalr_0_0_: UInt<1>
    wire _0_instr_jal_0_0_: UInt<1>
    wire _0_instr_auipc_0_0_: UInt<1>
    wire _0_instr_lui_0_0_: UInt<1>
    wire _0_pcpi_insn_31_0_: UInt<32>
    wire _3_dbg_insn_opcode_31_0_: UInt<32>
    wire _reduce_and_picorv32_v_808_167_Y: UInt<1>
    wire _2_dbg_insn_rd_4_0_: UInt<5>
    wire _2_dbg_insn_rs2_4_0_: UInt<5>
    wire _2_dbg_insn_rs1_4_0_: UInt<5>
    wire _2_dbg_insn_imm_31_0_: UInt<32>
    wire _2_dbg_ascii_instr_63_0_: UInt<64>
    wire _2_dbg_insn_opcode_31_0_: UInt<32>
    wire _1_dbg_insn_rd_4_0_: UInt<5>
    wire _1_dbg_insn_rs2_4_0_: UInt<5>
    wire _1_dbg_insn_rs1_4_0_: UInt<5>
    wire _1_dbg_insn_imm_31_0_: UInt<32>
    wire _1_dbg_ascii_instr_63_0_: UInt<64>
    wire _1_dbg_insn_opcode_31_0_: UInt<32>
    wire _0_dbg_insn_rd_4_0_: UInt<5>
    wire _0_dbg_insn_rs2_4_0_: UInt<5>
    wire _0_dbg_insn_rs1_4_0_: UInt<5>
    wire _0_dbg_insn_imm_31_0_: UInt<32>
    wire _0_dbg_ascii_instr_63_0_: UInt<64>
    wire _0_dbg_insn_opcode_31_0_: UInt<32>
    wire _reduce_and_picorv32_v_776_165_Y: UInt<1>
    wire _logic_or_picorv32_v_768_164_Y: UInt<1>
    wire _logic_not_picorv32_v_768_163_Y: UInt<1>
    wire _0_cached_insn_rd_4_0_: UInt<5>
    wire _0_cached_insn_rs2_4_0_: UInt<5>
    wire _0_cached_insn_rs1_4_0_: UInt<5>
    wire _0_cached_insn_opcode_31_0_: UInt<32>
    wire _0_cached_insn_imm_31_0_: UInt<32>
    wire _0_cached_ascii_instr_63_0_: UInt<64>
    wire _0_dbg_valid_insn_0_0_: UInt<1>
    wire _0_dbg_next_0_0_: UInt<1>
    wire _0_q_insn_rd_4_0_: UInt<5>
    wire _0_q_insn_rs2_4_0_: UInt<5>
    wire _0_q_insn_rs1_4_0_: UInt<5>
    wire _0_q_insn_opcode_31_0_: UInt<32>
    wire _0_q_insn_imm_31_0_: UInt<32>
    wire _0_q_ascii_instr_63_0_: UInt<64>
    wire _0_dbg_insn_addr_31_0_: UInt<32>
    wire _47_new_ascii_instr_63_0_: UInt<64>
    wire _46_new_ascii_instr_63_0_: UInt<64>
    wire _45_new_ascii_instr_63_0_: UInt<64>
    wire _44_new_ascii_instr_63_0_: UInt<64>
    wire _43_new_ascii_instr_63_0_: UInt<64>
    wire _42_new_ascii_instr_63_0_: UInt<64>
    wire _41_new_ascii_instr_63_0_: UInt<64>
    wire _40_new_ascii_instr_63_0_: UInt<64>
    wire _39_new_ascii_instr_63_0_: UInt<64>
    wire _38_new_ascii_instr_63_0_: UInt<64>
    wire _37_new_ascii_instr_63_0_: UInt<64>
    wire _36_new_ascii_instr_63_0_: UInt<64>
    wire _35_new_ascii_instr_63_0_: UInt<64>
    wire _34_new_ascii_instr_63_0_: UInt<64>
    wire _33_new_ascii_instr_63_0_: UInt<64>
    wire _32_new_ascii_instr_63_0_: UInt<64>
    wire _31_new_ascii_instr_63_0_: UInt<64>
    wire _30_new_ascii_instr_63_0_: UInt<64>
    wire _29_new_ascii_instr_63_0_: UInt<64>
    wire _28_new_ascii_instr_63_0_: UInt<64>
    wire _27_new_ascii_instr_63_0_: UInt<64>
    wire _26_new_ascii_instr_63_0_: UInt<64>
    wire _25_new_ascii_instr_63_0_: UInt<64>
    wire _24_new_ascii_instr_63_0_: UInt<64>
    wire _23_new_ascii_instr_63_0_: UInt<64>
    wire _22_new_ascii_instr_63_0_: UInt<64>
    wire _21_new_ascii_instr_63_0_: UInt<64>
    wire _20_new_ascii_instr_63_0_: UInt<64>
    wire _19_new_ascii_instr_63_0_: UInt<64>
    wire _18_new_ascii_instr_63_0_: UInt<64>
    wire _17_new_ascii_instr_63_0_: UInt<64>
    wire _16_new_ascii_instr_63_0_: UInt<64>
    wire _15_new_ascii_instr_63_0_: UInt<64>
    wire _14_new_ascii_instr_63_0_: UInt<64>
    wire _13_new_ascii_instr_63_0_: UInt<64>
    wire _12_new_ascii_instr_63_0_: UInt<64>
    wire _11_new_ascii_instr_63_0_: UInt<64>
    wire _10_new_ascii_instr_63_0_: UInt<64>
    wire _9_new_ascii_instr_63_0_: UInt<64>
    wire _8_new_ascii_instr_63_0_: UInt<64>
    wire _7_new_ascii_instr_63_0_: UInt<64>
    wire _6_new_ascii_instr_63_0_: UInt<64>
    wire _5_new_ascii_instr_63_0_: UInt<64>
    wire _4_new_ascii_instr_63_0_: UInt<64>
    wire _3_new_ascii_instr_63_0_: UInt<64>
    wire _2_new_ascii_instr_63_0_: UInt<64>
    wire _1_new_ascii_instr_63_0_: UInt<64>
    wire _0_new_ascii_instr_63_0_: UInt<64>
    wire _reduce_or_picorv32_v_670_160_Y: UInt<1>
    wire _logic_and_picorv32_v_667_159_Y: UInt<1>
    wire _logic_not_picorv32_v_667_158_Y: UInt<1>
    wire _ternary_picorv32_v_600_157_Y: UInt<32>
    wire _logic_or_picorv32_v_600_156_Y: UInt<1>
    wire _logic_or_picorv32_v_593_155_Y: UInt<1>
    wire _logic_not_picorv32_v_593_154_Y: UInt<1>
    wire _reduce_and_picorv32_v_593_153_Y: UInt<1>
    wire _logic_and_picorv32_v_592_152_Y: UInt<1>
    wire _logic_not_picorv32_v_592_151_Y: UInt<1>
    wire _logic_not_picorv32_v_587_150_Y: UInt<1>
    wire _logic_and_picorv32_v_584_149_Y: UInt<1>
    wire _logic_or_picorv32_v_568_148_Y: UInt<1>
    wire _logic_not_picorv32_v_567_147_Y: UInt<1>
    wire _logic_or_picorv32_v_566_146_Y: UInt<1>
    wire _logic_or_picorv32_v_566_145_Y: UInt<1>
    wire _and_picorv32_v_559_144_Y: UInt<4>
    wire _logic_or_picorv32_v_557_143_Y: UInt<1>
    wire _logic_or_picorv32_v_552_142_Y: UInt<1>
    wire _logic_not_picorv32_v_552_141_Y: UInt<1>
    wire _logic_not_picorv32_v_550_140_Y: UInt<1>
    wire _logic_or_picorv32_v_549_139_Y: UInt<1>
    wire _logic_not_picorv32_v_549_138_Y: UInt<1>
    wire _0_mem_16bit_buffer_15_0_: UInt<16>
    wire _0_prefetched_high_word_0_0_: UInt<1>
    wire _0_mem_la_secondword_0_0_: UInt<1>
    wire _0_mem_state_1_0_: UInt<2>
    wire _0_mem_wstrb_3_0_: UInt<4>
    wire _0_mem_wdata_31_0_: UInt<32>
    wire _0_mem_addr_31_0_: UInt<32>
    wire _0_mem_instr_0_0_: UInt<1>
    wire _0_mem_valid_0_0_: UInt<1>
    wire _logic_or_picorv32_v_543_136_Y: UInt<1>
    wire _eq_picorv32_v_543_135_Y: UInt<1>
    wire _eq_picorv32_v_543_134_Y: UInt<1>
    wire _logic_or_picorv32_v_534_133_Y: UInt<1>
    wire _logic_or_picorv32_v_531_132_Y: UInt<1>
    wire _logic_or_picorv32_v_531_131_Y: UInt<1>
    wire _logic_and_picorv32_v_530_130_Y: UInt<1>
    wire _logic_not_picorv32_v_530_129_Y: UInt<1>
    wire _logic_and_picorv32_v_514_127_Y: UInt<1>
    wire _ne_picorv32_v_514_126_Y: UInt<1>
    wire _ne_picorv32_v_514_125_Y: UInt<1>
    wire _logic_and_picorv32_v_510_124_Y: UInt<1>
    wire _eq_picorv32_v_510_123_Y: UInt<1>
    wire _logic_and_picorv32_v_510_122_Y: UInt<1>
    wire _ne_picorv32_v_510_121_Y: UInt<1>
    wire _ne_picorv32_v_510_120_Y: UInt<1>
    wire _logic_and_picorv32_v_506_119_Y: UInt<1>
    wire _ne_picorv32_v_506_118_Y: UInt<1>
    wire _eq_picorv32_v_506_117_Y: UInt<1>
    wire _logic_and_picorv32_v_502_116_Y: UInt<1>
    wire _eq_picorv32_v_502_115_Y: UInt<1>
    wire _eq_picorv32_v_502_114_Y: UInt<1>
    wire _ternary_picorv32_v_474_113_Y: UInt<7>
    wire _eq_picorv32_v_474_112_Y: UInt<1>
    wire _eq_picorv32_v_473_111_Y: UInt<1>
    wire _eq_picorv32_v_472_110_Y: UInt<1>
    wire _eq_picorv32_v_471_109_Y: UInt<1>
    wire _eq_picorv32_v_470_108_Y: UInt<1>
    wire _eq_picorv32_v_469_107_Y: UInt<1>
    wire _eq_picorv32_v_465_106_Y: UInt<1>
    wire _eq_picorv32_v_461_105_Y: UInt<1>
    wire _eq_picorv32_v_457_104_Y: UInt<1>
    wire _eq_picorv32_v_448_103_Y: UInt<1>
    wire _logic_and_picorv32_v_419_102_Y: UInt<1>
    wire _logic_or_picorv32_v_419_101_Y: UInt<1>
    wire _logic_and_picorv32_v_419_100_Y: UInt<1>
    wire _ternary_picorv32_v_416_99_Y: UInt<32>
    wire _ternary_picorv32_v_415_98_Y: UInt<32>
    wire _0_mem_rdata_q_31_0_: UInt<32>
    wire _0_next_insn_opcode_31_0_: UInt<32>
    wire _3_mem_rdata_word_31_0_: UInt<32>
    wire _shl_picorv32_v_402_96_Y: UInt<4>
    wire _2_mem_rdata_word_31_0_: UInt<32>
    wire _ternary_picorv32_v_394_95_Y: UInt<4>
    wire _1_mem_rdata_word_31_0_: UInt<32>
    wire _1_mem_la_wstrb_3_0_: UInt<4>
    wire _1_mem_la_wdata_31_0_: UInt<32>
    wire _0_mem_rdata_word_31_0_: UInt<32>
    wire _0_mem_la_wstrb_3_0_: UInt<4>
    wire _0_mem_la_wdata_31_0_: UInt<32>
    wire _logic_and_picorv32_v_380_93_Y: UInt<1>
    wire _logic_not_picorv32_v_380_92_Y: UInt<1>
    wire _logic_not_picorv32_v_378_91_Y: UInt<1>
    wire _logic_not_picorv32_v_374_90_Y: UInt<1>
    wire _0_last_mem_valid_0_0_: UInt<1>
    wire _0_mem_la_firstword_reg_0_0_: UInt<1>
    wire _ternary_picorv32_v_371_88_Y: UInt<32>
    wire _ternary_picorv32_v_371_87_Y: UInt<32>
    wire _ternary_picorv32_v_371_86_Y: UInt<32>
    wire _logic_and_picorv32_v_371_85_Y: UInt<1>
    wire _logic_and_picorv32_v_370_84_Y: UInt<1>
    wire _logic_and_picorv32_v_369_83_Y: UInt<1>
    wire _ternary_picorv32_v_367_82_Y: UInt<32>
    wire _logic_or_picorv32_v_367_81_Y: UInt<1>
    wire _ternary_picorv32_v_365_80_Y: UInt<32>
    wire _add_picorv32_v_365_79_Y: UInt<30>
    wire _logic_or_picorv32_v_365_78_Y: UInt<1>
    wire _logic_and_picorv32_v_364_77_Y: UInt<1>
    wire _logic_or_picorv32_v_364_76_Y: UInt<1>
    wire _logic_and_picorv32_v_364_75_Y: UInt<1>
    wire _reduce_and_picorv32_v_364_74_Y: UInt<1>
    wire _logic_and_picorv32_v_364_73_Y: UInt<1>
    wire _logic_not_picorv32_v_364_72_Y: UInt<1>
    wire _logic_and_picorv32_v_364_71_Y: UInt<1>
    wire _ternary_picorv32_v_364_70_Y: UInt<1>
    wire _logic_not_picorv32_v_364_69_Y: UInt<1>
    wire _logic_and_picorv32_v_364_68_Y: UInt<1>
    wire _logic_and_picorv32_v_363_67_Y: UInt<1>
    wire _logic_or_picorv32_v_363_66_Y: UInt<1>
    wire _logic_or_picorv32_v_363_65_Y: UInt<1>
    wire _logic_and_picorv32_v_363_64_Y: UInt<1>
    wire _logic_not_picorv32_v_363_63_Y: UInt<1>
    wire _logic_not_picorv32_v_363_62_Y: UInt<1>
    wire _logic_and_picorv32_v_362_61_Y: UInt<1>
    wire _logic_and_picorv32_v_362_60_Y: UInt<1>
    wire _logic_not_picorv32_v_362_59_Y: UInt<1>
    wire _logic_and_picorv32_v_360_58_Y: UInt<1>
    wire _logic_or_picorv32_v_360_57_Y: UInt<1>
    wire _logic_and_picorv32_v_360_56_Y: UInt<1>
    wire _logic_not_picorv32_v_360_55_Y: UInt<1>
    wire _reduce_and_picorv32_v_360_54_Y: UInt<1>
    wire _logic_not_picorv32_v_360_53_Y: UInt<1>
    wire _logic_and_picorv32_v_359_52_Y: UInt<1>
    wire _logic_or_picorv32_v_359_51_Y: UInt<1>
    wire _logic_and_picorv32_v_359_50_Y: UInt<1>
    wire _reduce_and_picorv32_v_359_49_Y: UInt<1>
    wire _logic_and_picorv32_v_359_48_Y: UInt<1>
    wire _logic_or_picorv32_v_359_47_Y: UInt<1>
    wire _logic_or_picorv32_v_359_46_Y: UInt<1>
    wire _logic_and_picorv32_v_359_45_Y: UInt<1>
    wire _reduce_or_picorv32_v_359_44_Y: UInt<1>
    wire _reduce_or_picorv32_v_358_43_Y: UInt<1>
    wire _logic_or_picorv32_v_356_42_Y: UInt<1>
    wire _logic_and_picorv32_v_356_41_Y: UInt<1>
    wire _logic_and_picorv32_v_356_40_Y: UInt<1>
    wire _logic_and_picorv32_v_355_39_Y: UInt<1>
    wire _logic_not_picorv32_v_355_38_Y: UInt<1>
    wire _logic_and_picorv32_v_355_37_Y: UInt<1>
    wire _logic_and_picorv32_v_355_36_Y: UInt<1>
    wire _logic_and_picorv32_v_346_35_Y: UInt<1>
    wire _ternary_picorv32_v_346_34_Y: UInt<1>
    wire _logic_not_picorv32_v_346_33_Y: UInt<1>
    wire _logic_and_picorv32_v_346_32_Y: UInt<1>
    wire _logic_and_picorv32_v_345_31_Y: UInt<1>
    wire _logic_not_picorv32_v_345_30_Y: UInt<1>
    wire _logic_and_picorv32_v_345_29_Y: UInt<1>
    wire _logic_and_picorv32_v_345_28_Y: UInt<1>
    wire _logic_or_picorv32_v_345_27_Y: UInt<1>
    wire _logic_and_picorv32_v_324_26_Y: UInt<1>
    wire _logic_and_picorv32_v_320_25_Y: UInt<1>
    wire _logic_and_picorv32_v_316_24_Y: UInt<1>
    wire _1_pcpi_int_rd_31_0_: UInt<32>
    wire _1_pcpi_int_wr_0_0_: UInt<1>
    wire _reduce_or_picorv32_v_312_23_Y: UInt<1>
    wire _logic_and_picorv32_v_312_22_Y: UInt<1>
    wire _logic_and_picorv32_v_312_21_Y: UInt<1>
    wire _logic_and_picorv32_v_312_20_Y: UInt<1>
    wire _reduce_or_picorv32_v_311_19_Y: UInt<1>
    wire _logic_and_picorv32_v_311_18_Y: UInt<1>
    wire _logic_and_picorv32_v_311_17_Y: UInt<1>
    wire _logic_and_picorv32_v_311_16_Y: UInt<1>
    wire _0_pcpi_int_ready_0_0_: UInt<1>
    wire _0_pcpi_int_wait_0_0_: UInt<1>
    wire _0_pcpi_int_rd_31_0_: UInt<32>
    wire _0_pcpi_int_wr_0_0_: UInt<1>
    wire _memwr__cpuregs_picorv32_v_1317_14_EN: UInt<32>
    wire _memwr__cpuregs_picorv32_v_1317_14_DATA: UInt<32>
    wire _memwr__cpuregs_picorv32_v_1317_14_ADDR: UInt<5>
    wire decoded_rs: UInt<5>
    wire cpuregs_rs2: UInt<32>
    wire cpuregs_rs1: UInt<32>
    wire cpuregs_wrdata: UInt<32>
    wire cpuregs_write: UInt<1>
    wire clear_prefetched_high_word_q: UInt<1>
    wire alu_lts: UInt<1>
    wire alu_ltu: UInt<1>
    wire alu_eq: UInt<1>
    wire alu_shr: UInt<32>
    wire alu_shl: UInt<32>
    wire alu_add_sub: UInt<32>
    wire alu_wait_2: UInt<1>
    wire alu_wait: UInt<1>
    wire alu_out_0_q: UInt<1>
    wire alu_out_0: UInt<1>
    wire alu_out_q: UInt<32>
    wire alu_out: UInt<32>
    wire do_waitirq: UInt<1>
    wire next_irq_pending: UInt<32>
    wire pcpi_timeout: UInt<1>
    wire pcpi_timeout_counter: UInt<4>
    wire current_pc: UInt<32>
    wire latched_rd: UInt<5>
    wire latched_is_lb: UInt<1>
    wire latched_is_lh: UInt<1>
    wire latched_is_lu: UInt<1>
    wire latched_trace: UInt<1>
    wire latched_compr: UInt<1>
    wire latched_branch: UInt<1>
    wire latched_stalu: UInt<1>
    wire latched_store: UInt<1>
    wire set_mem_do_wdata: UInt<1>
    wire set_mem_do_rdata: UInt<1>
    wire set_mem_do_rinst: UInt<1>
    wire dbg_ascii_state: UInt<128>
    wire irq_state: UInt<2>
    wire cpu_state: UInt<8>
    wire cached_insn_rd: UInt<5>
    wire cached_insn_rs2: UInt<5>
    wire cached_insn_rs1: UInt<5>
    wire cached_insn_opcode: UInt<32>
    wire cached_insn_imm: UInt<32>
    wire cached_ascii_instr: UInt<64>
    wire dbg_valid_insn: UInt<1>
    wire launch_next_insn: UInt<1>
    wire dbg_next: UInt<1>
    wire q_insn_rd: UInt<5>
    wire q_insn_rs2: UInt<5>
    wire q_insn_rs1: UInt<5>
    wire q_insn_opcode: UInt<32>
    wire q_insn_imm: UInt<32>
    wire q_ascii_instr: UInt<64>
    wire dbg_rs2val_valid: UInt<1>
    wire dbg_rs1val_valid: UInt<1>
    wire dbg_rs2val: UInt<32>
    wire dbg_rs1val: UInt<32>
    wire dbg_insn_rd: UInt<5>
    wire dbg_insn_rs2: UInt<5>
    wire dbg_insn_rs1: UInt<5>
    wire dbg_insn_imm: UInt<32>
    wire dbg_ascii_instr: UInt<64>
    wire new_ascii_instr: UInt<64>
    wire is_rdcycle_rdcycleh_rdinstr_rdinstrh: UInt<1>
    wire is_compare: UInt<1>
    wire is_alu_reg_reg: UInt<1>
    wire is_alu_reg_imm: UInt<1>
    wire is_lbu_lhu_lw: UInt<1>
    wire is_beq_bne_blt_bge_bltu_bgeu: UInt<1>
    wire is_sltiu_bltu_sltu: UInt<1>
    wire is_slti_blt_slt: UInt<1>
    wire is_lui_auipc_jal_jalr_addi_add_sub: UInt<1>
    wire is_sll_srl_sra: UInt<1>
    wire is_sb_sh_sw: UInt<1>
    wire is_jalr_addi_slti_sltiu_xori_ori_andi: UInt<1>
    wire is_slli_srli_srai: UInt<1>
    wire is_lb_lh_lw_lbu_lhu: UInt<1>
    wire is_lui_auipc_jal: UInt<1>
    wire compressed_instr: UInt<1>
    wire decoder_pseudo_trigger_q: UInt<1>
    wire decoder_pseudo_trigger: UInt<1>
    wire decoder_trigger_q: UInt<1>
    wire decoder_trigger: UInt<1>
    wire decoded_imm_uj: UInt<32>
    wire decoded_imm: UInt<32>
    wire decoded_rs2: UInt<5>
    wire decoded_rs1: UInt<5>
    wire decoded_rd: UInt<5>
    wire instr_trap: UInt<1>
    wire instr_timer: UInt<1>
    wire instr_waitirq: UInt<1>
    wire instr_maskirq: UInt<1>
    wire instr_retirq: UInt<1>
    wire instr_setq: UInt<1>
    wire instr_getq: UInt<1>
    wire instr_ecall_ebreak: UInt<1>
    wire instr_rdinstrh: UInt<1>
    wire instr_rdinstr: UInt<1>
    wire instr_rdcycleh: UInt<1>
    wire instr_rdcycle: UInt<1>
    wire instr_and: UInt<1>
    wire instr_or: UInt<1>
    wire instr_sra: UInt<1>
    wire instr_srl: UInt<1>
    wire instr_xor: UInt<1>
    wire instr_sltu: UInt<1>
    wire instr_slt: UInt<1>
    wire instr_sll: UInt<1>
    wire instr_sub: UInt<1>
    wire instr_add: UInt<1>
    wire instr_srai: UInt<1>
    wire instr_srli: UInt<1>
    wire instr_slli: UInt<1>
    wire instr_andi: UInt<1>
    wire instr_ori: UInt<1>
    wire instr_xori: UInt<1>
    wire instr_sltiu: UInt<1>
    wire instr_slti: UInt<1>
    wire instr_addi: UInt<1>
    wire instr_sw: UInt<1>
    wire instr_sh: UInt<1>
    wire instr_sb: UInt<1>
    wire instr_lhu: UInt<1>
    wire instr_lbu: UInt<1>
    wire instr_lw: UInt<1>
    wire instr_lh: UInt<1>
    wire instr_lb: UInt<1>
    wire instr_bgeu: UInt<1>
    wire instr_bltu: UInt<1>
    wire instr_bge: UInt<1>
    wire instr_blt: UInt<1>
    wire instr_bne: UInt<1>
    wire instr_beq: UInt<1>
    wire instr_jalr: UInt<1>
    wire instr_jal: UInt<1>
    wire instr_auipc: UInt<1>
    wire instr_lui: UInt<1>
    wire mem_done: UInt<1>
    wire mem_busy: UInt<1>
    wire mem_la_use_prefetched_high_word: UInt<1>
    wire mem_rdata_latched: UInt<32>
    wire mem_rdata_latched_noshuffle: UInt<32>
    wire mem_16bit_buffer: UInt<16>
    wire clear_prefetched_high_word: UInt<1>
    wire prefetched_high_word: UInt<1>
    wire mem_la_firstword_xfer: UInt<1>
    wire mem_la_firstword: UInt<1>
    wire last_mem_valid: UInt<1>
    wire mem_la_firstword_reg: UInt<1>
    wire mem_la_secondword: UInt<1>
    wire mem_xfer: UInt<1>
    wire mem_do_wdata: UInt<1>
    wire mem_do_rdata: UInt<1>
    wire mem_do_rinst: UInt<1>
    wire mem_do_prefetch: UInt<1>
    wire mem_rdata_q: UInt<32>
    wire mem_rdata_word: UInt<32>
    wire mem_wordsize: UInt<2>
    wire mem_state: UInt<2>
    wire pcpi_int_ready: UInt<1>
    wire pcpi_int_wait: UInt<1>
    wire pcpi_int_rd: UInt<32>
    wire pcpi_int_wr: UInt<1>
    wire pcpi_div_ready: UInt<1>
    wire pcpi_div_wait: UInt<1>
    wire pcpi_div_rd: UInt<32>
    wire pcpi_div_wr: UInt<1>
    wire pcpi_mul_ready: UInt<1>
    wire pcpi_mul_wait: UInt<1>
    wire pcpi_mul_rd: UInt<32>
    wire pcpi_mul_wr: UInt<1>
    wire i: UInt<32>
    wire timer: UInt<32>
    wire irq_pending: UInt<32>
    wire irq_mask: UInt<32>
    wire irq_active: UInt<1>
    wire irq_delay: UInt<1>
    wire next_pc: UInt<32>
    wire dbg_mem_rdata: UInt<32>
    wire dbg_mem_wstrb: UInt<4>
    wire dbg_mem_wdata: UInt<32>
    wire dbg_mem_addr: UInt<32>
    wire dbg_mem_ready: UInt<1>
    wire dbg_mem_instr: UInt<1>
    wire dbg_mem_valid: UInt<1>
    wire dbg_insn_addr: UInt<32>
    wire dbg_insn_opcode: UInt<32>
    wire next_insn_opcode: UInt<32>
    wire reg_sh: UInt<5>
    wire reg_out: UInt<32>
    wire reg_op2: UInt<32>
    wire reg_op1: UInt<32>
    wire reg_next_pc: UInt<32>
    wire reg_pc: UInt<32>
    wire count_instr: UInt<64>
    wire count_cycle: UInt<64>
    wire _sshr_picorv32_v_1223_687: UInt<33>
    wire _ternary_picorv32_v_1223_686: UInt<1>
    wire _logic_or_picorv32_v_1223_685: UInt<1>
    wire _shl_picorv32_v_1222_684: UInt<32>
    wire _lt_picorv32_v_1221_683: UInt<1>
    wire _lt_picorv32_v_1220_682: UInt<1>
    wire _eq_picorv32_v_1219_681: UInt<1>
    wire _ternary_picorv32_v_1218_680: UInt<32>
    wire _add_picorv32_v_1218_679: UInt<32>
    wire _sub_picorv32_v_1218_678: UInt<32>
    wire _and_picorv32_v_1934_676: UInt<32>
    wire _logic_or_picorv32_v_1920_675: UInt<1>
    wire _logic_not_picorv32_v_1920_674: UInt<1>
    wire _logic_and_picorv32_v_1916_673: UInt<1>
    wire _logic_and_picorv32_v_1916_672: UInt<1>
    wire _logic_not_picorv32_v_1916_671: UInt<1>
    wire _logic_and_picorv32_v_1916_670: UInt<1>
    wire _logic_and_picorv32_v_1911_669: UInt<1>
    wire _logic_not_picorv32_v_1911_668: UInt<1>
    wire _logic_and_picorv32_v_1911_667: UInt<1>
    wire _logic_not_picorv32_v_1911_666: UInt<1>
    wire _logic_and_picorv32_v_1909_665: UInt<1>
    wire _ternary_picorv32_v_1909_664: UInt<1>
    wire _reduce_or_picorv32_v_1909_663: UInt<1>
    wire _logic_and_picorv32_v_1909_662: UInt<1>
    wire _logic_and_picorv32_v_1909_661: UInt<1>
    wire _logic_and_picorv32_v_1903_660: UInt<1>
    wire _logic_not_picorv32_v_1903_659: UInt<1>
    wire _logic_and_picorv32_v_1903_658: UInt<1>
    wire _logic_not_picorv32_v_1903_657: UInt<1>
    wire _logic_and_picorv32_v_1901_656: UInt<1>
    wire _ne_picorv32_v_1901_655: UInt<1>
    wire _eq_picorv32_v_1901_654: UInt<1>
    wire _logic_and_picorv32_v_1896_653: UInt<1>
    wire _logic_not_picorv32_v_1896_652: UInt<1>
    wire _logic_and_picorv32_v_1896_651: UInt<1>
    wire _logic_not_picorv32_v_1896_650: UInt<1>
    wire _logic_and_picorv32_v_1894_649: UInt<1>
    wire _ne_picorv32_v_1894_648: UInt<1>
    wire _eq_picorv32_v_1894_647: UInt<1>
    wire _logic_and_picorv32_v_1893_646: UInt<1>
    wire _logic_or_picorv32_v_1893_645: UInt<1>
    wire _logic_and_picorv32_v_1893_644: UInt<1>
    wire _logic_and_picorv32_v_1878_643: UInt<1>
    wire _logic_not_picorv32_v_1878_642: UInt<1>
    wire _add_picorv32_v_1875_641: UInt<32>
    wire _logic_or_picorv32_v_1865_640: UInt<1>
    wire _logic_or_picorv32_v_1864_639: UInt<1>
    wire _logic_not_picorv32_v_1861_638: UInt<1>
    wire _logic_or_picorv32_v_1860_637: UInt<1>
    wire _logic_not_picorv32_v_1860_636: UInt<1>
    wire _logic_and_picorv32_v_1850_635: UInt<1>
    wire _logic_not_picorv32_v_1850_634: UInt<1>
    wire _add_picorv32_v_1847_633: UInt<32>
    wire _logic_not_picorv32_v_1836_632: UInt<1>
    wire _logic_or_picorv32_v_1835_631: UInt<1>
    wire _logic_not_picorv32_v_1835_630: UInt<1>
    wire _sub_picorv32_v_1828_629: UInt<32>
    wire _sshr_picorv32_v_1826_628: UInt<32>
    wire _logic_or_picorv32_v_1826_627: UInt<1>
    wire _shr_picorv32_v_1825_626: UInt<32>
    wire _logic_or_picorv32_v_1825_625: UInt<1>
    wire _shl_picorv32_v_1824_624: UInt<32>
    wire _logic_or_picorv32_v_1824_623: UInt<1>
    wire _sub_picorv32_v_1820_622: UInt<32>
    wire _sshr_picorv32_v_1818_621: UInt<32>
    wire _logic_or_picorv32_v_1818_620: UInt<1>
    wire _shr_picorv32_v_1817_619: UInt<32>
    wire _logic_or_picorv32_v_1817_618: UInt<1>
    wire _shl_picorv32_v_1816_617: UInt<32>
    wire _logic_or_picorv32_v_1816_616: UInt<1>
    wire _logic_and_picorv32_v_1813_615: UInt<1>
    wire _ge_picorv32_v_1813_614: UInt<1>
    wire _eq_picorv32_v_1809_613: UInt<1>
    wire _ternary_picorv32_v_1795_612: UInt<1>
    wire _ternary_picorv32_v_1792_611: UInt<1>
    wire _ternary_picorv32_v_1791_610: UInt<1>
    wire _logic_and_picorv32_v_1786_609: UInt<1>
    wire _logic_not_picorv32_v_1786_608: UInt<1>
    wire _logic_and_picorv32_v_1785_607: UInt<1>
    wire _logic_or_picorv32_v_1785_606: UInt<1>
    wire _add_picorv32_v_1784_605: UInt<32>
    wire _logic_and_picorv32_v_1774_604: UInt<1>
    wire _logic_and_picorv32_v_1774_603: UInt<1>
    wire _logic_or_picorv32_v_1773_602: UInt<1>
    wire _logic_and_picorv32_v_1773_601: UInt<1>
    wire _logic_and_picorv32_v_1769_600: UInt<1>
    wire _logic_and_picorv32_v_1758_599: UInt<1>
    wire _logic_not_picorv32_v_1758_598: UInt<1>
    wire _logic_and_picorv32_v_1758_597: UInt<1>
    wire _logic_not_picorv32_v_1758_596: UInt<1>
    wire _logic_and_picorv32_v_1755_595: UInt<1>
    wire _logic_or_picorv32_v_1755_594: UInt<1>
    wire _logic_and_picorv32_v_1746_593: UInt<1>
    wire _logic_and_picorv32_v_1724_592: UInt<1>
    wire _logic_and_picorv32_v_1724_591: UInt<1>
    wire _logic_or_picorv32_v_1723_590: UInt<1>
    wire _logic_and_picorv32_v_1723_589: UInt<1>
    wire _logic_and_picorv32_v_1719_588: UInt<1>
    wire _ternary_picorv32_v_1695_587: UInt<32>
    wire _logic_and_picorv32_v_1695_585: UInt<1>
    wire _logic_and_picorv32_v_1690_584: UInt<1>
    wire _logic_and_picorv32_v_1682_583: UInt<1>
    wire _logic_and_picorv32_v_1674_582: UInt<1>
    wire _logic_not_picorv32_v_1674_581: UInt<1>
    wire _logic_and_picorv32_v_1665_580: UInt<1>
    wire _or_picorv32_v_1660_579: UInt<32>
    wire _logic_and_picorv32_v_1656_578: UInt<1>
    wire _ternary_picorv32_v_1651_577: UInt<32>
    wire _and_picorv32_v_1651_576: UInt<32>
    wire _logic_and_picorv32_v_1645_575: UInt<1>
    wire _or_picorv32_v_1641_574: UInt<32>
    wire _logic_and_picorv32_v_1636_573: UInt<1>
    wire _logic_and_picorv32_v_1628_572: UInt<1>
    wire _ternary_picorv32_v_1620_571: UInt<32>
    wire _logic_and_picorv32_v_1613_570: UInt<1>
    wire _logic_and_picorv32_v_1609_569: UInt<1>
    wire _logic_and_picorv32_v_1604_568: UInt<1>
    wire _logic_and_picorv32_v_1597_567: UInt<1>
    wire _logic_not_picorv32_v_1597_566: UInt<1>
    wire _logic_and_picorv32_v_1597_565: UInt<1>
    wire _logic_not_picorv32_v_1597_564: UInt<1>
    wire _logic_and_picorv32_v_1563_563: UInt<1>
    wire _logic_and_picorv32_v_1551_562: UInt<1>
    wire _logic_not_picorv32_v_1551_561: UInt<1>
    wire _logic_not_picorv32_v_1551_560: UInt<1>
    wire _add_picorv32_v_1547_559: UInt<32>
    wire _add_picorv32_v_1542_558: UInt<64>
    wire _add_picorv32_v_1538_557: UInt<32>
    wire _ternary_picorv32_v_1538_556: UInt<32>
    wire _add_picorv32_v_1530_555: UInt<32>
    wire _ternary_picorv32_v_1530_554: UInt<32>
    wire _reduce_bool_picorv32_v_1527_553: UInt<1>
    wire _logic_and_picorv32_v_1526_552: UInt<1>
    wire _logic_and_picorv32_v_1526_551: UInt<1>
    wire _logic_or_picorv32_v_1526_550: UInt<1>
    wire _or_picorv32_v_1522_549: UInt<32>
    wire _ternary_picorv32_v_1519_548: UInt<2>
    wire _ternary_picorv32_v_1519_547: UInt<2>
    wire _eq_picorv32_v_1519_546: UInt<1>
    wire _eq_picorv32_v_1518_545: UInt<1>
    wire _logic_and_picorv32_v_1516_544: UInt<1>
    wire _logic_or_picorv32_v_1516_543: UInt<1>
    wire _logic_and_picorv32_v_1516_542: UInt<1>
    wire _reduce_or_picorv32_v_1516_541: UInt<1>
    wire _and_picorv32_v_1516_540: UInt<32>
    wire _not_picorv32_v_1516_539: UInt<32>
    wire _logic_and_picorv32_v_1516_538: UInt<1>
    wire _logic_not_picorv32_v_1516_537: UInt<1>
    wire _logic_and_picorv32_v_1516_536: UInt<1>
    wire _logic_not_picorv32_v_1516_535: UInt<1>
    wire _or_picorv32_v_1501_534: UInt<36>
    wire _ternary_picorv32_v_1501_533: UInt<32>
    wire _ternary_picorv32_v_1501_532: UInt<36>
    wire _or_picorv32_v_1499_530: UInt<36>
    wire _and_picorv32_v_1499_529: UInt<36>
    wire _or_picorv32_v_1499_528: UInt<36>
    wire _ternary_picorv32_v_1499_527: UInt<36>
    wire _logic_and_picorv32_v_1495_525: UInt<1>
    wire _and_picorv32_v_1491_524: UInt<32>
    wire _and_picorv32_v_1490_523: UInt<32>
    wire _not_picorv32_v_1490_522: UInt<32>
    wire _logic_and_picorv32_v_1489_521: UInt<1>
    wire _logic_and_picorv32_v_1484_520: UInt<1>
    wire _logic_and_picorv32_v_1481_519: UInt<1>
    wire _logic_not_picorv32_v_1481_518: UInt<1>
    wire _ternary_picorv32_v_1478_517: UInt<32>
    wire _and_picorv32_v_1478_516: UInt<32>
    wire _ternary_picorv32_v_1478_515: UInt<32>
    wire _logic_and_picorv32_v_1470_514: UInt<1>
    wire _logic_not_picorv32_v_1470_513: UInt<1>
    wire _logic_not_picorv32_v_1470_512: UInt<1>
    wire _logic_not_picorv32_v_1435_511: UInt<1>
    wire _logic_and_picorv32_v_1424_510: UInt<1>
    wire _sub_picorv32_v_1417_509: UInt<32>
    wire _eq_picorv32_v_1415_508: UInt<1>
    wire _sub_picorv32_v_1415_507: UInt<32>
    wire _logic_and_picorv32_v_1414_506: UInt<1>
    wire _ternary_picorv32_v_1405_505: UInt<64>
    wire _add_picorv32_v_1405_503: UInt<64>
    wire _logic_and_picorv32_v_1372_501: UInt<1>
    wire _logic_or_picorv32_v_1372_500: UInt<1>
    wire _logic_not_picorv32_v_1372_499: UInt<1>
    wire _and_picorv32_v_1372_498: UInt<32>
    wire _not_picorv32_v_1372_497: UInt<32>
    wire _logic_or_picorv32_v_1372_496: UInt<1>
    wire _logic_or_picorv32_v_1372_495: UInt<1>
    wire _logic_and_picorv32_v_1372_494: UInt<1>
    wire _eq_picorv32_v_1372_493: UInt<1>
    wire _ternary_picorv32_v_1325_492: UInt<32>
    wire _reduce_bool_picorv32_v_1325_491: UInt<1>
    wire _ternary_picorv32_v_1324_489: UInt<32>
    wire _reduce_bool_picorv32_v_1324_488: UInt<1>
    wire _logic_and_picorv32_v_1316_485: UInt<1>
    wire _logic_and_picorv32_v_1316_484: UInt<1>
    wire _and_picorv32_v_1307_479: UInt<32>
    wire _not_picorv32_v_1307_478: UInt<32>
    wire _logic_and_picorv32_v_1306_477: UInt<1>
    wire _or_picorv32_v_1303_476: UInt<32>
    wire _logic_and_picorv32_v_1302_475: UInt<1>
    wire _ternary_picorv32_v_1299_474: UInt<32>
    wire _logic_and_picorv32_v_1298_473: UInt<1>
    wire _logic_not_picorv32_v_1298_472: UInt<1>
    wire _add_picorv32_v_1295_471: UInt<32>
    wire _ternary_picorv32_v_1295_470: UInt<32>
    wire _eq_picorv32_v_1291_469: UInt<1>
    wire _logic_or_picorv32_v_1277_467: UInt<1>
    wire _logic_not_picorv32_v_1277_466: UInt<1>
    wire _logic_or_picorv32_v_1277_465: UInt<1>
    wire _logic_not_picorv32_v_1275_464: UInt<1>
    wire _logic_and_picorv32_v_1260_461: UInt<1>
    wire _logic_or_picorv32_v_1260_460: UInt<1>
    wire _logic_or_picorv32_v_1260_459: UInt<1>
    wire _logic_or_picorv32_v_1260_458: UInt<1>
    wire _logic_and_picorv32_v_1258_457: UInt<1>
    wire _logic_or_picorv32_v_1258_456: UInt<1>
    wire _and_picorv32_v_1257_455: UInt<32>
    wire _logic_or_picorv32_v_1256_454: UInt<1>
    wire _or_picorv32_v_1255_453: UInt<32>
    wire _logic_or_picorv32_v_1254_452: UInt<1>
    wire _xor_picorv32_v_1253_451: UInt<32>
    wire _logic_or_picorv32_v_1252_450: UInt<1>
    wire _logic_and_picorv32_v_1241_449: UInt<1>
    wire _logic_or_picorv32_v_1241_448: UInt<1>
    wire _logic_not_picorv32_v_1241_447: UInt<1>
    wire _logic_and_picorv32_v_1239_446: UInt<1>
    wire _logic_or_picorv32_v_1239_445: UInt<1>
    wire _logic_not_picorv32_v_1239_444: UInt<1>
    wire _logic_not_picorv32_v_1238_443: UInt<1>
    wire _logic_not_picorv32_v_1236_442: UInt<1>
    wire _logic_not_picorv32_v_1234_441: UInt<1>
    wire _ternary_picorv32_v_1191_439: UInt<32>
    wire _and_picorv32_v_1191_438: UInt<32>
    wire _logic_and_picorv32_v_1191_437: UInt<1>
    wire _eq_picorv32_v_1173_436: UInt<1>
    wire _eq_picorv32_v_1172_435: UInt<1>
    wire _eq_picorv32_v_1171_434: UInt<1>
    wire _eq_picorv32_v_1170_433: UInt<1>
    wire _eq_picorv32_v_1169_432: UInt<1>
    wire _eq_picorv32_v_1168_431: UInt<1>
    wire _eq_picorv32_v_1167_430: UInt<1>
    wire _eq_picorv32_v_1166_429: UInt<1>
    wire _logic_not_picorv32_v_1116_427: UInt<1>
    wire _reduce_or_picorv32_v_1105_426: UInt<1>
    wire _shl_picorv32_v_1104_425: UInt<32>
    wire _reduce_or_picorv32_v_1103_424: UInt<1>
    wire _logic_and_picorv32_v_1094_423: UInt<1>
    wire _reduce_or_picorv32_v_1094_422: UInt<1>
    wire _logic_and_picorv32_v_1091_421: UInt<1>
    wire _eq_picorv32_v_1091_420: UInt<1>
    wire _eq_picorv32_v_1091_419: UInt<1>
    wire _logic_and_picorv32_v_1092_418: UInt<1>
    wire _eq_picorv32_v_1092_417: UInt<1>
    wire _eq_picorv32_v_1092_416: UInt<1>
    wire _logic_and_picorv32_v_1094_415: UInt<1>
    wire _eq_picorv32_v_1094_414: UInt<1>
    wire _eq_picorv32_v_1093_413: UInt<1>
    wire _logic_or_picorv32_v_1088_412: UInt<1>
    wire _logic_and_picorv32_v_1088_411: UInt<1>
    wire _reduce_or_picorv32_v_1088_410: UInt<1>
    wire _eq_picorv32_v_1082_409: UInt<1>
    wire _eq_picorv32_v_1083_408: UInt<1>
    wire _eq_picorv32_v_1084_407: UInt<1>
    wire _eq_picorv32_v_1085_406: UInt<1>
    wire _eq_picorv32_v_1086_405: UInt<1>
    wire _eq_picorv32_v_1088_404: UInt<1>
    wire _logic_and_picorv32_v_1079_403: UInt<1>
    wire _reduce_or_picorv32_v_1079_402: UInt<1>
    wire _logic_and_picorv32_v_1076_401: UInt<1>
    wire _eq_picorv32_v_1076_400: UInt<1>
    wire _eq_picorv32_v_1076_399: UInt<1>
    wire _logic_and_picorv32_v_1077_398: UInt<1>
    wire _eq_picorv32_v_1077_397: UInt<1>
    wire _eq_picorv32_v_1077_396: UInt<1>
    wire _logic_and_picorv32_v_1079_395: UInt<1>
    wire _eq_picorv32_v_1079_394: UInt<1>
    wire _eq_picorv32_v_1078_393: UInt<1>
    wire _logic_and_picorv32_v_1073_392: UInt<1>
    wire _logic_and_picorv32_v_1073_391: UInt<1>
    wire _logic_and_picorv32_v_1073_390: UInt<1>
    wire _eq_picorv32_v_1073_389: UInt<1>
    wire _eq_picorv32_v_1073_388: UInt<1>
    wire _logic_and_picorv32_v_1072_387: UInt<1>
    wire _logic_and_picorv32_v_1072_386: UInt<1>
    wire _eq_picorv32_v_1072_385: UInt<1>
    wire _eq_picorv32_v_1072_384: UInt<1>
    wire _logic_and_picorv32_v_1071_383: UInt<1>
    wire _logic_and_picorv32_v_1071_382: UInt<1>
    wire _logic_and_picorv32_v_1071_381: UInt<1>
    wire _eq_picorv32_v_1071_380: UInt<1>
    wire _eq_picorv32_v_1071_379: UInt<1>
    wire _logic_and_picorv32_v_1070_378: UInt<1>
    wire _logic_and_picorv32_v_1070_377: UInt<1>
    wire _logic_and_picorv32_v_1070_376: UInt<1>
    wire _eq_picorv32_v_1070_375: UInt<1>
    wire _eq_picorv32_v_1070_374: UInt<1>
    wire _logic_or_picorv32_v_1068_373: UInt<1>
    wire _logic_and_picorv32_v_1068_372: UInt<1>
    wire _eq_picorv32_v_1068_371: UInt<1>
    wire _logic_and_picorv32_v_1067_370: UInt<1>
    wire _logic_not_picorv32_v_1067_369: UInt<1>
    wire _logic_and_picorv32_v_1067_368: UInt<1>
    wire _logic_not_picorv32_v_1067_367: UInt<1>
    wire _eq_picorv32_v_1067_366: UInt<1>
    wire _logic_and_picorv32_v_1065_365: UInt<1>
    wire _logic_and_picorv32_v_1065_364: UInt<1>
    wire _logic_and_picorv32_v_1065_363: UInt<1>
    wire _eq_picorv32_v_1065_362: UInt<1>
    wire _eq_picorv32_v_1065_361: UInt<1>
    wire _logic_and_picorv32_v_1064_360: UInt<1>
    wire _logic_and_picorv32_v_1064_359: UInt<1>
    wire _eq_picorv32_v_1064_358: UInt<1>
    wire _eq_picorv32_v_1064_357: UInt<1>
    wire _logic_and_picorv32_v_1063_356: UInt<1>
    wire _logic_and_picorv32_v_1063_355: UInt<1>
    wire _logic_or_picorv32_v_1063_354: UInt<1>
    wire _logic_and_picorv32_v_1063_353: UInt<1>
    wire _eq_picorv32_v_1063_352: UInt<1>
    wire _eq_picorv32_v_1063_351: UInt<1>
    wire _logic_and_picorv32_v_1062_350: UInt<1>
    wire _eq_picorv32_v_1062_349: UInt<1>
    wire _eq_picorv32_v_1062_348: UInt<1>
    wire _logic_and_picorv32_v_1061_347: UInt<1>
    wire _logic_or_picorv32_v_1061_346: UInt<1>
    wire _logic_and_picorv32_v_1061_345: UInt<1>
    wire _eq_picorv32_v_1061_344: UInt<1>
    wire _eq_picorv32_v_1061_343: UInt<1>
    wire _logic_and_picorv32_v_1060_342: UInt<1>
    wire _eq_picorv32_v_1060_341: UInt<1>
    wire _eq_picorv32_v_1060_340: UInt<1>
    wire _logic_and_picorv32_v_1058_339: UInt<1>
    wire _eq_picorv32_v_1058_338: UInt<1>
    wire _logic_and_picorv32_v_1058_337: UInt<1>
    wire _eq_picorv32_v_1058_336: UInt<1>
    wire _logic_and_picorv32_v_1057_335: UInt<1>
    wire _eq_picorv32_v_1057_334: UInt<1>
    wire _logic_and_picorv32_v_1057_333: UInt<1>
    wire _eq_picorv32_v_1057_332: UInt<1>
    wire _logic_and_picorv32_v_1056_331: UInt<1>
    wire _eq_picorv32_v_1056_330: UInt<1>
    wire _logic_and_picorv32_v_1056_329: UInt<1>
    wire _eq_picorv32_v_1056_328: UInt<1>
    wire _logic_and_picorv32_v_1055_327: UInt<1>
    wire _eq_picorv32_v_1055_326: UInt<1>
    wire _logic_and_picorv32_v_1055_325: UInt<1>
    wire _eq_picorv32_v_1055_324: UInt<1>
    wire _logic_and_picorv32_v_1054_323: UInt<1>
    wire _eq_picorv32_v_1054_322: UInt<1>
    wire _logic_and_picorv32_v_1054_321: UInt<1>
    wire _eq_picorv32_v_1054_320: UInt<1>
    wire _logic_and_picorv32_v_1053_319: UInt<1>
    wire _eq_picorv32_v_1053_318: UInt<1>
    wire _logic_and_picorv32_v_1053_317: UInt<1>
    wire _eq_picorv32_v_1053_316: UInt<1>
    wire _logic_and_picorv32_v_1052_315: UInt<1>
    wire _eq_picorv32_v_1052_314: UInt<1>
    wire _logic_and_picorv32_v_1052_313: UInt<1>
    wire _eq_picorv32_v_1052_312: UInt<1>
    wire _logic_and_picorv32_v_1051_311: UInt<1>
    wire _eq_picorv32_v_1051_310: UInt<1>
    wire _logic_and_picorv32_v_1051_309: UInt<1>
    wire _eq_picorv32_v_1051_308: UInt<1>
    wire _logic_and_picorv32_v_1050_307: UInt<1>
    wire _eq_picorv32_v_1050_306: UInt<1>
    wire _logic_and_picorv32_v_1050_305: UInt<1>
    wire _eq_picorv32_v_1050_304: UInt<1>
    wire _logic_and_picorv32_v_1049_303: UInt<1>
    wire _eq_picorv32_v_1049_302: UInt<1>
    wire _logic_and_picorv32_v_1049_301: UInt<1>
    wire _eq_picorv32_v_1049_300: UInt<1>
    wire _logic_and_picorv32_v_1047_299: UInt<1>
    wire _eq_picorv32_v_1047_298: UInt<1>
    wire _logic_and_picorv32_v_1047_297: UInt<1>
    wire _eq_picorv32_v_1047_296: UInt<1>
    wire _logic_and_picorv32_v_1046_295: UInt<1>
    wire _eq_picorv32_v_1046_294: UInt<1>
    wire _logic_and_picorv32_v_1046_293: UInt<1>
    wire _eq_picorv32_v_1046_292: UInt<1>
    wire _logic_and_picorv32_v_1045_291: UInt<1>
    wire _eq_picorv32_v_1045_290: UInt<1>
    wire _logic_and_picorv32_v_1045_289: UInt<1>
    wire _eq_picorv32_v_1045_288: UInt<1>
    wire _logic_and_picorv32_v_1043_287: UInt<1>
    wire _eq_picorv32_v_1043_286: UInt<1>
    wire _logic_and_picorv32_v_1042_285: UInt<1>
    wire _eq_picorv32_v_1042_284: UInt<1>
    wire _logic_and_picorv32_v_1041_283: UInt<1>
    wire _eq_picorv32_v_1041_282: UInt<1>
    wire _logic_and_picorv32_v_1040_281: UInt<1>
    wire _eq_picorv32_v_1040_280: UInt<1>
    wire _logic_and_picorv32_v_1039_279: UInt<1>
    wire _eq_picorv32_v_1039_278: UInt<1>
    wire _logic_and_picorv32_v_1038_277: UInt<1>
    wire _eq_picorv32_v_1038_276: UInt<1>
    wire _logic_and_picorv32_v_1036_275: UInt<1>
    wire _eq_picorv32_v_1036_274: UInt<1>
    wire _logic_and_picorv32_v_1035_273: UInt<1>
    wire _eq_picorv32_v_1035_272: UInt<1>
    wire _logic_and_picorv32_v_1034_271: UInt<1>
    wire _eq_picorv32_v_1034_270: UInt<1>
    wire _logic_and_picorv32_v_1032_269: UInt<1>
    wire _eq_picorv32_v_1032_268: UInt<1>
    wire _logic_and_picorv32_v_1031_267: UInt<1>
    wire _eq_picorv32_v_1031_266: UInt<1>
    wire _logic_and_picorv32_v_1030_265: UInt<1>
    wire _eq_picorv32_v_1030_264: UInt<1>
    wire _logic_and_picorv32_v_1029_263: UInt<1>
    wire _eq_picorv32_v_1029_262: UInt<1>
    wire _logic_and_picorv32_v_1028_261: UInt<1>
    wire _eq_picorv32_v_1028_260: UInt<1>
    wire _logic_and_picorv32_v_1026_259: UInt<1>
    wire _eq_picorv32_v_1026_258: UInt<1>
    wire _logic_and_picorv32_v_1025_257: UInt<1>
    wire _eq_picorv32_v_1025_256: UInt<1>
    wire _logic_and_picorv32_v_1024_255: UInt<1>
    wire _eq_picorv32_v_1024_254: UInt<1>
    wire _logic_and_picorv32_v_1023_253: UInt<1>
    wire _eq_picorv32_v_1023_252: UInt<1>
    wire _logic_and_picorv32_v_1022_251: UInt<1>
    wire _eq_picorv32_v_1022_250: UInt<1>
    wire _logic_and_picorv32_v_1021_249: UInt<1>
    wire _eq_picorv32_v_1021_248: UInt<1>
    wire _logic_and_picorv32_v_1018_247: UInt<1>
    wire _logic_not_picorv32_v_1018_246: UInt<1>
    wire _logic_and_picorv32_v_1000_245: UInt<1>
    wire _ne_picorv32_v_1000_244: UInt<1>
    wire _ne_picorv32_v_1000_243: UInt<1>
    wire _logic_and_picorv32_v_995_242: UInt<1>
    wire _eq_picorv32_v_995_241: UInt<1>
    wire _logic_and_picorv32_v_995_240: UInt<1>
    wire _ne_picorv32_v_995_239: UInt<1>
    wire _ne_picorv32_v_995_238: UInt<1>
    wire _logic_and_picorv32_v_989_237: UInt<1>
    wire _ne_picorv32_v_989_236: UInt<1>
    wire _eq_picorv32_v_989_235: UInt<1>
    wire _logic_and_picorv32_v_984_234: UInt<1>
    wire _eq_picorv32_v_984_233: UInt<1>
    wire _logic_and_picorv32_v_984_232: UInt<1>
    wire _ne_picorv32_v_984_231: UInt<1>
    wire _eq_picorv32_v_984_230: UInt<1>
    wire _reduce_bool_picorv32_v_977_229: UInt<1>
    wire _logic_not_picorv32_v_969_228: UInt<1>
    wire _add_picorv32_v_961_227: UInt<32>
    wire _add_picorv32_v_956_226: UInt<32>
    wire _add_picorv32_v_948_225: UInt<32>
    wire _add_picorv32_v_947_224: UInt<32>
    wire _add_picorv32_v_946_223: UInt<32>
    wire _eq_picorv32_v_944_222: UInt<1>
    wire _add_picorv32_v_942_221: UInt<32>
    wire _add_picorv32_v_941_220: UInt<32>
    wire _eq_picorv32_v_939_219: UInt<1>
    wire _add_picorv32_v_936_218: UInt<32>
    wire _add_picorv32_v_935_217: UInt<32>
    wire _logic_and_picorv32_v_933_216: UInt<1>
    wire _logic_not_picorv32_v_933_215: UInt<1>
    wire _logic_not_picorv32_v_933_214: UInt<1>
    wire _eq_picorv32_v_921_213: UInt<1>
    wire _logic_or_picorv32_v_920_212: UInt<1>
    wire _add_picorv32_v_899_211: UInt<32>
    wire _add_picorv32_v_898_210: UInt<32>
    wire _add_picorv32_v_894_209: UInt<32>
    wire _add_picorv32_v_893_208: UInt<32>
    wire _add_picorv32_v_889_207: UInt<32>
    wire _reduce_or_picorv32_v_887_206: UInt<1>
    wire _logic_and_picorv32_v_874_205: UInt<1>
    wire _ne_picorv32_v_874_204: UInt<1>
    wire _logic_and_picorv32_v_870_203: UInt<1>
    wire _logic_and_picorv32_v_870_202: UInt<1>
    wire _eq_picorv32_v_870_201: UInt<1>
    wire _eq_picorv32_v_870_200: UInt<1>
    wire _logic_and_picorv32_v_867_199: UInt<1>
    wire _logic_and_picorv32_v_867_198: UInt<1>
    wire _logic_and_picorv32_v_867_197: UInt<1>
    wire _eq_picorv32_v_867_196: UInt<1>
    wire _eq_picorv32_v_867_195: UInt<1>
    wire _eq_picorv32_v_859_194: UInt<1>
    wire _eq_picorv32_v_858_193: UInt<1>
    wire _eq_picorv32_v_857_192: UInt<1>
    wire _eq_picorv32_v_856_191: UInt<1>
    wire _eq_picorv32_v_855_190: UInt<1>
    wire _logic_and_picorv32_v_853_189: UInt<1>
    wire _logic_and_picorv32_v_853_188: UInt<1>
    wire _eq_picorv32_v_853_187: UInt<1>
    wire _eq_picorv32_v_853_186: UInt<1>
    wire _logic_and_picorv32_v_852_185: UInt<1>
    wire _logic_and_picorv32_v_852_184: UInt<1>
    wire _eq_picorv32_v_852_183: UInt<1>
    wire _eq_picorv32_v_852_182: UInt<1>
    wire _logic_and_picorv32_v_851_181: UInt<1>
    wire _eq_picorv32_v_851_180: UInt<1>
    wire _eq_picorv32_v_851_179: UInt<1>
    wire _eq_picorv32_v_850_178: UInt<1>
    wire _eq_picorv32_v_849_177: UInt<1>
    wire _eq_picorv32_v_848_176: UInt<1>
    wire _logic_and_picorv32_v_847_175: UInt<1>
    wire _reduce_or_picorv32_v_845_174: UInt<1>
    wire _reduce_or_picorv32_v_844_173: UInt<1>
    wire _reduce_or_picorv32_v_843_172: UInt<1>
    wire _reduce_or_picorv32_v_842_171: UInt<1>
    wire _reduce_or_picorv32_v_841_170: UInt<1>
    wire _reduce_or_picorv32_v_840_169: UInt<1>
    wire _reduce_and_picorv32_v_808_167: UInt<1>
    wire _reduce_and_picorv32_v_776_165: UInt<1>
    wire _logic_or_picorv32_v_768_164: UInt<1>
    wire _logic_not_picorv32_v_768_163: UInt<1>
    wire _reduce_or_picorv32_v_670_160: UInt<1>
    wire _logic_and_picorv32_v_667_159: UInt<1>
    wire _logic_not_picorv32_v_667_158: UInt<1>
    wire _ternary_picorv32_v_600_157: UInt<32>
    wire _logic_or_picorv32_v_600_156: UInt<1>
    wire _logic_or_picorv32_v_593_155: UInt<1>
    wire _logic_not_picorv32_v_593_154: UInt<1>
    wire _reduce_and_picorv32_v_593_153: UInt<1>
    wire _logic_and_picorv32_v_592_152: UInt<1>
    wire _logic_not_picorv32_v_592_151: UInt<1>
    wire _logic_not_picorv32_v_587_150: UInt<1>
    wire _logic_and_picorv32_v_584_149: UInt<1>
    wire _logic_or_picorv32_v_568_148: UInt<1>
    wire _logic_not_picorv32_v_567_147: UInt<1>
    wire _logic_or_picorv32_v_566_146: UInt<1>
    wire _logic_or_picorv32_v_566_145: UInt<1>
    wire _and_picorv32_v_559_144: UInt<4>
    wire _logic_or_picorv32_v_557_143: UInt<1>
    wire _logic_or_picorv32_v_552_142: UInt<1>
    wire _logic_not_picorv32_v_552_141: UInt<1>
    wire _logic_not_picorv32_v_550_140: UInt<1>
    wire _logic_or_picorv32_v_549_139: UInt<1>
    wire _logic_not_picorv32_v_549_138: UInt<1>
    wire _logic_or_picorv32_v_543_136: UInt<1>
    wire _eq_picorv32_v_543_135: UInt<1>
    wire _eq_picorv32_v_543_134: UInt<1>
    wire _logic_or_picorv32_v_534_133: UInt<1>
    wire _logic_or_picorv32_v_531_132: UInt<1>
    wire _logic_or_picorv32_v_531_131: UInt<1>
    wire _logic_and_picorv32_v_530_130: UInt<1>
    wire _logic_not_picorv32_v_530_129: UInt<1>
    wire _logic_and_picorv32_v_514_127: UInt<1>
    wire _ne_picorv32_v_514_126: UInt<1>
    wire _ne_picorv32_v_514_125: UInt<1>
    wire _logic_and_picorv32_v_510_124: UInt<1>
    wire _eq_picorv32_v_510_123: UInt<1>
    wire _logic_and_picorv32_v_510_122: UInt<1>
    wire _ne_picorv32_v_510_121: UInt<1>
    wire _ne_picorv32_v_510_120: UInt<1>
    wire _logic_and_picorv32_v_506_119: UInt<1>
    wire _ne_picorv32_v_506_118: UInt<1>
    wire _eq_picorv32_v_506_117: UInt<1>
    wire _logic_and_picorv32_v_502_116: UInt<1>
    wire _eq_picorv32_v_502_115: UInt<1>
    wire _eq_picorv32_v_502_114: UInt<1>
    wire _ternary_picorv32_v_474_113: UInt<7>
    wire _eq_picorv32_v_474_112: UInt<1>
    wire _eq_picorv32_v_473_111: UInt<1>
    wire _eq_picorv32_v_472_110: UInt<1>
    wire _eq_picorv32_v_471_109: UInt<1>
    wire _eq_picorv32_v_470_108: UInt<1>
    wire _eq_picorv32_v_469_107: UInt<1>
    wire _eq_picorv32_v_465_106: UInt<1>
    wire _eq_picorv32_v_461_105: UInt<1>
    wire _eq_picorv32_v_457_104: UInt<1>
    wire _eq_picorv32_v_448_103: UInt<1>
    wire _logic_and_picorv32_v_419_102: UInt<1>
    wire _logic_or_picorv32_v_419_101: UInt<1>
    wire _logic_and_picorv32_v_419_100: UInt<1>
    wire _ternary_picorv32_v_416_99: UInt<32>
    wire _ternary_picorv32_v_415_98: UInt<32>
    wire _shl_picorv32_v_402_96: UInt<4>
    wire _ternary_picorv32_v_394_95: UInt<4>
    wire _logic_and_picorv32_v_380_93: UInt<1>
    wire _logic_not_picorv32_v_380_92: UInt<1>
    wire _logic_not_picorv32_v_378_91: UInt<1>
    wire _logic_not_picorv32_v_374_90: UInt<1>
    wire _ternary_picorv32_v_371_88: UInt<32>
    wire _ternary_picorv32_v_371_87: UInt<32>
    wire _ternary_picorv32_v_371_86: UInt<32>
    wire _logic_and_picorv32_v_371_85: UInt<1>
    wire _logic_and_picorv32_v_370_84: UInt<1>
    wire _logic_and_picorv32_v_369_83: UInt<1>
    wire _ternary_picorv32_v_367_82: UInt<32>
    wire _logic_or_picorv32_v_367_81: UInt<1>
    wire _ternary_picorv32_v_365_80: UInt<32>
    wire _add_picorv32_v_365_79: UInt<30>
    wire _logic_or_picorv32_v_365_78: UInt<1>
    wire _logic_and_picorv32_v_364_77: UInt<1>
    wire _logic_or_picorv32_v_364_76: UInt<1>
    wire _logic_and_picorv32_v_364_75: UInt<1>
    wire _reduce_and_picorv32_v_364_74: UInt<1>
    wire _logic_and_picorv32_v_364_73: UInt<1>
    wire _logic_not_picorv32_v_364_72: UInt<1>
    wire _logic_and_picorv32_v_364_71: UInt<1>
    wire _ternary_picorv32_v_364_70: UInt<1>
    wire _logic_not_picorv32_v_364_69: UInt<1>
    wire _logic_and_picorv32_v_364_68: UInt<1>
    wire _logic_and_picorv32_v_363_67: UInt<1>
    wire _logic_or_picorv32_v_363_66: UInt<1>
    wire _logic_or_picorv32_v_363_65: UInt<1>
    wire _logic_and_picorv32_v_363_64: UInt<1>
    wire _logic_not_picorv32_v_363_63: UInt<1>
    wire _logic_not_picorv32_v_363_62: UInt<1>
    wire _logic_and_picorv32_v_362_61: UInt<1>
    wire _logic_and_picorv32_v_362_60: UInt<1>
    wire _logic_not_picorv32_v_362_59: UInt<1>
    wire _logic_and_picorv32_v_360_58: UInt<1>
    wire _logic_or_picorv32_v_360_57: UInt<1>
    wire _logic_and_picorv32_v_360_56: UInt<1>
    wire _logic_not_picorv32_v_360_55: UInt<1>
    wire _reduce_and_picorv32_v_360_54: UInt<1>
    wire _logic_not_picorv32_v_360_53: UInt<1>
    wire _logic_and_picorv32_v_359_52: UInt<1>
    wire _logic_or_picorv32_v_359_51: UInt<1>
    wire _logic_and_picorv32_v_359_50: UInt<1>
    wire _reduce_and_picorv32_v_359_49: UInt<1>
    wire _logic_and_picorv32_v_359_48: UInt<1>
    wire _logic_or_picorv32_v_359_47: UInt<1>
    wire _logic_or_picorv32_v_359_46: UInt<1>
    wire _logic_and_picorv32_v_359_45: UInt<1>
    wire _reduce_or_picorv32_v_359_44: UInt<1>
    wire _reduce_or_picorv32_v_358_43: UInt<1>
    wire _logic_or_picorv32_v_356_42: UInt<1>
    wire _logic_and_picorv32_v_356_41: UInt<1>
    wire _logic_and_picorv32_v_356_40: UInt<1>
    wire _logic_and_picorv32_v_355_39: UInt<1>
    wire _logic_not_picorv32_v_355_38: UInt<1>
    wire _logic_and_picorv32_v_355_37: UInt<1>
    wire _logic_and_picorv32_v_355_36: UInt<1>
    wire _logic_and_picorv32_v_346_35: UInt<1>
    wire _ternary_picorv32_v_346_34: UInt<1>
    wire _logic_not_picorv32_v_346_33: UInt<1>
    wire _logic_and_picorv32_v_346_32: UInt<1>
    wire _logic_and_picorv32_v_345_31: UInt<1>
    wire _logic_not_picorv32_v_345_30: UInt<1>
    wire _logic_and_picorv32_v_345_29: UInt<1>
    wire _logic_and_picorv32_v_345_28: UInt<1>
    wire _logic_or_picorv32_v_345_27: UInt<1>
    wire _logic_and_picorv32_v_324_26: UInt<1>
    wire _logic_and_picorv32_v_320_25: UInt<1>
    wire _logic_and_picorv32_v_316_24: UInt<1>
    wire _reduce_or_picorv32_v_312_23: UInt<1>
    wire _logic_and_picorv32_v_312_22: UInt<1>
    wire _logic_and_picorv32_v_312_21: UInt<1>
    wire _logic_and_picorv32_v_312_20: UInt<1>
    wire _reduce_or_picorv32_v_311_19: UInt<1>
    wire _logic_and_picorv32_v_311_18: UInt<1>
    wire _logic_and_picorv32_v_311_17: UInt<1>
    wire _logic_and_picorv32_v_311_16: UInt<1>
    wire _224: UInt<1>
    wire _225: UInt<1>
    wire _226: UInt<1>
    wire _227: UInt<32>
    wire _228: UInt<32>
    wire _229: UInt<4>
    wire _230: UInt<32>
    wire _231: UInt<32>
    wire _232: UInt<32>
    wire _233: UInt<1>
    wire _234: UInt<1>
    wire _235: UInt<1>
    wire _236: UInt<1>
    wire _237: UInt<1>
    wire _238: UInt<1>
    wire _239: UInt<1>
    wire _240: UInt<1>
    wire _241: UInt<32>
    wire _242: UInt<32>
    wire _243: UInt<32>
    wire _244: UInt<1>
    wire _245: UInt<1>
    wire _246: UInt<32>
    wire _247: UInt<1>
    wire _248: UInt<1>
    wire _249: UInt<32>
    wire _250: UInt<1>
    wire _251: UInt<1>
    wire _252: UInt<1>
    wire _253: UInt<32>
    wire _254: UInt<1>
    wire _255: UInt<1>
    _0_alu_lts_0_0_ is invalid
    _0_alu_ltu_0_0_ is invalid
    _0_alu_eq_0_0_ is invalid
    _0_alu_shr_31_0_ is invalid
    _0_alu_shl_31_0_ is invalid
    _0_alu_add_sub_31_0_ is invalid
    _19_next_irq_pending_2_2_ is invalid
    _18_next_irq_pending_2_2_ is invalid
    _17_next_irq_pending_2_2_ is invalid
    _16_next_irq_pending_2_2_ is invalid
    _15_next_irq_pending_2_2_ is invalid
    _14_next_irq_pending_2_2_ is invalid
    _13_next_irq_pending_2_2_ is invalid
    _4_set_mem_do_rdata_0_0_ is invalid
    _3_set_mem_do_rdata_0_0_ is invalid
    _4_set_mem_do_wdata_0_0_ is invalid
    _3_set_mem_do_wdata_0_0_ is invalid
    _5_set_mem_do_rinst_0_0_ is invalid
    _4_set_mem_do_rinst_0_0_ is invalid
    _3_set_mem_do_rinst_0_0_ is invalid
    _12_next_irq_pending_1_1_ is invalid
    _11_next_irq_pending_1_1_ is invalid
    _10_next_irq_pending_1_1_ is invalid
    _9_next_irq_pending_1_1_ is invalid
    _extend_picorv32_v_1695_586_Y is invalid
    _8_next_irq_pending_1_1_ is invalid
    _7_next_irq_pending_1_1_ is invalid
    _6_next_irq_pending_1_1_ is invalid
    _extend_picorv32_v_1501_531_Y is invalid
    _extend_picorv32_v_1499_526_Y is invalid
    _5_next_irq_pending_31_0_ is invalid
    _3_current_pc_31_0_ is invalid
    _4_next_irq_pending_31_0_ is invalid
    _2_current_pc_31_0_ is invalid
    _2_set_mem_do_wdata_0_0_ is invalid
    _2_set_mem_do_rdata_0_0_ is invalid
    _2_set_mem_do_rinst_0_0_ is invalid
    _3_next_irq_pending_31_0_ is invalid
    _1_current_pc_31_0_ is invalid
    _1_set_mem_do_wdata_0_0_ is invalid
    _1_set_mem_do_rdata_0_0_ is invalid
    _1_set_mem_do_rinst_0_0_ is invalid
    _2_next_irq_pending_0_0_ is invalid
    _1_next_irq_pending_0_0_ is invalid
    _extend_picorv32_v_1405_504_Y is invalid
    _0_alu_wait_2_0_0_ is invalid
    _0_alu_wait_0_0_ is invalid
    _0_alu_out_0_q_0_0_ is invalid
    _0_alu_out_q_31_0_ is invalid
    _0_do_waitirq_0_0_ is invalid
    _0_next_irq_pending_31_0_ is invalid
    _0_pcpi_timeout_0_0_ is invalid
    _0_current_pc_31_0_ is invalid
    _0_latched_rd_4_0_ is invalid
    _0_latched_is_lb_0_0_ is invalid
    _0_latched_is_lh_0_0_ is invalid
    _0_latched_is_lu_0_0_ is invalid
    _0_latched_trace_0_0_ is invalid
    _0_latched_compr_0_0_ is invalid
    _0_latched_branch_0_0_ is invalid
    _0_latched_stalu_0_0_ is invalid
    _0_latched_store_0_0_ is invalid
    _0_set_mem_do_wdata_0_0_ is invalid
    _0_set_mem_do_rdata_0_0_ is invalid
    _0_set_mem_do_rinst_0_0_ is invalid
    _0_irq_state_1_0_ is invalid
    _0_cpu_state_7_0_ is invalid
    _0_dbg_rs2val_valid_0_0_ is invalid
    _0_dbg_rs1val_valid_0_0_ is invalid
    _0_dbg_rs2val_31_0_ is invalid
    _0_dbg_rs1val_31_0_ is invalid
    _0_decoder_pseudo_trigger_q_0_0_ is invalid
    _0_decoder_pseudo_trigger_0_0_ is invalid
    _0_decoder_trigger_q_0_0_ is invalid
    _0_decoder_trigger_0_0_ is invalid
    _0_mem_do_wdata_0_0_ is invalid
    _0_mem_do_rdata_0_0_ is invalid
    _0_mem_do_rinst_0_0_ is invalid
    _0_mem_do_prefetch_0_0_ is invalid
    _0_mem_wordsize_1_0_ is invalid
    _0_timer_31_0_ is invalid
    _0_irq_pending_31_0_ is invalid
    _0_irq_mask_31_0_ is invalid
    _0_irq_active_0_0_ is invalid
    _0_irq_delay_0_0_ is invalid
    _0_reg_sh_4_0_ is invalid
    _0_reg_out_31_0_ is invalid
    _0_reg_op2_31_0_ is invalid
    _0_reg_op1_31_0_ is invalid
    _0_reg_next_pc_31_0_ is invalid
    _0_reg_pc_31_0_ is invalid
    _0_count_instr_63_0_ is invalid
    _0_count_cycle_63_0_ is invalid
    _0_trace_data_35_0_ is invalid
    _0_trace_valid_0_0_ is invalid
    _0_eoi_31_0_ is invalid
    _0_pcpi_valid_0_0_ is invalid
    _0_trap_0_0_ is invalid
    _memrd__cpuregs_picorv32_v_1325_490_DATA is invalid
    _memrd__cpuregs_picorv32_v_1324_487_DATA is invalid
    _1_cpuregs_rs2_31_0_ is invalid
    _1_cpuregs_rs1_31_0_ is invalid
    _0_decoded_rs_4_0_ is invalid
    _0_cpuregs_rs2_31_0_ is invalid
    _0_cpuregs_rs1_31_0_ is invalid
    _0_memwr__cpuregs_picorv32_v_1317_14_EN_31_0__483 is invalid
    _0_memwr__cpuregs_picorv32_v_1317_14_DATA_31_0__482 is invalid
    _0_memwr__cpuregs_picorv32_v_1317_14_ADDR_4_0__481 is invalid
    _2_cpuregs_wrdata_31_0_ is invalid
    _2_cpuregs_write_0_0_ is invalid
    _1_cpuregs_wrdata_31_0_ is invalid
    _1_cpuregs_write_0_0_ is invalid
    _0_cpuregs_wrdata_31_0_ is invalid
    _0_cpuregs_write_0_0_ is invalid
    _2_clear_prefetched_high_word_0_0_ is invalid
    _1_clear_prefetched_high_word_0_0_ is invalid
    _0_clear_prefetched_high_word_0_0_ is invalid
    _0_clear_prefetched_high_word_q_0_0_ is invalid
    _1_alu_out_31_0_ is invalid
    _1_alu_out_0_0_0_ is invalid
    _0_alu_out_0_0_0_ is invalid
    _0_alu_out_31_0_ is invalid
    _8_dbg_ascii_state_127_0_ is invalid
    _7_dbg_ascii_state_127_0_ is invalid
    _6_dbg_ascii_state_127_0_ is invalid
    _5_dbg_ascii_state_127_0_ is invalid
    _4_dbg_ascii_state_127_0_ is invalid
    _3_dbg_ascii_state_127_0_ is invalid
    _2_dbg_ascii_state_127_0_ is invalid
    _1_dbg_ascii_state_127_0_ is invalid
    _0_dbg_ascii_state_127_0_ is invalid
    _0_is_compare_0_0_ is invalid
    _0_is_alu_reg_reg_0_0_ is invalid
    _0_is_alu_reg_imm_0_0_ is invalid
    _0_is_lbu_lhu_lw_0_0_ is invalid
    _0_is_beq_bne_blt_bge_bltu_bgeu_0_0_ is invalid
    _0_is_sltiu_bltu_sltu_0_0_ is invalid
    _0_is_slti_blt_slt_0_0_ is invalid
    _0_is_lui_auipc_jal_jalr_addi_add_sub_0_0_ is invalid
    _0_is_sll_srl_sra_0_0_ is invalid
    _0_is_sb_sh_sw_0_0_ is invalid
    _0_is_jalr_addi_slti_sltiu_xori_ori_andi_0_0_ is invalid
    _0_is_slli_srli_srai_0_0_ is invalid
    _0_is_lb_lh_lw_lbu_lhu_0_0_ is invalid
    _0_is_lui_auipc_jal_0_0_ is invalid
    _0_compressed_instr_0_0_ is invalid
    _0_decoded_imm_uj_31_0_ is invalid
    _0_decoded_imm_31_0_ is invalid
    _0_decoded_rs2_4_0_ is invalid
    _0_decoded_rs1_4_0_ is invalid
    _0_decoded_rd_4_0_ is invalid
    _0_instr_timer_0_0_ is invalid
    _0_instr_waitirq_0_0_ is invalid
    _0_instr_maskirq_0_0_ is invalid
    _0_instr_retirq_0_0_ is invalid
    _0_instr_setq_0_0_ is invalid
    _0_instr_getq_0_0_ is invalid
    _0_instr_ecall_ebreak_0_0_ is invalid
    _0_instr_rdinstrh_0_0_ is invalid
    _0_instr_rdinstr_0_0_ is invalid
    _0_instr_rdcycleh_0_0_ is invalid
    _0_instr_rdcycle_0_0_ is invalid
    _0_instr_and_0_0_ is invalid
    _0_instr_or_0_0_ is invalid
    _0_instr_sra_0_0_ is invalid
    _0_instr_srl_0_0_ is invalid
    _0_instr_xor_0_0_ is invalid
    _0_instr_sltu_0_0_ is invalid
    _0_instr_slt_0_0_ is invalid
    _0_instr_sll_0_0_ is invalid
    _0_instr_sub_0_0_ is invalid
    _0_instr_add_0_0_ is invalid
    _0_instr_srai_0_0_ is invalid
    _0_instr_srli_0_0_ is invalid
    _0_instr_slli_0_0_ is invalid
    _0_instr_andi_0_0_ is invalid
    _0_instr_ori_0_0_ is invalid
    _0_instr_xori_0_0_ is invalid
    _0_instr_sltiu_0_0_ is invalid
    _0_instr_slti_0_0_ is invalid
    _0_instr_addi_0_0_ is invalid
    _0_instr_sw_0_0_ is invalid
    _0_instr_sh_0_0_ is invalid
    _0_instr_sb_0_0_ is invalid
    _0_instr_lhu_0_0_ is invalid
    _0_instr_lbu_0_0_ is invalid
    _0_instr_lw_0_0_ is invalid
    _0_instr_lh_0_0_ is invalid
    _0_instr_lb_0_0_ is invalid
    _0_instr_bgeu_0_0_ is invalid
    _0_instr_bltu_0_0_ is invalid
    _0_instr_bge_0_0_ is invalid
    _0_instr_blt_0_0_ is invalid
    _0_instr_bne_0_0_ is invalid
    _0_instr_beq_0_0_ is invalid
    _0_instr_jalr_0_0_ is invalid
    _0_instr_jal_0_0_ is invalid
    _0_instr_auipc_0_0_ is invalid
    _0_instr_lui_0_0_ is invalid
    _0_pcpi_insn_31_0_ is invalid
    _3_dbg_insn_opcode_31_0_ is invalid
    _2_dbg_insn_rd_4_0_ is invalid
    _2_dbg_insn_rs2_4_0_ is invalid
    _2_dbg_insn_rs1_4_0_ is invalid
    _2_dbg_insn_imm_31_0_ is invalid
    _2_dbg_ascii_instr_63_0_ is invalid
    _2_dbg_insn_opcode_31_0_ is invalid
    _1_dbg_insn_rd_4_0_ is invalid
    _1_dbg_insn_rs2_4_0_ is invalid
    _1_dbg_insn_rs1_4_0_ is invalid
    _1_dbg_insn_imm_31_0_ is invalid
    _1_dbg_ascii_instr_63_0_ is invalid
    _1_dbg_insn_opcode_31_0_ is invalid
    _0_dbg_insn_rd_4_0_ is invalid
    _0_dbg_insn_rs2_4_0_ is invalid
    _0_dbg_insn_rs1_4_0_ is invalid
    _0_dbg_insn_imm_31_0_ is invalid
    _0_dbg_ascii_instr_63_0_ is invalid
    _0_dbg_insn_opcode_31_0_ is invalid
    _0_cached_insn_rd_4_0_ is invalid
    _0_cached_insn_rs2_4_0_ is invalid
    _0_cached_insn_rs1_4_0_ is invalid
    _0_cached_insn_opcode_31_0_ is invalid
    _0_cached_insn_imm_31_0_ is invalid
    _0_cached_ascii_instr_63_0_ is invalid
    _0_dbg_valid_insn_0_0_ is invalid
    _0_dbg_next_0_0_ is invalid
    _0_q_insn_rd_4_0_ is invalid
    _0_q_insn_rs2_4_0_ is invalid
    _0_q_insn_rs1_4_0_ is invalid
    _0_q_insn_opcode_31_0_ is invalid
    _0_q_insn_imm_31_0_ is invalid
    _0_q_ascii_instr_63_0_ is invalid
    _0_dbg_insn_addr_31_0_ is invalid
    _47_new_ascii_instr_63_0_ is invalid
    _46_new_ascii_instr_63_0_ is invalid
    _45_new_ascii_instr_63_0_ is invalid
    _44_new_ascii_instr_63_0_ is invalid
    _43_new_ascii_instr_63_0_ is invalid
    _42_new_ascii_instr_63_0_ is invalid
    _41_new_ascii_instr_63_0_ is invalid
    _40_new_ascii_instr_63_0_ is invalid
    _39_new_ascii_instr_63_0_ is invalid
    _38_new_ascii_instr_63_0_ is invalid
    _37_new_ascii_instr_63_0_ is invalid
    _36_new_ascii_instr_63_0_ is invalid
    _35_new_ascii_instr_63_0_ is invalid
    _34_new_ascii_instr_63_0_ is invalid
    _33_new_ascii_instr_63_0_ is invalid
    _32_new_ascii_instr_63_0_ is invalid
    _31_new_ascii_instr_63_0_ is invalid
    _30_new_ascii_instr_63_0_ is invalid
    _29_new_ascii_instr_63_0_ is invalid
    _28_new_ascii_instr_63_0_ is invalid
    _27_new_ascii_instr_63_0_ is invalid
    _26_new_ascii_instr_63_0_ is invalid
    _25_new_ascii_instr_63_0_ is invalid
    _24_new_ascii_instr_63_0_ is invalid
    _23_new_ascii_instr_63_0_ is invalid
    _22_new_ascii_instr_63_0_ is invalid
    _21_new_ascii_instr_63_0_ is invalid
    _20_new_ascii_instr_63_0_ is invalid
    _19_new_ascii_instr_63_0_ is invalid
    _18_new_ascii_instr_63_0_ is invalid
    _17_new_ascii_instr_63_0_ is invalid
    _16_new_ascii_instr_63_0_ is invalid
    _15_new_ascii_instr_63_0_ is invalid
    _14_new_ascii_instr_63_0_ is invalid
    _13_new_ascii_instr_63_0_ is invalid
    _12_new_ascii_instr_63_0_ is invalid
    _11_new_ascii_instr_63_0_ is invalid
    _10_new_ascii_instr_63_0_ is invalid
    _9_new_ascii_instr_63_0_ is invalid
    _8_new_ascii_instr_63_0_ is invalid
    _7_new_ascii_instr_63_0_ is invalid
    _6_new_ascii_instr_63_0_ is invalid
    _5_new_ascii_instr_63_0_ is invalid
    _4_new_ascii_instr_63_0_ is invalid
    _3_new_ascii_instr_63_0_ is invalid
    _2_new_ascii_instr_63_0_ is invalid
    _1_new_ascii_instr_63_0_ is invalid
    _0_new_ascii_instr_63_0_ is invalid
    _0_mem_16bit_buffer_15_0_ is invalid
    _0_prefetched_high_word_0_0_ is invalid
    _0_mem_la_secondword_0_0_ is invalid
    _0_mem_state_1_0_ is invalid
    _0_mem_wstrb_3_0_ is invalid
    _0_mem_wdata_31_0_ is invalid
    _0_mem_addr_31_0_ is invalid
    _0_mem_instr_0_0_ is invalid
    _0_mem_valid_0_0_ is invalid
    _0_mem_rdata_q_31_0_ is invalid
    _0_next_insn_opcode_31_0_ is invalid
    _3_mem_rdata_word_31_0_ is invalid
    _2_mem_rdata_word_31_0_ is invalid
    _1_mem_rdata_word_31_0_ is invalid
    _1_mem_la_wstrb_3_0_ is invalid
    _1_mem_la_wdata_31_0_ is invalid
    _0_mem_rdata_word_31_0_ is invalid
    _0_mem_la_wstrb_3_0_ is invalid
    _0_mem_la_wdata_31_0_ is invalid
    _0_last_mem_valid_0_0_ is invalid
    _0_mem_la_firstword_reg_0_0_ is invalid
    _1_pcpi_int_rd_31_0_ is invalid
    _1_pcpi_int_wr_0_0_ is invalid
    _0_pcpi_int_ready_0_0_ is invalid
    _0_pcpi_int_wait_0_0_ is invalid
    _0_pcpi_int_rd_31_0_ is invalid
    _0_pcpi_int_wr_0_0_ is invalid
    _memwr__cpuregs_picorv32_v_1317_14_EN is invalid
    _memwr__cpuregs_picorv32_v_1317_14_DATA is invalid
    _memwr__cpuregs_picorv32_v_1317_14_ADDR is invalid
    decoded_rs is invalid
    cpuregs_rs2 is invalid
    cpuregs_rs1 is invalid
    cpuregs_wrdata is invalid
    cpuregs_write is invalid
    clear_prefetched_high_word_q is invalid
    alu_lts is invalid
    alu_ltu is invalid
    alu_eq is invalid
    alu_shr is invalid
    alu_shl is invalid
    alu_add_sub is invalid
    alu_wait_2 is invalid
    alu_wait is invalid
    alu_out_0_q is invalid
    alu_out_0 is invalid
    alu_out_q is invalid
    alu_out is invalid
    do_waitirq is invalid
    next_irq_pending is invalid
    pcpi_timeout is invalid
    pcpi_timeout_counter is invalid
    current_pc is invalid
    latched_rd is invalid
    latched_is_lb is invalid
    latched_is_lh is invalid
    latched_is_lu is invalid
    latched_trace is invalid
    latched_compr is invalid
    latched_branch is invalid
    latched_stalu is invalid
    latched_store is invalid
    set_mem_do_wdata is invalid
    set_mem_do_rdata is invalid
    set_mem_do_rinst is invalid
    dbg_ascii_state is invalid
    irq_state is invalid
    cpu_state is invalid
    cached_insn_rd is invalid
    cached_insn_rs2 is invalid
    cached_insn_rs1 is invalid
    cached_insn_opcode is invalid
    cached_insn_imm is invalid
    cached_ascii_instr is invalid
    dbg_valid_insn is invalid
    dbg_next is invalid
    q_insn_rd is invalid
    q_insn_rs2 is invalid
    q_insn_rs1 is invalid
    q_insn_opcode is invalid
    q_insn_imm is invalid
    q_ascii_instr is invalid
    dbg_rs2val_valid is invalid
    dbg_rs1val_valid is invalid
    dbg_rs2val is invalid
    dbg_rs1val is invalid
    dbg_insn_rd is invalid
    dbg_insn_rs2 is invalid
    dbg_insn_rs1 is invalid
    dbg_insn_imm is invalid
    dbg_ascii_instr is invalid
    new_ascii_instr is invalid
    is_compare is invalid
    is_alu_reg_reg is invalid
    is_alu_reg_imm is invalid
    is_lbu_lhu_lw is invalid
    is_beq_bne_blt_bge_bltu_bgeu is invalid
    is_sltiu_bltu_sltu is invalid
    is_slti_blt_slt is invalid
    is_lui_auipc_jal_jalr_addi_add_sub is invalid
    is_sll_srl_sra is invalid
    is_sb_sh_sw is invalid
    is_jalr_addi_slti_sltiu_xori_ori_andi is invalid
    is_slli_srli_srai is invalid
    is_lb_lh_lw_lbu_lhu is invalid
    is_lui_auipc_jal is invalid
    compressed_instr is invalid
    decoder_pseudo_trigger_q is invalid
    decoder_pseudo_trigger is invalid
    decoder_trigger_q is invalid
    decoder_trigger is invalid
    decoded_imm_uj is invalid
    decoded_imm is invalid
    decoded_rs2 is invalid
    decoded_rs1 is invalid
    decoded_rd is invalid
    instr_timer is invalid
    instr_waitirq is invalid
    instr_maskirq is invalid
    instr_retirq is invalid
    instr_setq is invalid
    instr_getq is invalid
    instr_ecall_ebreak is invalid
    instr_rdinstrh is invalid
    instr_rdinstr is invalid
    instr_rdcycleh is invalid
    instr_rdcycle is invalid
    instr_and is invalid
    instr_or is invalid
    instr_sra is invalid
    instr_srl is invalid
    instr_xor is invalid
    instr_sltu is invalid
    instr_slt is invalid
    instr_sll is invalid
    instr_sub is invalid
    instr_add is invalid
    instr_srai is invalid
    instr_srli is invalid
    instr_slli is invalid
    instr_andi is invalid
    instr_ori is invalid
    instr_xori is invalid
    instr_sltiu is invalid
    instr_slti is invalid
    instr_addi is invalid
    instr_sw is invalid
    instr_sh is invalid
    instr_sb is invalid
    instr_lhu is invalid
    instr_lbu is invalid
    instr_lw is invalid
    instr_lh is invalid
    instr_lb is invalid
    instr_bgeu is invalid
    instr_bltu is invalid
    instr_bge is invalid
    instr_blt is invalid
    instr_bne is invalid
    instr_beq is invalid
    instr_jalr is invalid
    instr_jal is invalid
    instr_auipc is invalid
    instr_lui is invalid
    mem_16bit_buffer is invalid
    clear_prefetched_high_word is invalid
    prefetched_high_word is invalid
    last_mem_valid is invalid
    mem_la_firstword_reg is invalid
    mem_la_secondword is invalid
    mem_do_wdata is invalid
    mem_do_rdata is invalid
    mem_do_rinst is invalid
    mem_do_prefetch is invalid
    mem_rdata_q is invalid
    mem_rdata_word is invalid
    mem_wordsize is invalid
    mem_state is invalid
    pcpi_int_ready is invalid
    pcpi_int_wait is invalid
    pcpi_int_rd is invalid
    pcpi_int_wr is invalid
    i is invalid
    timer is invalid
    irq_pending is invalid
    irq_mask is invalid
    irq_active is invalid
    irq_delay is invalid
    dbg_insn_addr is invalid
    dbg_insn_opcode is invalid
    next_insn_opcode is invalid
    reg_sh is invalid
    reg_out is invalid
    reg_op2 is invalid
    reg_op1 is invalid
    reg_next_pc is invalid
    reg_pc is invalid
    count_instr is invalid
    count_cycle is invalid
    trace_data is invalid
    trace_valid is invalid
    eoi is invalid
    pcpi_insn is invalid
    pcpi_valid is invalid
    mem_la_wstrb is invalid
    mem_la_wdata is invalid
    mem_wstrb is invalid
    mem_wdata is invalid
    mem_addr is invalid
    mem_instr is invalid
    mem_valid is invalid
    trap is invalid

    _sshr_picorv32_v_1223_687 <= asUInt(dshr(asSInt(cat(_ternary_picorv32_v_1223_686_Y, reg_op1)), asUInt(bits(reg_op2, 4, 0))))
    _ternary_picorv32_v_1223_686 <= mux(_logic_or_picorv32_v_1223_685_Y, bits(reg_op1, 31, 31), UInt<1>("h0"))
    _logic_or_picorv32_v_1223_685 <= or(neq(instr_sra, UInt(0)), asUInt(neq(instr_srai, UInt(0))))
    _shl_picorv32_v_1222_684 <= bits(dshl(reg_op1, asUInt(bits(reg_op2, 4, 0))), 31, 0)
    _lt_picorv32_v_1221_683 <= lt(reg_op1, asUInt(reg_op2))
    _lt_picorv32_v_1220_682 <= lt(asSInt(reg_op1), asSInt(reg_op2))
    _eq_picorv32_v_1219_681 <= eq(reg_op1, asUInt(reg_op2))
    _ternary_picorv32_v_1218_680 <= mux(instr_sub, _sub_picorv32_v_1218_678_Y, _add_picorv32_v_1218_679_Y)
    _add_picorv32_v_1218_679 <= add(reg_op1, asUInt(reg_op2))
    _sub_picorv32_v_1218_678 <= asUInt(sub(reg_op1, asUInt(reg_op2)))
    _and_picorv32_v_1934_676 <= and(cat(bits(_3_next_irq_pending_31_0_, 31, 3), cat(_18_next_irq_pending_2_2_, bits(_3_next_irq_pending_31_0_, 1, 0))), asUInt(UInt<32>("hffffffff")))
    _logic_or_picorv32_v_1920_675 <= or(neq(_logic_not_picorv32_v_1920_674_Y, UInt(0)), asUInt(neq(mem_done, UInt(0))))
    _logic_not_picorv32_v_1920_674 <= eq(resetn, UInt(0))
    _logic_and_picorv32_v_1916_673 <= and(neq(_logic_and_picorv32_v_1916_672_Y, UInt(0)), asUInt(neq(instr_ecall_ebreak, UInt(0))))
    _logic_and_picorv32_v_1916_672 <= and(neq(_logic_and_picorv32_v_1916_670_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1916_671_Y, UInt(0))))
    _logic_not_picorv32_v_1916_671 <= eq(decoder_pseudo_trigger_q, UInt(0))
    _logic_and_picorv32_v_1916_670 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(decoder_trigger_q, UInt(0))))
    _logic_and_picorv32_v_1911_669 <= and(neq(_logic_and_picorv32_v_1911_667_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1911_668_Y, UInt(0))))
    _logic_not_picorv32_v_1911_668 <= eq(irq_active, UInt(0))
    _logic_and_picorv32_v_1911_667 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_not_picorv32_v_1911_666_Y, UInt(0))))
    _logic_not_picorv32_v_1911_666 <= eq(bits(irq_mask, 2, 2), UInt(0))
    _logic_and_picorv32_v_1909_665 <= and(neq(_logic_and_picorv32_v_1909_662_Y, UInt(0)), asUInt(neq(_ternary_picorv32_v_1909_664_Y, UInt(0))))
    _ternary_picorv32_v_1909_664 <= mux(UInt<1>("h0"), bits(reg_pc, 0, 0), _reduce_or_picorv32_v_1909_663_Y)
    _reduce_or_picorv32_v_1909_663 <= orr(pad(bits(reg_pc, 1, 0), 1))
    _logic_and_picorv32_v_1909_662 <= and(neq(_logic_and_picorv32_v_1909_661_Y, UInt(0)), asUInt(neq(mem_do_rinst, UInt(0))))
    _logic_and_picorv32_v_1909_661 <= and(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(resetn, UInt(0))))
    _logic_and_picorv32_v_1903_660 <= and(neq(_logic_and_picorv32_v_1903_658_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1903_659_Y, UInt(0))))
    _logic_not_picorv32_v_1903_659 <= eq(irq_active, UInt(0))
    _logic_and_picorv32_v_1903_658 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_not_picorv32_v_1903_657_Y, UInt(0))))
    _logic_not_picorv32_v_1903_657 <= eq(bits(irq_mask, 2, 2), UInt(0))
    _logic_and_picorv32_v_1901_656 <= and(neq(_eq_picorv32_v_1901_654_Y, UInt(0)), asUInt(neq(_ne_picorv32_v_1901_655_Y, UInt(0))))
    _ne_picorv32_v_1901_655 <= neq(bits(reg_op1, 0, 0), asUInt(UInt<32>("h00000000")))
    _eq_picorv32_v_1901_654 <= eq(mem_wordsize, asUInt(UInt<32>("h00000001")))
    _logic_and_picorv32_v_1896_653 <= and(neq(_logic_and_picorv32_v_1896_651_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1896_652_Y, UInt(0))))
    _logic_not_picorv32_v_1896_652 <= eq(irq_active, UInt(0))
    _logic_and_picorv32_v_1896_651 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_not_picorv32_v_1896_650_Y, UInt(0))))
    _logic_not_picorv32_v_1896_650 <= eq(bits(irq_mask, 2, 2), UInt(0))
    _logic_and_picorv32_v_1894_649 <= and(neq(_eq_picorv32_v_1894_647_Y, UInt(0)), asUInt(neq(_ne_picorv32_v_1894_648_Y, UInt(0))))
    _ne_picorv32_v_1894_648 <= neq(bits(reg_op1, 1, 0), asUInt(UInt<32>("h00000000")))
    _eq_picorv32_v_1894_647 <= eq(mem_wordsize, asUInt(UInt<32>("h00000000")))
    _logic_and_picorv32_v_1893_646 <= and(neq(_logic_and_picorv32_v_1893_644_Y, UInt(0)), asUInt(neq(_logic_or_picorv32_v_1893_645_Y, UInt(0))))
    _logic_or_picorv32_v_1893_645 <= or(neq(mem_do_rdata, UInt(0)), asUInt(neq(mem_do_wdata, UInt(0))))
    _logic_and_picorv32_v_1893_644 <= and(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(resetn, UInt(0))))
    _logic_and_picorv32_v_1878_643 <= and(neq(_logic_not_picorv32_v_1878_642_Y, UInt(0)), asUInt(neq(mem_done, UInt(0))))
    _logic_not_picorv32_v_1878_642 <= eq(mem_do_prefetch, UInt(0))
    _add_picorv32_v_1875_641 <= add(reg_op1, asUInt(decoded_imm))
    _logic_or_picorv32_v_1865_640 <= or(neq(instr_lh, UInt(0)), asUInt(neq(instr_lhu, UInt(0))))
    _logic_or_picorv32_v_1864_639 <= or(neq(instr_lb, UInt(0)), asUInt(neq(instr_lbu, UInt(0))))
    _logic_not_picorv32_v_1861_638 <= eq(mem_do_rdata, UInt(0))
    _logic_or_picorv32_v_1860_637 <= or(neq(_logic_not_picorv32_v_1860_636_Y, UInt(0)), asUInt(neq(mem_done, UInt(0))))
    _logic_not_picorv32_v_1860_636 <= eq(mem_do_prefetch, UInt(0))
    _logic_and_picorv32_v_1850_635 <= and(neq(_logic_not_picorv32_v_1850_634_Y, UInt(0)), asUInt(neq(mem_done, UInt(0))))
    _logic_not_picorv32_v_1850_634 <= eq(mem_do_prefetch, UInt(0))
    _add_picorv32_v_1847_633 <= add(reg_op1, asUInt(decoded_imm))
    _logic_not_picorv32_v_1836_632 <= eq(mem_do_wdata, UInt(0))
    _logic_or_picorv32_v_1835_631 <= or(neq(_logic_not_picorv32_v_1835_630_Y, UInt(0)), asUInt(neq(mem_done, UInt(0))))
    _logic_not_picorv32_v_1835_630 <= eq(mem_do_prefetch, UInt(0))
    _sub_picorv32_v_1828_629 <= asUInt(sub(reg_sh, asUInt(UInt<32>("h00000001"))))
    _sshr_picorv32_v_1826_628 <= asUInt(shr(asSInt(reg_op1), UInt<32>("h00000001")))
    _logic_or_picorv32_v_1826_627 <= or(neq(instr_srai, UInt(0)), asUInt(neq(instr_sra, UInt(0))))
    _shr_picorv32_v_1825_626 <= shr(reg_op1, UInt<32>("h00000001"))
    _logic_or_picorv32_v_1825_625 <= or(neq(instr_srli, UInt(0)), asUInt(neq(instr_srl, UInt(0))))
    _shl_picorv32_v_1824_624 <= bits(shl(reg_op1, UInt<32>("h00000001")), 31, 0)
    _logic_or_picorv32_v_1824_623 <= or(neq(instr_slli, UInt(0)), asUInt(neq(instr_sll, UInt(0))))
    _sub_picorv32_v_1820_622 <= asUInt(sub(reg_sh, asUInt(UInt<32>("h00000004"))))
    _sshr_picorv32_v_1818_621 <= asUInt(shr(asSInt(reg_op1), UInt<32>("h00000004")))
    _logic_or_picorv32_v_1818_620 <= or(neq(instr_srai, UInt(0)), asUInt(neq(instr_sra, UInt(0))))
    _shr_picorv32_v_1817_619 <= shr(reg_op1, UInt<32>("h00000004"))
    _logic_or_picorv32_v_1817_618 <= or(neq(instr_srli, UInt(0)), asUInt(neq(instr_srl, UInt(0))))
    _shl_picorv32_v_1816_617 <= bits(shl(reg_op1, UInt<32>("h00000004")), 31, 0)
    _logic_or_picorv32_v_1816_616 <= or(neq(instr_slli, UInt(0)), asUInt(neq(instr_sll, UInt(0))))
    _logic_and_picorv32_v_1813_615 <= and(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(_ge_picorv32_v_1813_614_Y, UInt(0))))
    _ge_picorv32_v_1813_614 <= geq(reg_sh, asUInt(UInt<32>("h00000004")))
    _eq_picorv32_v_1809_613 <= eq(reg_sh, asUInt(UInt<32>("h00000000")))
    _ternary_picorv32_v_1795_612 <= mux(UInt<1>("h0"), alu_out_0_q, alu_out_0)
    _ternary_picorv32_v_1792_611 <= mux(UInt<1>("h0"), alu_out_0_q, alu_out_0)
    _ternary_picorv32_v_1791_610 <= mux(UInt<1>("h0"), alu_out_0_q, alu_out_0)
    _logic_and_picorv32_v_1786_609 <= and(neq(mem_do_prefetch, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1786_608_Y, UInt(0))))
    _logic_not_picorv32_v_1786_608 <= eq(alu_wait_2, UInt(0))
    _logic_and_picorv32_v_1785_607 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_or_picorv32_v_1785_606_Y, UInt(0))))
    _logic_or_picorv32_v_1785_606 <= or(neq(alu_wait, UInt(0)), asUInt(neq(alu_wait_2, UInt(0))))
    _add_picorv32_v_1784_605 <= add(reg_pc, asUInt(decoded_imm))
    _logic_and_picorv32_v_1774_604 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_and_picorv32_v_1774_603_Y, UInt(0))))
    _logic_and_picorv32_v_1774_603 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(is_beq_bne_blt_bge_bltu_bgeu, UInt(0))))
    _logic_or_picorv32_v_1773_602 <= or(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_and_picorv32_v_1773_601_Y, UInt(0))))
    _logic_and_picorv32_v_1773_601 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(is_beq_bne_blt_bge_bltu_bgeu, UInt(0))))
    _logic_and_picorv32_v_1769_600 <= and(neq(is_sll_srl_sra, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_1758_599 <= and(neq(_logic_and_picorv32_v_1758_597_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1758_598_Y, UInt(0))))
    _logic_not_picorv32_v_1758_598 <= eq(irq_active, UInt(0))
    _logic_and_picorv32_v_1758_597 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_not_picorv32_v_1758_596_Y, UInt(0))))
    _logic_not_picorv32_v_1758_596 <= eq(bits(irq_mask, 1, 1), UInt(0))
    _logic_and_picorv32_v_1755_595 <= and(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(_logic_or_picorv32_v_1755_594_Y, UInt(0))))
    _logic_or_picorv32_v_1755_594 <= or(neq(pcpi_timeout, UInt(0)), asUInt(neq(instr_ecall_ebreak, UInt(0))))
    _logic_and_picorv32_v_1746_593 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(instr_trap, UInt(0))))
    _logic_and_picorv32_v_1724_592 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_and_picorv32_v_1724_591_Y, UInt(0))))
    _logic_and_picorv32_v_1724_591 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(is_beq_bne_blt_bge_bltu_bgeu, UInt(0))))
    _logic_or_picorv32_v_1723_590 <= or(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_and_picorv32_v_1723_589_Y, UInt(0))))
    _logic_and_picorv32_v_1723_589 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(is_beq_bne_blt_bge_bltu_bgeu, UInt(0))))
    _logic_and_picorv32_v_1719_588 <= and(neq(is_sll_srl_sra, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _ternary_picorv32_v_1695_587 <= mux(_logic_and_picorv32_v_1695_585_Y, _extend_picorv32_v_1695_586_Y, decoded_imm)
    _logic_and_picorv32_v_1695_585 <= and(neq(is_slli_srli_srai, UInt(0)), asUInt(neq(UInt<1>("h0"), UInt(0))))
    _logic_and_picorv32_v_1690_584 <= and(neq(is_slli_srli_srai, UInt(0)), asUInt(neq(UInt<1>("h0"), UInt(0))))
    _logic_and_picorv32_v_1682_583 <= and(neq(is_slli_srli_srai, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_1674_582 <= and(neq(is_lb_lh_lw_lbu_lhu, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1674_581_Y, UInt(0))))
    _logic_not_picorv32_v_1674_581 <= eq(instr_trap, UInt(0))
    _logic_and_picorv32_v_1665_580 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(instr_timer, UInt(0))))
    _or_picorv32_v_1660_579 <= or(cpuregs_rs1, asUInt(UInt<32>("h00000000")))
    _logic_and_picorv32_v_1656_578 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(instr_maskirq, UInt(0))))
    _ternary_picorv32_v_1651_577 <= mux(UInt<1>("h1"), _and_picorv32_v_1651_576_Y, cpuregs_rs1)
    _and_picorv32_v_1651_576 <= and(cpuregs_rs1, asUInt(UInt<32>("hfffffffe")))
    _logic_and_picorv32_v_1645_575 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(instr_retirq, UInt(0))))
    _or_picorv32_v_1641_574 <= or(latched_rd, asUInt(UInt<32>("h00000020")))
    _logic_and_picorv32_v_1636_573 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(instr_setq, UInt(0))))
    _logic_and_picorv32_v_1628_572 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(instr_getq, UInt(0))))
    _ternary_picorv32_v_1620_571 <= mux(instr_lui, UInt<32>("h00000000"), reg_pc)
    _logic_and_picorv32_v_1613_570 <= and(neq(instr_rdinstrh, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_1609_569 <= and(neq(instr_rdcycleh, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_1604_568 <= and(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(is_rdcycle_rdcycleh_rdinstr_rdinstrh, UInt(0))))
    _logic_and_picorv32_v_1597_567 <= and(neq(_logic_and_picorv32_v_1597_565_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1597_566_Y, UInt(0))))
    _logic_not_picorv32_v_1597_566 <= eq(irq_active, UInt(0))
    _logic_and_picorv32_v_1597_565 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_not_picorv32_v_1597_564_Y, UInt(0))))
    _logic_not_picorv32_v_1597_564 <= eq(bits(irq_mask, 1, 1), UInt(0))
    _logic_and_picorv32_v_1563_563 <= and(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(instr_trap, UInt(0))))
    _logic_and_picorv32_v_1551_562 <= and(neq(_logic_not_picorv32_v_1551_560_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1551_561_Y, UInt(0))))
    _logic_not_picorv32_v_1551_561 <= eq(instr_retirq, UInt(0))
    _logic_not_picorv32_v_1551_560 <= eq(instr_jalr, UInt(0))
    _add_picorv32_v_1547_559 <= add(_3_current_pc_31_0_, asUInt(decoded_imm_uj))
    _add_picorv32_v_1542_558 <= add(count_instr, asUInt(UInt<32>("h00000001")))
    _add_picorv32_v_1538_557 <= add(_3_current_pc_31_0_, asUInt(_ternary_picorv32_v_1538_556_Y))
    _ternary_picorv32_v_1538_556 <= mux(compressed_instr, UInt<32>("h00000002"), UInt<32>("h00000004"))
    _add_picorv32_v_1530_555 <= add(_3_current_pc_31_0_, asUInt(_ternary_picorv32_v_1530_554_Y))
    _ternary_picorv32_v_1530_554 <= mux(compressed_instr, UInt<32>("h00000002"), UInt<32>("h00000004"))
    _reduce_bool_picorv32_v_1527_553 <= neq(irq_pending, UInt<32>(0))
    _logic_and_picorv32_v_1526_552 <= and(neq(_logic_and_picorv32_v_1526_551_Y, UInt(0)), asUInt(neq(instr_waitirq, UInt(0))))
    _logic_and_picorv32_v_1526_551 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_or_picorv32_v_1526_550_Y, UInt(0))))
    _logic_or_picorv32_v_1526_550 <= or(neq(decoder_trigger, UInt(0)), asUInt(neq(do_waitirq, UInt(0))))
    _or_picorv32_v_1522_549 <= or(UInt<32>("h00000020"), asUInt(bits(irq_state, 0, 0)))
    _ternary_picorv32_v_1519_548 <= mux(_eq_picorv32_v_1518_545_Y, UInt<2>("h1"), _ternary_picorv32_v_1519_547_Y)
    _ternary_picorv32_v_1519_547 <= mux(_eq_picorv32_v_1519_546_Y, UInt<2>("h2"), UInt<2>("h0"))
    _eq_picorv32_v_1519_546 <= eq(irq_state, asUInt(UInt<2>("h1")))
    _eq_picorv32_v_1518_545 <= eq(irq_state, asUInt(UInt<2>("h0")))
    _logic_and_picorv32_v_1516_544 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_or_picorv32_v_1516_543_Y, UInt(0))))
    _logic_or_picorv32_v_1516_543 <= or(neq(_logic_and_picorv32_v_1516_542_Y, UInt(0)), asUInt(neq(irq_state, UInt(0))))
    _logic_and_picorv32_v_1516_542 <= and(neq(_logic_and_picorv32_v_1516_538_Y, UInt(0)), asUInt(neq(_reduce_or_picorv32_v_1516_541_Y, UInt(0))))
    _reduce_or_picorv32_v_1516_541 <= orr(pad(_and_picorv32_v_1516_540_Y, 1))
    _and_picorv32_v_1516_540 <= and(irq_pending, asUInt(_not_picorv32_v_1516_539_Y))
    _not_picorv32_v_1516_539 <= not(pad(irq_mask, 32))
    _logic_and_picorv32_v_1516_538 <= and(neq(_logic_and_picorv32_v_1516_536_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1516_537_Y, UInt(0))))
    _logic_not_picorv32_v_1516_537 <= eq(irq_delay, UInt(0))
    _logic_and_picorv32_v_1516_536 <= and(neq(decoder_trigger, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1516_535_Y, UInt(0))))
    _logic_not_picorv32_v_1516_535 <= eq(irq_active, UInt(0))
    _or_picorv32_v_1501_534 <= or(_ternary_picorv32_v_1501_532_Y, asUInt(cat(UInt<4>("h0"), _ternary_picorv32_v_1501_533_Y)))
    _ternary_picorv32_v_1501_533 <= mux(latched_stalu, alu_out_q, reg_out)
    _ternary_picorv32_v_1501_532 <= mux(irq_active, UInt<36>("h800000000"), _extend_picorv32_v_1501_531_Y)
    _or_picorv32_v_1499_530 <= or(_or_picorv32_v_1499_528_Y, asUInt(_and_picorv32_v_1499_529_Y))
    _and_picorv32_v_1499_529 <= and(_3_current_pc_31_0_, asUInt(UInt<32>("hfffffffe")))
    _or_picorv32_v_1499_528 <= or(_ternary_picorv32_v_1499_527_Y, asUInt(UInt<36>("h100000000")))
    _ternary_picorv32_v_1499_527 <= mux(irq_active, UInt<36>("h800000000"), _extend_picorv32_v_1499_526_Y)
    _logic_and_picorv32_v_1495_525 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(latched_trace, UInt(0))))
    _and_picorv32_v_1491_524 <= and(cat(UInt<31>("h00000000"), _1_next_irq_pending_0_0_), asUInt(irq_mask))
    _and_picorv32_v_1490_523 <= and(irq_pending, asUInt(_not_picorv32_v_1490_522_Y))
    _not_picorv32_v_1490_522 <= not(pad(irq_mask, 32))
    _logic_and_picorv32_v_1489_521 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(bits(irq_state, 1, 1), UInt(0))))
    _logic_and_picorv32_v_1484_520 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(bits(irq_state, 0, 0), UInt(0))))
    _logic_and_picorv32_v_1481_519 <= and(neq(latched_store, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1481_518_Y, UInt(0))))
    _logic_not_picorv32_v_1481_518 <= eq(latched_branch, UInt(0))
    _ternary_picorv32_v_1478_517 <= mux(latched_store, _and_picorv32_v_1478_516_Y, reg_next_pc)
    _and_picorv32_v_1478_516 <= and(_ternary_picorv32_v_1478_515_Y, asUInt(UInt<32>("hfffffffe")))
    _ternary_picorv32_v_1478_515 <= mux(latched_stalu, alu_out_q, reg_out)
    _logic_and_picorv32_v_1470_514 <= and(neq(_logic_not_picorv32_v_1470_512_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1470_513_Y, UInt(0))))
    _logic_not_picorv32_v_1470_513 <= eq(do_waitirq, UInt(0))
    _logic_not_picorv32_v_1470_512 <= eq(decoder_trigger, UInt(0))
    _logic_not_picorv32_v_1435_511 <= eq(resetn, UInt(0))
    _logic_and_picorv32_v_1424_510 <= and(neq(mem_do_rinst, UInt(0)), asUInt(neq(mem_done, UInt(0))))
    _sub_picorv32_v_1417_509 <= asUInt(sub(timer, asUInt(UInt<32>("h00000001"))))
    _eq_picorv32_v_1415_508 <= eq(_sub_picorv32_v_1415_507_Y, asUInt(UInt<32>("h00000000")))
    _sub_picorv32_v_1415_507 <= asUInt(sub(timer, asUInt(UInt<32>("h00000001"))))
    _logic_and_picorv32_v_1414_506 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(timer, UInt(0))))
    _ternary_picorv32_v_1405_505 <= mux(resetn, _add_picorv32_v_1405_503_Y, _extend_picorv32_v_1405_504_Y)
    _add_picorv32_v_1405_503 <= add(count_cycle, asUInt(UInt<32>("h00000001")))
    _logic_and_picorv32_v_1372_501 <= and(neq(_logic_and_picorv32_v_1372_494_Y, UInt(0)), asUInt(neq(_logic_or_picorv32_v_1372_500_Y, UInt(0))))
    _logic_or_picorv32_v_1372_500 <= or(neq(_logic_or_picorv32_v_1372_496_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1372_499_Y, UInt(0))))
    _logic_not_picorv32_v_1372_499 <= eq(_and_picorv32_v_1372_498_Y, UInt(0))
    _and_picorv32_v_1372_498 <= and(irq_pending, asUInt(_not_picorv32_v_1372_497_Y))
    _not_picorv32_v_1372_497 <= not(pad(irq_mask, 32))
    _logic_or_picorv32_v_1372_496 <= or(neq(_logic_or_picorv32_v_1372_495_Y, UInt(0)), asUInt(neq(irq_active, UInt(0))))
    _logic_or_picorv32_v_1372_495 <= or(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(irq_delay, UInt(0))))
    _logic_and_picorv32_v_1372_494 <= and(neq(_eq_picorv32_v_1372_493_Y, UInt(0)), asUInt(neq(decoder_trigger, UInt(0))))
    _eq_picorv32_v_1372_493 <= eq(cpu_state, asUInt(UInt<8>("h40")))
    _ternary_picorv32_v_1325_492 <= mux(_reduce_bool_picorv32_v_1325_491_Y, _memrd__cpuregs_picorv32_v_1325_490_DATA, UInt<32>("h00000000"))
    _reduce_bool_picorv32_v_1325_491 <= neq(decoded_rs2, UInt<5>(0))
    _ternary_picorv32_v_1324_489 <= mux(_reduce_bool_picorv32_v_1324_488_Y, _memrd__cpuregs_picorv32_v_1324_487_DATA, UInt<32>("h00000000"))
    _reduce_bool_picorv32_v_1324_488 <= neq(decoded_rs1, UInt<5>(0))
    _logic_and_picorv32_v_1316_485 <= and(neq(_logic_and_picorv32_v_1316_484_Y, UInt(0)), asUInt(neq(latched_rd, UInt(0))))
    _logic_and_picorv32_v_1316_484 <= and(neq(resetn, UInt(0)), asUInt(neq(cpuregs_write, UInt(0))))
    _and_picorv32_v_1307_479 <= and(irq_pending, asUInt(_not_picorv32_v_1307_478_Y))
    _not_picorv32_v_1307_478 <= not(pad(irq_mask, 32))
    _logic_and_picorv32_v_1306_477 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(bits(irq_state, 1, 1), UInt(0))))
    _or_picorv32_v_1303_476 <= or(reg_next_pc, asUInt(latched_compr))
    _logic_and_picorv32_v_1302_475 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(bits(irq_state, 0, 0), UInt(0))))
    _ternary_picorv32_v_1299_474 <= mux(latched_stalu, alu_out_q, reg_out)
    _logic_and_picorv32_v_1298_473 <= and(neq(latched_store, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1298_472_Y, UInt(0))))
    _logic_not_picorv32_v_1298_472 <= eq(latched_branch, UInt(0))
    _add_picorv32_v_1295_471 <= add(reg_pc, asUInt(_ternary_picorv32_v_1295_470_Y))
    _ternary_picorv32_v_1295_470 <= mux(latched_compr, UInt<32>("h00000002"), UInt<32>("h00000004"))
    _eq_picorv32_v_1291_469 <= eq(cpu_state, asUInt(UInt<8>("h40")))
    _logic_or_picorv32_v_1277_467 <= or(neq(_logic_or_picorv32_v_1277_465_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1277_466_Y, UInt(0))))
    _logic_not_picorv32_v_1277_466 <= eq(resetn, UInt(0))
    _logic_or_picorv32_v_1277_465 <= or(neq(latched_branch, UInt(0)), asUInt(neq(irq_state, UInt(0))))
    _logic_not_picorv32_v_1275_464 <= eq(prefetched_high_word, UInt(0))
    _logic_and_picorv32_v_1260_461 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_or_picorv32_v_1260_460_Y, UInt(0))))
    _logic_or_picorv32_v_1260_460 <= or(neq(_logic_or_picorv32_v_1260_459_Y, UInt(0)), asUInt(neq(instr_srai, UInt(0))))
    _logic_or_picorv32_v_1260_459 <= or(neq(_logic_or_picorv32_v_1260_458_Y, UInt(0)), asUInt(neq(instr_sra, UInt(0))))
    _logic_or_picorv32_v_1260_458 <= or(neq(instr_srl, UInt(0)), asUInt(neq(instr_srli, UInt(0))))
    _logic_and_picorv32_v_1258_457 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_or_picorv32_v_1258_456_Y, UInt(0))))
    _logic_or_picorv32_v_1258_456 <= or(neq(instr_sll, UInt(0)), asUInt(neq(instr_slli, UInt(0))))
    _and_picorv32_v_1257_455 <= and(reg_op1, asUInt(reg_op2))
    _logic_or_picorv32_v_1256_454 <= or(neq(instr_andi, UInt(0)), asUInt(neq(instr_and, UInt(0))))
    _or_picorv32_v_1255_453 <= or(reg_op1, asUInt(reg_op2))
    _logic_or_picorv32_v_1254_452 <= or(neq(instr_ori, UInt(0)), asUInt(neq(instr_or, UInt(0))))
    _xor_picorv32_v_1253_451 <= xor(reg_op1, asUInt(reg_op2))
    _logic_or_picorv32_v_1252_450 <= or(neq(instr_xori, UInt(0)), asUInt(neq(instr_xor, UInt(0))))
    _logic_and_picorv32_v_1241_449 <= and(neq(is_sltiu_bltu_sltu, UInt(0)), asUInt(neq(_logic_or_picorv32_v_1241_448_Y, UInt(0))))
    _logic_or_picorv32_v_1241_448 <= or(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(_logic_not_picorv32_v_1241_447_Y, UInt(0))))
    _logic_not_picorv32_v_1241_447 <= eq(cat(instr_beq, cat(instr_bne, cat(instr_bge, instr_bgeu))), UInt(0))
    _logic_and_picorv32_v_1239_446 <= and(neq(is_slti_blt_slt, UInt(0)), asUInt(neq(_logic_or_picorv32_v_1239_445_Y, UInt(0))))
    _logic_or_picorv32_v_1239_445 <= or(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(_logic_not_picorv32_v_1239_444_Y, UInt(0))))
    _logic_not_picorv32_v_1239_444 <= eq(cat(instr_beq, cat(instr_bne, cat(instr_bge, instr_bgeu))), UInt(0))
    _logic_not_picorv32_v_1238_443 <= eq(alu_ltu, UInt(0))
    _logic_not_picorv32_v_1236_442 <= eq(alu_lts, UInt(0))
    _logic_not_picorv32_v_1234_441 <= eq(alu_eq, UInt(0))
    _ternary_picorv32_v_1191_439 <= mux(_logic_and_picorv32_v_1191_437_Y, _and_picorv32_v_1191_438_Y, reg_next_pc)
    _and_picorv32_v_1191_438 <= and(reg_out, asUInt(UInt<32>("hfffffffe")))
    _logic_and_picorv32_v_1191_437 <= and(neq(latched_store, UInt(0)), asUInt(neq(latched_branch, UInt(0))))
    _eq_picorv32_v_1173_436 <= eq(cpu_state, asUInt(UInt<8>("h01")))
    _eq_picorv32_v_1172_435 <= eq(cpu_state, asUInt(UInt<8>("h02")))
    _eq_picorv32_v_1171_434 <= eq(cpu_state, asUInt(UInt<8>("h04")))
    _eq_picorv32_v_1170_433 <= eq(cpu_state, asUInt(UInt<8>("h08")))
    _eq_picorv32_v_1169_432 <= eq(cpu_state, asUInt(UInt<8>("h10")))
    _eq_picorv32_v_1168_431 <= eq(cpu_state, asUInt(UInt<8>("h20")))
    _eq_picorv32_v_1167_430 <= eq(cpu_state, asUInt(UInt<8>("h40")))
    _eq_picorv32_v_1166_429 <= eq(cpu_state, asUInt(UInt<8>("h80")))
    _logic_not_picorv32_v_1116_427 <= eq(resetn, UInt(0))
    _reduce_or_picorv32_v_1105_426 <= orr(pad(cat(instr_jalr, cat(is_lb_lh_lw_lbu_lhu, is_alu_reg_imm)), 1))
    _shl_picorv32_v_1104_425 <= bits(shl(bits(mem_rdata_q, 31, 12), UInt<32>("h0000000c")), 31, 0)
    _reduce_or_picorv32_v_1103_424 <= orr(pad(cat(instr_lui, instr_auipc), 1))
    _logic_and_picorv32_v_1094_423 <= and(neq(is_alu_reg_reg, UInt(0)), asUInt(neq(_reduce_or_picorv32_v_1094_422_Y, UInt(0))))
    _reduce_or_picorv32_v_1094_422 <= orr(pad(cat(_logic_and_picorv32_v_1091_421_Y, cat(_logic_and_picorv32_v_1092_418_Y, _logic_and_picorv32_v_1094_415_Y)), 1))
    _logic_and_picorv32_v_1091_421 <= and(neq(_eq_picorv32_v_1091_419_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1091_420_Y, UInt(0))))
    _eq_picorv32_v_1091_420 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _eq_picorv32_v_1091_419 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h1")))
    _logic_and_picorv32_v_1092_418 <= and(neq(_eq_picorv32_v_1092_416_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1092_417_Y, UInt(0))))
    _eq_picorv32_v_1092_417 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _eq_picorv32_v_1092_416 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h5")))
    _logic_and_picorv32_v_1094_415 <= and(neq(_eq_picorv32_v_1093_413_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1094_414_Y, UInt(0))))
    _eq_picorv32_v_1094_414 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h20")))
    _eq_picorv32_v_1093_413 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h5")))
    _logic_or_picorv32_v_1088_412 <= or(neq(instr_jalr, UInt(0)), asUInt(neq(_logic_and_picorv32_v_1088_411_Y, UInt(0))))
    _logic_and_picorv32_v_1088_411 <= and(neq(is_alu_reg_imm, UInt(0)), asUInt(neq(_reduce_or_picorv32_v_1088_410_Y, UInt(0))))
    _reduce_or_picorv32_v_1088_410 <= orr(pad(cat(_eq_picorv32_v_1082_409_Y, cat(_eq_picorv32_v_1083_408_Y, cat(_eq_picorv32_v_1084_407_Y, cat(_eq_picorv32_v_1085_406_Y, cat(_eq_picorv32_v_1086_405_Y, _eq_picorv32_v_1088_404_Y))))), 1))
    _eq_picorv32_v_1082_409 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h0")))
    _eq_picorv32_v_1083_408 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h2")))
    _eq_picorv32_v_1084_407 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h3")))
    _eq_picorv32_v_1085_406 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h4")))
    _eq_picorv32_v_1086_405 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h6")))
    _eq_picorv32_v_1088_404 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h7")))
    _logic_and_picorv32_v_1079_403 <= and(neq(is_alu_reg_imm, UInt(0)), asUInt(neq(_reduce_or_picorv32_v_1079_402_Y, UInt(0))))
    _reduce_or_picorv32_v_1079_402 <= orr(pad(cat(_logic_and_picorv32_v_1076_401_Y, cat(_logic_and_picorv32_v_1077_398_Y, _logic_and_picorv32_v_1079_395_Y)), 1))
    _logic_and_picorv32_v_1076_401 <= and(neq(_eq_picorv32_v_1076_399_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1076_400_Y, UInt(0))))
    _eq_picorv32_v_1076_400 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _eq_picorv32_v_1076_399 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h1")))
    _logic_and_picorv32_v_1077_398 <= and(neq(_eq_picorv32_v_1077_396_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1077_397_Y, UInt(0))))
    _eq_picorv32_v_1077_397 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _eq_picorv32_v_1077_396 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h5")))
    _logic_and_picorv32_v_1079_395 <= and(neq(_eq_picorv32_v_1078_393_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1079_394_Y, UInt(0))))
    _eq_picorv32_v_1079_394 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h20")))
    _eq_picorv32_v_1078_393 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h5")))
    _logic_and_picorv32_v_1073_392 <= and(neq(_logic_and_picorv32_v_1073_391_Y, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_1073_391 <= and(neq(_logic_and_picorv32_v_1073_390_Y, UInt(0)), asUInt(neq(UInt<1>("h0"), UInt(0))))
    _logic_and_picorv32_v_1073_390 <= and(neq(_eq_picorv32_v_1073_388_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1073_389_Y, UInt(0))))
    _eq_picorv32_v_1073_389 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h05")))
    _eq_picorv32_v_1073_388 <= eq(bits(mem_rdata_q, 6, 0), asUInt(UInt<7>("h0b")))
    _logic_and_picorv32_v_1072_387 <= and(neq(_logic_and_picorv32_v_1072_386_Y, UInt(0)), asUInt(neq(UInt<1>("h0"), UInt(0))))
    _logic_and_picorv32_v_1072_386 <= and(neq(_eq_picorv32_v_1072_384_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1072_385_Y, UInt(0))))
    _eq_picorv32_v_1072_385 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h03")))
    _eq_picorv32_v_1072_384 <= eq(bits(mem_rdata_q, 6, 0), asUInt(UInt<7>("h0b")))
    _logic_and_picorv32_v_1071_383 <= and(neq(_logic_and_picorv32_v_1071_382_Y, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_1071_382 <= and(neq(_logic_and_picorv32_v_1071_381_Y, UInt(0)), asUInt(neq(UInt<1>("h0"), UInt(0))))
    _logic_and_picorv32_v_1071_381 <= and(neq(_eq_picorv32_v_1071_379_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1071_380_Y, UInt(0))))
    _eq_picorv32_v_1071_380 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h01")))
    _eq_picorv32_v_1071_379 <= eq(bits(mem_rdata_q, 6, 0), asUInt(UInt<7>("h0b")))
    _logic_and_picorv32_v_1070_378 <= and(neq(_logic_and_picorv32_v_1070_377_Y, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_1070_377 <= and(neq(_logic_and_picorv32_v_1070_376_Y, UInt(0)), asUInt(neq(UInt<1>("h0"), UInt(0))))
    _logic_and_picorv32_v_1070_376 <= and(neq(_eq_picorv32_v_1070_374_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1070_375_Y, UInt(0))))
    _eq_picorv32_v_1070_375 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _eq_picorv32_v_1070_374 <= eq(bits(mem_rdata_q, 6, 0), asUInt(UInt<7>("h0b")))
    _logic_or_picorv32_v_1068_373 <= or(neq(_logic_and_picorv32_v_1067_370_Y, UInt(0)), asUInt(neq(_logic_and_picorv32_v_1068_372_Y, UInt(0))))
    _logic_and_picorv32_v_1068_372 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_eq_picorv32_v_1068_371_Y, UInt(0))))
    _eq_picorv32_v_1068_371 <= eq(bits(mem_rdata_q, 15, 0), asUInt(UInt<16>("h9002")))
    _logic_and_picorv32_v_1067_370 <= and(neq(_logic_and_picorv32_v_1067_368_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1067_369_Y, UInt(0))))
    _logic_not_picorv32_v_1067_369 <= eq(bits(mem_rdata_q, 19, 7), UInt(0))
    _logic_and_picorv32_v_1067_368 <= and(neq(_eq_picorv32_v_1067_366_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1067_367_Y, UInt(0))))
    _logic_not_picorv32_v_1067_367 <= eq(bits(mem_rdata_q, 31, 21), UInt(0))
    _eq_picorv32_v_1067_366 <= eq(bits(mem_rdata_q, 6, 0), asUInt(UInt<7>("h73")))
    _logic_and_picorv32_v_1065_365 <= and(neq(_logic_and_picorv32_v_1065_364_Y, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_1065_364 <= and(neq(_logic_and_picorv32_v_1065_363_Y, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_1065_363 <= and(neq(_eq_picorv32_v_1065_361_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1065_362_Y, UInt(0))))
    _eq_picorv32_v_1065_362 <= eq(bits(mem_rdata_q, 31, 12), asUInt(UInt<32>("h000c8202")))
    _eq_picorv32_v_1065_361 <= eq(bits(mem_rdata_q, 6, 0), asUInt(UInt<7>("h73")))
    _logic_and_picorv32_v_1064_360 <= and(neq(_logic_and_picorv32_v_1064_359_Y, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_1064_359 <= and(neq(_eq_picorv32_v_1064_357_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1064_358_Y, UInt(0))))
    _eq_picorv32_v_1064_358 <= eq(bits(mem_rdata_q, 31, 12), asUInt(UInt<32>("h000c0202")))
    _eq_picorv32_v_1064_357 <= eq(bits(mem_rdata_q, 6, 0), asUInt(UInt<7>("h73")))
    _logic_and_picorv32_v_1063_356 <= and(neq(_logic_and_picorv32_v_1063_355_Y, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_1063_355 <= and(neq(_logic_or_picorv32_v_1063_354_Y, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_or_picorv32_v_1063_354 <= or(neq(_logic_and_picorv32_v_1062_350_Y, UInt(0)), asUInt(neq(_logic_and_picorv32_v_1063_353_Y, UInt(0))))
    _logic_and_picorv32_v_1063_353 <= and(neq(_eq_picorv32_v_1063_351_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1063_352_Y, UInt(0))))
    _eq_picorv32_v_1063_352 <= eq(bits(mem_rdata_q, 31, 12), asUInt(UInt<32>("h000c8102")))
    _eq_picorv32_v_1063_351 <= eq(bits(mem_rdata_q, 6, 0), asUInt(UInt<7>("h73")))
    _logic_and_picorv32_v_1062_350 <= and(neq(_eq_picorv32_v_1062_348_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1062_349_Y, UInt(0))))
    _eq_picorv32_v_1062_349 <= eq(bits(mem_rdata_q, 31, 12), asUInt(UInt<32>("h000c8002")))
    _eq_picorv32_v_1062_348 <= eq(bits(mem_rdata_q, 6, 0), asUInt(UInt<7>("h73")))
    _logic_and_picorv32_v_1061_347 <= and(neq(_logic_or_picorv32_v_1061_346_Y, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_or_picorv32_v_1061_346 <= or(neq(_logic_and_picorv32_v_1060_342_Y, UInt(0)), asUInt(neq(_logic_and_picorv32_v_1061_345_Y, UInt(0))))
    _logic_and_picorv32_v_1061_345 <= and(neq(_eq_picorv32_v_1061_343_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1061_344_Y, UInt(0))))
    _eq_picorv32_v_1061_344 <= eq(bits(mem_rdata_q, 31, 12), asUInt(UInt<32>("h000c0102")))
    _eq_picorv32_v_1061_343 <= eq(bits(mem_rdata_q, 6, 0), asUInt(UInt<7>("h73")))
    _logic_and_picorv32_v_1060_342 <= and(neq(_eq_picorv32_v_1060_340_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1060_341_Y, UInt(0))))
    _eq_picorv32_v_1060_341 <= eq(bits(mem_rdata_q, 31, 12), asUInt(UInt<32>("h000c0002")))
    _eq_picorv32_v_1060_340 <= eq(bits(mem_rdata_q, 6, 0), asUInt(UInt<7>("h73")))
    _logic_and_picorv32_v_1058_339 <= and(neq(_logic_and_picorv32_v_1058_337_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1058_338_Y, UInt(0))))
    _eq_picorv32_v_1058_338 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _logic_and_picorv32_v_1058_337 <= and(neq(is_alu_reg_reg, UInt(0)), asUInt(neq(_eq_picorv32_v_1058_336_Y, UInt(0))))
    _eq_picorv32_v_1058_336 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h7")))
    _logic_and_picorv32_v_1057_335 <= and(neq(_logic_and_picorv32_v_1057_333_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1057_334_Y, UInt(0))))
    _eq_picorv32_v_1057_334 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _logic_and_picorv32_v_1057_333 <= and(neq(is_alu_reg_reg, UInt(0)), asUInt(neq(_eq_picorv32_v_1057_332_Y, UInt(0))))
    _eq_picorv32_v_1057_332 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h6")))
    _logic_and_picorv32_v_1056_331 <= and(neq(_logic_and_picorv32_v_1056_329_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1056_330_Y, UInt(0))))
    _eq_picorv32_v_1056_330 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h20")))
    _logic_and_picorv32_v_1056_329 <= and(neq(is_alu_reg_reg, UInt(0)), asUInt(neq(_eq_picorv32_v_1056_328_Y, UInt(0))))
    _eq_picorv32_v_1056_328 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h5")))
    _logic_and_picorv32_v_1055_327 <= and(neq(_logic_and_picorv32_v_1055_325_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1055_326_Y, UInt(0))))
    _eq_picorv32_v_1055_326 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _logic_and_picorv32_v_1055_325 <= and(neq(is_alu_reg_reg, UInt(0)), asUInt(neq(_eq_picorv32_v_1055_324_Y, UInt(0))))
    _eq_picorv32_v_1055_324 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h5")))
    _logic_and_picorv32_v_1054_323 <= and(neq(_logic_and_picorv32_v_1054_321_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1054_322_Y, UInt(0))))
    _eq_picorv32_v_1054_322 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _logic_and_picorv32_v_1054_321 <= and(neq(is_alu_reg_reg, UInt(0)), asUInt(neq(_eq_picorv32_v_1054_320_Y, UInt(0))))
    _eq_picorv32_v_1054_320 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h4")))
    _logic_and_picorv32_v_1053_319 <= and(neq(_logic_and_picorv32_v_1053_317_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1053_318_Y, UInt(0))))
    _eq_picorv32_v_1053_318 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _logic_and_picorv32_v_1053_317 <= and(neq(is_alu_reg_reg, UInt(0)), asUInt(neq(_eq_picorv32_v_1053_316_Y, UInt(0))))
    _eq_picorv32_v_1053_316 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h3")))
    _logic_and_picorv32_v_1052_315 <= and(neq(_logic_and_picorv32_v_1052_313_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1052_314_Y, UInt(0))))
    _eq_picorv32_v_1052_314 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _logic_and_picorv32_v_1052_313 <= and(neq(is_alu_reg_reg, UInt(0)), asUInt(neq(_eq_picorv32_v_1052_312_Y, UInt(0))))
    _eq_picorv32_v_1052_312 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h2")))
    _logic_and_picorv32_v_1051_311 <= and(neq(_logic_and_picorv32_v_1051_309_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1051_310_Y, UInt(0))))
    _eq_picorv32_v_1051_310 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _logic_and_picorv32_v_1051_309 <= and(neq(is_alu_reg_reg, UInt(0)), asUInt(neq(_eq_picorv32_v_1051_308_Y, UInt(0))))
    _eq_picorv32_v_1051_308 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h1")))
    _logic_and_picorv32_v_1050_307 <= and(neq(_logic_and_picorv32_v_1050_305_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1050_306_Y, UInt(0))))
    _eq_picorv32_v_1050_306 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h20")))
    _logic_and_picorv32_v_1050_305 <= and(neq(is_alu_reg_reg, UInt(0)), asUInt(neq(_eq_picorv32_v_1050_304_Y, UInt(0))))
    _eq_picorv32_v_1050_304 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h0")))
    _logic_and_picorv32_v_1049_303 <= and(neq(_logic_and_picorv32_v_1049_301_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1049_302_Y, UInt(0))))
    _eq_picorv32_v_1049_302 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _logic_and_picorv32_v_1049_301 <= and(neq(is_alu_reg_reg, UInt(0)), asUInt(neq(_eq_picorv32_v_1049_300_Y, UInt(0))))
    _eq_picorv32_v_1049_300 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h0")))
    _logic_and_picorv32_v_1047_299 <= and(neq(_logic_and_picorv32_v_1047_297_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1047_298_Y, UInt(0))))
    _eq_picorv32_v_1047_298 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h20")))
    _logic_and_picorv32_v_1047_297 <= and(neq(is_alu_reg_imm, UInt(0)), asUInt(neq(_eq_picorv32_v_1047_296_Y, UInt(0))))
    _eq_picorv32_v_1047_296 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h5")))
    _logic_and_picorv32_v_1046_295 <= and(neq(_logic_and_picorv32_v_1046_293_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1046_294_Y, UInt(0))))
    _eq_picorv32_v_1046_294 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _logic_and_picorv32_v_1046_293 <= and(neq(is_alu_reg_imm, UInt(0)), asUInt(neq(_eq_picorv32_v_1046_292_Y, UInt(0))))
    _eq_picorv32_v_1046_292 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h5")))
    _logic_and_picorv32_v_1045_291 <= and(neq(_logic_and_picorv32_v_1045_289_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_1045_290_Y, UInt(0))))
    _eq_picorv32_v_1045_290 <= eq(bits(mem_rdata_q, 31, 25), asUInt(UInt<7>("h00")))
    _logic_and_picorv32_v_1045_289 <= and(neq(is_alu_reg_imm, UInt(0)), asUInt(neq(_eq_picorv32_v_1045_288_Y, UInt(0))))
    _eq_picorv32_v_1045_288 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h1")))
    _logic_and_picorv32_v_1043_287 <= and(neq(is_alu_reg_imm, UInt(0)), asUInt(neq(_eq_picorv32_v_1043_286_Y, UInt(0))))
    _eq_picorv32_v_1043_286 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h7")))
    _logic_and_picorv32_v_1042_285 <= and(neq(is_alu_reg_imm, UInt(0)), asUInt(neq(_eq_picorv32_v_1042_284_Y, UInt(0))))
    _eq_picorv32_v_1042_284 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h6")))
    _logic_and_picorv32_v_1041_283 <= and(neq(is_alu_reg_imm, UInt(0)), asUInt(neq(_eq_picorv32_v_1041_282_Y, UInt(0))))
    _eq_picorv32_v_1041_282 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h4")))
    _logic_and_picorv32_v_1040_281 <= and(neq(is_alu_reg_imm, UInt(0)), asUInt(neq(_eq_picorv32_v_1040_280_Y, UInt(0))))
    _eq_picorv32_v_1040_280 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h3")))
    _logic_and_picorv32_v_1039_279 <= and(neq(is_alu_reg_imm, UInt(0)), asUInt(neq(_eq_picorv32_v_1039_278_Y, UInt(0))))
    _eq_picorv32_v_1039_278 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h2")))
    _logic_and_picorv32_v_1038_277 <= and(neq(is_alu_reg_imm, UInt(0)), asUInt(neq(_eq_picorv32_v_1038_276_Y, UInt(0))))
    _eq_picorv32_v_1038_276 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h0")))
    _logic_and_picorv32_v_1036_275 <= and(neq(is_sb_sh_sw, UInt(0)), asUInt(neq(_eq_picorv32_v_1036_274_Y, UInt(0))))
    _eq_picorv32_v_1036_274 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h2")))
    _logic_and_picorv32_v_1035_273 <= and(neq(is_sb_sh_sw, UInt(0)), asUInt(neq(_eq_picorv32_v_1035_272_Y, UInt(0))))
    _eq_picorv32_v_1035_272 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h1")))
    _logic_and_picorv32_v_1034_271 <= and(neq(is_sb_sh_sw, UInt(0)), asUInt(neq(_eq_picorv32_v_1034_270_Y, UInt(0))))
    _eq_picorv32_v_1034_270 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h0")))
    _logic_and_picorv32_v_1032_269 <= and(neq(is_lb_lh_lw_lbu_lhu, UInt(0)), asUInt(neq(_eq_picorv32_v_1032_268_Y, UInt(0))))
    _eq_picorv32_v_1032_268 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h5")))
    _logic_and_picorv32_v_1031_267 <= and(neq(is_lb_lh_lw_lbu_lhu, UInt(0)), asUInt(neq(_eq_picorv32_v_1031_266_Y, UInt(0))))
    _eq_picorv32_v_1031_266 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h4")))
    _logic_and_picorv32_v_1030_265 <= and(neq(is_lb_lh_lw_lbu_lhu, UInt(0)), asUInt(neq(_eq_picorv32_v_1030_264_Y, UInt(0))))
    _eq_picorv32_v_1030_264 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h2")))
    _logic_and_picorv32_v_1029_263 <= and(neq(is_lb_lh_lw_lbu_lhu, UInt(0)), asUInt(neq(_eq_picorv32_v_1029_262_Y, UInt(0))))
    _eq_picorv32_v_1029_262 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h1")))
    _logic_and_picorv32_v_1028_261 <= and(neq(is_lb_lh_lw_lbu_lhu, UInt(0)), asUInt(neq(_eq_picorv32_v_1028_260_Y, UInt(0))))
    _eq_picorv32_v_1028_260 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h0")))
    _logic_and_picorv32_v_1026_259 <= and(neq(is_beq_bne_blt_bge_bltu_bgeu, UInt(0)), asUInt(neq(_eq_picorv32_v_1026_258_Y, UInt(0))))
    _eq_picorv32_v_1026_258 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h7")))
    _logic_and_picorv32_v_1025_257 <= and(neq(is_beq_bne_blt_bge_bltu_bgeu, UInt(0)), asUInt(neq(_eq_picorv32_v_1025_256_Y, UInt(0))))
    _eq_picorv32_v_1025_256 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h6")))
    _logic_and_picorv32_v_1024_255 <= and(neq(is_beq_bne_blt_bge_bltu_bgeu, UInt(0)), asUInt(neq(_eq_picorv32_v_1024_254_Y, UInt(0))))
    _eq_picorv32_v_1024_254 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h5")))
    _logic_and_picorv32_v_1023_253 <= and(neq(is_beq_bne_blt_bge_bltu_bgeu, UInt(0)), asUInt(neq(_eq_picorv32_v_1023_252_Y, UInt(0))))
    _eq_picorv32_v_1023_252 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h4")))
    _logic_and_picorv32_v_1022_251 <= and(neq(is_beq_bne_blt_bge_bltu_bgeu, UInt(0)), asUInt(neq(_eq_picorv32_v_1022_250_Y, UInt(0))))
    _eq_picorv32_v_1022_250 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h1")))
    _logic_and_picorv32_v_1021_249 <= and(neq(is_beq_bne_blt_bge_bltu_bgeu, UInt(0)), asUInt(neq(_eq_picorv32_v_1021_248_Y, UInt(0))))
    _eq_picorv32_v_1021_248 <= eq(bits(mem_rdata_q, 14, 12), asUInt(UInt<3>("h0")))
    _logic_and_picorv32_v_1018_247 <= and(neq(decoder_trigger, UInt(0)), asUInt(neq(_logic_not_picorv32_v_1018_246_Y, UInt(0))))
    _logic_not_picorv32_v_1018_246 <= eq(decoder_pseudo_trigger, UInt(0))
    _logic_and_picorv32_v_1000_245 <= and(neq(_ne_picorv32_v_1000_243_Y, UInt(0)), asUInt(neq(_ne_picorv32_v_1000_244_Y, UInt(0))))
    _ne_picorv32_v_1000_244 <= neq(bits(mem_rdata_latched, 6, 2), asUInt(UInt<32>("h00000000")))
    _ne_picorv32_v_1000_243 <= neq(bits(mem_rdata_latched, 12, 12), asUInt(UInt<32>("h00000000")))
    _logic_and_picorv32_v_995_242 <= and(neq(_logic_and_picorv32_v_995_240_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_995_241_Y, UInt(0))))
    _eq_picorv32_v_995_241 <= eq(bits(mem_rdata_latched, 6, 2), asUInt(UInt<32>("h00000000")))
    _logic_and_picorv32_v_995_240 <= and(neq(_ne_picorv32_v_995_238_Y, UInt(0)), asUInt(neq(_ne_picorv32_v_995_239_Y, UInt(0))))
    _ne_picorv32_v_995_239 <= neq(bits(mem_rdata_latched, 11, 7), asUInt(UInt<32>("h00000000")))
    _ne_picorv32_v_995_238 <= neq(bits(mem_rdata_latched, 12, 12), asUInt(UInt<32>("h00000000")))
    _logic_and_picorv32_v_989_237 <= and(neq(_eq_picorv32_v_989_235_Y, UInt(0)), asUInt(neq(_ne_picorv32_v_989_236_Y, UInt(0))))
    _ne_picorv32_v_989_236 <= neq(bits(mem_rdata_latched, 6, 2), asUInt(UInt<32>("h00000000")))
    _eq_picorv32_v_989_235 <= eq(bits(mem_rdata_latched, 12, 12), asUInt(UInt<32>("h00000000")))
    _logic_and_picorv32_v_984_234 <= and(neq(_logic_and_picorv32_v_984_232_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_984_233_Y, UInt(0))))
    _eq_picorv32_v_984_233 <= eq(bits(mem_rdata_latched, 6, 2), asUInt(UInt<32>("h00000000")))
    _logic_and_picorv32_v_984_232 <= and(neq(_eq_picorv32_v_984_230_Y, UInt(0)), asUInt(neq(_ne_picorv32_v_984_231_Y, UInt(0))))
    _ne_picorv32_v_984_231 <= neq(bits(mem_rdata_latched, 11, 7), asUInt(UInt<32>("h00000000")))
    _eq_picorv32_v_984_230 <= eq(bits(mem_rdata_latched, 12, 12), asUInt(UInt<32>("h00000000")))
    _reduce_bool_picorv32_v_977_229 <= neq(bits(mem_rdata_latched, 11, 7), UInt<5>(0))
    _logic_not_picorv32_v_969_228 <= eq(bits(mem_rdata_latched, 12, 12), UInt(0))
    _add_picorv32_v_961_227 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 9, 7)))
    _add_picorv32_v_956_226 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 9, 7)))
    _add_picorv32_v_948_225 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 4, 2)))
    _add_picorv32_v_947_224 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 9, 7)))
    _add_picorv32_v_946_223 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 9, 7)))
    _eq_picorv32_v_944_222 <= eq(bits(mem_rdata_latched, 12, 10), asUInt(UInt<3>("h3")))
    _add_picorv32_v_942_221 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 9, 7)))
    _add_picorv32_v_941_220 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 9, 7)))
    _eq_picorv32_v_939_219 <= eq(bits(mem_rdata_latched, 11, 10), asUInt(UInt<2>("h2")))
    _add_picorv32_v_936_218 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 9, 7)))
    _add_picorv32_v_935_217 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 9, 7)))
    _logic_and_picorv32_v_933_216 <= and(neq(_logic_not_picorv32_v_933_214_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_933_215_Y, UInt(0))))
    _logic_not_picorv32_v_933_215 <= eq(bits(mem_rdata_latched, 12, 12), UInt(0))
    _logic_not_picorv32_v_933_214 <= eq(bits(mem_rdata_latched, 11, 11), UInt(0))
    _eq_picorv32_v_921_213 <= eq(bits(mem_rdata_latched, 11, 7), asUInt(UInt<32>("h00000002")))
    _logic_or_picorv32_v_920_212 <= or(neq(bits(mem_rdata_latched, 12, 12), UInt(0)), asUInt(neq(bits(mem_rdata_latched, 6, 2), UInt(0))))
    _add_picorv32_v_899_211 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 4, 2)))
    _add_picorv32_v_898_210 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 9, 7)))
    _add_picorv32_v_894_209 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 4, 2)))
    _add_picorv32_v_893_208 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 9, 7)))
    _add_picorv32_v_889_207 <= add(UInt<32>("h00000008"), asUInt(bits(mem_rdata_latched, 4, 2)))
    _reduce_or_picorv32_v_887_206 <= orr(pad(bits(mem_rdata_latched, 12, 5), 1))
    _logic_and_picorv32_v_874_205 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_ne_picorv32_v_874_204_Y, UInt(0))))
    _ne_picorv32_v_874_204 <= neq(bits(mem_rdata_latched, 1, 0), asUInt(UInt<2>("h3")))
    _logic_and_picorv32_v_870_203 <= and(neq(_logic_and_picorv32_v_870_202_Y, UInt(0)), asUInt(neq(UInt<1>("h0"), UInt(0))))
    _logic_and_picorv32_v_870_202 <= and(neq(_eq_picorv32_v_870_200_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_870_201_Y, UInt(0))))
    _eq_picorv32_v_870_201 <= eq(bits(mem_rdata_latched, 31, 25), asUInt(UInt<7>("h02")))
    _eq_picorv32_v_870_200 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h0b")))
    _logic_and_picorv32_v_867_199 <= and(neq(_logic_and_picorv32_v_867_198_Y, UInt(0)), asUInt(neq(UInt<1>("h1"), UInt(0))))
    _logic_and_picorv32_v_867_198 <= and(neq(_logic_and_picorv32_v_867_197_Y, UInt(0)), asUInt(neq(UInt<1>("h0"), UInt(0))))
    _logic_and_picorv32_v_867_197 <= and(neq(_eq_picorv32_v_867_195_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_867_196_Y, UInt(0))))
    _eq_picorv32_v_867_196 <= eq(bits(mem_rdata_latched, 31, 25), asUInt(UInt<7>("h00")))
    _eq_picorv32_v_867_195 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h0b")))
    _eq_picorv32_v_859_194 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h33")))
    _eq_picorv32_v_858_193 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h13")))
    _eq_picorv32_v_857_192 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h23")))
    _eq_picorv32_v_856_191 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h03")))
    _eq_picorv32_v_855_190 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h63")))
    _logic_and_picorv32_v_853_189 <= and(neq(_logic_and_picorv32_v_853_188_Y, UInt(0)), asUInt(neq(UInt<1>("h0"), UInt(0))))
    _logic_and_picorv32_v_853_188 <= and(neq(_eq_picorv32_v_853_186_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_853_187_Y, UInt(0))))
    _eq_picorv32_v_853_187 <= eq(bits(mem_rdata_latched, 31, 25), asUInt(UInt<7>("h04")))
    _eq_picorv32_v_853_186 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h0b")))
    _logic_and_picorv32_v_852_185 <= and(neq(_logic_and_picorv32_v_852_184_Y, UInt(0)), asUInt(neq(UInt<1>("h0"), UInt(0))))
    _logic_and_picorv32_v_852_184 <= and(neq(_eq_picorv32_v_852_182_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_852_183_Y, UInt(0))))
    _eq_picorv32_v_852_183 <= eq(bits(mem_rdata_latched, 31, 25), asUInt(UInt<7>("h02")))
    _eq_picorv32_v_852_182 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h0b")))
    _logic_and_picorv32_v_851_181 <= and(neq(_eq_picorv32_v_851_179_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_851_180_Y, UInt(0))))
    _eq_picorv32_v_851_180 <= eq(bits(mem_rdata_latched, 14, 12), asUInt(UInt<3>("h0")))
    _eq_picorv32_v_851_179 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h67")))
    _eq_picorv32_v_850_178 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h6f")))
    _eq_picorv32_v_849_177 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h17")))
    _eq_picorv32_v_848_176 <= eq(bits(mem_rdata_latched, 6, 0), asUInt(UInt<7>("h37")))
    _logic_and_picorv32_v_847_175 <= and(neq(mem_do_rinst, UInt(0)), asUInt(neq(mem_done, UInt(0))))
    _reduce_or_picorv32_v_845_174 <= orr(pad(cat(is_beq_bne_blt_bge_bltu_bgeu, cat(instr_slti, cat(instr_slt, cat(instr_sltiu, instr_sltu)))), 1))
    _reduce_or_picorv32_v_844_173 <= orr(pad(cat(instr_lbu, cat(instr_lhu, instr_lw)), 1))
    _reduce_or_picorv32_v_843_172 <= orr(pad(cat(instr_sltiu, cat(instr_bltu, instr_sltu)), 1))
    _reduce_or_picorv32_v_842_171 <= orr(pad(cat(instr_slti, cat(instr_blt, instr_slt)), 1))
    _reduce_or_picorv32_v_841_170 <= orr(pad(cat(instr_lui, cat(instr_auipc, cat(instr_jal, cat(instr_jalr, cat(instr_addi, cat(instr_add, instr_sub)))))), 1))
    _reduce_or_picorv32_v_840_169 <= orr(pad(cat(instr_lui, cat(instr_auipc, instr_jal)), 1))
    _reduce_and_picorv32_v_808_167 <= andr(pad(bits(next_insn_opcode, 1, 0), 1))
    _reduce_and_picorv32_v_776_165 <= andr(pad(bits(next_insn_opcode, 1, 0), 1))
    _logic_or_picorv32_v_768_164 <= or(neq(_logic_not_picorv32_v_768_163_Y, UInt(0)), asUInt(neq(trap, UInt(0))))
    _logic_not_picorv32_v_768_163 <= eq(resetn, UInt(0))
    _reduce_or_picorv32_v_670_160 <= orr(pad(cat(instr_rdcycle, cat(instr_rdcycleh, cat(instr_rdinstr, instr_rdinstrh))), 1))
    _logic_and_picorv32_v_667_159 <= and(neq(UInt<1>("h1"), UInt(0)), asUInt(neq(_logic_not_picorv32_v_667_158_Y, UInt(0))))
    _logic_not_picorv32_v_667_158 <= eq(cat(instr_lui, cat(instr_auipc, cat(instr_jal, cat(instr_jalr, cat(instr_beq, cat(instr_bne, cat(instr_blt, cat(instr_bge, cat(instr_bltu, cat(instr_bgeu, cat(instr_lb, cat(instr_lh, cat(instr_lw, cat(instr_lbu, cat(instr_lhu, cat(instr_sb, cat(instr_sh, cat(instr_sw, cat(instr_addi, cat(instr_slti, cat(instr_sltiu, cat(instr_xori, cat(instr_ori, cat(instr_andi, cat(instr_slli, cat(instr_srli, cat(instr_srai, cat(instr_add, cat(instr_sub, cat(instr_sll, cat(instr_slt, cat(instr_sltu, cat(instr_xor, cat(instr_srl, cat(instr_sra, cat(instr_or, cat(instr_and, cat(instr_rdcycle, cat(instr_rdcycleh, cat(instr_rdinstr, cat(instr_rdinstrh, cat(instr_getq, cat(instr_setq, cat(instr_retirq, cat(instr_maskirq, cat(instr_waitirq, instr_timer)))))))))))))))))))))))))))))))))))))))))))))), UInt(0))
    _ternary_picorv32_v_600_157 <= mux(_logic_or_picorv32_v_600_156_Y, UInt<32>("h00000000"), UInt<32>("h00000003"))
    _logic_or_picorv32_v_600_156 <= or(neq(mem_do_rinst, UInt(0)), asUInt(neq(mem_do_rdata, UInt(0))))
    _logic_or_picorv32_v_593_155 <= or(neq(_logic_not_picorv32_v_593_154_Y, UInt(0)), asUInt(neq(mem_la_secondword, UInt(0))))
    _logic_not_picorv32_v_593_154 <= eq(_reduce_and_picorv32_v_593_153_Y, UInt(0))
    _reduce_and_picorv32_v_593_153 <= andr(pad(bits(mem_rdata, 1, 0), 1))
    _logic_and_picorv32_v_592_152 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_not_picorv32_v_592_151_Y, UInt(0))))
    _logic_not_picorv32_v_592_151 <= eq(mem_do_rdata, UInt(0))
    _logic_not_picorv32_v_587_150 <= eq(mem_la_use_prefetched_high_word, UInt(0))
    _logic_and_picorv32_v_584_149 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(mem_la_read, UInt(0))))
    _logic_or_picorv32_v_568_148 <= or(neq(mem_do_prefetch, UInt(0)), asUInt(neq(mem_do_rinst, UInt(0))))
    _logic_not_picorv32_v_567_147 <= eq(mem_la_use_prefetched_high_word, UInt(0))
    _logic_or_picorv32_v_566_146 <= or(neq(_logic_or_picorv32_v_566_145_Y, UInt(0)), asUInt(neq(mem_do_rdata, UInt(0))))
    _logic_or_picorv32_v_566_145 <= or(neq(mem_do_prefetch, UInt(0)), asUInt(neq(mem_do_rinst, UInt(0))))
    _and_picorv32_v_559_144 <= and(mem_la_wstrb, asUInt(cat(mem_la_write, cat(mem_la_write, cat(mem_la_write, mem_la_write)))))
    _logic_or_picorv32_v_557_143 <= or(neq(mem_la_read, UInt(0)), asUInt(neq(mem_la_write, UInt(0))))
    _logic_or_picorv32_v_552_142 <= or(neq(_logic_not_picorv32_v_552_141_Y, UInt(0)), asUInt(neq(mem_ready, UInt(0))))
    _logic_not_picorv32_v_552_141 <= eq(resetn, UInt(0))
    _logic_not_picorv32_v_550_140 <= eq(resetn, UInt(0))
    _logic_or_picorv32_v_549_139 <= or(neq(_logic_not_picorv32_v_549_138_Y, UInt(0)), asUInt(neq(trap, UInt(0))))
    _logic_not_picorv32_v_549_138 <= eq(resetn, UInt(0))
    _logic_or_picorv32_v_543_136 <= or(neq(_eq_picorv32_v_543_134_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_543_135_Y, UInt(0))))
    _eq_picorv32_v_543_135 <= eq(mem_state, asUInt(UInt<32>("h00000003")))
    _eq_picorv32_v_543_134 <= eq(mem_state, asUInt(UInt<32>("h00000002")))
    _logic_or_picorv32_v_534_133 <= or(neq(mem_do_prefetch, UInt(0)), asUInt(neq(mem_do_rinst, UInt(0))))
    _logic_or_picorv32_v_531_132 <= or(neq(_logic_or_picorv32_v_531_131_Y, UInt(0)), asUInt(neq(mem_do_rdata, UInt(0))))
    _logic_or_picorv32_v_531_131 <= or(neq(mem_do_prefetch, UInt(0)), asUInt(neq(mem_do_rinst, UInt(0))))
    _logic_and_picorv32_v_530_130 <= and(neq(resetn, UInt(0)), asUInt(neq(_logic_not_picorv32_v_530_129_Y, UInt(0))))
    _logic_not_picorv32_v_530_129 <= eq(trap, UInt(0))
    _logic_and_picorv32_v_514_127 <= and(neq(_ne_picorv32_v_514_125_Y, UInt(0)), asUInt(neq(_ne_picorv32_v_514_126_Y, UInt(0))))
    _ne_picorv32_v_514_126 <= neq(bits(mem_rdata_latched, 6, 2), asUInt(UInt<32>("h00000000")))
    _ne_picorv32_v_514_125 <= neq(bits(mem_rdata_latched, 12, 12), asUInt(UInt<32>("h00000000")))
    _logic_and_picorv32_v_510_124 <= and(neq(_logic_and_picorv32_v_510_122_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_510_123_Y, UInt(0))))
    _eq_picorv32_v_510_123 <= eq(bits(mem_rdata_latched, 6, 2), asUInt(UInt<32>("h00000000")))
    _logic_and_picorv32_v_510_122 <= and(neq(_ne_picorv32_v_510_120_Y, UInt(0)), asUInt(neq(_ne_picorv32_v_510_121_Y, UInt(0))))
    _ne_picorv32_v_510_121 <= neq(bits(mem_rdata_latched, 11, 7), asUInt(UInt<32>("h00000000")))
    _ne_picorv32_v_510_120 <= neq(bits(mem_rdata_latched, 12, 12), asUInt(UInt<32>("h00000000")))
    _logic_and_picorv32_v_506_119 <= and(neq(_eq_picorv32_v_506_117_Y, UInt(0)), asUInt(neq(_ne_picorv32_v_506_118_Y, UInt(0))))
    _ne_picorv32_v_506_118 <= neq(bits(mem_rdata_latched, 6, 2), asUInt(UInt<32>("h00000000")))
    _eq_picorv32_v_506_117 <= eq(bits(mem_rdata_latched, 12, 12), asUInt(UInt<32>("h00000000")))
    _logic_and_picorv32_v_502_116 <= and(neq(_eq_picorv32_v_502_114_Y, UInt(0)), asUInt(neq(_eq_picorv32_v_502_115_Y, UInt(0))))
    _eq_picorv32_v_502_115 <= eq(bits(mem_rdata_latched, 6, 2), asUInt(UInt<32>("h00000000")))
    _eq_picorv32_v_502_114 <= eq(bits(mem_rdata_latched, 12, 12), asUInt(UInt<32>("h00000000")))
    _ternary_picorv32_v_474_113 <= mux(_eq_picorv32_v_474_112_Y, UInt<7>("h20"), UInt<7>("h00"))
    _eq_picorv32_v_474_112 <= eq(bits(mem_rdata_latched, 6, 5), asUInt(UInt<2>("h0")))
    _eq_picorv32_v_473_111 <= eq(bits(mem_rdata_latched, 6, 5), asUInt(UInt<2>("h3")))
    _eq_picorv32_v_472_110 <= eq(bits(mem_rdata_latched, 6, 5), asUInt(UInt<2>("h2")))
    _eq_picorv32_v_471_109 <= eq(bits(mem_rdata_latched, 6, 5), asUInt(UInt<2>("h1")))
    _eq_picorv32_v_470_108 <= eq(bits(mem_rdata_latched, 6, 5), asUInt(UInt<2>("h0")))
    _eq_picorv32_v_469_107 <= eq(bits(mem_rdata_latched, 12, 10), asUInt(UInt<3>("h3")))
    _eq_picorv32_v_465_106 <= eq(bits(mem_rdata_latched, 11, 10), asUInt(UInt<2>("h2")))
    _eq_picorv32_v_461_105 <= eq(bits(mem_rdata_latched, 11, 10), asUInt(UInt<2>("h1")))
    _eq_picorv32_v_457_104 <= eq(bits(mem_rdata_latched, 11, 10), asUInt(UInt<2>("h0")))
    _eq_picorv32_v_448_103 <= eq(bits(mem_rdata_latched, 11, 7), asUInt(UInt<32>("h00000002")))
    _logic_and_picorv32_v_419_102 <= and(neq(_logic_and_picorv32_v_419_100_Y, UInt(0)), asUInt(neq(_logic_or_picorv32_v_419_101_Y, UInt(0))))
    _logic_or_picorv32_v_419_101 <= or(neq(mem_do_prefetch, UInt(0)), asUInt(neq(mem_do_rinst, UInt(0))))
    _logic_and_picorv32_v_419_100 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(mem_done, UInt(0))))
    _ternary_picorv32_v_416_99 <= mux(UInt<1>("h0"), mem_rdata_latched, mem_rdata)
    _ternary_picorv32_v_415_98 <= mux(UInt<1>("h0"), mem_rdata_latched, mem_rdata)
    _shl_picorv32_v_402_96 <= bits(dshl(UInt<4>("h1"), asUInt(bits(reg_op1, 1, 0))), 3, 0)
    _ternary_picorv32_v_394_95 <= mux(bits(reg_op1, 1, 1), UInt<4>("hc"), UInt<4>("h3"))
    _logic_and_picorv32_v_380_93 <= and(neq(mem_valid, UInt(0)), asUInt(neq(_logic_not_picorv32_v_380_92_Y, UInt(0))))
    _logic_not_picorv32_v_380_92 <= eq(mem_ready, UInt(0))
    _logic_not_picorv32_v_378_91 <= eq(last_mem_valid, UInt(0))
    _logic_not_picorv32_v_374_90 <= eq(resetn, UInt(0))
    _ternary_picorv32_v_371_88 <= mux(_logic_and_picorv32_v_369_83_Y, cat(UInt<16>("h0000"), mem_16bit_buffer), _ternary_picorv32_v_371_87_Y)
    _ternary_picorv32_v_371_87 <= mux(_logic_and_picorv32_v_370_84_Y, cat(bits(mem_rdata_latched_noshuffle, 15, 0), mem_16bit_buffer), _ternary_picorv32_v_371_86_Y)
    _ternary_picorv32_v_371_86 <= mux(_logic_and_picorv32_v_371_85_Y, cat(UInt<16>("h0000"), bits(mem_rdata_latched_noshuffle, 31, 16)), mem_rdata_latched_noshuffle)
    _logic_and_picorv32_v_371_85 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(mem_la_firstword, UInt(0))))
    _logic_and_picorv32_v_370_84 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(mem_la_secondword, UInt(0))))
    _logic_and_picorv32_v_369_83 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(mem_la_use_prefetched_high_word, UInt(0))))
    _ternary_picorv32_v_367_82 <= mux(_logic_or_picorv32_v_367_81_Y, mem_rdata, mem_rdata_q)
    _logic_or_picorv32_v_367_81 <= or(neq(mem_xfer, UInt(0)), asUInt(neq(UInt<1>("h0"), UInt(0))))
    _ternary_picorv32_v_365_80 <= mux(_logic_or_picorv32_v_365_78_Y, cat(_add_picorv32_v_365_79_Y, UInt<2>("h0")), cat(bits(reg_op1, 31, 2), UInt<2>("h0")))
    _add_picorv32_v_365_79 <= add(bits(next_pc, 31, 2), asUInt(mem_la_firstword_xfer))
    _logic_or_picorv32_v_365_78 <= or(neq(mem_do_prefetch, UInt(0)), asUInt(neq(mem_do_rinst, UInt(0))))
    _logic_and_picorv32_v_364_77 <= and(neq(resetn, UInt(0)), asUInt(neq(_logic_or_picorv32_v_364_76_Y, UInt(0))))
    _logic_or_picorv32_v_364_76 <= or(neq(_logic_and_picorv32_v_363_67_Y, UInt(0)), asUInt(neq(_logic_and_picorv32_v_364_75_Y, UInt(0))))
    _logic_and_picorv32_v_364_75 <= and(neq(_logic_and_picorv32_v_364_73_Y, UInt(0)), asUInt(neq(_reduce_and_picorv32_v_364_74_Y, UInt(0))))
    _reduce_and_picorv32_v_364_74 <= andr(pad(bits(mem_rdata_latched, 1, 0), 1))
    _logic_and_picorv32_v_364_73 <= and(neq(_logic_and_picorv32_v_364_71_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_364_72_Y, UInt(0))))
    _logic_not_picorv32_v_364_72 <= eq(mem_la_secondword, UInt(0))
    _logic_and_picorv32_v_364_71 <= and(neq(_logic_and_picorv32_v_364_68_Y, UInt(0)), asUInt(neq(_ternary_picorv32_v_364_70_Y, UInt(0))))
    _ternary_picorv32_v_364_70 <= mux(_logic_not_picorv32_v_364_69_Y, mem_la_firstword, mem_la_firstword_reg)
    _logic_not_picorv32_v_364_69 <= eq(last_mem_valid, UInt(0))
    _logic_and_picorv32_v_364_68 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(mem_xfer, UInt(0))))
    _logic_and_picorv32_v_363_67 <= and(neq(_logic_and_picorv32_v_363_64_Y, UInt(0)), asUInt(neq(_logic_or_picorv32_v_363_66_Y, UInt(0))))
    _logic_or_picorv32_v_363_66 <= or(neq(_logic_or_picorv32_v_363_65_Y, UInt(0)), asUInt(neq(mem_do_rdata, UInt(0))))
    _logic_or_picorv32_v_363_65 <= or(neq(mem_do_rinst, UInt(0)), asUInt(neq(mem_do_prefetch, UInt(0))))
    _logic_and_picorv32_v_363_64 <= and(neq(_logic_not_picorv32_v_363_62_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_363_63_Y, UInt(0))))
    _logic_not_picorv32_v_363_63 <= eq(mem_state, UInt(0))
    _logic_not_picorv32_v_363_62 <= eq(mem_la_use_prefetched_high_word, UInt(0))
    _logic_and_picorv32_v_362_61 <= and(neq(_logic_and_picorv32_v_362_60_Y, UInt(0)), asUInt(neq(mem_do_wdata, UInt(0))))
    _logic_and_picorv32_v_362_60 <= and(neq(resetn, UInt(0)), asUInt(neq(_logic_not_picorv32_v_362_59_Y, UInt(0))))
    _logic_not_picorv32_v_362_59 <= eq(mem_state, UInt(0))
    _logic_and_picorv32_v_360_58 <= and(neq(_logic_and_picorv32_v_359_52_Y, UInt(0)), asUInt(neq(_logic_or_picorv32_v_360_57_Y, UInt(0))))
    _logic_or_picorv32_v_360_57 <= or(neq(_logic_not_picorv32_v_360_53_Y, UInt(0)), asUInt(neq(_logic_and_picorv32_v_360_56_Y, UInt(0))))
    _logic_and_picorv32_v_360_56 <= and(neq(_logic_not_picorv32_v_360_55_Y, UInt(0)), asUInt(neq(mem_xfer, UInt(0))))
    _logic_not_picorv32_v_360_55 <= eq(_reduce_and_picorv32_v_360_54_Y, UInt(0))
    _reduce_and_picorv32_v_360_54 <= andr(pad(bits(mem_rdata_latched, 1, 0), 1))
    _logic_not_picorv32_v_360_53 <= eq(mem_la_firstword, UInt(0))
    _logic_and_picorv32_v_359_52 <= and(neq(resetn, UInt(0)), asUInt(neq(_logic_or_picorv32_v_359_51_Y, UInt(0))))
    _logic_or_picorv32_v_359_51 <= or(neq(_logic_and_picorv32_v_359_48_Y, UInt(0)), asUInt(neq(_logic_and_picorv32_v_359_50_Y, UInt(0))))
    _logic_and_picorv32_v_359_50 <= and(neq(_reduce_and_picorv32_v_359_49_Y, UInt(0)), asUInt(neq(mem_do_rinst, UInt(0))))
    _reduce_and_picorv32_v_359_49 <= andr(pad(mem_state, 1))
    _logic_and_picorv32_v_359_48 <= and(neq(_logic_and_picorv32_v_359_45_Y, UInt(0)), asUInt(neq(_logic_or_picorv32_v_359_47_Y, UInt(0))))
    _logic_or_picorv32_v_359_47 <= or(neq(_logic_or_picorv32_v_359_46_Y, UInt(0)), asUInt(neq(mem_do_wdata, UInt(0))))
    _logic_or_picorv32_v_359_46 <= or(neq(mem_do_rinst, UInt(0)), asUInt(neq(mem_do_rdata, UInt(0))))
    _logic_and_picorv32_v_359_45 <= and(neq(mem_xfer, UInt(0)), asUInt(neq(_reduce_or_picorv32_v_359_44_Y, UInt(0))))
    _reduce_or_picorv32_v_359_44 <= orr(pad(mem_state, 1))
    _reduce_or_picorv32_v_358_43 <= orr(pad(cat(mem_do_prefetch, cat(mem_do_rinst, cat(mem_do_rdata, mem_do_wdata))), 1))
    _logic_or_picorv32_v_356_42 <= or(neq(_logic_and_picorv32_v_356_40_Y, UInt(0)), asUInt(neq(_logic_and_picorv32_v_356_41_Y, UInt(0))))
    _logic_and_picorv32_v_356_41 <= and(neq(mem_la_use_prefetched_high_word, UInt(0)), asUInt(neq(mem_do_rinst, UInt(0))))
    _logic_and_picorv32_v_356_40 <= and(neq(mem_valid, UInt(0)), asUInt(neq(mem_ready, UInt(0))))
    _logic_and_picorv32_v_355_39 <= and(neq(_logic_and_picorv32_v_355_37_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_355_38_Y, UInt(0))))
    _logic_not_picorv32_v_355_38 <= eq(clear_prefetched_high_word, UInt(0))
    _logic_and_picorv32_v_355_37 <= and(neq(_logic_and_picorv32_v_355_36_Y, UInt(0)), asUInt(neq(prefetched_high_word, UInt(0))))
    _logic_and_picorv32_v_355_36 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(mem_la_firstword, UInt(0))))
    _logic_and_picorv32_v_346_35 <= and(neq(_logic_and_picorv32_v_346_32_Y, UInt(0)), asUInt(neq(_ternary_picorv32_v_346_34_Y, UInt(0))))
    _ternary_picorv32_v_346_34 <= mux(_logic_not_picorv32_v_346_33_Y, mem_la_firstword, mem_la_firstword_reg)
    _logic_not_picorv32_v_346_33 <= eq(last_mem_valid, UInt(0))
    _logic_and_picorv32_v_346_32 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(mem_xfer, UInt(0))))
    _logic_and_picorv32_v_345_31 <= and(neq(_logic_and_picorv32_v_345_29_Y, UInt(0)), asUInt(neq(_logic_not_picorv32_v_345_30_Y, UInt(0))))
    _logic_not_picorv32_v_345_30 <= eq(mem_la_secondword, UInt(0))
    _logic_and_picorv32_v_345_29 <= and(neq(_logic_and_picorv32_v_345_28_Y, UInt(0)), asUInt(neq(bits(next_pc, 1, 1), UInt(0))))
    _logic_and_picorv32_v_345_28 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(_logic_or_picorv32_v_345_27_Y, UInt(0))))
    _logic_or_picorv32_v_345_27 <= or(neq(mem_do_prefetch, UInt(0)), asUInt(neq(mem_do_rinst, UInt(0))))
    _logic_and_picorv32_v_324_26 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(pcpi_div_ready, UInt(0))))
    _logic_and_picorv32_v_320_25 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(pcpi_mul_ready, UInt(0))))
    _logic_and_picorv32_v_316_24 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(pcpi_ready, UInt(0))))
    _reduce_or_picorv32_v_312_23 <= orr(pad(cat(_logic_and_picorv32_v_312_22_Y, cat(_logic_and_picorv32_v_312_21_Y, _logic_and_picorv32_v_312_20_Y)), 1))
    _logic_and_picorv32_v_312_22 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(pcpi_ready, UInt(0))))
    _logic_and_picorv32_v_312_21 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(pcpi_mul_ready, UInt(0))))
    _logic_and_picorv32_v_312_20 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(pcpi_div_ready, UInt(0))))
    _reduce_or_picorv32_v_311_19 <= orr(pad(cat(_logic_and_picorv32_v_311_18_Y, cat(_logic_and_picorv32_v_311_17_Y, _logic_and_picorv32_v_311_16_Y)), 1))
    _logic_and_picorv32_v_311_18 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(pcpi_wait, UInt(0))))
    _logic_and_picorv32_v_311_17 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(pcpi_mul_wait, UInt(0))))
    _logic_and_picorv32_v_311_16 <= and(neq(UInt<1>("h0"), UInt(0)), asUInt(neq(pcpi_div_wait, UInt(0))))
    _224 <= mem_valid
    _225 <= mem_instr
    _226 <= mem_ready
    _227 <= mem_addr
    _228 <= mem_wdata
    _229 <= mem_wstrb
    _230 <= mem_rdata
    _231 <= reg_op1
    _232 <= reg_op2
    _233 <= _logic_and_picorv32_v_345_31_Y
    _234 <= _logic_and_picorv32_v_346_35_Y
    _235 <= _logic_and_picorv32_v_355_39_Y
    _236 <= _logic_or_picorv32_v_356_42_Y
    _237 <= _reduce_or_picorv32_v_358_43_Y
    _238 <= _logic_and_picorv32_v_360_58_Y
    _239 <= _logic_and_picorv32_v_362_61_Y
    _240 <= _logic_and_picorv32_v_364_77_Y
    _241 <= _ternary_picorv32_v_365_80_Y
    _242 <= _ternary_picorv32_v_367_82_Y
    _243 <= _ternary_picorv32_v_371_88_Y
    _244 <= _logic_and_picorv32_v_667_159_Y
    _245 <= _reduce_or_picorv32_v_670_160_Y
    _246 <= _ternary_picorv32_v_1191_439_Y
    _247 <= _logic_and_picorv32_v_1372_501_Y
    _248 <= UInt<1>("h0")
    _249 <= UInt<32>("h00000000")
    _250 <= UInt<1>("h0")
    _251 <= UInt<1>("h0")
    _252 <= UInt<1>("h0")
    _253 <= UInt<32>("h00000000")
    _254 <= UInt<1>("h0")
    _255 <= UInt<1>("h0")

    _sshr_picorv32_v_1223_687_Y <= bits(_sshr_picorv32_v_1223_687, 32, 0)
    _ternary_picorv32_v_1223_686_Y <= bits(_ternary_picorv32_v_1223_686, 0, 0)
    _logic_or_picorv32_v_1223_685_Y <= bits(_logic_or_picorv32_v_1223_685, 0, 0)
    _shl_picorv32_v_1222_684_Y <= bits(_shl_picorv32_v_1222_684, 31, 0)
    _lt_picorv32_v_1221_683_Y <= bits(_lt_picorv32_v_1221_683, 0, 0)
    _lt_picorv32_v_1220_682_Y <= bits(_lt_picorv32_v_1220_682, 0, 0)
    _eq_picorv32_v_1219_681_Y <= bits(_eq_picorv32_v_1219_681, 0, 0)
    _ternary_picorv32_v_1218_680_Y <= bits(_ternary_picorv32_v_1218_680, 31, 0)
    _add_picorv32_v_1218_679_Y <= bits(_add_picorv32_v_1218_679, 31, 0)
    _sub_picorv32_v_1218_678_Y <= bits(_sub_picorv32_v_1218_678, 31, 0)
    _and_picorv32_v_1934_676_Y <= bits(_and_picorv32_v_1934_676, 31, 0)
    _logic_or_picorv32_v_1920_675_Y <= bits(_logic_or_picorv32_v_1920_675, 0, 0)
    _logic_not_picorv32_v_1920_674_Y <= bits(_logic_not_picorv32_v_1920_674, 0, 0)
    _logic_and_picorv32_v_1916_673_Y <= bits(_logic_and_picorv32_v_1916_673, 0, 0)
    _logic_and_picorv32_v_1916_672_Y <= bits(_logic_and_picorv32_v_1916_672, 0, 0)
    _logic_not_picorv32_v_1916_671_Y <= bits(_logic_not_picorv32_v_1916_671, 0, 0)
    _logic_and_picorv32_v_1916_670_Y <= bits(_logic_and_picorv32_v_1916_670, 0, 0)
    _logic_and_picorv32_v_1911_669_Y <= bits(_logic_and_picorv32_v_1911_669, 0, 0)
    _logic_not_picorv32_v_1911_668_Y <= bits(_logic_not_picorv32_v_1911_668, 0, 0)
    _logic_and_picorv32_v_1911_667_Y <= bits(_logic_and_picorv32_v_1911_667, 0, 0)
    _logic_not_picorv32_v_1911_666_Y <= bits(_logic_not_picorv32_v_1911_666, 0, 0)
    _logic_and_picorv32_v_1909_665_Y <= bits(_logic_and_picorv32_v_1909_665, 0, 0)
    _ternary_picorv32_v_1909_664_Y <= bits(_ternary_picorv32_v_1909_664, 0, 0)
    _reduce_or_picorv32_v_1909_663_Y <= bits(_reduce_or_picorv32_v_1909_663, 0, 0)
    _logic_and_picorv32_v_1909_662_Y <= bits(_logic_and_picorv32_v_1909_662, 0, 0)
    _logic_and_picorv32_v_1909_661_Y <= bits(_logic_and_picorv32_v_1909_661, 0, 0)
    _logic_and_picorv32_v_1903_660_Y <= bits(_logic_and_picorv32_v_1903_660, 0, 0)
    _logic_not_picorv32_v_1903_659_Y <= bits(_logic_not_picorv32_v_1903_659, 0, 0)
    _logic_and_picorv32_v_1903_658_Y <= bits(_logic_and_picorv32_v_1903_658, 0, 0)
    _logic_not_picorv32_v_1903_657_Y <= bits(_logic_not_picorv32_v_1903_657, 0, 0)
    _logic_and_picorv32_v_1901_656_Y <= bits(_logic_and_picorv32_v_1901_656, 0, 0)
    _ne_picorv32_v_1901_655_Y <= bits(_ne_picorv32_v_1901_655, 0, 0)
    _eq_picorv32_v_1901_654_Y <= bits(_eq_picorv32_v_1901_654, 0, 0)
    _logic_and_picorv32_v_1896_653_Y <= bits(_logic_and_picorv32_v_1896_653, 0, 0)
    _logic_not_picorv32_v_1896_652_Y <= bits(_logic_not_picorv32_v_1896_652, 0, 0)
    _logic_and_picorv32_v_1896_651_Y <= bits(_logic_and_picorv32_v_1896_651, 0, 0)
    _logic_not_picorv32_v_1896_650_Y <= bits(_logic_not_picorv32_v_1896_650, 0, 0)
    _logic_and_picorv32_v_1894_649_Y <= bits(_logic_and_picorv32_v_1894_649, 0, 0)
    _ne_picorv32_v_1894_648_Y <= bits(_ne_picorv32_v_1894_648, 0, 0)
    _eq_picorv32_v_1894_647_Y <= bits(_eq_picorv32_v_1894_647, 0, 0)
    _logic_and_picorv32_v_1893_646_Y <= bits(_logic_and_picorv32_v_1893_646, 0, 0)
    _logic_or_picorv32_v_1893_645_Y <= bits(_logic_or_picorv32_v_1893_645, 0, 0)
    _logic_and_picorv32_v_1893_644_Y <= bits(_logic_and_picorv32_v_1893_644, 0, 0)
    _logic_and_picorv32_v_1878_643_Y <= bits(_logic_and_picorv32_v_1878_643, 0, 0)
    _logic_not_picorv32_v_1878_642_Y <= bits(_logic_not_picorv32_v_1878_642, 0, 0)
    _add_picorv32_v_1875_641_Y <= bits(_add_picorv32_v_1875_641, 31, 0)
    _logic_or_picorv32_v_1865_640_Y <= bits(_logic_or_picorv32_v_1865_640, 0, 0)
    _logic_or_picorv32_v_1864_639_Y <= bits(_logic_or_picorv32_v_1864_639, 0, 0)
    _logic_not_picorv32_v_1861_638_Y <= bits(_logic_not_picorv32_v_1861_638, 0, 0)
    _logic_or_picorv32_v_1860_637_Y <= bits(_logic_or_picorv32_v_1860_637, 0, 0)
    _logic_not_picorv32_v_1860_636_Y <= bits(_logic_not_picorv32_v_1860_636, 0, 0)
    _logic_and_picorv32_v_1850_635_Y <= bits(_logic_and_picorv32_v_1850_635, 0, 0)
    _logic_not_picorv32_v_1850_634_Y <= bits(_logic_not_picorv32_v_1850_634, 0, 0)
    _add_picorv32_v_1847_633_Y <= bits(_add_picorv32_v_1847_633, 31, 0)
    _logic_not_picorv32_v_1836_632_Y <= bits(_logic_not_picorv32_v_1836_632, 0, 0)
    _logic_or_picorv32_v_1835_631_Y <= bits(_logic_or_picorv32_v_1835_631, 0, 0)
    _logic_not_picorv32_v_1835_630_Y <= bits(_logic_not_picorv32_v_1835_630, 0, 0)
    _sub_picorv32_v_1828_629_Y <= bits(_sub_picorv32_v_1828_629, 31, 0)
    _sshr_picorv32_v_1826_628_Y <= bits(_sshr_picorv32_v_1826_628, 31, 0)
    _logic_or_picorv32_v_1826_627_Y <= bits(_logic_or_picorv32_v_1826_627, 0, 0)
    _shr_picorv32_v_1825_626_Y <= bits(_shr_picorv32_v_1825_626, 31, 0)
    _logic_or_picorv32_v_1825_625_Y <= bits(_logic_or_picorv32_v_1825_625, 0, 0)
    _shl_picorv32_v_1824_624_Y <= bits(_shl_picorv32_v_1824_624, 31, 0)
    _logic_or_picorv32_v_1824_623_Y <= bits(_logic_or_picorv32_v_1824_623, 0, 0)
    _sub_picorv32_v_1820_622_Y <= bits(_sub_picorv32_v_1820_622, 31, 0)
    _sshr_picorv32_v_1818_621_Y <= bits(_sshr_picorv32_v_1818_621, 31, 0)
    _logic_or_picorv32_v_1818_620_Y <= bits(_logic_or_picorv32_v_1818_620, 0, 0)
    _shr_picorv32_v_1817_619_Y <= bits(_shr_picorv32_v_1817_619, 31, 0)
    _logic_or_picorv32_v_1817_618_Y <= bits(_logic_or_picorv32_v_1817_618, 0, 0)
    _shl_picorv32_v_1816_617_Y <= bits(_shl_picorv32_v_1816_617, 31, 0)
    _logic_or_picorv32_v_1816_616_Y <= bits(_logic_or_picorv32_v_1816_616, 0, 0)
    _logic_and_picorv32_v_1813_615_Y <= bits(_logic_and_picorv32_v_1813_615, 0, 0)
    _ge_picorv32_v_1813_614_Y <= bits(_ge_picorv32_v_1813_614, 0, 0)
    _eq_picorv32_v_1809_613_Y <= bits(_eq_picorv32_v_1809_613, 0, 0)
    _ternary_picorv32_v_1795_612_Y <= bits(_ternary_picorv32_v_1795_612, 0, 0)
    _ternary_picorv32_v_1792_611_Y <= bits(_ternary_picorv32_v_1792_611, 0, 0)
    _ternary_picorv32_v_1791_610_Y <= bits(_ternary_picorv32_v_1791_610, 0, 0)
    _logic_and_picorv32_v_1786_609_Y <= bits(_logic_and_picorv32_v_1786_609, 0, 0)
    _logic_not_picorv32_v_1786_608_Y <= bits(_logic_not_picorv32_v_1786_608, 0, 0)
    _logic_and_picorv32_v_1785_607_Y <= bits(_logic_and_picorv32_v_1785_607, 0, 0)
    _logic_or_picorv32_v_1785_606_Y <= bits(_logic_or_picorv32_v_1785_606, 0, 0)
    _add_picorv32_v_1784_605_Y <= bits(_add_picorv32_v_1784_605, 31, 0)
    _logic_and_picorv32_v_1774_604_Y <= bits(_logic_and_picorv32_v_1774_604, 0, 0)
    _logic_and_picorv32_v_1774_603_Y <= bits(_logic_and_picorv32_v_1774_603, 0, 0)
    _logic_or_picorv32_v_1773_602_Y <= bits(_logic_or_picorv32_v_1773_602, 0, 0)
    _logic_and_picorv32_v_1773_601_Y <= bits(_logic_and_picorv32_v_1773_601, 0, 0)
    _logic_and_picorv32_v_1769_600_Y <= bits(_logic_and_picorv32_v_1769_600, 0, 0)
    _logic_and_picorv32_v_1758_599_Y <= bits(_logic_and_picorv32_v_1758_599, 0, 0)
    _logic_not_picorv32_v_1758_598_Y <= bits(_logic_not_picorv32_v_1758_598, 0, 0)
    _logic_and_picorv32_v_1758_597_Y <= bits(_logic_and_picorv32_v_1758_597, 0, 0)
    _logic_not_picorv32_v_1758_596_Y <= bits(_logic_not_picorv32_v_1758_596, 0, 0)
    _logic_and_picorv32_v_1755_595_Y <= bits(_logic_and_picorv32_v_1755_595, 0, 0)
    _logic_or_picorv32_v_1755_594_Y <= bits(_logic_or_picorv32_v_1755_594, 0, 0)
    _logic_and_picorv32_v_1746_593_Y <= bits(_logic_and_picorv32_v_1746_593, 0, 0)
    _logic_and_picorv32_v_1724_592_Y <= bits(_logic_and_picorv32_v_1724_592, 0, 0)
    _logic_and_picorv32_v_1724_591_Y <= bits(_logic_and_picorv32_v_1724_591, 0, 0)
    _logic_or_picorv32_v_1723_590_Y <= bits(_logic_or_picorv32_v_1723_590, 0, 0)
    _logic_and_picorv32_v_1723_589_Y <= bits(_logic_and_picorv32_v_1723_589, 0, 0)
    _logic_and_picorv32_v_1719_588_Y <= bits(_logic_and_picorv32_v_1719_588, 0, 0)
    _ternary_picorv32_v_1695_587_Y <= bits(_ternary_picorv32_v_1695_587, 31, 0)
    _logic_and_picorv32_v_1695_585_Y <= bits(_logic_and_picorv32_v_1695_585, 0, 0)
    _logic_and_picorv32_v_1690_584_Y <= bits(_logic_and_picorv32_v_1690_584, 0, 0)
    _logic_and_picorv32_v_1682_583_Y <= bits(_logic_and_picorv32_v_1682_583, 0, 0)
    _logic_and_picorv32_v_1674_582_Y <= bits(_logic_and_picorv32_v_1674_582, 0, 0)
    _logic_not_picorv32_v_1674_581_Y <= bits(_logic_not_picorv32_v_1674_581, 0, 0)
    _logic_and_picorv32_v_1665_580_Y <= bits(_logic_and_picorv32_v_1665_580, 0, 0)
    _or_picorv32_v_1660_579_Y <= bits(_or_picorv32_v_1660_579, 31, 0)
    _logic_and_picorv32_v_1656_578_Y <= bits(_logic_and_picorv32_v_1656_578, 0, 0)
    _ternary_picorv32_v_1651_577_Y <= bits(_ternary_picorv32_v_1651_577, 31, 0)
    _and_picorv32_v_1651_576_Y <= bits(_and_picorv32_v_1651_576, 31, 0)
    _logic_and_picorv32_v_1645_575_Y <= bits(_logic_and_picorv32_v_1645_575, 0, 0)
    _or_picorv32_v_1641_574_Y <= bits(_or_picorv32_v_1641_574, 31, 0)
    _logic_and_picorv32_v_1636_573_Y <= bits(_logic_and_picorv32_v_1636_573, 0, 0)
    _logic_and_picorv32_v_1628_572_Y <= bits(_logic_and_picorv32_v_1628_572, 0, 0)
    _ternary_picorv32_v_1620_571_Y <= bits(_ternary_picorv32_v_1620_571, 31, 0)
    _logic_and_picorv32_v_1613_570_Y <= bits(_logic_and_picorv32_v_1613_570, 0, 0)
    _logic_and_picorv32_v_1609_569_Y <= bits(_logic_and_picorv32_v_1609_569, 0, 0)
    _logic_and_picorv32_v_1604_568_Y <= bits(_logic_and_picorv32_v_1604_568, 0, 0)
    _logic_and_picorv32_v_1597_567_Y <= bits(_logic_and_picorv32_v_1597_567, 0, 0)
    _logic_not_picorv32_v_1597_566_Y <= bits(_logic_not_picorv32_v_1597_566, 0, 0)
    _logic_and_picorv32_v_1597_565_Y <= bits(_logic_and_picorv32_v_1597_565, 0, 0)
    _logic_not_picorv32_v_1597_564_Y <= bits(_logic_not_picorv32_v_1597_564, 0, 0)
    _logic_and_picorv32_v_1563_563_Y <= bits(_logic_and_picorv32_v_1563_563, 0, 0)
    _logic_and_picorv32_v_1551_562_Y <= bits(_logic_and_picorv32_v_1551_562, 0, 0)
    _logic_not_picorv32_v_1551_561_Y <= bits(_logic_not_picorv32_v_1551_561, 0, 0)
    _logic_not_picorv32_v_1551_560_Y <= bits(_logic_not_picorv32_v_1551_560, 0, 0)
    _add_picorv32_v_1547_559_Y <= bits(_add_picorv32_v_1547_559, 31, 0)
    _add_picorv32_v_1542_558_Y <= bits(_add_picorv32_v_1542_558, 63, 0)
    _add_picorv32_v_1538_557_Y <= bits(_add_picorv32_v_1538_557, 31, 0)
    _ternary_picorv32_v_1538_556_Y <= bits(_ternary_picorv32_v_1538_556, 31, 0)
    _add_picorv32_v_1530_555_Y <= bits(_add_picorv32_v_1530_555, 31, 0)
    _ternary_picorv32_v_1530_554_Y <= bits(_ternary_picorv32_v_1530_554, 31, 0)
    _reduce_bool_picorv32_v_1527_553_Y <= bits(_reduce_bool_picorv32_v_1527_553, 0, 0)
    _logic_and_picorv32_v_1526_552_Y <= bits(_logic_and_picorv32_v_1526_552, 0, 0)
    _logic_and_picorv32_v_1526_551_Y <= bits(_logic_and_picorv32_v_1526_551, 0, 0)
    _logic_or_picorv32_v_1526_550_Y <= bits(_logic_or_picorv32_v_1526_550, 0, 0)
    _or_picorv32_v_1522_549_Y <= bits(_or_picorv32_v_1522_549, 31, 0)
    _ternary_picorv32_v_1519_548_Y <= bits(_ternary_picorv32_v_1519_548, 1, 0)
    _ternary_picorv32_v_1519_547_Y <= bits(_ternary_picorv32_v_1519_547, 1, 0)
    _eq_picorv32_v_1519_546_Y <= bits(_eq_picorv32_v_1519_546, 0, 0)
    _eq_picorv32_v_1518_545_Y <= bits(_eq_picorv32_v_1518_545, 0, 0)
    _logic_and_picorv32_v_1516_544_Y <= bits(_logic_and_picorv32_v_1516_544, 0, 0)
    _logic_or_picorv32_v_1516_543_Y <= bits(_logic_or_picorv32_v_1516_543, 0, 0)
    _logic_and_picorv32_v_1516_542_Y <= bits(_logic_and_picorv32_v_1516_542, 0, 0)
    _reduce_or_picorv32_v_1516_541_Y <= bits(_reduce_or_picorv32_v_1516_541, 0, 0)
    _and_picorv32_v_1516_540_Y <= bits(_and_picorv32_v_1516_540, 31, 0)
    _not_picorv32_v_1516_539_Y <= bits(_not_picorv32_v_1516_539, 31, 0)
    _logic_and_picorv32_v_1516_538_Y <= bits(_logic_and_picorv32_v_1516_538, 0, 0)
    _logic_not_picorv32_v_1516_537_Y <= bits(_logic_not_picorv32_v_1516_537, 0, 0)
    _logic_and_picorv32_v_1516_536_Y <= bits(_logic_and_picorv32_v_1516_536, 0, 0)
    _logic_not_picorv32_v_1516_535_Y <= bits(_logic_not_picorv32_v_1516_535, 0, 0)
    _or_picorv32_v_1501_534_Y <= bits(_or_picorv32_v_1501_534, 35, 0)
    _ternary_picorv32_v_1501_533_Y <= bits(_ternary_picorv32_v_1501_533, 31, 0)
    _ternary_picorv32_v_1501_532_Y <= bits(_ternary_picorv32_v_1501_532, 35, 0)
    _or_picorv32_v_1499_530_Y <= bits(_or_picorv32_v_1499_530, 35, 0)
    _and_picorv32_v_1499_529_Y <= bits(_and_picorv32_v_1499_529, 35, 0)
    _or_picorv32_v_1499_528_Y <= bits(_or_picorv32_v_1499_528, 35, 0)
    _ternary_picorv32_v_1499_527_Y <= bits(_ternary_picorv32_v_1499_527, 35, 0)
    _logic_and_picorv32_v_1495_525_Y <= bits(_logic_and_picorv32_v_1495_525, 0, 0)
    _and_picorv32_v_1491_524_Y <= bits(_and_picorv32_v_1491_524, 31, 0)
    _and_picorv32_v_1490_523_Y <= bits(_and_picorv32_v_1490_523, 31, 0)
    _not_picorv32_v_1490_522_Y <= bits(_not_picorv32_v_1490_522, 31, 0)
    _logic_and_picorv32_v_1489_521_Y <= bits(_logic_and_picorv32_v_1489_521, 0, 0)
    _logic_and_picorv32_v_1484_520_Y <= bits(_logic_and_picorv32_v_1484_520, 0, 0)
    _logic_and_picorv32_v_1481_519_Y <= bits(_logic_and_picorv32_v_1481_519, 0, 0)
    _logic_not_picorv32_v_1481_518_Y <= bits(_logic_not_picorv32_v_1481_518, 0, 0)
    _ternary_picorv32_v_1478_517_Y <= bits(_ternary_picorv32_v_1478_517, 31, 0)
    _and_picorv32_v_1478_516_Y <= bits(_and_picorv32_v_1478_516, 31, 0)
    _ternary_picorv32_v_1478_515_Y <= bits(_ternary_picorv32_v_1478_515, 31, 0)
    _logic_and_picorv32_v_1470_514_Y <= bits(_logic_and_picorv32_v_1470_514, 0, 0)
    _logic_not_picorv32_v_1470_513_Y <= bits(_logic_not_picorv32_v_1470_513, 0, 0)
    _logic_not_picorv32_v_1470_512_Y <= bits(_logic_not_picorv32_v_1470_512, 0, 0)
    _logic_not_picorv32_v_1435_511_Y <= bits(_logic_not_picorv32_v_1435_511, 0, 0)
    _logic_and_picorv32_v_1424_510_Y <= bits(_logic_and_picorv32_v_1424_510, 0, 0)
    _sub_picorv32_v_1417_509_Y <= bits(_sub_picorv32_v_1417_509, 31, 0)
    _eq_picorv32_v_1415_508_Y <= bits(_eq_picorv32_v_1415_508, 0, 0)
    _sub_picorv32_v_1415_507_Y <= bits(_sub_picorv32_v_1415_507, 31, 0)
    _logic_and_picorv32_v_1414_506_Y <= bits(_logic_and_picorv32_v_1414_506, 0, 0)
    _ternary_picorv32_v_1405_505_Y <= bits(_ternary_picorv32_v_1405_505, 63, 0)
    _add_picorv32_v_1405_503_Y <= bits(_add_picorv32_v_1405_503, 63, 0)
    _logic_and_picorv32_v_1372_501_Y <= bits(_logic_and_picorv32_v_1372_501, 0, 0)
    _logic_or_picorv32_v_1372_500_Y <= bits(_logic_or_picorv32_v_1372_500, 0, 0)
    _logic_not_picorv32_v_1372_499_Y <= bits(_logic_not_picorv32_v_1372_499, 0, 0)
    _and_picorv32_v_1372_498_Y <= bits(_and_picorv32_v_1372_498, 31, 0)
    _not_picorv32_v_1372_497_Y <= bits(_not_picorv32_v_1372_497, 31, 0)
    _logic_or_picorv32_v_1372_496_Y <= bits(_logic_or_picorv32_v_1372_496, 0, 0)
    _logic_or_picorv32_v_1372_495_Y <= bits(_logic_or_picorv32_v_1372_495, 0, 0)
    _logic_and_picorv32_v_1372_494_Y <= bits(_logic_and_picorv32_v_1372_494, 0, 0)
    _eq_picorv32_v_1372_493_Y <= bits(_eq_picorv32_v_1372_493, 0, 0)
    _ternary_picorv32_v_1325_492_Y <= bits(_ternary_picorv32_v_1325_492, 31, 0)
    _reduce_bool_picorv32_v_1325_491_Y <= bits(_reduce_bool_picorv32_v_1325_491, 0, 0)
    _ternary_picorv32_v_1324_489_Y <= bits(_ternary_picorv32_v_1324_489, 31, 0)
    _reduce_bool_picorv32_v_1324_488_Y <= bits(_reduce_bool_picorv32_v_1324_488, 0, 0)
    _logic_and_picorv32_v_1316_485_Y <= bits(_logic_and_picorv32_v_1316_485, 0, 0)
    _logic_and_picorv32_v_1316_484_Y <= bits(_logic_and_picorv32_v_1316_484, 0, 0)
    _and_picorv32_v_1307_479_Y <= bits(_and_picorv32_v_1307_479, 31, 0)
    _not_picorv32_v_1307_478_Y <= bits(_not_picorv32_v_1307_478, 31, 0)
    _logic_and_picorv32_v_1306_477_Y <= bits(_logic_and_picorv32_v_1306_477, 0, 0)
    _or_picorv32_v_1303_476_Y <= bits(_or_picorv32_v_1303_476, 31, 0)
    _logic_and_picorv32_v_1302_475_Y <= bits(_logic_and_picorv32_v_1302_475, 0, 0)
    _ternary_picorv32_v_1299_474_Y <= bits(_ternary_picorv32_v_1299_474, 31, 0)
    _logic_and_picorv32_v_1298_473_Y <= bits(_logic_and_picorv32_v_1298_473, 0, 0)
    _logic_not_picorv32_v_1298_472_Y <= bits(_logic_not_picorv32_v_1298_472, 0, 0)
    _add_picorv32_v_1295_471_Y <= bits(_add_picorv32_v_1295_471, 31, 0)
    _ternary_picorv32_v_1295_470_Y <= bits(_ternary_picorv32_v_1295_470, 31, 0)
    _eq_picorv32_v_1291_469_Y <= bits(_eq_picorv32_v_1291_469, 0, 0)
    _logic_or_picorv32_v_1277_467_Y <= bits(_logic_or_picorv32_v_1277_467, 0, 0)
    _logic_not_picorv32_v_1277_466_Y <= bits(_logic_not_picorv32_v_1277_466, 0, 0)
    _logic_or_picorv32_v_1277_465_Y <= bits(_logic_or_picorv32_v_1277_465, 0, 0)
    _logic_not_picorv32_v_1275_464_Y <= bits(_logic_not_picorv32_v_1275_464, 0, 0)
    _logic_and_picorv32_v_1260_461_Y <= bits(_logic_and_picorv32_v_1260_461, 0, 0)
    _logic_or_picorv32_v_1260_460_Y <= bits(_logic_or_picorv32_v_1260_460, 0, 0)
    _logic_or_picorv32_v_1260_459_Y <= bits(_logic_or_picorv32_v_1260_459, 0, 0)
    _logic_or_picorv32_v_1260_458_Y <= bits(_logic_or_picorv32_v_1260_458, 0, 0)
    _logic_and_picorv32_v_1258_457_Y <= bits(_logic_and_picorv32_v_1258_457, 0, 0)
    _logic_or_picorv32_v_1258_456_Y <= bits(_logic_or_picorv32_v_1258_456, 0, 0)
    _and_picorv32_v_1257_455_Y <= bits(_and_picorv32_v_1257_455, 31, 0)
    _logic_or_picorv32_v_1256_454_Y <= bits(_logic_or_picorv32_v_1256_454, 0, 0)
    _or_picorv32_v_1255_453_Y <= bits(_or_picorv32_v_1255_453, 31, 0)
    _logic_or_picorv32_v_1254_452_Y <= bits(_logic_or_picorv32_v_1254_452, 0, 0)
    _xor_picorv32_v_1253_451_Y <= bits(_xor_picorv32_v_1253_451, 31, 0)
    _logic_or_picorv32_v_1252_450_Y <= bits(_logic_or_picorv32_v_1252_450, 0, 0)
    _logic_and_picorv32_v_1241_449_Y <= bits(_logic_and_picorv32_v_1241_449, 0, 0)
    _logic_or_picorv32_v_1241_448_Y <= bits(_logic_or_picorv32_v_1241_448, 0, 0)
    _logic_not_picorv32_v_1241_447_Y <= bits(_logic_not_picorv32_v_1241_447, 0, 0)
    _logic_and_picorv32_v_1239_446_Y <= bits(_logic_and_picorv32_v_1239_446, 0, 0)
    _logic_or_picorv32_v_1239_445_Y <= bits(_logic_or_picorv32_v_1239_445, 0, 0)
    _logic_not_picorv32_v_1239_444_Y <= bits(_logic_not_picorv32_v_1239_444, 0, 0)
    _logic_not_picorv32_v_1238_443_Y <= bits(_logic_not_picorv32_v_1238_443, 0, 0)
    _logic_not_picorv32_v_1236_442_Y <= bits(_logic_not_picorv32_v_1236_442, 0, 0)
    _logic_not_picorv32_v_1234_441_Y <= bits(_logic_not_picorv32_v_1234_441, 0, 0)
    _ternary_picorv32_v_1191_439_Y <= bits(_ternary_picorv32_v_1191_439, 31, 0)
    _and_picorv32_v_1191_438_Y <= bits(_and_picorv32_v_1191_438, 31, 0)
    _logic_and_picorv32_v_1191_437_Y <= bits(_logic_and_picorv32_v_1191_437, 0, 0)
    _eq_picorv32_v_1173_436_Y <= bits(_eq_picorv32_v_1173_436, 0, 0)
    _eq_picorv32_v_1172_435_Y <= bits(_eq_picorv32_v_1172_435, 0, 0)
    _eq_picorv32_v_1171_434_Y <= bits(_eq_picorv32_v_1171_434, 0, 0)
    _eq_picorv32_v_1170_433_Y <= bits(_eq_picorv32_v_1170_433, 0, 0)
    _eq_picorv32_v_1169_432_Y <= bits(_eq_picorv32_v_1169_432, 0, 0)
    _eq_picorv32_v_1168_431_Y <= bits(_eq_picorv32_v_1168_431, 0, 0)
    _eq_picorv32_v_1167_430_Y <= bits(_eq_picorv32_v_1167_430, 0, 0)
    _eq_picorv32_v_1166_429_Y <= bits(_eq_picorv32_v_1166_429, 0, 0)
    _logic_not_picorv32_v_1116_427_Y <= bits(_logic_not_picorv32_v_1116_427, 0, 0)
    _reduce_or_picorv32_v_1105_426_Y <= bits(_reduce_or_picorv32_v_1105_426, 0, 0)
    _shl_picorv32_v_1104_425_Y <= bits(_shl_picorv32_v_1104_425, 31, 0)
    _reduce_or_picorv32_v_1103_424_Y <= bits(_reduce_or_picorv32_v_1103_424, 0, 0)
    _logic_and_picorv32_v_1094_423_Y <= bits(_logic_and_picorv32_v_1094_423, 0, 0)
    _reduce_or_picorv32_v_1094_422_Y <= bits(_reduce_or_picorv32_v_1094_422, 0, 0)
    _logic_and_picorv32_v_1091_421_Y <= bits(_logic_and_picorv32_v_1091_421, 0, 0)
    _eq_picorv32_v_1091_420_Y <= bits(_eq_picorv32_v_1091_420, 0, 0)
    _eq_picorv32_v_1091_419_Y <= bits(_eq_picorv32_v_1091_419, 0, 0)
    _logic_and_picorv32_v_1092_418_Y <= bits(_logic_and_picorv32_v_1092_418, 0, 0)
    _eq_picorv32_v_1092_417_Y <= bits(_eq_picorv32_v_1092_417, 0, 0)
    _eq_picorv32_v_1092_416_Y <= bits(_eq_picorv32_v_1092_416, 0, 0)
    _logic_and_picorv32_v_1094_415_Y <= bits(_logic_and_picorv32_v_1094_415, 0, 0)
    _eq_picorv32_v_1094_414_Y <= bits(_eq_picorv32_v_1094_414, 0, 0)
    _eq_picorv32_v_1093_413_Y <= bits(_eq_picorv32_v_1093_413, 0, 0)
    _logic_or_picorv32_v_1088_412_Y <= bits(_logic_or_picorv32_v_1088_412, 0, 0)
    _logic_and_picorv32_v_1088_411_Y <= bits(_logic_and_picorv32_v_1088_411, 0, 0)
    _reduce_or_picorv32_v_1088_410_Y <= bits(_reduce_or_picorv32_v_1088_410, 0, 0)
    _eq_picorv32_v_1082_409_Y <= bits(_eq_picorv32_v_1082_409, 0, 0)
    _eq_picorv32_v_1083_408_Y <= bits(_eq_picorv32_v_1083_408, 0, 0)
    _eq_picorv32_v_1084_407_Y <= bits(_eq_picorv32_v_1084_407, 0, 0)
    _eq_picorv32_v_1085_406_Y <= bits(_eq_picorv32_v_1085_406, 0, 0)
    _eq_picorv32_v_1086_405_Y <= bits(_eq_picorv32_v_1086_405, 0, 0)
    _eq_picorv32_v_1088_404_Y <= bits(_eq_picorv32_v_1088_404, 0, 0)
    _logic_and_picorv32_v_1079_403_Y <= bits(_logic_and_picorv32_v_1079_403, 0, 0)
    _reduce_or_picorv32_v_1079_402_Y <= bits(_reduce_or_picorv32_v_1079_402, 0, 0)
    _logic_and_picorv32_v_1076_401_Y <= bits(_logic_and_picorv32_v_1076_401, 0, 0)
    _eq_picorv32_v_1076_400_Y <= bits(_eq_picorv32_v_1076_400, 0, 0)
    _eq_picorv32_v_1076_399_Y <= bits(_eq_picorv32_v_1076_399, 0, 0)
    _logic_and_picorv32_v_1077_398_Y <= bits(_logic_and_picorv32_v_1077_398, 0, 0)
    _eq_picorv32_v_1077_397_Y <= bits(_eq_picorv32_v_1077_397, 0, 0)
    _eq_picorv32_v_1077_396_Y <= bits(_eq_picorv32_v_1077_396, 0, 0)
    _logic_and_picorv32_v_1079_395_Y <= bits(_logic_and_picorv32_v_1079_395, 0, 0)
    _eq_picorv32_v_1079_394_Y <= bits(_eq_picorv32_v_1079_394, 0, 0)
    _eq_picorv32_v_1078_393_Y <= bits(_eq_picorv32_v_1078_393, 0, 0)
    _logic_and_picorv32_v_1073_392_Y <= bits(_logic_and_picorv32_v_1073_392, 0, 0)
    _logic_and_picorv32_v_1073_391_Y <= bits(_logic_and_picorv32_v_1073_391, 0, 0)
    _logic_and_picorv32_v_1073_390_Y <= bits(_logic_and_picorv32_v_1073_390, 0, 0)
    _eq_picorv32_v_1073_389_Y <= bits(_eq_picorv32_v_1073_389, 0, 0)
    _eq_picorv32_v_1073_388_Y <= bits(_eq_picorv32_v_1073_388, 0, 0)
    _logic_and_picorv32_v_1072_387_Y <= bits(_logic_and_picorv32_v_1072_387, 0, 0)
    _logic_and_picorv32_v_1072_386_Y <= bits(_logic_and_picorv32_v_1072_386, 0, 0)
    _eq_picorv32_v_1072_385_Y <= bits(_eq_picorv32_v_1072_385, 0, 0)
    _eq_picorv32_v_1072_384_Y <= bits(_eq_picorv32_v_1072_384, 0, 0)
    _logic_and_picorv32_v_1071_383_Y <= bits(_logic_and_picorv32_v_1071_383, 0, 0)
    _logic_and_picorv32_v_1071_382_Y <= bits(_logic_and_picorv32_v_1071_382, 0, 0)
    _logic_and_picorv32_v_1071_381_Y <= bits(_logic_and_picorv32_v_1071_381, 0, 0)
    _eq_picorv32_v_1071_380_Y <= bits(_eq_picorv32_v_1071_380, 0, 0)
    _eq_picorv32_v_1071_379_Y <= bits(_eq_picorv32_v_1071_379, 0, 0)
    _logic_and_picorv32_v_1070_378_Y <= bits(_logic_and_picorv32_v_1070_378, 0, 0)
    _logic_and_picorv32_v_1070_377_Y <= bits(_logic_and_picorv32_v_1070_377, 0, 0)
    _logic_and_picorv32_v_1070_376_Y <= bits(_logic_and_picorv32_v_1070_376, 0, 0)
    _eq_picorv32_v_1070_375_Y <= bits(_eq_picorv32_v_1070_375, 0, 0)
    _eq_picorv32_v_1070_374_Y <= bits(_eq_picorv32_v_1070_374, 0, 0)
    _logic_or_picorv32_v_1068_373_Y <= bits(_logic_or_picorv32_v_1068_373, 0, 0)
    _logic_and_picorv32_v_1068_372_Y <= bits(_logic_and_picorv32_v_1068_372, 0, 0)
    _eq_picorv32_v_1068_371_Y <= bits(_eq_picorv32_v_1068_371, 0, 0)
    _logic_and_picorv32_v_1067_370_Y <= bits(_logic_and_picorv32_v_1067_370, 0, 0)
    _logic_not_picorv32_v_1067_369_Y <= bits(_logic_not_picorv32_v_1067_369, 0, 0)
    _logic_and_picorv32_v_1067_368_Y <= bits(_logic_and_picorv32_v_1067_368, 0, 0)
    _logic_not_picorv32_v_1067_367_Y <= bits(_logic_not_picorv32_v_1067_367, 0, 0)
    _eq_picorv32_v_1067_366_Y <= bits(_eq_picorv32_v_1067_366, 0, 0)
    _logic_and_picorv32_v_1065_365_Y <= bits(_logic_and_picorv32_v_1065_365, 0, 0)
    _logic_and_picorv32_v_1065_364_Y <= bits(_logic_and_picorv32_v_1065_364, 0, 0)
    _logic_and_picorv32_v_1065_363_Y <= bits(_logic_and_picorv32_v_1065_363, 0, 0)
    _eq_picorv32_v_1065_362_Y <= bits(_eq_picorv32_v_1065_362, 0, 0)
    _eq_picorv32_v_1065_361_Y <= bits(_eq_picorv32_v_1065_361, 0, 0)
    _logic_and_picorv32_v_1064_360_Y <= bits(_logic_and_picorv32_v_1064_360, 0, 0)
    _logic_and_picorv32_v_1064_359_Y <= bits(_logic_and_picorv32_v_1064_359, 0, 0)
    _eq_picorv32_v_1064_358_Y <= bits(_eq_picorv32_v_1064_358, 0, 0)
    _eq_picorv32_v_1064_357_Y <= bits(_eq_picorv32_v_1064_357, 0, 0)
    _logic_and_picorv32_v_1063_356_Y <= bits(_logic_and_picorv32_v_1063_356, 0, 0)
    _logic_and_picorv32_v_1063_355_Y <= bits(_logic_and_picorv32_v_1063_355, 0, 0)
    _logic_or_picorv32_v_1063_354_Y <= bits(_logic_or_picorv32_v_1063_354, 0, 0)
    _logic_and_picorv32_v_1063_353_Y <= bits(_logic_and_picorv32_v_1063_353, 0, 0)
    _eq_picorv32_v_1063_352_Y <= bits(_eq_picorv32_v_1063_352, 0, 0)
    _eq_picorv32_v_1063_351_Y <= bits(_eq_picorv32_v_1063_351, 0, 0)
    _logic_and_picorv32_v_1062_350_Y <= bits(_logic_and_picorv32_v_1062_350, 0, 0)
    _eq_picorv32_v_1062_349_Y <= bits(_eq_picorv32_v_1062_349, 0, 0)
    _eq_picorv32_v_1062_348_Y <= bits(_eq_picorv32_v_1062_348, 0, 0)
    _logic_and_picorv32_v_1061_347_Y <= bits(_logic_and_picorv32_v_1061_347, 0, 0)
    _logic_or_picorv32_v_1061_346_Y <= bits(_logic_or_picorv32_v_1061_346, 0, 0)
    _logic_and_picorv32_v_1061_345_Y <= bits(_logic_and_picorv32_v_1061_345, 0, 0)
    _eq_picorv32_v_1061_344_Y <= bits(_eq_picorv32_v_1061_344, 0, 0)
    _eq_picorv32_v_1061_343_Y <= bits(_eq_picorv32_v_1061_343, 0, 0)
    _logic_and_picorv32_v_1060_342_Y <= bits(_logic_and_picorv32_v_1060_342, 0, 0)
    _eq_picorv32_v_1060_341_Y <= bits(_eq_picorv32_v_1060_341, 0, 0)
    _eq_picorv32_v_1060_340_Y <= bits(_eq_picorv32_v_1060_340, 0, 0)
    _logic_and_picorv32_v_1058_339_Y <= bits(_logic_and_picorv32_v_1058_339, 0, 0)
    _eq_picorv32_v_1058_338_Y <= bits(_eq_picorv32_v_1058_338, 0, 0)
    _logic_and_picorv32_v_1058_337_Y <= bits(_logic_and_picorv32_v_1058_337, 0, 0)
    _eq_picorv32_v_1058_336_Y <= bits(_eq_picorv32_v_1058_336, 0, 0)
    _logic_and_picorv32_v_1057_335_Y <= bits(_logic_and_picorv32_v_1057_335, 0, 0)
    _eq_picorv32_v_1057_334_Y <= bits(_eq_picorv32_v_1057_334, 0, 0)
    _logic_and_picorv32_v_1057_333_Y <= bits(_logic_and_picorv32_v_1057_333, 0, 0)
    _eq_picorv32_v_1057_332_Y <= bits(_eq_picorv32_v_1057_332, 0, 0)
    _logic_and_picorv32_v_1056_331_Y <= bits(_logic_and_picorv32_v_1056_331, 0, 0)
    _eq_picorv32_v_1056_330_Y <= bits(_eq_picorv32_v_1056_330, 0, 0)
    _logic_and_picorv32_v_1056_329_Y <= bits(_logic_and_picorv32_v_1056_329, 0, 0)
    _eq_picorv32_v_1056_328_Y <= bits(_eq_picorv32_v_1056_328, 0, 0)
    _logic_and_picorv32_v_1055_327_Y <= bits(_logic_and_picorv32_v_1055_327, 0, 0)
    _eq_picorv32_v_1055_326_Y <= bits(_eq_picorv32_v_1055_326, 0, 0)
    _logic_and_picorv32_v_1055_325_Y <= bits(_logic_and_picorv32_v_1055_325, 0, 0)
    _eq_picorv32_v_1055_324_Y <= bits(_eq_picorv32_v_1055_324, 0, 0)
    _logic_and_picorv32_v_1054_323_Y <= bits(_logic_and_picorv32_v_1054_323, 0, 0)
    _eq_picorv32_v_1054_322_Y <= bits(_eq_picorv32_v_1054_322, 0, 0)
    _logic_and_picorv32_v_1054_321_Y <= bits(_logic_and_picorv32_v_1054_321, 0, 0)
    _eq_picorv32_v_1054_320_Y <= bits(_eq_picorv32_v_1054_320, 0, 0)
    _logic_and_picorv32_v_1053_319_Y <= bits(_logic_and_picorv32_v_1053_319, 0, 0)
    _eq_picorv32_v_1053_318_Y <= bits(_eq_picorv32_v_1053_318, 0, 0)
    _logic_and_picorv32_v_1053_317_Y <= bits(_logic_and_picorv32_v_1053_317, 0, 0)
    _eq_picorv32_v_1053_316_Y <= bits(_eq_picorv32_v_1053_316, 0, 0)
    _logic_and_picorv32_v_1052_315_Y <= bits(_logic_and_picorv32_v_1052_315, 0, 0)
    _eq_picorv32_v_1052_314_Y <= bits(_eq_picorv32_v_1052_314, 0, 0)
    _logic_and_picorv32_v_1052_313_Y <= bits(_logic_and_picorv32_v_1052_313, 0, 0)
    _eq_picorv32_v_1052_312_Y <= bits(_eq_picorv32_v_1052_312, 0, 0)
    _logic_and_picorv32_v_1051_311_Y <= bits(_logic_and_picorv32_v_1051_311, 0, 0)
    _eq_picorv32_v_1051_310_Y <= bits(_eq_picorv32_v_1051_310, 0, 0)
    _logic_and_picorv32_v_1051_309_Y <= bits(_logic_and_picorv32_v_1051_309, 0, 0)
    _eq_picorv32_v_1051_308_Y <= bits(_eq_picorv32_v_1051_308, 0, 0)
    _logic_and_picorv32_v_1050_307_Y <= bits(_logic_and_picorv32_v_1050_307, 0, 0)
    _eq_picorv32_v_1050_306_Y <= bits(_eq_picorv32_v_1050_306, 0, 0)
    _logic_and_picorv32_v_1050_305_Y <= bits(_logic_and_picorv32_v_1050_305, 0, 0)
    _eq_picorv32_v_1050_304_Y <= bits(_eq_picorv32_v_1050_304, 0, 0)
    _logic_and_picorv32_v_1049_303_Y <= bits(_logic_and_picorv32_v_1049_303, 0, 0)
    _eq_picorv32_v_1049_302_Y <= bits(_eq_picorv32_v_1049_302, 0, 0)
    _logic_and_picorv32_v_1049_301_Y <= bits(_logic_and_picorv32_v_1049_301, 0, 0)
    _eq_picorv32_v_1049_300_Y <= bits(_eq_picorv32_v_1049_300, 0, 0)
    _logic_and_picorv32_v_1047_299_Y <= bits(_logic_and_picorv32_v_1047_299, 0, 0)
    _eq_picorv32_v_1047_298_Y <= bits(_eq_picorv32_v_1047_298, 0, 0)
    _logic_and_picorv32_v_1047_297_Y <= bits(_logic_and_picorv32_v_1047_297, 0, 0)
    _eq_picorv32_v_1047_296_Y <= bits(_eq_picorv32_v_1047_296, 0, 0)
    _logic_and_picorv32_v_1046_295_Y <= bits(_logic_and_picorv32_v_1046_295, 0, 0)
    _eq_picorv32_v_1046_294_Y <= bits(_eq_picorv32_v_1046_294, 0, 0)
    _logic_and_picorv32_v_1046_293_Y <= bits(_logic_and_picorv32_v_1046_293, 0, 0)
    _eq_picorv32_v_1046_292_Y <= bits(_eq_picorv32_v_1046_292, 0, 0)
    _logic_and_picorv32_v_1045_291_Y <= bits(_logic_and_picorv32_v_1045_291, 0, 0)
    _eq_picorv32_v_1045_290_Y <= bits(_eq_picorv32_v_1045_290, 0, 0)
    _logic_and_picorv32_v_1045_289_Y <= bits(_logic_and_picorv32_v_1045_289, 0, 0)
    _eq_picorv32_v_1045_288_Y <= bits(_eq_picorv32_v_1045_288, 0, 0)
    _logic_and_picorv32_v_1043_287_Y <= bits(_logic_and_picorv32_v_1043_287, 0, 0)
    _eq_picorv32_v_1043_286_Y <= bits(_eq_picorv32_v_1043_286, 0, 0)
    _logic_and_picorv32_v_1042_285_Y <= bits(_logic_and_picorv32_v_1042_285, 0, 0)
    _eq_picorv32_v_1042_284_Y <= bits(_eq_picorv32_v_1042_284, 0, 0)
    _logic_and_picorv32_v_1041_283_Y <= bits(_logic_and_picorv32_v_1041_283, 0, 0)
    _eq_picorv32_v_1041_282_Y <= bits(_eq_picorv32_v_1041_282, 0, 0)
    _logic_and_picorv32_v_1040_281_Y <= bits(_logic_and_picorv32_v_1040_281, 0, 0)
    _eq_picorv32_v_1040_280_Y <= bits(_eq_picorv32_v_1040_280, 0, 0)
    _logic_and_picorv32_v_1039_279_Y <= bits(_logic_and_picorv32_v_1039_279, 0, 0)
    _eq_picorv32_v_1039_278_Y <= bits(_eq_picorv32_v_1039_278, 0, 0)
    _logic_and_picorv32_v_1038_277_Y <= bits(_logic_and_picorv32_v_1038_277, 0, 0)
    _eq_picorv32_v_1038_276_Y <= bits(_eq_picorv32_v_1038_276, 0, 0)
    _logic_and_picorv32_v_1036_275_Y <= bits(_logic_and_picorv32_v_1036_275, 0, 0)
    _eq_picorv32_v_1036_274_Y <= bits(_eq_picorv32_v_1036_274, 0, 0)
    _logic_and_picorv32_v_1035_273_Y <= bits(_logic_and_picorv32_v_1035_273, 0, 0)
    _eq_picorv32_v_1035_272_Y <= bits(_eq_picorv32_v_1035_272, 0, 0)
    _logic_and_picorv32_v_1034_271_Y <= bits(_logic_and_picorv32_v_1034_271, 0, 0)
    _eq_picorv32_v_1034_270_Y <= bits(_eq_picorv32_v_1034_270, 0, 0)
    _logic_and_picorv32_v_1032_269_Y <= bits(_logic_and_picorv32_v_1032_269, 0, 0)
    _eq_picorv32_v_1032_268_Y <= bits(_eq_picorv32_v_1032_268, 0, 0)
    _logic_and_picorv32_v_1031_267_Y <= bits(_logic_and_picorv32_v_1031_267, 0, 0)
    _eq_picorv32_v_1031_266_Y <= bits(_eq_picorv32_v_1031_266, 0, 0)
    _logic_and_picorv32_v_1030_265_Y <= bits(_logic_and_picorv32_v_1030_265, 0, 0)
    _eq_picorv32_v_1030_264_Y <= bits(_eq_picorv32_v_1030_264, 0, 0)
    _logic_and_picorv32_v_1029_263_Y <= bits(_logic_and_picorv32_v_1029_263, 0, 0)
    _eq_picorv32_v_1029_262_Y <= bits(_eq_picorv32_v_1029_262, 0, 0)
    _logic_and_picorv32_v_1028_261_Y <= bits(_logic_and_picorv32_v_1028_261, 0, 0)
    _eq_picorv32_v_1028_260_Y <= bits(_eq_picorv32_v_1028_260, 0, 0)
    _logic_and_picorv32_v_1026_259_Y <= bits(_logic_and_picorv32_v_1026_259, 0, 0)
    _eq_picorv32_v_1026_258_Y <= bits(_eq_picorv32_v_1026_258, 0, 0)
    _logic_and_picorv32_v_1025_257_Y <= bits(_logic_and_picorv32_v_1025_257, 0, 0)
    _eq_picorv32_v_1025_256_Y <= bits(_eq_picorv32_v_1025_256, 0, 0)
    _logic_and_picorv32_v_1024_255_Y <= bits(_logic_and_picorv32_v_1024_255, 0, 0)
    _eq_picorv32_v_1024_254_Y <= bits(_eq_picorv32_v_1024_254, 0, 0)
    _logic_and_picorv32_v_1023_253_Y <= bits(_logic_and_picorv32_v_1023_253, 0, 0)
    _eq_picorv32_v_1023_252_Y <= bits(_eq_picorv32_v_1023_252, 0, 0)
    _logic_and_picorv32_v_1022_251_Y <= bits(_logic_and_picorv32_v_1022_251, 0, 0)
    _eq_picorv32_v_1022_250_Y <= bits(_eq_picorv32_v_1022_250, 0, 0)
    _logic_and_picorv32_v_1021_249_Y <= bits(_logic_and_picorv32_v_1021_249, 0, 0)
    _eq_picorv32_v_1021_248_Y <= bits(_eq_picorv32_v_1021_248, 0, 0)
    _logic_and_picorv32_v_1018_247_Y <= bits(_logic_and_picorv32_v_1018_247, 0, 0)
    _logic_not_picorv32_v_1018_246_Y <= bits(_logic_not_picorv32_v_1018_246, 0, 0)
    _logic_and_picorv32_v_1000_245_Y <= bits(_logic_and_picorv32_v_1000_245, 0, 0)
    _ne_picorv32_v_1000_244_Y <= bits(_ne_picorv32_v_1000_244, 0, 0)
    _ne_picorv32_v_1000_243_Y <= bits(_ne_picorv32_v_1000_243, 0, 0)
    _logic_and_picorv32_v_995_242_Y <= bits(_logic_and_picorv32_v_995_242, 0, 0)
    _eq_picorv32_v_995_241_Y <= bits(_eq_picorv32_v_995_241, 0, 0)
    _logic_and_picorv32_v_995_240_Y <= bits(_logic_and_picorv32_v_995_240, 0, 0)
    _ne_picorv32_v_995_239_Y <= bits(_ne_picorv32_v_995_239, 0, 0)
    _ne_picorv32_v_995_238_Y <= bits(_ne_picorv32_v_995_238, 0, 0)
    _logic_and_picorv32_v_989_237_Y <= bits(_logic_and_picorv32_v_989_237, 0, 0)
    _ne_picorv32_v_989_236_Y <= bits(_ne_picorv32_v_989_236, 0, 0)
    _eq_picorv32_v_989_235_Y <= bits(_eq_picorv32_v_989_235, 0, 0)
    _logic_and_picorv32_v_984_234_Y <= bits(_logic_and_picorv32_v_984_234, 0, 0)
    _eq_picorv32_v_984_233_Y <= bits(_eq_picorv32_v_984_233, 0, 0)
    _logic_and_picorv32_v_984_232_Y <= bits(_logic_and_picorv32_v_984_232, 0, 0)
    _ne_picorv32_v_984_231_Y <= bits(_ne_picorv32_v_984_231, 0, 0)
    _eq_picorv32_v_984_230_Y <= bits(_eq_picorv32_v_984_230, 0, 0)
    _reduce_bool_picorv32_v_977_229_Y <= bits(_reduce_bool_picorv32_v_977_229, 0, 0)
    _logic_not_picorv32_v_969_228_Y <= bits(_logic_not_picorv32_v_969_228, 0, 0)
    _add_picorv32_v_961_227_Y <= bits(_add_picorv32_v_961_227, 31, 0)
    _add_picorv32_v_956_226_Y <= bits(_add_picorv32_v_956_226, 31, 0)
    _add_picorv32_v_948_225_Y <= bits(_add_picorv32_v_948_225, 31, 0)
    _add_picorv32_v_947_224_Y <= bits(_add_picorv32_v_947_224, 31, 0)
    _add_picorv32_v_946_223_Y <= bits(_add_picorv32_v_946_223, 31, 0)
    _eq_picorv32_v_944_222_Y <= bits(_eq_picorv32_v_944_222, 0, 0)
    _add_picorv32_v_942_221_Y <= bits(_add_picorv32_v_942_221, 31, 0)
    _add_picorv32_v_941_220_Y <= bits(_add_picorv32_v_941_220, 31, 0)
    _eq_picorv32_v_939_219_Y <= bits(_eq_picorv32_v_939_219, 0, 0)
    _add_picorv32_v_936_218_Y <= bits(_add_picorv32_v_936_218, 31, 0)
    _add_picorv32_v_935_217_Y <= bits(_add_picorv32_v_935_217, 31, 0)
    _logic_and_picorv32_v_933_216_Y <= bits(_logic_and_picorv32_v_933_216, 0, 0)
    _logic_not_picorv32_v_933_215_Y <= bits(_logic_not_picorv32_v_933_215, 0, 0)
    _logic_not_picorv32_v_933_214_Y <= bits(_logic_not_picorv32_v_933_214, 0, 0)
    _eq_picorv32_v_921_213_Y <= bits(_eq_picorv32_v_921_213, 0, 0)
    _logic_or_picorv32_v_920_212_Y <= bits(_logic_or_picorv32_v_920_212, 0, 0)
    _add_picorv32_v_899_211_Y <= bits(_add_picorv32_v_899_211, 31, 0)
    _add_picorv32_v_898_210_Y <= bits(_add_picorv32_v_898_210, 31, 0)
    _add_picorv32_v_894_209_Y <= bits(_add_picorv32_v_894_209, 31, 0)
    _add_picorv32_v_893_208_Y <= bits(_add_picorv32_v_893_208, 31, 0)
    _add_picorv32_v_889_207_Y <= bits(_add_picorv32_v_889_207, 31, 0)
    _reduce_or_picorv32_v_887_206_Y <= bits(_reduce_or_picorv32_v_887_206, 0, 0)
    _logic_and_picorv32_v_874_205_Y <= bits(_logic_and_picorv32_v_874_205, 0, 0)
    _ne_picorv32_v_874_204_Y <= bits(_ne_picorv32_v_874_204, 0, 0)
    _logic_and_picorv32_v_870_203_Y <= bits(_logic_and_picorv32_v_870_203, 0, 0)
    _logic_and_picorv32_v_870_202_Y <= bits(_logic_and_picorv32_v_870_202, 0, 0)
    _eq_picorv32_v_870_201_Y <= bits(_eq_picorv32_v_870_201, 0, 0)
    _eq_picorv32_v_870_200_Y <= bits(_eq_picorv32_v_870_200, 0, 0)
    _logic_and_picorv32_v_867_199_Y <= bits(_logic_and_picorv32_v_867_199, 0, 0)
    _logic_and_picorv32_v_867_198_Y <= bits(_logic_and_picorv32_v_867_198, 0, 0)
    _logic_and_picorv32_v_867_197_Y <= bits(_logic_and_picorv32_v_867_197, 0, 0)
    _eq_picorv32_v_867_196_Y <= bits(_eq_picorv32_v_867_196, 0, 0)
    _eq_picorv32_v_867_195_Y <= bits(_eq_picorv32_v_867_195, 0, 0)
    _eq_picorv32_v_859_194_Y <= bits(_eq_picorv32_v_859_194, 0, 0)
    _eq_picorv32_v_858_193_Y <= bits(_eq_picorv32_v_858_193, 0, 0)
    _eq_picorv32_v_857_192_Y <= bits(_eq_picorv32_v_857_192, 0, 0)
    _eq_picorv32_v_856_191_Y <= bits(_eq_picorv32_v_856_191, 0, 0)
    _eq_picorv32_v_855_190_Y <= bits(_eq_picorv32_v_855_190, 0, 0)
    _logic_and_picorv32_v_853_189_Y <= bits(_logic_and_picorv32_v_853_189, 0, 0)
    _logic_and_picorv32_v_853_188_Y <= bits(_logic_and_picorv32_v_853_188, 0, 0)
    _eq_picorv32_v_853_187_Y <= bits(_eq_picorv32_v_853_187, 0, 0)
    _eq_picorv32_v_853_186_Y <= bits(_eq_picorv32_v_853_186, 0, 0)
    _logic_and_picorv32_v_852_185_Y <= bits(_logic_and_picorv32_v_852_185, 0, 0)
    _logic_and_picorv32_v_852_184_Y <= bits(_logic_and_picorv32_v_852_184, 0, 0)
    _eq_picorv32_v_852_183_Y <= bits(_eq_picorv32_v_852_183, 0, 0)
    _eq_picorv32_v_852_182_Y <= bits(_eq_picorv32_v_852_182, 0, 0)
    _logic_and_picorv32_v_851_181_Y <= bits(_logic_and_picorv32_v_851_181, 0, 0)
    _eq_picorv32_v_851_180_Y <= bits(_eq_picorv32_v_851_180, 0, 0)
    _eq_picorv32_v_851_179_Y <= bits(_eq_picorv32_v_851_179, 0, 0)
    _eq_picorv32_v_850_178_Y <= bits(_eq_picorv32_v_850_178, 0, 0)
    _eq_picorv32_v_849_177_Y <= bits(_eq_picorv32_v_849_177, 0, 0)
    _eq_picorv32_v_848_176_Y <= bits(_eq_picorv32_v_848_176, 0, 0)
    _logic_and_picorv32_v_847_175_Y <= bits(_logic_and_picorv32_v_847_175, 0, 0)
    _reduce_or_picorv32_v_845_174_Y <= bits(_reduce_or_picorv32_v_845_174, 0, 0)
    _reduce_or_picorv32_v_844_173_Y <= bits(_reduce_or_picorv32_v_844_173, 0, 0)
    _reduce_or_picorv32_v_843_172_Y <= bits(_reduce_or_picorv32_v_843_172, 0, 0)
    _reduce_or_picorv32_v_842_171_Y <= bits(_reduce_or_picorv32_v_842_171, 0, 0)
    _reduce_or_picorv32_v_841_170_Y <= bits(_reduce_or_picorv32_v_841_170, 0, 0)
    _reduce_or_picorv32_v_840_169_Y <= bits(_reduce_or_picorv32_v_840_169, 0, 0)
    _reduce_and_picorv32_v_808_167_Y <= bits(_reduce_and_picorv32_v_808_167, 0, 0)
    _reduce_and_picorv32_v_776_165_Y <= bits(_reduce_and_picorv32_v_776_165, 0, 0)
    _logic_or_picorv32_v_768_164_Y <= bits(_logic_or_picorv32_v_768_164, 0, 0)
    _logic_not_picorv32_v_768_163_Y <= bits(_logic_not_picorv32_v_768_163, 0, 0)
    _reduce_or_picorv32_v_670_160_Y <= bits(_reduce_or_picorv32_v_670_160, 0, 0)
    _logic_and_picorv32_v_667_159_Y <= bits(_logic_and_picorv32_v_667_159, 0, 0)
    _logic_not_picorv32_v_667_158_Y <= bits(_logic_not_picorv32_v_667_158, 0, 0)
    _ternary_picorv32_v_600_157_Y <= bits(_ternary_picorv32_v_600_157, 31, 0)
    _logic_or_picorv32_v_600_156_Y <= bits(_logic_or_picorv32_v_600_156, 0, 0)
    _logic_or_picorv32_v_593_155_Y <= bits(_logic_or_picorv32_v_593_155, 0, 0)
    _logic_not_picorv32_v_593_154_Y <= bits(_logic_not_picorv32_v_593_154, 0, 0)
    _reduce_and_picorv32_v_593_153_Y <= bits(_reduce_and_picorv32_v_593_153, 0, 0)
    _logic_and_picorv32_v_592_152_Y <= bits(_logic_and_picorv32_v_592_152, 0, 0)
    _logic_not_picorv32_v_592_151_Y <= bits(_logic_not_picorv32_v_592_151, 0, 0)
    _logic_not_picorv32_v_587_150_Y <= bits(_logic_not_picorv32_v_587_150, 0, 0)
    _logic_and_picorv32_v_584_149_Y <= bits(_logic_and_picorv32_v_584_149, 0, 0)
    _logic_or_picorv32_v_568_148_Y <= bits(_logic_or_picorv32_v_568_148, 0, 0)
    _logic_not_picorv32_v_567_147_Y <= bits(_logic_not_picorv32_v_567_147, 0, 0)
    _logic_or_picorv32_v_566_146_Y <= bits(_logic_or_picorv32_v_566_146, 0, 0)
    _logic_or_picorv32_v_566_145_Y <= bits(_logic_or_picorv32_v_566_145, 0, 0)
    _and_picorv32_v_559_144_Y <= bits(_and_picorv32_v_559_144, 3, 0)
    _logic_or_picorv32_v_557_143_Y <= bits(_logic_or_picorv32_v_557_143, 0, 0)
    _logic_or_picorv32_v_552_142_Y <= bits(_logic_or_picorv32_v_552_142, 0, 0)
    _logic_not_picorv32_v_552_141_Y <= bits(_logic_not_picorv32_v_552_141, 0, 0)
    _logic_not_picorv32_v_550_140_Y <= bits(_logic_not_picorv32_v_550_140, 0, 0)
    _logic_or_picorv32_v_549_139_Y <= bits(_logic_or_picorv32_v_549_139, 0, 0)
    _logic_not_picorv32_v_549_138_Y <= bits(_logic_not_picorv32_v_549_138, 0, 0)
    _logic_or_picorv32_v_543_136_Y <= bits(_logic_or_picorv32_v_543_136, 0, 0)
    _eq_picorv32_v_543_135_Y <= bits(_eq_picorv32_v_543_135, 0, 0)
    _eq_picorv32_v_543_134_Y <= bits(_eq_picorv32_v_543_134, 0, 0)
    _logic_or_picorv32_v_534_133_Y <= bits(_logic_or_picorv32_v_534_133, 0, 0)
    _logic_or_picorv32_v_531_132_Y <= bits(_logic_or_picorv32_v_531_132, 0, 0)
    _logic_or_picorv32_v_531_131_Y <= bits(_logic_or_picorv32_v_531_131, 0, 0)
    _logic_and_picorv32_v_530_130_Y <= bits(_logic_and_picorv32_v_530_130, 0, 0)
    _logic_not_picorv32_v_530_129_Y <= bits(_logic_not_picorv32_v_530_129, 0, 0)
    _logic_and_picorv32_v_514_127_Y <= bits(_logic_and_picorv32_v_514_127, 0, 0)
    _ne_picorv32_v_514_126_Y <= bits(_ne_picorv32_v_514_126, 0, 0)
    _ne_picorv32_v_514_125_Y <= bits(_ne_picorv32_v_514_125, 0, 0)
    _logic_and_picorv32_v_510_124_Y <= bits(_logic_and_picorv32_v_510_124, 0, 0)
    _eq_picorv32_v_510_123_Y <= bits(_eq_picorv32_v_510_123, 0, 0)
    _logic_and_picorv32_v_510_122_Y <= bits(_logic_and_picorv32_v_510_122, 0, 0)
    _ne_picorv32_v_510_121_Y <= bits(_ne_picorv32_v_510_121, 0, 0)
    _ne_picorv32_v_510_120_Y <= bits(_ne_picorv32_v_510_120, 0, 0)
    _logic_and_picorv32_v_506_119_Y <= bits(_logic_and_picorv32_v_506_119, 0, 0)
    _ne_picorv32_v_506_118_Y <= bits(_ne_picorv32_v_506_118, 0, 0)
    _eq_picorv32_v_506_117_Y <= bits(_eq_picorv32_v_506_117, 0, 0)
    _logic_and_picorv32_v_502_116_Y <= bits(_logic_and_picorv32_v_502_116, 0, 0)
    _eq_picorv32_v_502_115_Y <= bits(_eq_picorv32_v_502_115, 0, 0)
    _eq_picorv32_v_502_114_Y <= bits(_eq_picorv32_v_502_114, 0, 0)
    _ternary_picorv32_v_474_113_Y <= bits(_ternary_picorv32_v_474_113, 6, 0)
    _eq_picorv32_v_474_112_Y <= bits(_eq_picorv32_v_474_112, 0, 0)
    _eq_picorv32_v_473_111_Y <= bits(_eq_picorv32_v_473_111, 0, 0)
    _eq_picorv32_v_472_110_Y <= bits(_eq_picorv32_v_472_110, 0, 0)
    _eq_picorv32_v_471_109_Y <= bits(_eq_picorv32_v_471_109, 0, 0)
    _eq_picorv32_v_470_108_Y <= bits(_eq_picorv32_v_470_108, 0, 0)
    _eq_picorv32_v_469_107_Y <= bits(_eq_picorv32_v_469_107, 0, 0)
    _eq_picorv32_v_465_106_Y <= bits(_eq_picorv32_v_465_106, 0, 0)
    _eq_picorv32_v_461_105_Y <= bits(_eq_picorv32_v_461_105, 0, 0)
    _eq_picorv32_v_457_104_Y <= bits(_eq_picorv32_v_457_104, 0, 0)
    _eq_picorv32_v_448_103_Y <= bits(_eq_picorv32_v_448_103, 0, 0)
    _logic_and_picorv32_v_419_102_Y <= bits(_logic_and_picorv32_v_419_102, 0, 0)
    _logic_or_picorv32_v_419_101_Y <= bits(_logic_or_picorv32_v_419_101, 0, 0)
    _logic_and_picorv32_v_419_100_Y <= bits(_logic_and_picorv32_v_419_100, 0, 0)
    _ternary_picorv32_v_416_99_Y <= bits(_ternary_picorv32_v_416_99, 31, 0)
    _ternary_picorv32_v_415_98_Y <= bits(_ternary_picorv32_v_415_98, 31, 0)
    _shl_picorv32_v_402_96_Y <= bits(_shl_picorv32_v_402_96, 3, 0)
    _ternary_picorv32_v_394_95_Y <= bits(_ternary_picorv32_v_394_95, 3, 0)
    _logic_and_picorv32_v_380_93_Y <= bits(_logic_and_picorv32_v_380_93, 0, 0)
    _logic_not_picorv32_v_380_92_Y <= bits(_logic_not_picorv32_v_380_92, 0, 0)
    _logic_not_picorv32_v_378_91_Y <= bits(_logic_not_picorv32_v_378_91, 0, 0)
    _logic_not_picorv32_v_374_90_Y <= bits(_logic_not_picorv32_v_374_90, 0, 0)
    _ternary_picorv32_v_371_88_Y <= bits(_ternary_picorv32_v_371_88, 31, 0)
    _ternary_picorv32_v_371_87_Y <= bits(_ternary_picorv32_v_371_87, 31, 0)
    _ternary_picorv32_v_371_86_Y <= bits(_ternary_picorv32_v_371_86, 31, 0)
    _logic_and_picorv32_v_371_85_Y <= bits(_logic_and_picorv32_v_371_85, 0, 0)
    _logic_and_picorv32_v_370_84_Y <= bits(_logic_and_picorv32_v_370_84, 0, 0)
    _logic_and_picorv32_v_369_83_Y <= bits(_logic_and_picorv32_v_369_83, 0, 0)
    _ternary_picorv32_v_367_82_Y <= bits(_ternary_picorv32_v_367_82, 31, 0)
    _logic_or_picorv32_v_367_81_Y <= bits(_logic_or_picorv32_v_367_81, 0, 0)
    _ternary_picorv32_v_365_80_Y <= bits(_ternary_picorv32_v_365_80, 31, 0)
    _add_picorv32_v_365_79_Y <= bits(_add_picorv32_v_365_79, 29, 0)
    _logic_or_picorv32_v_365_78_Y <= bits(_logic_or_picorv32_v_365_78, 0, 0)
    _logic_and_picorv32_v_364_77_Y <= bits(_logic_and_picorv32_v_364_77, 0, 0)
    _logic_or_picorv32_v_364_76_Y <= bits(_logic_or_picorv32_v_364_76, 0, 0)
    _logic_and_picorv32_v_364_75_Y <= bits(_logic_and_picorv32_v_364_75, 0, 0)
    _reduce_and_picorv32_v_364_74_Y <= bits(_reduce_and_picorv32_v_364_74, 0, 0)
    _logic_and_picorv32_v_364_73_Y <= bits(_logic_and_picorv32_v_364_73, 0, 0)
    _logic_not_picorv32_v_364_72_Y <= bits(_logic_not_picorv32_v_364_72, 0, 0)
    _logic_and_picorv32_v_364_71_Y <= bits(_logic_and_picorv32_v_364_71, 0, 0)
    _ternary_picorv32_v_364_70_Y <= bits(_ternary_picorv32_v_364_70, 0, 0)
    _logic_not_picorv32_v_364_69_Y <= bits(_logic_not_picorv32_v_364_69, 0, 0)
    _logic_and_picorv32_v_364_68_Y <= bits(_logic_and_picorv32_v_364_68, 0, 0)
    _logic_and_picorv32_v_363_67_Y <= bits(_logic_and_picorv32_v_363_67, 0, 0)
    _logic_or_picorv32_v_363_66_Y <= bits(_logic_or_picorv32_v_363_66, 0, 0)
    _logic_or_picorv32_v_363_65_Y <= bits(_logic_or_picorv32_v_363_65, 0, 0)
    _logic_and_picorv32_v_363_64_Y <= bits(_logic_and_picorv32_v_363_64, 0, 0)
    _logic_not_picorv32_v_363_63_Y <= bits(_logic_not_picorv32_v_363_63, 0, 0)
    _logic_not_picorv32_v_363_62_Y <= bits(_logic_not_picorv32_v_363_62, 0, 0)
    _logic_and_picorv32_v_362_61_Y <= bits(_logic_and_picorv32_v_362_61, 0, 0)
    _logic_and_picorv32_v_362_60_Y <= bits(_logic_and_picorv32_v_362_60, 0, 0)
    _logic_not_picorv32_v_362_59_Y <= bits(_logic_not_picorv32_v_362_59, 0, 0)
    _logic_and_picorv32_v_360_58_Y <= bits(_logic_and_picorv32_v_360_58, 0, 0)
    _logic_or_picorv32_v_360_57_Y <= bits(_logic_or_picorv32_v_360_57, 0, 0)
    _logic_and_picorv32_v_360_56_Y <= bits(_logic_and_picorv32_v_360_56, 0, 0)
    _logic_not_picorv32_v_360_55_Y <= bits(_logic_not_picorv32_v_360_55, 0, 0)
    _reduce_and_picorv32_v_360_54_Y <= bits(_reduce_and_picorv32_v_360_54, 0, 0)
    _logic_not_picorv32_v_360_53_Y <= bits(_logic_not_picorv32_v_360_53, 0, 0)
    _logic_and_picorv32_v_359_52_Y <= bits(_logic_and_picorv32_v_359_52, 0, 0)
    _logic_or_picorv32_v_359_51_Y <= bits(_logic_or_picorv32_v_359_51, 0, 0)
    _logic_and_picorv32_v_359_50_Y <= bits(_logic_and_picorv32_v_359_50, 0, 0)
    _reduce_and_picorv32_v_359_49_Y <= bits(_reduce_and_picorv32_v_359_49, 0, 0)
    _logic_and_picorv32_v_359_48_Y <= bits(_logic_and_picorv32_v_359_48, 0, 0)
    _logic_or_picorv32_v_359_47_Y <= bits(_logic_or_picorv32_v_359_47, 0, 0)
    _logic_or_picorv32_v_359_46_Y <= bits(_logic_or_picorv32_v_359_46, 0, 0)
    _logic_and_picorv32_v_359_45_Y <= bits(_logic_and_picorv32_v_359_45, 0, 0)
    _reduce_or_picorv32_v_359_44_Y <= bits(_reduce_or_picorv32_v_359_44, 0, 0)
    _reduce_or_picorv32_v_358_43_Y <= bits(_reduce_or_picorv32_v_358_43, 0, 0)
    _logic_or_picorv32_v_356_42_Y <= bits(_logic_or_picorv32_v_356_42, 0, 0)
    _logic_and_picorv32_v_356_41_Y <= bits(_logic_and_picorv32_v_356_41, 0, 0)
    _logic_and_picorv32_v_356_40_Y <= bits(_logic_and_picorv32_v_356_40, 0, 0)
    _logic_and_picorv32_v_355_39_Y <= bits(_logic_and_picorv32_v_355_39, 0, 0)
    _logic_not_picorv32_v_355_38_Y <= bits(_logic_not_picorv32_v_355_38, 0, 0)
    _logic_and_picorv32_v_355_37_Y <= bits(_logic_and_picorv32_v_355_37, 0, 0)
    _logic_and_picorv32_v_355_36_Y <= bits(_logic_and_picorv32_v_355_36, 0, 0)
    _logic_and_picorv32_v_346_35_Y <= bits(_logic_and_picorv32_v_346_35, 0, 0)
    _ternary_picorv32_v_346_34_Y <= bits(_ternary_picorv32_v_346_34, 0, 0)
    _logic_not_picorv32_v_346_33_Y <= bits(_logic_not_picorv32_v_346_33, 0, 0)
    _logic_and_picorv32_v_346_32_Y <= bits(_logic_and_picorv32_v_346_32, 0, 0)
    _logic_and_picorv32_v_345_31_Y <= bits(_logic_and_picorv32_v_345_31, 0, 0)
    _logic_not_picorv32_v_345_30_Y <= bits(_logic_not_picorv32_v_345_30, 0, 0)
    _logic_and_picorv32_v_345_29_Y <= bits(_logic_and_picorv32_v_345_29, 0, 0)
    _logic_and_picorv32_v_345_28_Y <= bits(_logic_and_picorv32_v_345_28, 0, 0)
    _logic_or_picorv32_v_345_27_Y <= bits(_logic_or_picorv32_v_345_27, 0, 0)
    _logic_and_picorv32_v_324_26_Y <= bits(_logic_and_picorv32_v_324_26, 0, 0)
    _logic_and_picorv32_v_320_25_Y <= bits(_logic_and_picorv32_v_320_25, 0, 0)
    _logic_and_picorv32_v_316_24_Y <= bits(_logic_and_picorv32_v_316_24, 0, 0)
    _reduce_or_picorv32_v_312_23_Y <= bits(_reduce_or_picorv32_v_312_23, 0, 0)
    _logic_and_picorv32_v_312_22_Y <= bits(_logic_and_picorv32_v_312_22, 0, 0)
    _logic_and_picorv32_v_312_21_Y <= bits(_logic_and_picorv32_v_312_21, 0, 0)
    _logic_and_picorv32_v_312_20_Y <= bits(_logic_and_picorv32_v_312_20, 0, 0)
    _reduce_or_picorv32_v_311_19_Y <= bits(_reduce_or_picorv32_v_311_19, 0, 0)
    _logic_and_picorv32_v_311_18_Y <= bits(_logic_and_picorv32_v_311_18, 0, 0)
    _logic_and_picorv32_v_311_17_Y <= bits(_logic_and_picorv32_v_311_17, 0, 0)
    _logic_and_picorv32_v_311_16_Y <= bits(_logic_and_picorv32_v_311_16, 0, 0)
    launch_next_insn <= bits(_247, 0, 0)
    is_rdcycle_rdcycleh_rdinstr_rdinstrh <= bits(_245, 0, 0)
    instr_trap <= bits(_244, 0, 0)
    mem_done <= bits(_238, 0, 0)
    mem_busy <= bits(_237, 0, 0)
    mem_la_use_prefetched_high_word <= bits(_235, 0, 0)
    mem_rdata_latched <= bits(_243, 31, 0)
    mem_rdata_latched_noshuffle <= bits(_242, 31, 0)
    mem_la_firstword_xfer <= bits(_234, 0, 0)
    mem_la_firstword <= bits(_233, 0, 0)
    mem_xfer <= bits(_236, 0, 0)
    pcpi_div_ready <= bits(_255, 0, 0)
    pcpi_div_wait <= bits(_254, 0, 0)
    pcpi_div_rd <= bits(_253, 31, 0)
    pcpi_div_wr <= bits(_252, 0, 0)
    pcpi_mul_ready <= bits(_251, 0, 0)
    pcpi_mul_wait <= bits(_250, 0, 0)
    pcpi_mul_rd <= bits(_249, 31, 0)
    pcpi_mul_wr <= bits(_248, 0, 0)
    next_pc <= bits(_246, 31, 0)
    dbg_mem_rdata <= bits(_230, 31, 0)
    dbg_mem_wstrb <= bits(_229, 3, 0)
    dbg_mem_wdata <= bits(_228, 31, 0)
    dbg_mem_addr <= bits(_227, 31, 0)
    dbg_mem_ready <= bits(_226, 0, 0)
    dbg_mem_instr <= bits(_225, 0, 0)
    dbg_mem_valid <= bits(_224, 0, 0)
    pcpi_rs2 <= bits(_232, 31, 0)
    pcpi_rs1 <= bits(_231, 31, 0)
    mem_la_addr <= bits(_241, 31, 0)
    mem_la_write <= bits(_239, 0, 0)
    mem_la_read <= bits(_240, 0, 0)
