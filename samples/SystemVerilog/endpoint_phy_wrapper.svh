module endpoint_phy_wrapper
  (
   input clk_sys_i,
   input clk_ref_i,
   input clk_rx_i,
   input rst_n_i,

   IWishboneMaster.master src,
   IWishboneSlave.slave snk,
   IWishboneMaster.master sys,
   
   output [9:0] td_o,
   input [9:0] rd_i,

   output txn_o,
   output txp_o,

   input rxn_i,
   input rxp_i
   );

   wire rx_clock;
   
   parameter g_phy_type   = "GTP";   

   wire[15:0] gtx_data;
   wire [1:0]gtx_k;
   wire gtx_disparity;
   wire gtx_enc_error;
   wire [15:0] grx_data;
   wire grx_clk;
   wire [1:0]grx_k;
   wire grx_enc_error;
   wire [3:0] grx_bitslide;
   wire gtp_rst;
   wire tx_clock;
   
   generate
      if(g_phy_type == "TBI") begin

         assign rx_clock  = clk_ref_i;
         assign tx_clock  = clk_rx_i;
         
         
         wr_tbi_phy U_Phy 
           (
            .serdes_rst_i (gtp_rst),
            .serdes_loopen_i(1'b0),
            .serdes_prbsen_i(1'b0),
            .serdes_enable_i(1'b1), 
            .serdes_syncen_i(1'b1),

            .serdes_tx_data_i  (gtx_data[7:0]),
            .serdes_tx_k_i     (gtx_k[0]),
            .serdes_tx_disparity_o (gtx_disparity),
            .serdes_tx_enc_err_o   (gtx_enc_error),

            .serdes_rx_data_o      (grx_data[7:0]),
            .serdes_rx_k_o        (grx_k[0]),
            .serdes_rx_enc_err_o  (grx_enc_error),
            .serdes_rx_bitslide_o (grx_bitslide),


            .tbi_refclk_i (clk_ref_i),
            .tbi_rbclk_i  (clk_rx_i),

            .tbi_td_o     (td_o),
            .tbi_rd_i     (rd_i),
            .tbi_syncen_o (),
            .tbi_loopen_o (),
            .tbi_prbsen_o (),
            .tbi_enable_o ()
            );

      end else if (g_phy_type == "GTX") begin // if (g_phy_type == "TBI")
         wr_gtx_phy_virtex6
           #(
             .g_simulation(1)
             ) U_PHY 
             (
              .clk_ref_i(clk_ref_i),
              
              .tx_clk_o   (tx_clock),
              .tx_data_i (gtx_data),
              .tx_k_i (gtx_k),
              .tx_disparity_o (gtx_disparity),
              .tx_enc_err_o(gtx_enc_error),
              .rx_rbclk_o (rx_clock),
              .rx_data_o (grx_data),
              .rx_k_o  (grx_k),
              .rx_enc_err_o (grx_enc_error),
              .rx_bitslide_o  (),

              .rst_i    (!rst_n_i),
              .loopen_i (1'b0),

              .pad_txn_o (txn_o),
              .pad_txp_o (txp_o),

              .pad_rxn_i (rxn_i),
              .pad_rxp_i (rxp_i)
    );
                   
      end  else if (g_phy_type == "GTP") begin // if (g_phy_type == "TBI")
         assign #1 tx_clock  = clk_ref_i;
         
         wr_gtp_phy_spartan6
           #(
             .g_simulation(1)
             ) U_PHY 
             (
              .gtp_clk_i(clk_ref_i),
              .ch0_ref_clk_i(clk_ref_i),
              
              .ch0_tx_data_i (gtx_data[7:0]),
              .ch0_tx_k_i (gtx_k[0]),
              .ch0_tx_disparity_o (gtx_disparity),
              .ch0_tx_enc_err_o(gtx_enc_error),
              .ch0_rx_rbclk_o (rx_clock),
              .ch0_rx_data_o (grx_data[7:0]),
              .ch0_rx_k_o  (grx_k[0]),
              .ch0_rx_enc_err_o (grx_enc_error),
              .ch0_rx_bitslide_o  (),

              .ch0_rst_i    (!rst_n_i),
              .ch0_loopen_i (1'b0),

              .pad_txn0_o (txn_o),
              .pad_txp0_o (txp_o),

              .pad_rxn0_i (rxn_i),
              .pad_rxp0_i (rxp_i)
              );
         
      end // else: !if(g_phy_type == "TBI")
   endgenerate
   
   wr_endpoint
     #(
       .g_simulation          (1),
       .g_pcs_16bit(g_phy_type == "GTX" ? 1: 0),
       .g_rx_buffer_size (1024),
       .g_with_rx_buffer(0),
       .g_with_timestamper    (1),
       .g_with_dmtd           (0),
       .g_with_dpi_classifier (1),
       .g_with_vlans          (0),
       .g_with_rtu            (0)
       ) DUT (
              .clk_ref_i (clk_ref_i),
              .clk_sys_i (clk_sys_i),
              .clk_dmtd_i (clk_ref_i),
              .rst_n_i  (rst_n_i),
              .pps_csync_p1_i (1'b0),

              .phy_rst_o   (),
              .phy_loopen_o (),
              .phy_enable_o (),
              .phy_syncen_o (),

              .phy_ref_clk_i      (tx_clock),
              .phy_tx_data_o      (gtx_data),
              .phy_tx_k_o         (gtx_k),
              .phy_tx_disparity_i (gtx_disparity),
              .phy_tx_enc_err_i   (gtx_enc_error),

              .phy_rx_data_i     (grx_data),
              .phy_rx_clk_i      (rx_clock),
              .phy_rx_k_i        (grx_k),
              .phy_rx_enc_err_i  (grx_enc_error),
              .phy_rx_bitslide_i (5'b0),

              .src_dat_o   (snk.dat_i),
              .src_adr_o   (snk.adr),
              .src_sel_o   (snk.sel),
              .src_cyc_o   (snk.cyc),
              .src_stb_o   (snk.stb),
              .src_we_o    (snk.we),
              .src_stall_i (snk.stall),
              .src_ack_i   (snk.ack),
              .src_err_i(1'b0),

              .snk_dat_i   (src.dat_o[15:0]),
              .snk_adr_i   (src.adr[1:0]),
              .snk_sel_i   (src.sel[1:0]),
              .snk_cyc_i   (src.cyc),
              .snk_stb_i   (src.stb),
              .snk_we_i    (src.we),
              .snk_stall_o (src.stall),
              .snk_ack_o   (src.ack),
              .snk_err_o   (src.err),
              .snk_rty_o   (src.rty),

              .txtsu_ack_i (1'b1),

              .rtu_full_i (1'b0),
              .rtu_almost_full_i (1'b0),
              .rtu_rq_strobe_p1_o  (),
              .rtu_rq_smac_o  (),
              .rtu_rq_dmac_o (),
              .rtu_rq_vid_o  (),
              .rtu_rq_has_vid_o (),
              .rtu_rq_prio_o (),
              .rtu_rq_has_prio_o (),

              .wb_cyc_i(sys.cyc),
              .wb_stb_i (sys.stb),
              .wb_we_i (sys.we),
              .wb_sel_i(sys.sel),
              .wb_adr_i(sys.adr[7:0]),
              .wb_dat_i(sys.dat_o),
              .wb_dat_o(sys.dat_i),
              .wb_ack_o (sys.ack)
    );

endmodule // endpoint_phy_wrapper
