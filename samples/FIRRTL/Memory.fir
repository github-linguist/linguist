FIRRTL version 4.0.0
circuit Memory:
  public module Memory:
    input clock: Clock
    input rAddr: UInt<4>
    input rEn: UInt<1>
    output rData: UInt<8>
    input wMask: UInt<1>
    input wData: UInt<8>

    mem tbMemoryKind1:
      data-type => UInt<1>
      depth => 16
      reader => r
      writer => w
      read-latency => 1
      write-latency => 1
      read-under-write => undefined

    tbMemoryKind1.r.clk <= clock
    tbMemoryKind1.r.en <= rEn
    tbMemoryKind1.r.addr <= rAddr
    rData <= tbMemoryKind1.r.data
    tbMemoryKind1.w.clk <= clock
    tbMemoryKind1.w.en <= rEn
    tbMemoryKind1.w.addr <= rAddr
    tbMemoryKind1.w.mask <= wMask
    tbMemoryKind1.w.data <= wData
