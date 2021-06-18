package Spinal1802

import spinal.core._
import spinal.core.sim._
import spinal.lib._

import scala.util.control.Breaks

//Line 14 cpu cycles, 6 opcode and 8 DMA. 14 x 8 = 112
//Lines 262
//There are 29 CPU cycles after going to interrupt

class CDP1861 extends Component{
    val io = new Bundle {
        val Reset_ = in Bool
        val Disp_On = in Bool
        val Disp_Off = in Bool
        val TPA = in Bool
        val TPB = in Bool
        val SC = in Bits (2 bit)
        val DataIn = in Bits (8 bit)

        val Clear = out Bool
        val INT = out Bool
        val DMAO = out Bool
        val EFx = out Bool

        val Video = out Bool
        val CompSync_ = out Bool
        val Locked = out Bool

        val VSync = out Bool
        val HSync = out Bool
    }

    io.Clear := RegNext(io.Reset_)

    // configure the clock domain
    val InvertedClockDomain = ClockDomain(
        clock  = clockDomain.clock,
        reset  = clockDomain.reset,
        config = ClockDomainConfig(
            clockEdge        = FALLING,
            resetKind        = ASYNC,
            resetActiveLevel = HIGH
        )
    )

    //Line and Machine Cycle counter
    val lineCounter = Counter(263)
    val MCycleCounter = Counter(28)
    val syncCounter = Counter(12)

    when(!io.Reset_){
        lineCounter.clear()
        syncCounter.clear()
        MCycleCounter.clear()
    }

    when(syncCounter =/= 0 || (MCycleCounter === 26 && lineCounter === 0 && io.TPA && io.SC =/= 0)){
        syncCounter.increment()
    }

    io.Locked := syncCounter === 0

    when((io.TPB || io.TPA) && syncCounter === 0) {
        MCycleCounter.increment()
    }

    when(MCycleCounter.willOverflow) {
        lineCounter.increment()
    }

    //Display On flag for controlling the DMA and Interrupt output
    val DisplayOn = Reg(Bool) init(False)
    when(io.Disp_On.rise()) {
        DisplayOn := True
    } elsewhen (io.Disp_Off.rise() || !io.Reset_) {
        DisplayOn := False
    }

    //Flag Logic
    when((lineCounter === 78 || lineCounter === 79) && DisplayOn){
        io.INT := False
    }otherwise(io.INT := True)

    when((lineCounter >= 76 && lineCounter <= 79) || (lineCounter >= 205 && lineCounter <= 207)){
        io.EFx := False
    }otherwise(io.EFx := True)

    val InvertedArea = new ClockingArea(InvertedClockDomain) {

        //Sync Timing
        val VSync = Bool()
        val HSync = Bool()

        io.CompSync_ := !(HSync ^ VSync)

        //VSync Logic
        when(lineCounter >= 16) {
            VSync := True
        } otherwise (VSync := False)

        //HSync Logic
        when(MCycleCounter >= 3 | (MCycleCounter === 2 && io.TPA)) {
            HSync := True
        } otherwise (HSync := False)

        //DMA Logic
        when(lineCounter >= 80 && lineCounter <= 207 && ((MCycleCounter === 2 && io.TPA) || MCycleCounter >= 3 && MCycleCounter <= 19) && DisplayOn){
            io.DMAO := False
        }otherwise(io.DMAO := True)

        //Video shift Register
        val VideoShiftReg = Reg(Bits(8 bit)) init (0)
        when(io.SC === 2 && io.TPB) {
            VideoShiftReg := io.DataIn
        } elsewhen (!io.Reset_) {
            VideoShiftReg := 0x00
        } otherwise (VideoShiftReg := VideoShiftReg |<< 1)

        io.Video := VideoShiftReg.msb

        io.VSync := VSync
        io.HSync := !HSync
    }
}

//Define a custom SpinalHDL configuration with synchronous reset instead of the default asynchronous one. This configuration can be resued everywhere
object CDP1861SpinalConfig extends SpinalConfig(
    targetDirectory = ".",
    defaultConfigForClockDomains = ClockDomainConfig(resetKind = SYNC)
)

//Generate the MyTopLevel's Verilog using the above custom configuration.
object CDP1861Gen {
    def main(args: Array[String]) {
        CDP1861SpinalConfig.generateVerilog(new CDP1861).printPruned
    }
}

class CDP1861_Div10 extends Component{
    val io = new Bundle {
        val Reset_ = in Bool
        val Disp_On = in Bool
        val Disp_Off = in Bool
        val TPA = in Bool
        val TPB = in Bool
        val SC = in Bits (2 bit)
        val DataIn = in Bits (8 bit)

        val Clear = out Bool
        val INT = out Bool
        val DMAO = out Bool
        val EFx = out Bool

        val Video = out Bool
        val CompSync_ = out Bool
        val Locked = out Bool

        val VSync = out Bool
        val HSync = out Bool
    }

    io.Clear := RegNext(io.Reset_)

    // configure the clock domain
    val InvertedClockDomain = ClockDomain(
        clock  = clockDomain.clock,
        reset  = clockDomain.reset,
        config = ClockDomainConfig(
            clockEdge        = FALLING,
            resetKind        = ASYNC,
            resetActiveLevel = HIGH
        )
    )

    //Line and Machine Cycle counter
    val lineCounter = Counter(263)
    val MCycleCounter = Counter(28)
    val syncCounter = Counter(12)

    when(!io.Reset_){
        lineCounter.clear()
        syncCounter.clear()
        MCycleCounter.clear()

    }

    when(syncCounter =/= 0 || (MCycleCounter === 26 && lineCounter === 0 && io.TPA && io.SC =/= 0)){
        syncCounter.increment()
    }

    io.Locked := syncCounter === 0

    when((io.TPB || io.TPA) && syncCounter === 0) {
        MCycleCounter.increment()
    }

    when(MCycleCounter.willOverflow) {
        lineCounter.increment()
    }

    //Display On flag for controlling the DMA and Interrupt output
    val DisplayOn = Reg(Bool) init(False)
    when(io.Disp_On.rise()) {
        DisplayOn := True
    } elsewhen (io.Disp_Off.rise() || !io.Reset_) {
        DisplayOn := False
    }

    //Flag Logic
    when((lineCounter === 78 || lineCounter === 79) && DisplayOn){
        io.INT := False
    }otherwise(io.INT := True)

    when((lineCounter >= 76 && lineCounter <= 79) || (lineCounter >= 205 && lineCounter <= 207)){
        io.EFx := False
    }otherwise(io.EFx := True)

    val InvertedArea = new ClockingArea(InvertedClockDomain) {

        //Sync Timing
        val VSync = Bool()
        val HSync = Bool()

        io.CompSync_ := !(HSync ^ VSync)

        //VSync Logic
        when(lineCounter >= 16) {
            VSync := True
        } otherwise (VSync := False)

        //HSync Logic
        when(MCycleCounter >= 3 | (MCycleCounter === 2 && io.TPA)) {
            HSync := True
        } otherwise (HSync := False)

        //DMA Logic
        when(lineCounter >= 80 && lineCounter <= 207 && ((MCycleCounter === 2 && io.TPA) || MCycleCounter >= 3 && MCycleCounter <= 19) && DisplayOn){
            io.DMAO := False
        }otherwise(io.DMAO := True)

        val areaDiv10 = new SlowArea(70) {
            //Video shift Register
            val VideoShiftReg = Reg(Bits(8 bit)) init (0)
            when(io.SC === 2 && io.TPB) {
                VideoShiftReg := io.DataIn
            } elsewhen (!io.Reset_) {
                VideoShiftReg := 0x00
            } otherwise (VideoShiftReg := VideoShiftReg |<< 1)

            io.Video := VideoShiftReg.msb
        }

        io.VSync := VSync
        io.HSync := !HSync
    }
}

class CosmacVIP extends Component {
    val io = new Bundle {
        val Reset_ = in Bool
        val Addr = out Bits(16 bit)

        val MRD = out Bool
        val MWR = out Bool
        val DataIn = in Bits(8 bit)
        val DataOut = out Bits(8 bit)
    }

    val Cpu = new CDP1802()
    Cpu.io.DataIn := io.DataIn
    io.DataOut := Cpu.io.DataOut
    io.MWR := Cpu.io.MWR
    io.MRD := Cpu.io.MRD
    io.Addr := Cpu.io.Addr16
    Cpu.io.Wait_n := True
    Cpu.io.DMA_In_n := True

    val Pixie = new CDP1861()
    Pixie.io.DataIn := Cpu.io.DataOut
    Pixie.io.SC := Cpu.io.SC
    Pixie.io.TPA := Cpu.io.TPA
    Pixie.io.TPB := Cpu.io.TPB
    Pixie.io.Disp_On := (Cpu.io.N === 1 && Cpu.io.TPB && !Cpu.io.MWR)
    Pixie.io.Disp_Off := (Cpu.io.N === 1 && Cpu.io.TPB && !Cpu.io.MRD)
    Pixie.io.Reset_ := io.Reset_

    Cpu.io.Interrupt_n := Pixie.io.INT
    Cpu.io.EF_n := Cat(B"111", Pixie.io.EFx)
    Cpu.io.DMA_Out_n := Pixie.io.DMAO
    Cpu.io.Clear_n := Pixie.io.Clear

}

object VIP_Test {
    def main(args: Array[String]) {
        SimConfig.withWave.compile{
            val dut = new CosmacVIP()
            dut.Pixie.lineCounter.willOverflow.simPublic()
            dut
        }.doSim { dut =>
            val ram = new Memory(log2Up(0x1fff))
            ram.loadBin(0x00000, "./data/test_1861.bin")
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)
            dut.io.Reset_ #= false
            dut.clockDomain.waitRisingEdge()
            dut.clockDomain.waitRisingEdge()
            dut.clockDomain.waitRisingEdge()
            dut.clockDomain.waitRisingEdge()
            dut.io.Reset_ #= true
            var c = 0;

            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    if (dut.io.MRD.toBoolean == false) {
                        dut.io.DataIn #= ram.read(dut.io.Addr.toInt)
                    } else {
                        dut.io.DataIn #= 0x00
                    }

                    if (dut.io.MWR.toBoolean == false) {
                        ram.write(dut.io.Addr.toInt, dut.io.DataOut.toInt.toByte)
                    }

                    if(dut.Pixie.lineCounter.willOverflow.toBoolean){
                        if(c==1) loop.break()
                        c+=1
                    }
                    dut.clockDomain.waitRisingEdge()
                }
            }
        }
    }
}


object CDP1861_Test {
    def main(args: Array[String]) {
        SimConfig.withWave.compile{
            val dut = new CDP1861()
            dut.lineCounter.willOverflow.simPublic()
            dut
        }.doSim { dut =>
            //Fork a process to generate the reset and the clock on the dut
            dut.clockDomain.forkStimulus(period = 10)

            dut.clockDomain.waitRisingEdge()
            dut.io.TPA #= false
            dut.io.TPB #= false
            dut.io.Disp_On #= false
            dut.io.SC #= 0
            dut.io.DataIn #= 0x00
            var c = 0;
            var cc = 0;
            var ccc = 0;
            var t = 0
            val loop = new Breaks;
            loop.breakable {
                while (true) {
                    if(dut.lineCounter.willOverflow.toBoolean){
                        if(cc == 1) loop.break;
                        cc+=1
                    }
                    if(c == 0)
                    {
                        if(dut.io.DMAO.toBoolean == false && (t == 1 || t == 2)){
                            t = 0x2
                            dut.io.DataIn #= 0x00
                        }else {
                            if(t == 0){
                                t = 0x1
                            }else{
                                t = 0x0
                            }
                        }
                    }
                    dut.io.SC #= t & 0x3

                    if(c == 2){ dut.io.TPA #= true } else { dut.io.TPA #= false }
                    if(c == 7){ dut.io.TPB #= true } else { dut.io.TPB #= false }
                    if(ccc == 500){dut.io.Disp_On #= true} else {dut.io.Disp_On #= false}
                    c+=1
                    ccc+=1
                    if(c == 8) c=0
                    dut.clockDomain.waitRisingEdge()
                }
            }
        }
    }
}
