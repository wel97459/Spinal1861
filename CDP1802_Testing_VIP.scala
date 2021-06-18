package Spinal1861

import spinal.core._
import spinal.core.sim._

import scala.util.control._
import Spinal1802._

class Sim_VIP extends Component {
  val io = new Bundle {
    val compVideo = out Bool
    val compSync  = out Bool

    val TPA  = out Bool

    val MWR  = out Bool
    val MRD  = out Bool

    val DataIn = in Bits(8 bits)
    val DataOut = out Bits(8 bits)

    val Addr16 = out Bits(13 bits)
  }

  val areaDiv10 = new SlowArea(70) { 
      val addressRemapped = Bits(13 bits)
      val remapper = Reg(Bool) init (False)

      //Setup CPU
      val Cpu = new CDP1802()
      Cpu.io.Wait_n := True
      Cpu.io.DMA_In_n := True

      val Pixie = new CDP1861_Div10()
      Pixie.io.DataIn := Cpu.io.DataOut
      Pixie.io.SC := Cpu.io.SC
      Pixie.io.TPA := Cpu.io.TPA
      Pixie.io.TPB := Cpu.io.TPB
      Pixie.io.Disp_On := (Cpu.io.N === 1 && Cpu.io.TPB && !Cpu.io.MWR)
      Pixie.io.Disp_Off := (Cpu.io.N === 1 && Cpu.io.TPB && !Cpu.io.MRD)
      Pixie.io.Reset_ := True

      io.compVideo := Pixie.io.Video
      io.compSync := Pixie.io.CompSync_

      Cpu.io.Interrupt_n := Pixie.io.INT
      Cpu.io.EF_n := Cat(B"111", Pixie.io.EFx)
      Cpu.io.DMA_Out_n := Pixie.io.DMAO
      Cpu.io.Clear_n := Pixie.io.Clear

      io.MRD := Cpu.io.MRD
      io.MWR := Cpu.io.MWR || (addressRemapped.asUInt > 0x1e00)

      io.DataOut := Cpu.io.DataOut
      Cpu.io.DataIn := io.DataIn
      io.TPA := Cpu.io.TPA

      when(Cpu.io.N === 0x4){
        remapper := True
      }elsewhen(!Pixie.io.Clear){
        remapper := False
      }


      when(!remapper || Cpu.io.Addr16.asUInt >= 0x8000){
        addressRemapped := 0x1e00 | Cpu.io.Addr16(8 downto 0).resize(13)
      }otherwise(addressRemapped := Cpu.io.Addr16(12 downto 0))

      io.Addr16 := addressRemapped
  }
}



object cpu1802_Testing_VIP {
  def main(args: Array[String]) {
    SimConfig.withWave.compile{
      val dut = new Sim_VIP
      dut.areaDiv10.Pixie.lineCounter.willOverflow.simPublic()
      dut
    }.doSim { dut =>
      //Fork a process to generate the reset and the clock on the dut
      dut.clockDomain.forkStimulus(period = 10)

      val ram = new Memory(0x1fff)
      ram.loadBin(0x00000, "./data/test_1861.bin")

      var c = 0;
      var t = false;
      val loop = new Breaks;
      loop.breakable {
        while (true) {
          dut.clockDomain.waitRisingEdge()

          if (dut.io.MWR.toBoolean == false) {
            ram.write(dut.io.Addr16.toInt, dut.io.DataOut.toInt.toByte)
          }

          if (dut.io.MRD.toBoolean == false) {
            dut.io.DataIn #= ram.read(dut.io.Addr16.toInt)
          }else{
            dut.io.DataIn #= 0x00
          }

          
          if(dut.areaDiv10.Pixie.lineCounter.willOverflow.toBoolean && !t)
          {
            c += 1
          }
          t = dut.areaDiv10.Pixie.lineCounter.willOverflow.toBoolean
          if(c == 1) {
              loop.break;
          }
        }
      }
    }
  }
}
