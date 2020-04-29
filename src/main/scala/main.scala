import chisel3._
import chisel3.util._

// generate Verilog
object FFTMain extends App {

  chisel3.Driver.execute(args, () => new FFT(128,16,8))


}