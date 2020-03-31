import chisel3._
import chisel3.util._
import org.scalatest._

import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}


class FFTTestModule(bitwidth: Int, samples: Int) extends Module{
  
  val io = IO(new Bundle{

    //val audio_ready = Input(Bool()) //Indicates that the audio memory is filled
    val running     = Output(Bool())//Indicates that the FFT is being calculated
    val start       = Input(Bool())


    val write_test  = Input(Bool())
    val write_addr_a= Input(UInt(log2Ceil(samples).W))
    val write_addr_b= Input(UInt(log2Ceil(samples).W))
    
    val write_data_a= Input(UInt((bitwidth*2).W))
    val write_data_b= Input(UInt((bitwidth*2).W))

    val read_data_a = Output(UInt((bitwidth*2).W))
    val read_data_b = Output(UInt((bitwidth*2).W))


    val readB_addr_a = Input(UInt(log2Ceil(samples).W))
    val readB_addr_b = Input(UInt(log2Ceil(samples).W))
  
    val readB_data_a = Output(UInt((bitwidth*2).W))
    val readB_data_b = Output(UInt((bitwidth*2).W))
  })


  val fft = Module(new FFT(samples,bitwidth))

  io.running := fft.io.running
  //fft.io.audio_ready := io.audio_ready
  fft.io.start := io.start
  

  val memA = SyncReadMem(samples, UInt((bitwidth*2).W))
  val memB = SyncReadMem(samples, UInt((bitwidth*2).W))

  io.read_data_a := memA(io.write_addr_a)
  io.read_data_b := memA(io.write_addr_b)
  
  io.readB_data_a := memB(io.readB_addr_a)
  io.readB_data_b := memB(io.readB_addr_b)
  



  when(io.write_test){
    memA.write(io.write_addr_a, io.write_data_a)
    memA.write(io.write_addr_b, io.write_data_b)
  }

  when(fft.io.mem_audio_write){
    memA.write(fft.io.mem_audioA_addr,fft.io.mem_audioA_dataOut)
    memA.write(fft.io.mem_audioB_addr,fft.io.mem_audioB_dataOut)
  }

  when(fft.io.mem_FFT_write){
    memB.write(fft.io.mem_FFTA_addr,fft.io.mem_FFTA_dataOut)
    memB.write(fft.io.mem_FFTB_addr,fft.io.mem_FFTB_dataOut)
  }


  fft.io.mem_audioA_dataIn := memA(fft.io.mem_audioA_addr)
  fft.io.mem_audioB_dataIn := memA(fft.io.mem_audioB_addr)

  fft.io.mem_FFTA_dataIn := memB(fft.io.mem_FFTA_addr)
  fft.io.mem_FFTB_dataIn := memB(fft.io.mem_FFTB_addr)

}

class FFTTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "FFTTester"

  it should "Calculate an 8 point FFT" in {
    test(new FFTTestModule(16,8)).withAnnotations(Seq(WriteVcdAnnotation,VerilatorBackendAnnotation)) { c => 


      c.io.write_test.poke(1.B)
      //Fill out memory
      val input = Seq((1<<10)-1,0,(1<<10)-1,0,(1<<10)-1,0,0,0)
      for(i <- 0 until 4){
        c.io.write_addr_a.poke((i*2).U)
        c.io.write_addr_b.poke((i*2+1).U)
        
        c.io.write_data_a.poke(input(i*2).U)
        c.io.write_data_b.poke(input(i*2+1).U)
        
        c.clock.step(1)
      }

      c.io.write_test.poke(0.B)

      c.clock.step(10)

      //Test that memory has been filled correctly
      for(i <- 0 until 4){
        c.io.write_addr_a.poke((i*2).U)
        c.io.write_addr_b.poke((i*2+1).U)
        
        c.clock.step(1)

        c.io.read_data_a.expect(input(i*2).U)
        c.io.read_data_b.expect(input(i*2+1).U)
      }


      c.clock.step(10)
      c.io.start.poke(1.B)

      c.clock.step(2)
      c.io.start.poke(0.B)
      while(c.io.running.peek().litToBoolean == true){
        c.clock.step(1)
      }
      c.clock.step(1)
    }
  }
}



class FFTComponentTest extends FlatSpec with ChiselScalatestTester with Matchers {
  

  behavior of "AGUTester"
  it should "Generate correct memory addressing for 8 samples" in {
    test(new AGU(8)).withAnnotations(Seq(WriteVcdAnnotation)) { c =>

      val bits = log2Ceil(8)

      c.io.start.poke(0.B)
      while(c.io.done.peek == 0){
        c.clock.step()
      }

      c.io.start.poke(1.B)
      c.clock.step()
      c.io.start.poke(0.B)

      c.io.a_addr.expect(0.U)
      c.io.b_addr.expect(4.U)
      c.clock.step()

      c.io.a_addr.expect(2.U)
      c.io.b_addr.expect(6.U)
      c.clock.step()

      c.io.a_addr.expect(1.U)
      c.io.b_addr.expect(5.U)
      c.clock.step()

      c.io.a_addr.expect(3.U)
      c.io.b_addr.expect(7.U)
      c.clock.step()
    }
  }

  //Float 2 fixed
  def f2f(floatNumber: Double, bitwidth: Int, bp: Int, add: Int = 0) : Int ={
    val multiplied = floatNumber*(1<<bp)
    var integer = math.round(multiplied)


    val result = (integer+add).toInt

    return result
  } 
    


  
  behavior of "BFUTester"
  it should "Calculate some different factors" in {
    test(new BFU(16,10)).withAnnotations(Seq(WriteVcdAnnotation)){ c =>
    
      val a = f2f(1.0,16,10,-1)
      val b = f2f(1.0,16,10,-1)
      val twiddle = f2f(1.0,16,10,-1)

      //ToDo implement some different tests

      c.io.a_real.poke( a.S)
      c.io.a_imag.poke( 0.S) 

      c.io.b_real.poke( b.S)
      c.io.b_imag.poke( 0.S)

      c.io.twiddle_real.poke(  twiddle.S)
      c.io.twiddle_imag.poke(  0.S)
      

      c.clock.step()

      c.io.a_out_real.expect((((a<<10)+b*twiddle)>>10).S)

      c.io.b_out_real.expect(1.S)
      

    }
  }
  
}