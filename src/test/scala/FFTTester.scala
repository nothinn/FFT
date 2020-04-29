import chisel3._
import chisel3.util._
import org.scalatest._

import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, WriteVcdAnnotation}


class FFTTestModule(bitwidth: Int, samples: Int, bp: Int) extends Module{
  
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


  val fft = Module(new FFT(samples,bitwidth, bp))

  val counter = Counter(6)

  //fft.io.stall := 0.B// ~counter.inc()
  fft.io.stall := ~counter.inc()

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

  when(fft.io.mem_audioA.en){
    when(fft.io.mem_audioA.write){
      memA.write(fft.io.mem_audioA.addr,fft.io.mem_audioA.data_out)
    }
  }
  when(fft.io.mem_audioB.en){
    when(fft.io.mem_audioB.write){
      memA.write(fft.io.mem_audioB.addr,fft.io.mem_audioB.data_out)
    }
  }


  when(fft.io.mem_FFTA.en){
    when(fft.io.mem_FFTA.write){
      memB.write(fft.io.mem_FFTA.addr,fft.io.mem_FFTA.data_out)
    }
  }
  when(fft.io.mem_FFTB.en){
    when(fft.io.mem_FFTB.write){
      memB.write(fft.io.mem_FFTB.addr,fft.io.mem_FFTB.data_out)
    }
  }

  when(fft.io.mem_audioA.en){
    fft.io.mem_audioA.data_in := memA(fft.io.mem_audioA.addr)
    fft.io.mem_audioB.data_in := memA(fft.io.mem_audioB.addr)
  }.otherwise{
    fft.io.mem_audioA.data_in := RegEnable(memA(fft.io.mem_audioA.addr),fft.io.mem_audioA.en)
    fft.io.mem_audioB.data_in := RegEnable(memA(fft.io.mem_audioB.addr),fft.io.mem_audioB.en)
  }

  when(fft.io.mem_FFTA.en){
    fft.io.mem_FFTA.data_in := memB(fft.io.mem_FFTA.addr)
    fft.io.mem_FFTB.data_in := memB(fft.io.mem_FFTB.addr)
  }.otherwise{
    fft.io.mem_FFTA.data_in := RegEnable(memB(fft.io.mem_FFTA.addr),fft.io.mem_FFTA.en)
    fft.io.mem_FFTB.data_in := RegEnable(memB(fft.io.mem_FFTB.addr),fft.io.mem_FFTB.en)
  }
}

class FFTTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "FFTTester"

  it should "Calculate an 8 point FFT" in {

    val bp = 11 //Binary point, number of fractional bits.
    val one = (1<<bp)-1
    val input = Seq(one,one,0,0,one,one,0,0,one,one,0,0,one,one,0,one)
    val samples = input.length
    val bitwidth = bp + 1 + log2Ceil(samples) //Bitgrowth = number of fractional bits + 1 for sign + number of levels
    test(new FFTTestModule(bitwidth,samples, bp)).withAnnotations(Seq(WriteVcdAnnotation,VerilatorBackendAnnotation)) { c => 

      c.io.write_test.poke(1.B)
      //Fill out memory

      for(i <- 0 until samples/2){
        c.io.write_addr_a.poke((i*2).U)
        c.io.write_addr_b.poke((i*2+1).U)
        
        c.io.write_data_a.poke(input(i*2).U)
        c.io.write_data_b.poke(input(i*2+1).U)
        
        c.clock.step(1)
      }

      c.io.write_test.poke(0.B)

      c.clock.step(10)

      //Test that memory has been filled correctly
      for(i <- 0 until samples/2){
        c.io.write_addr_a.poke((i*2).U)
        c.io.write_addr_b.poke((i*2+1).U)
        
        c.clock.step(1)

        c.io.read_data_a.expect(input(i*2).U)
        c.io.read_data_b.expect(input(i*2+1).U)
      }


      c.clock.step(10)
      c.io.start.poke(1.B)

      c.clock.step(4)
      c.io.start.poke(0.B)
      while(c.io.running.peek().litToBoolean == true){
        c.clock.step(1)
      }
      c.clock.step(1)

      val finalMemory = c.fft.finalMemory

      for(index <- 0 until samples/2){
        c.io.write_addr_a.poke((index*2).U)
        c.io.write_addr_b.poke((index*2+1).U)

        c.io.readB_addr_a.poke((index*2).U)
        c.io.readB_addr_b.poke((index*2+1).U)
        
        c.clock.step(1)

        val out0 = if(c.fft.finalMemory == 0) c.io.read_data_a.peek() else c.io.readB_data_a.peek()
        val out1 = if(c.fft.finalMemory == 0) c.io.read_data_b.peek() else c.io.readB_data_b.peek()

        //Float 2 fixed
        def int2float(int: BigInt, bitwidth: Int, bp: Int) = {

          var real_int = int & ((1<<bitwidth)-1)
          var imag_int = (int >> bitwidth) & ((1<<bitwidth)-1)


          if (((real_int>>(bitwidth-1))&0x1) == 1){ //Sign bit
            real_int = ~real_int
            real_int += 1
            real_int = real_int & ((1<<bitwidth)-1)
            real_int *= -1
          }

          if (((imag_int>>(bitwidth-1))&0x1) == 1){
            imag_int = ~imag_int //Twos complent *-1
            imag_int += 1
            imag_int = imag_int & ((1<<bitwidth)-1) //Only keep lower bits
            imag_int *= -1 //Make bigint negative
          }
          
          val real = real_int.toDouble / ((1<<(bp)).toDouble)
          val imag = imag_int.toDouble / ((1<<(bp)).toDouble)

          (real, imag)
        } 

        val (real_0, imag_0) = int2float(out0.litValue(), bitwidth, bp)
        val (real_1, imag_1) = int2float(out1.litValue(), bitwidth, bp)
        
        println("Index %d: %f; %fj".format(index*2, real_0, imag_0))
        println("Index %d: %f; %fj".format(index*2+1, real_1, imag_1))

      }
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

      c.io.a_addr_read.expect(0.U)
      c.io.b_addr_read.expect(4.U)
      c.clock.step()

      c.io.a_addr_read.expect(2.U)
      c.io.b_addr_read.expect(6.U)
      c.clock.step()

      c.io.a_addr_read.expect(1.U)
      c.io.b_addr_read.expect(5.U)
      c.clock.step()

      c.io.a_addr_read.expect(3.U)
      c.io.b_addr_read.expect(7.U)
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