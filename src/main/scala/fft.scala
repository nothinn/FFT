import chisel3._
import chisel3.util._

class FFT(samples: Int, bitwidth: Int) extends Module{ //bitwidth is the incoming bitwidth

    val io = IO(new Bundle{

        val audio_ready = Input(Bool()) //Indicates that the audio memory is filled
        val running     = Output(Bool())//Indicates that the FFT is being calculated

        //Memory containing the audio samples
        val mem_audio_data = Input(UInt(bitwidth.W))
        val mem_audio_addr = Output(UInt(log2Ceil(samples).W))
        val mem_audio_en   = Output(Bool())

        //Memory containing the FFT output
        val mem_FFT_data = Input(UInt(bitwidth.W))
        val mem_FFT_addr = Output(UInt(log2Ceil(samples).W))
        val mem_FFT_en   = Output(Bool())
    })



    //val twiddler = new Module(Twiddle(samples, ))



}


class Twiddle(samples: Int, bitwidth: Int) extends Module{
    val io = IO(new Bundle{
        val addr    = Input(UInt((samples/2).W))
        val twiddle_real = Output(SInt(bitwidth.W))
        val twiddle_imag = Output(SInt(bitwidth.W))
        
    })

    //We can either use a lookup table or we can use a twiddle calculator.
    

    //Here we calculate a ROM

    val cos_array = for(k <- 0 until samples/2) yield math.round(math.cos(2*math.Pi*k/samples)*(1<<bitwidth-1))


    //If larger than what can be shown
    val cos = VecInit((cos_array.map(t => (if(t == (1<<bitwidth-1)) (t-1) else t).asSInt(bitwidth.W) )))


    io.twiddle_real := cos(io.addr)
    io.twiddle_imag := cos(io.addr-(samples/2).U)
    

}

class AGU(samples: Int) extends Module{ //Address Generation Unit
    val addr_bits = log2Ceil(samples)
    val io = IO(new Bundle{
        val start = Input(Bool()) //Treated as a pulsed signal.
        val done = Output(Bool())

        val twiddle_addr = Output(UInt((addr_bits/2).W))

        val a_addr = Output(UInt(addr_bits.W))
        val b_addr = Output(UInt(addr_bits.W))
    
        val a_s_addr = Output(UInt(addr_bits.W))
        val b_s_addr = Output(UInt(addr_bits.W))
    })

    val level = RegInit(0.U(log2Ceil(samples))) //There are log2 levels
    val index = RegInit(0.U((addr_bits/2).W)) //Index is for each level.

    index := index + 2.U;

    io.a_addr := Reverse(index)
    io.b_addr := Reverse(index + 1.U)

    io.twiddle_addr := index(addr_bits/2,1) && ((((1<<addr_bits/2)-1) << (addr_bits/2)) >> level).U


}

class BFU(bitwidth: Int) extends Module{ //ButterFly Unit
    val io = IO(new Bundle{
        val a_real = Input(UInt(bitwidth.W))
        val a_img = Input(UInt(bitwidth.W))

        val b_real = Input(UInt(bitwidth.W))
        val b_img = Input(UInt(bitwidth.W))

        val twiddle_real = Input(UInt(bitwidth.W))
        val twiddle_img = Input(UInt(bitwidth.W))

        val a_out_real = Output(UInt(bitwidth.W))
        val a_out_img = Output(UInt(bitwidth.W))
        
        val b_out_real = Output(UInt(bitwidth.W))
        val b_out_img = Output(UInt(bitwidth.W))
    })

    //Complex multiplication of B and twiddle factor
    //(a+bi)(c+di) = ac + adi + bci + bdi^2
    //             = ac + adi + bci - bd
    

    val a = WireDefault(io.b_real)
    val b = WireDefault(io.b_img)
    val c = WireDefault(io.twiddle_real)
    val d = WireDefault(io.twiddle_img)

    val real = WireDefault(a*c - b*d) //Gives bitwidth*2-1 bits
    val img  = WireDefault(a*d + b*c)


    val high_bit = bitwidth*2-1
    val low_bit = bitwidth-1


    io.a_out_real := io.a_real + real(high_bit,low_bit)
    io.a_out_img  := io.a_img  + img(high_bit,low_bit)
    
    io.b_out_real := io.a_real - real(high_bit,low_bit)
    io.b_out_img  := io.a_img  - img(high_bit,low_bit)
}