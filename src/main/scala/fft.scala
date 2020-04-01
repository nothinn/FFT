import chisel3._
import chisel3.util._



class FFT_IO(val bitwidth: Int, val samples: Int) extends Bundle{
        //val audio_ready = Input(Bool()) //Indicates that the audio memory is filled
        val running     = Output(Bool())//Indicates that the FFT is being calculated

        val start       = Input(Bool())

        //Memory containing the audio samples
        val mem_audioA_dataIn = Input(UInt((bitwidth*2).W))
        val mem_audioA_dataOut = Output(UInt((bitwidth*2).W))
        val mem_audioA_addr = Output(UInt(log2Ceil(samples).W))

        val mem_audioB_dataIn = Input(UInt((bitwidth*2).W))
        val mem_audioB_dataOut = Output(UInt((bitwidth*2).W))
        val mem_audioB_addr = Output(UInt(log2Ceil(samples).W))

        val mem_audioA_en   = Output(Bool())
        val mem_audioB_en   = Output(Bool())

        val mem_audio_write = Output(Bool()) //Write for both ports
        

        //Memory containing the FFT output
        val mem_FFTA_dataIn = Input(UInt((bitwidth*2).W))
        val mem_FFTA_dataOut = Output(UInt((bitwidth*2).W))
        val mem_FFTA_addr = Output(UInt(log2Ceil(samples).W))
        val mem_FFTA_en   = Output(Bool())

        val mem_FFTB_dataIn = Input(UInt((bitwidth*2).W))
        val mem_FFTB_dataOut = Output(UInt((bitwidth*2).W))
        val mem_FFTB_addr = Output(UInt(log2Ceil(samples).W))
        val mem_FFTB_en   = Output(Bool())

        val mem_FFT_write = Output(Bool()) //Write for both ports
}

class FFT(samples: Int, bitwidth: Int, bp: Int) extends Module{ //bitwidth is the incoming bitwidth

    val io = IO(new FFT_IO(bitwidth,samples))

    val agu = Module(new AGU(samples))
    
    //If even, result is in same memory as samples were stored.
    //Else, it is in the opposite memory.
    val finalMemory = log2Ceil(samples) % 2 

    io.running := ~agu.io.done | RegNext(~agu.io.done) //Show running for one more cycle

    //Hard applied
    io.mem_audioA_en := 1.B
    io.mem_audioB_en := 1.B

    io.mem_FFTA_en := 1.B
    io.mem_FFTB_en := 1.B

    
 
    

    agu.io.start := io.start

    val mem_chooser = RegInit(0.B)

    when(agu.io.switch){
        mem_chooser := ~mem_chooser
    }

    //When switch is high, mem_chooser is delayed one cycle. Hence the MUX.
    io.mem_audioA_addr := Mux(RegNext(mem_chooser),RegNext(agu.io.a_addr_write),agu.io.a_addr_read)
    io.mem_audioB_addr := Mux(RegNext(mem_chooser),RegNext(agu.io.b_addr_write),agu.io.b_addr_read)
    
    io.mem_FFTA_addr := Mux(RegNext(mem_chooser),agu.io.a_addr_read,RegNext(agu.io.a_addr_write))
    io.mem_FFTB_addr := Mux(RegNext(mem_chooser),agu.io.b_addr_read,RegNext(agu.io.b_addr_write))
    

    val twiddle = Module(new Twiddle(samples,bp+1)) //bp+1 to include sign

    twiddle.io.addr := RegNext(agu.io.twiddle_addr)

    val bfu = Module(new BFU(bitwidth,bp))

    //Real part is lower bits, while imaginary part is upper bits.
    bfu.io.a_real := Mux(RegNext(RegNext(~mem_chooser)), io.mem_audioA_dataIn(bitwidth-1,0).asSInt, io.mem_FFTA_dataIn(bitwidth-1,0).asSInt)
    bfu.io.a_imag := Mux(RegNext(RegNext(~mem_chooser)), io.mem_audioA_dataIn(bitwidth*2-1,bitwidth).asSInt, io.mem_FFTA_dataIn(bitwidth*2-1,bitwidth).asSInt)
    

    bfu.io.b_real := Mux(RegNext(RegNext(~mem_chooser)), io.mem_audioB_dataIn(bitwidth-1,0).asSInt, io.mem_FFTB_dataIn(bitwidth-1,0).asSInt)
    bfu.io.b_imag := Mux(RegNext(RegNext(~mem_chooser)), io.mem_audioB_dataIn(bitwidth*2-1,bitwidth).asSInt, io.mem_FFTB_dataIn(bitwidth*2-1,bitwidth).asSInt)
    

    bfu.io.twiddle_real := twiddle.io.twiddle_real
    bfu.io.twiddle_imag := twiddle.io.twiddle_imag

    io.mem_audioA_dataOut := bfu.io.a_out_imag ## bfu.io.a_out_real
    io.mem_audioB_dataOut := bfu.io.b_out_imag ## bfu.io.b_out_real

    io.mem_FFTA_dataOut := bfu.io.a_out_imag ## bfu.io.a_out_real
    io.mem_FFTB_dataOut := bfu.io.b_out_imag ## bfu.io.b_out_real


    io.mem_audio_write := RegNext(mem_chooser && (~agu.io.done)) & RegNext(RegNext(~agu.io.skip))
    io.mem_FFT_write :=   RegNext(~mem_chooser && (~agu.io.done)) & RegNext(RegNext(~agu.io.skip))

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
    val sin_array = for(k <- 0 until samples/2) yield math.round(-math.sin(2*math.Pi*k/samples)*(1<<bitwidth-1))

    println(cos_array)
    println(sin_array)

    //If larger than what can be shown
    val cos = VecInit((cos_array.map(t => (if(t == (1<<bitwidth-1)) (t-1) else t).asSInt(bitwidth.W) )))
    val sin = VecInit((sin_array.map(t => (if(t == (1<<bitwidth-1)) (t-1) else t).asSInt(bitwidth.W) )))


    io.twiddle_real := cos(io.addr)
    io.twiddle_imag := sin(io.addr)
}

class AGU(samples: Int) extends Module{ //Address Generation Unit
    val addr_bits = log2Ceil(samples)
    val io = IO(new Bundle{
        val start = Input(Bool()) //Treated as a pulsed signal.
        val done = Output(Bool())

        val skip = Output(Bool())

        val switch = Output(Bool()) //tick that indicates that memories needs to be swapped

        val twiddle_addr = Output(UInt((addr_bits-1).W))

        val a_addr_write = Output(UInt(addr_bits.W))
        val b_addr_write = Output(UInt(addr_bits.W))

        val a_addr_read = Output(UInt(addr_bits.W))
        val b_addr_read = Output(UInt(addr_bits.W))

        
    })

    val level = RegInit(0.U(log2Ceil(log2Ceil(samples)).W)) //There are log2 levels
    val index = RegInit(0.U((addr_bits).W)) //Index is for each level.


    val index0_reversed = WireDefault(Reverse(index))
    val index1_reversed = WireDefault(Reverse(index+1.U))


    val index0_rotated = Wire(Vec(addr_bits, Bool()))
    val index1_rotated = Wire(Vec(addr_bits, Bool()))

    val running = RegInit(0.B)

    io.done := ~running

    io.skip := 0.B

    val skipped = RegNext(io.skip)

    when(io.start){
        running := 1.B
    }

    io.switch := 0.B //Default value
    when(running){
        when(~skipped){
            index := index + 2.U;
        }

        when(index === (samples-2).U ){ //When counter resets, except for the first occurence.

            when(level === (log2Ceil(samples)-1).U){ //Done
                level := 0.U

                running := 0.B


            }.otherwise{
                level := level + 1.U
                io.switch := 1.B
                io.skip := 1.B
            }
        }
    }

    val testWire = WireDefault(0.U)
    dontTouch(testWire)
    //Bit rotation
    for(i <- 0 until addr_bits){
        index0_rotated(i) := 0.B //Default value, not used.
        index1_rotated(i) := 0.B

        for(j <- 0 until log2Ceil(samples)){
            when(j.U === level){
                testWire := j.U
                
                index0_rotated(i) := (index)(((i - j + log2Ceil(samples)) % log2Ceil(samples) ))
                index1_rotated(i) := (index+1.U)(((i - j + log2Ceil(samples)) % log2Ceil(samples) ))
            }
        }
    }

    io.a_addr_read := Mux(level =/= 0.U,index0_rotated.asUInt,Reverse(index))
    io.b_addr_read := Mux(level =/= 0.U,index1_rotated.asUInt,Reverse(index+1.U))

    io.a_addr_write := index0_rotated.asUInt
    io.b_addr_write := index1_rotated.asUInt
    

    // For 32 samples, 5 bits, this is (0xFFF0 >> level) & (index/2)
    val tmp0 = WireDefault((((1<<(addr_bits-1))-1) << (addr_bits-1)).U)
    dontTouch(tmp0)
    val tmp1 = WireDefault((tmp0 >> level))
    val tmp2 = WireDefault(index(addr_bits-1,1))
    val tmp3 = WireDefault(tmp1 & tmp2)
    io.twiddle_addr := tmp3
    
    
}

class BFU(bitwidth: Int, bp : Int) extends Module{ //ButterFly Unit, bp = Binary points. Number of fractional bits.
    val io = IO(new Bundle{
        val a_real = Input(SInt(bitwidth.W))
        val a_imag = Input(SInt(bitwidth.W))

        val b_real = Input(SInt(bitwidth.W))
        val b_imag = Input(SInt(bitwidth.W))

        val twiddle_real = Input(SInt(bitwidth.W))
        val twiddle_imag = Input(SInt(bitwidth.W))

        val a_out_real = Output(SInt(bitwidth.W))
        val a_out_imag = Output(SInt(bitwidth.W))
        
        val b_out_real = Output(SInt(bitwidth.W))
        val b_out_imag = Output(SInt(bitwidth.W))
    })

    //Complex multiplication of B and twiddle factor
    //(a+bi)(c+di) = ac + adi + bci + bdi^2
    //             = ac + adi + bci - bd
    val a = WireDefault(io.b_real)
    val b = WireDefault(io.b_imag)
    val c = WireDefault(io.twiddle_real)
    val d = WireDefault(io.twiddle_imag)

    val real = WireDefault((a*c - b*d).asSInt ) //Gives bitwidth*2-1 bits
    val imag  = WireDefault((a*d + b*c).asSInt) //TODO add rounding to minimize quantization error.


    val high_bit = bp + bitwidth -1 
    val low_bit = bp - 1


    io.a_out_real  := (io.a_real + (real >> bp).asSInt).asSInt
    io.a_out_imag  := (io.a_imag + (imag >> bp).asSInt).asSInt
    
    io.b_out_real  := (io.a_real - (real >> bp).asSInt).asSInt
    io.b_out_imag  := (io.a_imag - (imag >> bp).asSInt).asSInt
}