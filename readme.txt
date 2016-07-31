This project is an implementation of a Reed Solomon decoder and encoder in VHDL.  A Reed Solomon code is an error correcting block code.  The encoder takes a fixed-size block of data and appends another fixed-size block data, called parity, to it.  The parity data provides redundancy that allows the decoder to correct errors that may have been introduced between the encoder and the decoder.

This Reed Solomon decoder is relatively light on features.  It does not detect decoding errors, and it does not support erasures.  It was designed for high throughput operation, and validated on a Xilinx Zynq 7010 FPGA.  It implements the Xilinx AXI Streaming interface, however it does not reference any Xilinx IP cores – so it may also work on non-Xilinx FPGAs (although this has not been tested).

The encoder top level is the rsEncoderWrapper module, and takes the following generics as inputs:

M – This is the width, in bits, of the input and output.  The encoder uses a finite field who's size is 2 to the power of M.

n – This is the number of elements in the encoded block.  It must be less then 2 to the power of M.

k – This is the number of elements in the input block.  The value (n-k) must be positive and be divisible by 2.  The resulting code can correct at most (n-k)/2 errors.

primPoly – This value is used to construct the finite field.  If you set it to zero, an appropriate value will be chosen for you.

alpha – This is the generator element in the finite field.  If you don't know what it is, set it to 2.

The decoder top level is the rsDecoderWrapper module.  It has the same generics as inputs listed above, in addition to two more generics.  The encoder and decoder common generics values should match.  The additional decoder generics are as follows:

outputParity – If this is true, the decoder will output output the parity bits, otherwise it will only output the information bits.

extraCycles – The decoder throughput is 1 block every (n + extraCycles) cycles (the latency is longer).  If this value is zero, the decoder can accept a new input symbol each cycle (assuming the user accepts the decoder output each cycle by keeping dataOut_tready low).  Increasing this value in some cases can reduce the resources required by the decoder, at the expense of reducing the decoding throughput.

Both the encoder and decoder implement two AXI streaming interfaces – one for input and one for output.  This README does not fully document the AXI streaming interface, however here is a brief description.  The data producer asserts the valid signal when it has data, and the consumer asserts the ready signal if it is ready to receive data.  If valid and ready are both asserted (active high), the data word is transferred.  Usually if valid is asserted and ready is not asserted, the valid signal will remain asserted (and the data will remain valid) until the consumer asserts ready.  The AXI streaming interface last signal denotes a frame boundary.

When the last, valid, and ready signals are all asserted, the transferred byte is last byte in the current frame.  The next transferred byte is the first byte of the next frame.  Frames are useful if you want encode a group of bytes that is different than the block size.  If you don't want to use them, simply hard code the last inputs to logic low.  If you do use them, the rsEncoderWrapper and rsDecoderWrapper modules will automatically break up each frame into blocks for encoding and decoding.  If the block size dose not evenly divide the frame size, the blocks will be padded internally.
