import numpy as np


one = 1
samples = [one,one,one,0,0,0,0,0,one,one,one,0,0,0,0,0,
        one,one,one,0,0,0,0,0,one,one,one,0,0,0,0,0,
        one,one,one,0,0,0,0,0,one,one,one,0,0,0,0,0,
        one,one,one,0,0,0,0,0,one,one,one,0,0,0,0,0,
        one,one,one,0,0,0,0,0,one,one,one,0,0,0,0,0,
        one,one,one,0,0,0,0,0,one,one,one,0,0,0,0,0,
        one,one,one,0,0,0,0,0,one,one,one,0,0,0,0,0,
        one,one,one,0,0,0,0,0,one,one,one,0,0,0,0,0,]

fft = np.fft.fft(samples)

for index, val in enumerate(fft):
    print(index, val)