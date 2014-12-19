#!/usr/bin/env python3

import matplotlib.pyplot as plt
from numpy import pi, exp, tan, arange, vectorize, cos, sqrt
from numpy import power as pow

dl, sigma, dt = 5.0, 0.1, 0.01
x = arange(0, dl+dt, dt)

def hgauss(x, event, var):
    J = pi/(dl*pow(cos(pi/(2*dl)*(2*x-dl*dl)), 2))
    # mul gauss with the jacobian of the transformation, so that it's area stays the same
    xh = tan(pi/(2*dl)*(2*x-pow(dl, 2))) + dl/2
    return var*J*exp(-pow((xh-event)*var, 2))

vhg = vectorize(hgauss)

for i in arange(0, dl+0.5, 0.5):
    plt.plot(x, vhg(x, i, 3))
plt.show()

