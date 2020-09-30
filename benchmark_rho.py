#!/usr/bin/python
import numpy as np
import numba
from numba import njit, float64, vectorize, threading_layer
import time

pc_r_d = 287.05
pc_rvd_o = 461.51 / pc_r_d - 1.0

@njit(float64[:](float64[:], float64[:], float64[:], float64[:], float64[:]),parallel=True, nogil=True, fastmath=True)
#@njit(float64[:,:,:](float64[:,:,:], float64[:,:,:], float64[:,:,:], float64[:,:,:], float64[:,:,:]),parallel=True, nogil=True, fastmath=True)
#@vectorize([float64(float64, float64, float64, float64, float64)], target='parallel')
def rho(t, p, qv, qc, qi):
  rhof = p / ( pc_r_d * t * (1.0 + pc_rvd_o * qv - qc - qi) )
  return rhof

if __name__ == '__main__':
  shapes = [128*128*80]
  t = np.ones(shapes, dtype = np.float64)
  p = np.ones(shapes, dtype = np.float64)
  qv = np.ones(shapes, dtype = np.float64)
  qc = np.ones(shapes, dtype = np.float64)
  qi = np.ones(shapes, dtype = np.float64)
  rhof = np.ones(shapes, dtype = np.float64) 

  start = time.time()
  rhof = rho(t,p,qv,qc,qi) 
#  rho.parallel_diagnostics(level=4)
  end = time.time()
  print("Elapsed (with compilation) = %s" % (end - start))


  times=np.empty(100)
  for count in range(100):
    start = time.time()
    rho(t,p,qv,qc,qi) 
    end = time.time()
    times[count] = (end-start)

  print(times)
  print("Elapsed time = {0}, {1}".format(np.mean(times), np.std(times)))
  print("Threading layer chosen: %s" % threading_layer())
  print("Num threads: %s" % numba.get_num_threads())
