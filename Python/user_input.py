
import os
import numpy as np
from INLAPLUS import inla1234,plot_marginals

size_time = 10
size_space = 100
rankdef = np.array([2,1])

Qx_time = np.loadtxt(os.path.join('Input','Qx_time.txt'))
Qx_space = np.loadtxt(os.path.join('Input','Qx_space.txt'))
y_response  = np.loadtxt(os.path.join('Input','ysim.txt'))
offset = np.loadtxt(os.path.join('Input','offset.txt'))


data = {'y_response': y_response}
model = {'like': 'Poisson', 'offset': offset}
control_strategy = {'startegy': "GA"}
blocks = {'struc_time_Q' : Qx_time , 'struc_space_Q' : Qx_space , 'n': size_time, 'm': size_space, 'rankdef' : rankdef}
blocks_effects = {'struc_time': 'RW2', 'struc_space' : 'besag', 'interaction' : 'type4', 'idd_time' : True, 'idd_space' : True}
control_opt = {'safemode' : False}
control_parallel = {'num_omp' : 8,'num_proc' : 6}
priors = {'prior' : 'pc.joint'}

if __name__=='__main__':

    inla1234(data,model,control_strategy,blocks,blocks_effects,control_parallel, priors, control_opt)

    plot_marginals("hyperparameter")
    plot_marginals("structured_time")
    plot_marginals("unstructured_time")

