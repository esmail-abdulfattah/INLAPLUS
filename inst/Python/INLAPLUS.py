import numpy as np
import os

from matplotlib import pyplot as plt
import glob

plt.rcParams.update({'figure.max_open_warning': 0})

def create_direct(path):
    isExist = os.path.exists(path)
    if not isExist:
        os.makedirs(path)

def create_subdirect(path1,path2):
    path3 = os.path.join(path1, path2)
    isExist = os.path.exists(path3)
    if not isExist:
        os.makedirs(path3)

def is_list(a):
    if not isinstance(a, list):
        print(a, " is not list")
    else:
        print(a, "is list", sep=" ")

def is_vector(param):
    if isinstance(param, (np.ndarray, np.generic)):    
        if param.ndim==1:
            return True
        else:
            return False
    else:
        return False

def is_matrix(param):
    if isinstance(param, (np.ndarray, np.generic)):    
        if param.ndim==2:
            return True
        else:
            return False
    else:
        return False

def is_boolean(param):
    if isinstance(param, bool):
        return True
    else:
        return False

def is_symmetric(a):
    b = a.transpose()
    if np.array_equal(a, b):
        return True
    else:
        return False

def export(param1, param2):
    param1 = os.path.join('.','Data', param1 + ".txt")
    file1 = open(param1,"w")
    # file1.truncate(0) 
    param3 = '\n'.join(map(str, param2))
    file1.write(param3)
    file1.close()

def export_matrix(param1,mat):
    param1 = os.path.join('.','Data', param1 + ".txt")
    ii = 0
    f = open(param1, "w")
    for x in mat:
        k = mat.shape[0] - ii
        i = 0
        for y in x:
            if i<(mat.shape[0]-k+1):
                f.write(str(y))
                f.write("\t")
            i = i + 1
        f.write("\n")
        ii = ii + 1
    f.close()

def inla1234(data : dict, model: dict, control_strategy: dict, blocks : dict, blocks_effects : dict, control_parallel : dict, priors = {},  control_opt={}):
    print("Welcome to inla1234() function in Python! ")

    create_direct('Data')
    create_direct('Plots')
    create_direct('Results')
    create_subdirect('Results','GA')
    create_subdirect('Results','HP')

    #Testing if all inputs are dicts!
    x = [blocks_effects, blocks, model, control_strategy, data, control_opt]
    for i in x:
        if not isinstance(i, dict):
            raise ValueError("Invalid Input type")
            
    #testing data:
    if 'y_response' not in data:
        raise ValueError("Please add response")
    
    y_response = data['y_response']
    if len(y_response.shape)>1:
        raise ValueError("Input must be a vector")

    y_size = y_response.shape[0]

    export("y_response",y_response)
    export("y_size",[y_size])

    #testing Model
    like_list = ['Gaussian', 'Poisson', 'Binomial']

    if 'like' not in model:
        raise ValueError("PLease add likelihood")

    if model['like'] not in like_list:
        raise ValueError("Likelihood is not implemented")

    Model = model['like'] 
    export("Model",[Model])

    if Model == 'Gaussian':
        if 'Gaussian_Noise' in model:
            Gaussian_Noise = model['Gaussian_Noise']
            if Gaussian_Noise <= 0:
                raise ValueError("Precision should be positive")
            else: 
                export("Gaussian_Noise",[Gaussian_Noise]) 
    elif Model == 'Poisson':
        Ntrials = np.ones(y_size) 
        if 'offset' in model:
            Ntrials = model['offset']
            if not is_vector(Ntrials):
                raise ValueError("offset should be np array")
        export("Ntrials",Ntrials)            
    elif Model == 'Binomial':
        if not 'Ntrials' in model:
            raise ValueError("Please add Ntrials")
        Ntrials = model['Ntrials']
        if not is_vector(Ntrials):
                raise ValueError("Ntrials should be np array")
        if not Ntrials.size == y_size:
            raise ValueError("size of Ntrials and reponse y should be equal")
        export("Ntrials",Ntrials)   

    #testing control_strategy
    if 'strategy' in model:
        strategy = model['strategy']
        if not strategy == 'GA':
            print("The chosen strategy is not implemented")
    else:
        strategy = 'GA'
        export("strategy",[strategy])   
    if 'vbc' in model:
        value_vbc = model['vbc']
        if not is_boolean(value_vbc):
            raise ValueError("vbc should be true or false")
        if value_vbc==True:
            vbc = 1
            export("vbc",[vbc]) 
    else:
        vbc = 0
        export("vbc",[vbc])   

    #testing blocks_effects
    RE_time = ['RW1','RW2','generic_time']
    RE_space = ['besag','generic_space','ICAR'] 
    RE_interaction = ['type1','type2','type3','type4','interaction1','interaction2','interaction3','interaction4']

    # Qt = np.empty([]); Qs = np.empty([])

    if blocks_effects['struc_time'] not in RE_time:
        raise ValueError("a random effect is still not implemented")
    else: 
        if 'struc_time_Q' in blocks:
            Qt = blocks['struc_time_Q']
            if not is_matrix(Qt):
                raise ValueError("The precision matrix should be np array of 2D")
            export_matrix("Qxeff1",Qt) 
        else: 
            raise ValueError("Please add precision matrix for time")

    if blocks_effects['struc_space'] not in RE_space:
        raise ValueError("a random effect is still not implemented")
    else: 
        if 'struc_space_Q' in blocks:
            Qs = blocks['struc_space_Q']
            if not is_matrix(Qs):
                raise ValueError("The precision matrix should be np array of 2D")
        else: 
            raise ValueError("Please add precision matrix for space")

    if blocks_effects['interaction'] not in RE_interaction:
        raise ValueError("interaction type is not implemented")

    RE_size = 3
    test = 0
    if 'idd_time' in blocks_effects:
        RE_size = RE_size + 1
        test = test + 1
    if 'idd_space' in blocks_effects:
        RE_size = RE_size + 1
        test = test + 1
    
    if test==1:
        raise ValueError("please include both iid for time and space")

    if not(RE_size==3 or RE_size==5):
        raise ValueError("This case is still not implemented")


    possible_priors = ['loggama.prec', 'pc.prec', 'pc.joint', '']
    if 'prior' in priors:
        if priors['prior'] not in possible_priors:
            raise ValueError("Prior is not implemented")
        else: 
            myprior = priors['prior']
            priors_types = np.tile(np.array([myprior]), RE_size)
            export("priors_types",priors_types)
            if not myprior=='pc.joint':
                export("prior",['default'])
            else:
                export("prior",['pc.joint'])
                priors_p1 = np.tile('1', RE_size)
                priors_p2 = np.tile('1', RE_size)
                export("priors_p1",priors_p1)
                export("priors_p2",priors_p2)
            if myprior=='loggama.prec':
                if 'priors_p1' in priors :
                    p1 = priors['p1']
                    if is_vector(p1) and p1.shape[0]==RE_size:
                        export("priors_p1",p1)
                    else:
                        raise ValueError("p1 should be 1D numpy array")
                else:
                    priors_p1 = np.tile('1', RE_size)
                    export("priors_p1",priors_p1)
                if 'priors_p2' in priors :
                    p2 = priors['p2']
                    if is_vector(p2) and p2.shape[0]==RE_size:
                        export("priors_p2",[p2])
                    else:
                        raise ValueError("p2 should be 1D numpy array")
                else:
                    priors_p2 = np.tile('1', RE_size)
                    export("priors_p2",[priors_p2])                
    else: 
        priors_types = np.tile('pc.joint', RE_size)
        export("priors_types",priors_types)
        export("prior",['pc.joint'])
        priors_p1 = np.tile('1', RE_size)
        priors_p2 = np.tile('1', RE_size)
        export("priors_p1",priors_p1)
        export("priors_p2",priors_p2)


    num_blocks = RE_size + 1 #for intercept
    prior_blocks = np.tile('1', RE_size)
    initial = np.tile('4', RE_size)

    export("central",['0'])
    export("grad_stepsize",['0.005'])
    export("theta_size",[RE_size])
    export("max_linesearch",['5'])
    export("num_blocks",[num_blocks])
    export("num_threads",['1'])
    export("x_mu",['1'])
    export("smartGrad",['0'])
    export("prec_mu",['0.001'])
    export("prior_blocks",prior_blocks)
    export("size_priors_types",[RE_size])
    export("initial",initial)
    export("num_effs",[RE_size])
    export("theta_size_Qx",[RE_size]) #we are not considering to add a prior to gaussian noise.
    export("Qx_type",['generic_Qx'])
    export("Cov_Z",['0'])

    n = blocks['n']
    m = blocks['m']
    x_size = n + m + n*m + 1

    while Qt.shape[0] != n and Qt.shape[1] != n: 
        raise ValueError("Time size is not correct")

    while Qs.shape[0] != m and Qs.shape[1] != m: 
        raise ValueError("Space size is not correct")    

    if 'rankdef' not in blocks:
        raise ValueError("Please add rank deficiency for time and space")
    RD = blocks['rankdef']
    if not RD.size==2:
        raise ValueError("The length of rank deficiency should be 2 (use np array)")
    r1 = RD[0]
    r2 = RD[1]
    r3 = 0

    RE_interaction = np.array(['type1','type2','type3','type4','interaction1','interaction2','interaction3','interaction4'])
    if blocks_effects['interaction']=='type1' or blocks_effects['interaction']=='interaction1':
        r3 = 0
    elif blocks_effects['interaction']=='type2' or blocks_effects['interaction']=='interaction2':
        r3 = n*m - (n-r1)*(m) #type2
    elif blocks_effects['interaction']=='type3' or blocks_effects['interaction']=='interaction3':
        r3 = n*m - (n)*(m-r2) #type3
    elif blocks_effects['interaction']=='type4' or blocks_effects['interaction']=='interaction4':
        r3 = n*m - (n-r1)*(m-r2) #type4

    #print(np.tril(Qt))
    if RE_size == 3:
        #np.savetxt('Qxeff2.txt', Qs)
        export_matrix("Qxeff2",Qs)
        sizes = np.array([n,m,n*m])
        export("sizes",sizes)
        RD_effi = np.array([blocks_effects['struc_time'],blocks_effects['struc_space'],blocks_effects['interaction']])
        export("RD_effi",RD_effi)
        effi = np.array(['eff1','eff2','eff3'])
        export("effi",effi)
        rankdef = np.array(RD[0],RD[1],r3)
        export("RD",rankdef)
    elif RE_size == 5:
        #np.savetxt('Qxeff3.txt', Qs) 
        export_matrix("Qxeff3",Qs) 
        export_matrix("Qxeff2",np.identity(Qt.shape[0])) 
        export_matrix("Qxeff4",np.identity(Qs.shape[0])) 
        x_size = x_size + n + m
        sizes = np.array([n,n,m,m,n*m])
        export("sizes",sizes)
        RD_effi = np.array([blocks_effects['struc_time'],'iid_time',blocks_effects['struc_space'],'iid_space',blocks_effects['interaction']])
        export("RD_effi",RD_effi)
        effi = np.array(['eff1','eff2','eff3','eff4','eff5'])
        export("effi",effi)
        rankdef = np.array([RD[0],0,RD[1],0,r3])
        export("RD",rankdef)

    if 'safemode' in control_opt:
        value_safemode = control_opt['safemode']
        if not is_boolean(value_safemode):
            raise ValueError("safemode should be true or false")
        if control_opt['safemode']==True:
            export("safemode", [1])
        else:
            export("safemode", [0])
    else: 
        export("safemode", [0])

    
    export("x_size",[x_size])

    RE1 = np.repeat(np.arange(1, n+1, 1),m)
    RE2 = np.tile(np.arange(1, m+1, 1), n)
    RE3 = np.arange(1, (n*m)+1, 1)

    if RE_size == 3:
        RE2 = RE1[-1] + RE2
        RE3 = RE2[-1] + RE3
        id_effi = np.concatenate((RE1,RE2,RE3),axis=0)
        export("id_effi",id_effi)
    elif RE_size == 5:
        RE11 = RE1
        RE22 = RE1 + RE11[-1]
        RE33 = RE2 + RE22[-1]
        RE44 = RE2 + RE33[-1]
        RE55 = RE3 + RE44[-1]
        id_effi = np.concatenate((RE11,RE22,RE33,RE44,RE55),axis=0)
        export("id_effi",id_effi)

    #export("envi",'usingPython')

    if 'num_omp' not in control_parallel and 'num_proc' not in control_parallel and 'resource' not in control_parallel:
        raise ValueError("Please make sure number of processes, number of threads and resource  are added")
    
    num_omp = control_parallel['num_omp']
    num_proc = control_parallel['num_proc']
    command = "export OMP_NUM_THREADS="+ str(num_omp) +"; mpirun -np "+ str(num_proc) +" --map-by socket:PE=${OMP_NUM_THREADS} ./output_server"
    
    # os.chdir("..")
    #os.system("ls")

    #print(command)
    os.system(command)

def plot_marginals(param1):

    sizes = np.loadtxt(os.path.join('Data','sizes.txt'))

    if param1 == "hyperparameter":
        p1 = 'HP'
        p2 = 1
        file_list = glob.glob(os.path.join('Results',p1,'*.txt'))
        p3 = len(file_list)//2+1

        for idx in range(int(p2),int(p3)):
        
            x = np.loadtxt(os.path.join('Results',p1,'x{}.txt'.format(idx)))
            y = np.loadtxt(os.path.join('Results',p1,'y{}.txt'.format(idx)))

            #print(os.path.join('Results',p1,'x{}.txt'.format(idx)))
            plt.figure()
            plt.plot(x,y)
            plt.title('theta{}'.format(idx))
            plt.grid()
            plt.xlabel('X')
            plt.ylabel('Density')
            plt.savefig(os.path.join('Plots','theta{}.png'.format(idx)))
        print("The marginal posteriors of the {} are exported to Plots folder".format(param1))
    else:
        if param1 == "structured_time":
            p1 = 'GA'
            p2 = 1
            p3 = p2 + sizes[0]
        elif param1 == "unstructured_time" and sizes.size==5:
            p1 = 'GA'
            p2 = 1 + sizes[0]
            p3 = p2 + sizes[0]
        elif param1 == "structured_space":
            p1 = 'GA'
            if sizes.size==3:
                p2 = 1 + sizes[0] 
                p3 = p2 + sizes[1]
            elif sizes.size==5:
                p2 = 1 + sizes[0] + sizes[0] 
                p3 = p2 + sizes[2]
        elif param1 == "unstructured_space" and sizes.size==5:
            p1 = 'GA'
            p2 = 1 + sizes[0] + sizes[0] + sizes[2] 
            p3 = p2 + sizes[3]
        else:
            raise ValueError("Wrong Input")

        for idx in range(int(p2),int(p3)):
            
                x = np.loadtxt(os.path.join('Results',p1,'x{}.txt'.format(idx)))
                y = np.loadtxt(os.path.join('Results',p1,'y{}.txt'.format(idx)))

                #print(os.path.join('Results',p1,'x{}.txt'.format(idx)))
                plt.figure()
                plt.plot(x,y)
                plt.title('latent field{}'.format(idx))
                plt.grid()
                plt.xlabel('x')
                plt.ylabel('Density')
                plt.savefig(os.path.join('Plots','latent field{}.png'.format(idx)))

        print("The marginal posteriors of the {} are exported to Plots folder".format(param1))
