import os
import numpy as np
import GPy as gp

from eshotgun import batch_methods

# force numpy, blas, and mkl to use a max number of threads
n_threads = 4
os.environ["MKL_NUM_THREADS"] = "{:d}" .format(n_threads)
os.environ["NUMEXPR_NUM_THREADS"] = "{:d}" .format(n_threads)
os.environ["OMP_NUM_THREADS"] = "{:d}" .format(n_threads)
os.environ["OPENBLAS_NUM_THREADS"] = "{:d}" .format(n_threads)
os.environ["VECLIB_MAXIMUM_THREADS"] = "{:d}" .format(n_threads)

def build_and_fit_GP(Xtr, Ytr):
    # create a gp model with the training data and fit it
    kernel = gp.kern.Matern52(input_dim=Xtr.shape[1], ARD=False)
    model = gp.models.GPRegression(Xtr, Ytr, kernel, normalizer=True)
    
    model.constrain_positive('')
    (kern_variance, kern_lengthscale,
     gaussian_noise) = model.parameter_names()
     
    model[kern_variance].constrain_bounded(1e-6, 1e6, warning=False)
    model[kern_lengthscale].constrain_bounded(1e-6, 1e6, warning=False)
    model[gaussian_noise].constrain_fixed(1e-6, warning=False)
    model.optimize_restarts(optimizer='lbfgs',
                            num_restarts=10,
                            num_processes=1,
                            verbose=False)
    return model
  

def callShotgun(Xtr, Ytr, f_lb, f_ub, q=10, epsilon = 0.1):
    model = build_and_fit_GP(Xtr, Ytr)
    Xnew = batch_methods.egreedy_shotgun.egreedy_shotgun_v2(model, f_lb, f_ub, 1000, q, None, epsilon)
    return Xnew
  
  
def callShotgunDim1(Xtr, Ytr, f_lb, f_ub, q=10, epsilon = 0.1):
    model = build_and_fit_GP(Xtr, Ytr)
    Xnew = batch_methods.egreedy_shotgun.egreedy_shotgun_v2(model, np.array([f_lb]), np.array([f_ub]), 1000, q, None, epsilon)
    return Xnew