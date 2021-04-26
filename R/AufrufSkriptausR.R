library(reticulate)

py_config()

getwd()

checkLibrarys <- function() {
  library(reticulate)

  # check librarys
  # Pip's requirement check is faster, also conda doesn't include all needed packages
  py_install("pygame", pip=TRUE)
  py_install("pyfoam", pip=TRUE)
  py_install("GPy==1.9.9", pip=TRUE)
  py_install("cma", pip=TRUE)
  py_install("cython", pip=TRUE)
  py_install("nlopt", pip=TRUE)
  py_install("pyDOE2", pip=TRUE)
  py_install("box2d-py", pip=TRUE)
  py_install("numpy-stl", pip=TRUE)
  py_install("pygmo", pip=TRUE)
  py_install("matplotlib", pip=TRUE)
  py_install("tqdm", pip=TRUE)
  py_install("statsmodels", pip=TRUE)
}


callEshotgun <- function(Xtr, Ytr, f_lb, f_ub, q, epsilon) {
  py_run_file("../eshotgun/AufrufScript.py")
  np <- import("numpy", convert = FALSE)

  Xnew <- tryCatch({

    #is.null(dim(x))
    #x <- matrix(x, ncol=1)
    xrow <- nrow(Xtr)
    xcol <- ncol(Xtr)

    yrow <- nrow(Ytr)
    ycol <- ncol(Ytr)

    dimLb <- length(f_lb)
    dimUb <- length(f_ub)

    # check for equal dimensions for Xtr and Ytr
    if(xrow != yrow) {
      stop("Xtr and Ytr have different Shapes")
    }


    #check lower and upper bound for a fitting dimension
    if(!(dimLb == dimUb && dimLb == xcol)) {

      if(dimLb != dimUb) {
        errorMsg <- c("Dimension of bounds don't match!",
                      paste("Dimension of Lower bound: ", dimLb, sep=""),
                      paste("Dimension of Upper bound: ", dimUb, sep=""))
        cat(errorMsg, sep="\n")
      }else {
        errorMsg <- c("Dimension of bounds and Xtr don't match!",
                      paste("Dimension of Lower bound: ", dimLb, sep=""),
                      paste("Dimension of Xtr: ", xcol, sep=""))
        cat(errorMsg, sep="\n")
      }

      stop()
    }


    #Notwendig? eshotgun wirft keinen Fehler
    #check epsilon between 0.0 and 1.0
    if(!(epsilon >= 0.0 && epsilon <= 1.0)) {
      cat("Epsilon has to be between 0.0 and 1.0")
      stop()
    }

    # if the column is 1 choose special case
    if(xcol >= 2) {
      py$callShotgun(np$array(Xtr), np$array(Ytr), np$array(f_lb), np$array(f_ub), q, epsilon)
    }else {
      py$callShotgun(np$array(Xtr), np$array(Ytr), f_lb, f_ub, q, epsilon)
    }

  }, warning = function(w) {

  }, error = function(e) {
    print(e)
  }, finally = {

  })

  return(Xnew)
}


sphere <- function (x) {
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(x) {
                 sum(x ^ 2)  # objective function
               }),
         , 1) # number of columns
}


Xtr <- matrix(runif(20),ncol=2)
Ytr <- sphere(Xtr)
print(callEshotgun(Xtr, Ytr, c(-5.12,-1), c(5.12, 0), 10L, 0.1))
#print(callEshotgun(Xtr, Ytr, c(-5.12,-1), c(5.12, 0,1), 10L, 0.1))


Xtr <- matrix(runif(20),ncol=1)
Ytr <- sphere(Xtr)
print(callEshotgun(Xtr, Ytr, c(-5.12), c(5.12), 10L, 0.2))

#testthat
#x <- c(3)
#x <- matrix(x, ncol=1)
#print(x)
