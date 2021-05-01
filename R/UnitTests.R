#install.packages("testthat")
#library(testthat)

source("CallEshotgun.R")


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
print(callEshotgun(Xtr, Ytr, c(-5.12,-1), c(5.12, 0,1), 10L, 0.1))


Xtr <- matrix(runif(20),ncol=1)
Ytr <- sphere(Xtr)
print(callEshotgun(Xtr, Ytr, c(-5.12), c(5.12), 10L, 0.2))

#testthat
#x <- c(3)
#x <- matrix(x, ncol=1)
#print(x)