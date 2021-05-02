#install.packages("testthat")
#library(testthat)
#local_edition(3)

source("~/R/rshotgunsmall/R/CallEshotgun.R")


sphere <- function (x) {
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(x) {
                 sum(x ^ 2)  # objective function
               }),
         , 1) # number of columns
}

egg <- function(xx) {
  ##########################################################################
  #
  # EGGHOLDER FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it
  # and/or modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; version 2.0 of the License.
  # Accordingly, this program is distributed in the hope that it will be
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2)
  #
  ##########################################################################

  x1 <- xx[1]
  x2 <- xx[2]

  term1 <- -(x2+47) * sin(sqrt(abs(x2+x1/2+47)))
  term2 <- -x1 * sin(sqrt(abs(x1-(x2+47))))

  y <- term1 + term2
  return(y)
}


schwef <- function(xx) {
  ##########################################################################
  #
  # SCHWEFEL FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it
  # and/or modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; version 2.0 of the License.
  # Accordingly, this program is distributed in the hope that it will be
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################

  d <- length(xx)

  sum <- sum(xx*sin(sqrt(abs(xx))))

  y <- 418.9829*d - sum
  return(y)
}


levy <- function(xx) {
  ##########################################################################
  #
  # LEVY FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it
  # and/or modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; version 2.0 of the License.
  # Accordingly, this program is distributed in the hope that it will be
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUT:
  #
  # xx = c(x1, x2, ..., xd)
  #
  ##########################################################################

  d <- length(xx)
  w <- 1 + (xx - 1)/4

  term1 <- (sin(pi*w[1]))^2
  term3 <- (w[d]-1)^2 * (1+1*(sin(2*pi*w[d]))^2)

  wi <- w[1:(d-1)]
  sum <- sum((wi-1)^2 * (1+10*(sin(pi*wi+1))^2))

  y <- term1 + sum + term3
  return(y)
}
modifiedLevy <- function(x) {
  matrix(apply(x, # matrix
               1, # margin (apply over rows)
               function(x){
                 levy(x)
               }),
         , 1) # number of columns
}


branin <- function(xx, a=1, b=5.1/(4*pi^2), c=5/pi, r=6, s=10, t=1/(8*pi)) {
  ##########################################################################
  #
  # BRANIN FUNCTION
  #
  # Authors: Sonja Surjanovic, Simon Fraser University
  #          Derek Bingham, Simon Fraser University
  # Questions/Comments: Please email Derek Bingham at dbingham@stat.sfu.ca.
  #
  # Copyright 2013. Derek Bingham, Simon Fraser University.
  #
  # THERE IS NO WARRANTY, EXPRESS OR IMPLIED. WE DO NOT ASSUME ANY LIABILITY
  # FOR THE USE OF THIS SOFTWARE.  If software is modified to produce
  # derivative works, such modified software should be clearly marked.
  # Additionally, this program is free software; you can redistribute it
  # and/or modify it under the terms of the GNU General Public License as
  # published by the Free Software Foundation; version 2.0 of the License.
  # Accordingly, this program is distributed in the hope that it will be
  # useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  # of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  # General Public License for more details.
  #
  # For function details and reference information, see:
  # http://www.sfu.ca/~ssurjano/
  #
  ##########################################################################
  #
  # INPUTS:
  #
  # xx = c(x1, x2)
  # a = constant (optional), with default value 1
  # b = constant (optional), with default value 5.1/(4*pi^2)
  # c = constant (optional), with default value 5/pi
  # r = constant (optional), with default value 6
  # s = constant (optional), with default value 10
  # t = constant (optional), with default value 1/(8*pi)
  #
  ##########################################################################

  x1 <- xx[1]
  x2 <- xx[2]

  term1 <- a * (x2 - b*x1^2 + c*x1 - r)^2
  term2 <- s*(1-t)*cos(x1)

  y <- term1 + term2 + s
  return(y)
}
modifiedBranin <- function(x) {
    matrix(apply(x, # matrix
                 1, # margin (apply over rows)
                 function(x){
                   branin(x)
                 }),
           , 1) # number of columns
  }


#Xtr <- matrix(runif(20),ncol=2)
#Ytr <- sphere(Xtr)
#print(callEshotgun(Xtr, Ytr, c(-5.12,-1), c(5.12, 0), 10L, 0.1))
#print(callEshotgun(Xtr, Ytr, c(-5.12,-1), c(5.12, 0,1), 10L, 0.1))

#Xtr <- matrix(runif(20),ncol=1)
#Ytr <- sphere(Xtr)
#print(callEshotgun(Xtr, Ytr, c(-5.12), c(5.12), 10L, 0.2))

Xtr <- matrix(runif(20),ncol=2)
Ytr <- modifiedLevy(Xtr)
print(callEshotGunUnchecked(Xtr, Ytr, c(-5,-1), c(5,1), 10L, 0.1))

Xtr <- matrix(runif(20),ncol=1)
Ytr <- modifiedLevy(Xtr)
print(callEshotGunUnchecked(Xtr, Ytr, c(-5), c(5), 10L, 0.1))


Xtr <- matrix(runif(20),ncol=2)
Ytr <- modifiedBranin(Xtr)
print(callEshotGunUnchecked(Xtr, Ytr, c(-5,-1), c(5,1), 10L, 0.1))

Xtr <- matrix(runif(20),ncol=3)
Ytr <- modifiedBranin(Xtr)
print(callEshotGunUnchecked(Xtr, Ytr, c(-5,-4,-3), c(5,6,7), 10L, 0.1))
