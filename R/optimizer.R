runSampleOpt <- function(fn, budget = 100){
  initBudget <- budget
  Xtr <- matrix(runif(20),ncol=2)
  Ytr <- fn(Xtr)

  budget <- initBudget - nrow(Xtr)
  while(budget > 0){
    newX <- callEshotgun(Xtr, Ytr, c(-5,-5), c(5,5))
    print("initial worked")
    print(newX)
    newY <- fn(newX)
    Xtr <- rbind(Xtr, newX)
    print(Xtr)
    Ytr <- c(Ytr, newY)
    print(Ytr)
    budget <- initBudget - nrow(Xtr)
  }
}

