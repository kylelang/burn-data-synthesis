### Title:    Custom Quickpred
### Author:   Gerko Vink (with modifications by Kyle M. Lang)
### Created:  2015-06
### Modified: 2021-10-25
### Purpose:  Updated mice::quickpred() to accomodate maximum number of predictors as a selection criterion

quickpredCustom <- function(data, 
                            maxnumber = NULL, 
                            mincor = 0.1, 
                            minpuc = 0,
                            include = "", 
                            exclude = "", 
                            method = "pearson") 
{
  ## Argument checking:
  if (!(is.matrix(data) | is.data.frame(data)))
    stop("Data should be a matrix or data frame")
  
  if ((nvar <- ncol(data)) < 2)
    stop("Data should contain at least two columns")
  
  if(!is.null(maxnumber)){
    if (maxnumber > (ncol(data) - 1)) # Added GV 7 Dec 2014
      stop("The maximum number of predictors per variable is exceeds the
           number of variables. Solution: decrease `maxnumber`")
  }
  
  ## Initialize
  predictorMatrix <- matrix(0, 
                            nrow = nvar, 
                            ncol = nvar,
                            dimnames = list(names(data), names(data))
  )
  
  x <- data.matrix(data)
  r <- !is.na(x)
  
  ## Calculate correlations among data:
  suppressWarnings(
    v <- abs(cor(x, use = "pairwise.complete.obs", method = method))
  )
  v[is.na(v)] <- 0
  
  ## Calculate correlations between data and response indicators:
  suppressWarnings(
    u <- abs(cor(y = x, x = r, use = "pairwise.complete.obs", method = method))
  )
  u[is.na(u)] <- 0
  
  ## Choose the stronger of the two correlations from above:
  maxc <- pmax(v, u)
  
  ## Include only the `maxnumber` highest predictors
  if(!is.null(maxnumber)) {
    diag(maxc) <- 0
    varRanks <- t(apply(maxc, 1, function(x) rank(x, ties = "first")))
    predictorMatrix[varRanks > (nvar - maxnumber)] <- 1
  } else {
    predictorMatrix[maxc > mincor] <- 1
  }
  
  ## Exclude predictors with a percentage usable cases below minpuc:
  if(minpuc > 0) {
    p <- md.pairs(data)
    puc <- p$mr/(p$mr + p$mm)
    predictorMatrix[puc < minpuc] <- 0
  }
  
  ## Exclude predictors listed in the exclude argument
  yz <- pmatch(exclude, names(data))
  predictorMatrix[, yz] <- 0
  
  ## Include predictors listed in the include argument
  yz <- pmatch(include, names(data))
  predictorMatrix[, yz] <- 1
  
  ## Some final processing
  diag(predictorMatrix) <- 0
  predictorMatrix[colSums(!r) == 0, ] <- 0
  
  predictorMatrix
}

