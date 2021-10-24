### Title:    Select Predictors for Elementary Imputation Models
### Author:   Kyle M. Lang
### Created:  2021-10-22
### Modified: 2021-10-22
### Purpose:  To generate a predictor matrix for mice by selecting the 'nPreds'
###           most important predictors of each imputation target.
###           - "Importance" is defined via the variable importance measures
###             from a fitted Random Forest model.
###           - This probably won't work very well, though, because the RF model
###             can only be fit to complete data.

library(caret)
library(randomForest)

###--------------------------------------------------------------------------###

.findPreds <- function(y, X, ...) {
  form <- paste(y, paste(X, collapse = " + "), sep = " ~ ")
  imp  <- randomForest(formula = as.formula(form), 
                       data = data, 
                       importance = FALSE, 
                       na.action = "na.omit")$importance
  
  rownames(imp)[order(imp, decreasing = TRUE)][1 : nPreds]
}

###--------------------------------------------------------------------------###

.findAllPreds <- function(data, 
                          nPreds = 1, 
                          candidates = colnames(data), 
                          targets = colnames(data)
) 
{
  out <- matrix(NA, length(targets), nPreds, dimnames = list(targets, NULL))
  
  for(v in targets)
    out[v, ] <- .findPreds(y = v, 
                           X = setdiff(candidates, v), 
                           data = data, 
                           nPreds = nPreds)
   
out
}

###--------------------------------------------------------------------------###

makePredMat <- function(data, nPreds, include = NULL, exclude = NULL) {
  pm      <- colMeans(is.na(data))
  targets <- colnames(data)[pm > 0 & pm < 1]
  
  preds <- .findAllPreds(data = data, 
                         nPreds = nPreds, 
                         targets = targets, 
                         candidates = setdiff(colnames(data), exclude)
                         )
  
  predMat <- matrix(0, 
                    ncol(data), 
                    ncol(data), 
                    dimnames = list(colnames(data), colnames(data))
                    )

  if(!is.null(include))  
    predMat[ , include] <- 1
  
  for(v in targets) predMat[v, preds[v, ]] <- 1
  
  predMat[pm == 0, ] <- 0
  diag(predMat)      <- 0
  
  predMat
}
