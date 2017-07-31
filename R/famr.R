## Families of multiple regression models
## base calculations and visualization
##
## author: Tomasz Konopka


#' Perform a family of multiple regression analyses on a dataset
#'
#' @param data - matrix or data frame
#' @param y - character, name of column in data containing modeled variable;
#'            alternatively, a list with one function operating on data.
#' @param x - character, name of column in data containing primary variable;
#'            alternatively, a list with one function operating on data.
#' @param models - description of secondary variables to include in
#'   multiple regressions.
#'   When NULL, all numeric/factor columns in data are used.
#'   Otherwise, models must be a named list, see output of famrModels() for
#'   specific format.
#'
#' @export
famr = function(data, y, x, models=NULL) {

  checkDataMatrix(data)

  ## make sure models is in the correct format
  if (is.null(models)) {
    avoid = c(getNames(x), getNames(y))
    models = famrModels(data, avoid=avoid)
  }

  ## adjust the data matrix (reorders columns, applies functions)
  data = famrPrep(data, y, x, models)

  ## check x/y are numeric
  checkClass(data[,2], "numeric", msg="x must be numeric")
  checkClass(data[,1], "numeric", msg="y must be numeric")

  ## start preparing the output
  result = list(data=data, vars=c(colnames(data)[1:2]),
  	   	basemodel=NA, models=list())  
  baseformula = paste(result$vars[1], "~", result$vars[2])

  # identify the set of background variables
  other = colnames(data)[-c(1,2)]
 
  ## perform a basic fit
  m0 = lm(as.formula(baseformula), data)
  result$basemodel = m0

  if (ncol(data)<=2) {
     return (result)
  }

  for (altmodel in names(models)) {
    altformula = baseformula
    for (altvar in names(models[[altmodel]])) {
      if (class(data[,altvar]) %in% c("numeric", "factor")) {
        altformula = paste(altformula, "+", altvar)
      }
    }
    mw = lm(as.formula(altformula), data)
    result$models[[altmodel]] = mw
  }

  ## set the class to enable summary() and plot()
  class(result) = "famr"

  result
}



#' Display basic information about the model family
#'
#' @param x - famr object
#' @param ... - additional paramters (not used)
#'
#' @export
print.famr = function(x, ...) {
  checkClass(x, "famr")
  cat("Family of", 1+length(x$models), "multiple regression models\n")
  cat("Base regression is for", x$vars[1], "vs.", x$vars[2], "\n")
}




#' Display a matrix of information summarizing the models
#'
#' @param object - famr object
#' @param ... - additional parameters (not used)
#'
#' @export
summary.famr = function(object, ...) {
  checkClass(object, "famr")


  ## helper - produce a summary table
  ## contains primary covariate p, secondary covariate p
  makeOneSummary = function(z, modelname) {
    ## use the lm model summary
    zsc = summary(z)$coefficients
    pcol = "Pr(>|t|)"
    
    ## create output matrix
    scols = c("model", "primary.p", "secondary.p", "secondary.var")
    res = matrix(NA, ncol=4, nrow=nrow(zsc)-2+(nrow(zsc)==2),
    	  	     dimnames=list(NULL, scols))
    res = data.frame(res, stringsAsFactors=F)
    res[, "model"] = modelname
    res[, "primary.p"] = zsc[2, pcol]    

    ## for multiple regression models, copy variables and ps
    if (nrow(zsc)>=3) {
      ## multiple regression, y vs x and w
      res[, "secondary.p"] = zsc[3:nrow(zsc), pcol]
      res[, "secondary.var"] = rownames(zsc)[3:nrow(zsc)]
    }
    
    res
  }

  result = list()
  result[[1]] = makeOneSummary(object$basemodel, "basemodel")
  for (m in names(object$models)) {
    result[[m]] = makeOneSummary(object$models[[m]], m)
  }

  data.frame(rbindlist(result), stringsAsFactors=F)  
}



