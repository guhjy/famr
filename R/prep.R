## Helper functions applied before or during a famr analysis
##
## author: Tomasz Konopka


# Look at an object and get strings suitable for column names
#
# @param z - a character vector or list
#
# (this is an internal helper function)
getNames = function(z) {
  zc = class(z)
  if (zc=="character") {
    return(z)	
  } else if (zc=="list") {
    checkNamedList(z)
    return(names(z))
  }
}



#' Prepare a dataset for mr analysis.
#'
#' The main functionality here is to allow applying functions
#' on the input data.
#'
#' @param data - see help for famr()
#' @param y - see help for famr()
#' @param x - see help for famr()
#' @param models - see help for famr()
#'
#' (this is an internal helper function)
famrPrep = function(data, y, x, models=NULL) {

  ## prepare a new matrix for the data
  xname = getNames(x)
  checkLength(xname, 1, "specification of x must be of length 1\n")
  yname = getNames(y)
  checkLength(yname, 1, "specification of y must be of length 1\n")
  if (is.null(models)) {
    models = famrModels(data, avoid=c(xname, yname))
  }
  ## collect all model names and all variables that appear in the
  ## alternative models
  modelnames = names(models)
  varnames = unique(unlist(lapply(models, names)))

  checkUnique(c(xname, yname, varnames))
  result = data.frame(matrix(0, ncol=2+length(varnames), nrow=nrow(data)))
  colnames(result) = c(yname, xname, varnames)
  if (!is.null(rownames(data))) {
    rownames(result) = rownames(data)
  }

  ## helper: compute values for the result matrix
  getColData = function(data, cfun) {
    if (class(cfun)=="function") {
      return(cfun(data))
    } else {
      if (length(cfun)==1) {
        return(data[, cfun])
      } else {
        stop("variable definition in model of length > 1\n")
      }
    }
  }
  
  ## create a results object with all the variables in separate columns
  result[, yname] = getColData(data, y[[1]])
  result[, xname] = getColData(data, x[[1]])
  for (altmodel in modelnames) {
    for (varname in names(models[[altmodel]])) {
      result[, varname] = getColData(data, models[[altmodel]][[varname]])
    }
  }

  result
}



#' Prepare an object with a definition of models
#'
#' @param data - data frame with column names
#' @param avoid - vector with columns to avoid 
#' @return a nested list, suitable for passing as
#'         'other' into function famr()
#'
#' @export
famrModels = function(data, avoid=NULL) {

  result = list()

  ## create models 
  for (d1 in colnames(data)) {
    temp = list()
    temp[[d1]] = d1
    result[[d1]] = temp
  }

  ## remove some models if present in avoid
  result = result[!names(result) %in% avoid]

  result
}