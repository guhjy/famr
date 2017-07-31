## A collection of helper functions - checks on input objects
##
## author: Tomasz Konopka


## require object to be of certain class
checkClass = function(x, cname, msg=NULL) {
  if (is.null(msg)) {
    msg = paste0("object is of improper class (expected ", cname, ")")
  }
  if (class(x)!=cname) {
    stop(msg, "\n")
  }
}



## require data to be nice matrix with column names
checkDataMatrix = function(data) {
  dfc = class(data)
  if (dfc!="matrix" & dfc!="data.frame") {
    stop("input data must be a data frame or matrix\n")
  }
  if (is.null(colnames(data))) {
    stop("input data must have column names\n")
  }
}


## require data matrix to have specified column
checkMatrixColumns = function(data, cols) {
  missing = cols[!cols %in% colnames(data)]
  if (length(missing)>0) {
    misscols = paste(missing, collapse=", ")
    stop(paste0("input data has missing columns: ", misscols, "\n"))
  }  
}


# require that a list have nontrivial names
checkNamedList = function(ll) {
  if (class(ll)=="list") {
    if (is.null(names(ll))) {
      stop("list must have non-null names\n")
    }
    lnames = names(ll)
    if ("" %in% lnames) {
      stop("list must have non-empty names\n")
    }
    if (length(unique(lnames)) != length(ll)) {
      stop("list must contain unique names\n")
    }
  }
}


# require that a vector has all 
checkUnique = function(x) {
  if (length(x)!=length(unique(x))) {
    stop("vector is not unique")
  }
}



# require that objects have specified length
checkLength = function(x, xlen, msg=NULL) {
  if (is.null(msg)) {
    msg = paste0("object of incorrect length (expected length ", xlen, ")")
  }
  if (length(x)!=xlen) {
    stop(msg, "\n")
  }
}



