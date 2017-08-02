## Families of multiple regression models
## Visualization functions
##
## author: Tomasz Konopka



#' Produce a visual representation of a multiple regression
#'
#' @param x - a famr object
#' @param type - character, instruction as to what kind of chart to draw
#' @param label.pdiff - threshold above which to display text labels
#'                      on the pvalues chart
#' @param Rcss  - style object used by Rcssplot
#' @param Rcssclass - style class used by Rcssplot
#' @param ... - additional graphical arguments used in base plot,
#'              e.g. xlim and ylim values
#'
#' @export
plot.famr = function(x, type=c("scatter", "pvalues"),
	             label.pdiff=1, 
                     Rcss="famr", Rcssclass="famr", ...) {

  checkClass(x, "famr")

  # internal type is a single value (one of the specified options)
  type = match.arg(type)

  ## prep for Rcssplot
  RcssOverload()
  if (Rcss=="famr") {
     ## load the package default style sheet (from
     rcssfile = system.file("famr.Rcss", package="famr")
     famrRcss = Rcssplot::Rcss(rcssfile)
     RcssDefaultStyle = RcssGetDefaultStyle(famrRcss)     
  } else {
     RcssDefaultStyle = RcssGetDefaultStyle(Rcss)
  }
  RcssCompulsoryClass = RcssGetCompulsoryClass(Rcssclass)

  par()

  if (type=="scatter") {
    plotFamrScatter(x, ...)
  } else if (type=="pvalues") {
    plotFamrPvalues(x, ...)
  }

}



#' Plot a x-y scatter chart with linear models
#'
#' This is an internal function invoked through plot()
#'
#' @param x - famr object
#' @param ... - additional parameters passed on to graphics::plot()
#'
plotFamrScatter = function(x, ...) {

  checkClass(x, "famr")
  dots = list(...)
  RcssOverload()

  ## find out what variables to plot on the x and y axes
  xvar = x$vars[2]
  yvar = x$vars[1]
  ## get the values
  xvals = x$data[, xvar]
  yvals = x$data[, yvar]
  ## create a base plot with dots
  xrange = xplotrange = range(xvals, na.rm=TRUE)
  yrange = yplotrange = range(yvals, na.rm=TRUE)
  if ("xlim" %in% names(dots)) {
     xplotrange= dots$xlim
  }  
  if ("ylim" %in% names(dots)) {
    yplotrange = dots$ylim
  }
  plot(xplotrange, yplotrange, type="n", xlab="", ylab="", ...)
  axis(1, at=xplotrange, labels=c("", ""), tck=0, line=0, Rcssclass="x")
  axis(1, labels=NA, pos=yplotrange[1], Rcssclass="x")
  axis(1, lwd=0, Rcssclass="x")
  axis(2, at=yplotrange, labels=c("", ""), tck=0, line=0, Rcssclass="y")
  axis(2, labels=NA, pos=xplotrange[1], Rcssclass="y")
  axis(2, lwd=0, Rcssclass="y")  
  mtext(xvar, side=1, Rcssclass="xlab")
  mtext(yvar, side=2, Rcssclass="ylab")	

  ## add the data as points
  points(xvals, yvals)

  xmeans = apply(x$data, 2, function(z) {
    if (class(z) %in% c("numeric", "integer")) {
      return (mean(z, na.rm=TRUE))
    }
    # for non-numerical, give zero
    0    
  })

  ## helper: render one model on the plot
  drawOneModel = function(model, lineclass) {
    coeff = summary(model)$coefficients
    # the intercept will be the (intercept) plus contributions
    # from all the other variables (use mean as a filler)
    intercept = coeff["(Intercept)", "Estimate"]
    if (nrow(coeff)>2) {
      for (i in 3:nrow(coeff)) {
        iname = rownames(coeff)[i]
	islope = coeff[i, "Estimate"]
	if (iname %in% names(xmeans)) {
	  intercept = intercept + (islope*xmeans[iname])
	}
      }
    }
    slope = coeff[xvar, "Estimate"]
    lines(xrange, intercept + (xrange*slope), Rcssclass=lineclass)
  }
  ## draw all the multiple-regression models
  for (w in names(x$models)) {
    wm = x$models[[w]]
    drawOneModel(wm, "othermodels")
  }
  ## draw the base model
  wm = x$basemodel
  drawOneModel(wm, "basefit")
  
}


#' Plot a x-y scatter chart with model p-values
#'
#' This is an internal function invoked through plot()
#'
#' @param x - famr object
#' @param label.pdiff - numerical, determines how far
#'  away a dot has to be from base model to include a label
#' @param ... - additional parameters passed on to graphics::plot()
#'
plotFamrPvalues = function(x, label.pdiff=1.3, ...) {

  checkClass(x, "famr")
  dots = list(...)
  RcssOverload()

  ## get the summary data.frame
  xs = summary.famr(x)

  ## create the plot with p-value scales on axes
  xvar = x$vars[2]
  yvar = x$vars[1] 
  xvals = xs[, "primary.p"]
  yvals = xs[, "secondary.p"]
  ## create a base plot with dots
  xrange = xplotrange = c(0, max(-log10(xvals), na.rm=TRUE))
  yrange = yplotrange = c(0, max(-log10(yvals), na.rm=TRUE))
  if ("xlim" %in% names(dots)) {
     xplotrange= dots$xlim
  }  
  if ("ylim" %in% names(dots)) {
    yplotrange = dots$ylim
  }
  xplotlab = paste0("-log10 (", x$vars[2], " p-value)")
  yplotlab = paste0("-log10 (secondary p-value)")
   
  plot(xplotrange, yplotrange, type="n", xlab="", ylab="", ...)
  axis(1, at=xplotrange, labels=c("", ""), tck=0, line=0, Rcssclass="x")
  axis(1, labels=NA, pos=yrange[1], Rcssclass="x")
  axis(1, lwd=0, Rcssclass="x")	
  axis(2, at=yplotrange, labels=c("", ""), tck=0, line=0, Rcssclass="y")
  axis(2, labels=NA, pos=xrange[1], Rcssclass="y")
  axis(2, lwd=0, Rcssclass="y")
  mtext(xplotlab, side=1, Rcssclass="xlab")
  mtext(yplotlab, side=2, Rcssclass="ylab")	
  

  ## plot the dots associated with each model
  points(-log10(xs[,"primary.p"]), -log10(xs[,"secondary.p"]))

  ## plot the base line model as vertical line
  basep = xs[xs[,"model"]=="basemodel", "primary.p"]
  lines(rep(-log10(basep), 2), yrange, Rcssclass="basemodel")

  ## perhaps label some of the points
  basep = -log10(xs[1, "primary.p"])
  otherp = -log10(xs[, "primary.p"])
  secondp = -log10(xs[, "secondary.p"])
  which.lab = which(abs(otherp-basep)>label.pdiff | secondp>label.pdiff)
  if (length(which.lab)>0) {
    xs.lab = xs[, c("model", "secondary.var")]
    xs.lab = apply(xs.lab, 1, function(z) {
      paste(unique(z), collapse="; ")
    })    
    text(-log10(xs[which.lab, "primary.p"]),
    	 -log10(xs[which.lab, "secondary.p"]),
	 xs.lab[which.lab]
	 )
  }
  
}