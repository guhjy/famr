## Tests for data preperation transformation
## i.e. mainly function famrPrep()


## some data elements used in several test_that blocks
i3x4 = data.frame(matrix(1:12, ncol=4, nrow=3))
colnames(i3x4) = letters[1:4]
rownames(i3x4) = as.character(1:3)

i3x5 = matrix(1:15, ncol=5, nrow=3)
colnames(i3x5) = letters[1:5]
rownames(i3x5) = as.character(1:3)
  


test_that("dataprep no functions",
  {
    ## compute the output
    output = famrPrep(i3x4, "b", "a")
    ## expected matrix will be similar to input, but
    ## with columns re-arranged (y,x,other)
    expected = i3x4[, c("b", "a", "c", "d")]
    ## compare
    expect_equal(output, expected)
  })
  



test_that("building models without avoid",
  {
    ## compute all alt models
    output = famrModels(i3x4)
    ## expected output is a list of lists
    expected = list(a=list(a="a"), b=list(b="b"), c=list(c="c"), d=list(d="d"))
    ## compare
    expect_equal(output, expected)
  })




test_that("building models with avoid",
  {
    ## compute all alt models
    output = famrModels(i3x4, avoid=c("a", "b"))
    ## expected output is a list of lists
    expected = list(c=list(c="c"), d=list(d="d"))
    ## compare
    expect_equal(output, expected)
  })




test_that("dataprep with other=list",
  {
    ## create a list of functions for manipulating input
    ofun1 = function(x) {
      x[,"c"]+x[,"d"]
    }
    ofun2 = function(x) {
      x[,"c"]-x[,"d"]
    }    
    models = list(f1=list(f1=ofun1), f2=list(f2=ofun2))
    ## manually construct expected output matrix
    expected = data.frame(cbind(i3x4[, c("a", "b")], "f1"=0, "f2"=0))
    expected[,"f1"] = ofun1(i3x4)
    expected[,"f2"] = ofun2(i3x4)
    ## calculate actual output
    output = famrPrep(i3x4, "a", "b", models)
    ## compare
    expect_equal(output, expected)
  })




test_that("dataprep with x and y as lists",
  {
    ## create a list of functions for manipulating input
    afun = function(x) {
      x[,"a"]+1
    }
    bfun = function(x) {
      x[,"b"]-1
    }
    alist = list(aa=afun)
    blist = list(bb=bfun)
    ## calculate actual output
    altmodels = famrModels(i3x5, avoid=c("a", "b", "e"))
    output = famrPrep(i3x5, alist, blist, altmodels)
    ## manually construct expected output matrix
    expected = data.frame(cbind(aa=0, bb=0, i3x5[, c("c", "d")]))
    expected[,"aa"] = afun(i3x5)
    expected[,"bb"] = bfun(i3x5)
    ## compare
    expect_equal(output, expected)
  })




