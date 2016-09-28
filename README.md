# R-Ladies-London

Code and Slides for "Whose Scat Is That? An 'Easily Digestible' Introduction to Predictive Modeling in R and the caret Package"


# Required Packages

```r
pkgs <- c("caret", "nnet", "glmnet", "RANN", "ipred")
for(i in pkgs) 
  install.packages(i, dependencies = c("Depends", "Imports", "Suggests"))
```

Please make sure that the most recent version of `caret` (6.0-71 or greater) is installed; the data are in that version. 

If there are any issues getting things installed, let me know at `mxkuhn@gmail.com`. 