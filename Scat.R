#####################################################################
## Code for "Whose Scat Is That? An 'Easily Digestible' Introduction 
## to Predictive Modeling in R and the caret Package" for the 
## R-Ladies London RUG. Files are at 
## https://github.com/topepo/R-Ladies-London

library(ggplot2)

theme_set(theme_bw())
plot_col <- scale_color_manual(values=c("#0072B2", "#D55E00", "#E69F00",
                                        "#56B4E9", "#009E73", "#F0E442", 
                                        "#000000", "#CC79A7"))

opts_chunk$set(comment=NA, 
               digits = 3, 
               size = 'scriptsize', 
               prompt = TRUE,
               cache = FALSE, 
               tidy = FALSE,
               message = FALSE,
               warning = FALSE,
               fig.path = "Figures/",
               background = 'white')
             
## A trick to make sure in-line code is formatted the same as in chunks             
hook_inline = knit_hooks$get('inline')
knit_hooks$set(inline = function(x) {
  if (is.character(x)) highr::hi_latex(x) else hook_inline(x)
})


###################################################################
## Slide 5: Load Data

library(caret)
data(scat)
str(scat)

###################################################################
## Slide 6: Missing Data Profile

# pct_nonmissing <- function(x) mean(!is.na(x))
# unlist(lapply(scat, pct_nonmissing))  # sapply(scat, pct_nonmissing)

count_nonmissing <- function(x) {
  sapply(x, function(x) {
    mean(!is.na(x))
  })
}
count_nonmissing(scat)

###################################################################
## Slide 8: Split the Data

set.seed(11218)
in_train <- createDataPartition(scat$Species, p = 3/4, list = FALSE)
head(in_train)
train_data <- scat[ in_train,]
test_data  <- scat[-in_train,]
## It isn't much data but it's better than nothing...
table(test_data$Species)

###################################################################
## Slide 9: Interaction Plot Code

int_plot <- function(dat, y, x, group) {
  library(plyr)
  if(!is.factor(dat[,group])) dat[,group] <- factor(dat[,group])
  means <- ddply(dat, c(y, group), 
                 function(obj) c(mean = mean(obj[,x], na.rm = TRUE)))
  ggplot(dat, 
         aes_string(y = x,  x = y, color = group, group = group)) + 
    geom_point(position = position_dodge(width = 0.2)) + 
    geom_line(data = means, aes_string(y = "mean")) +
    theme(legend.position = "top")
}

int_plot_v2 <- function(dat, x, y, group) {
  if(!is.factor(dat[,group])) dat[,group] <- factor(dat[,group])
  # Remove rows with missing data
  complete_ids <- complete.cases(dat[, c(x, y, group)])
  dat <- dat[complete_ids, ]
  ggplot(dat,
         aes_string(x = x, y = y, color = group, group = group)) +
    geom_point(position = position_dodge(width = 0.2)) +
    stat_summary(fun.y="mean", geom="line", aes_string(group = group)) + 
    theme(legend.position = "top")
}

###################################################################
## Slides 10-15

int_plot(train_data, y = "Species", x = "Mass", group = "ropey")
int_plot_v2(train_data, x = "Species", y = "Mass", group = "ropey")

int_plot(train_data, y = "Species", x = "Mass", group = "Location")

int_plot(train_data, y = "Species", x = "Mass", group = "segmented")

int_plot(train_data, y = "Species", x = "Length", group = "Location")

int_plot(train_data, y = "Species", x = "Length", group = "ropey")

int_plot(train_data, y = "Species", x = "CN", group = "ropey")

###################################################################
## Slide 21: An Initial Model Specification

small_form <- paste("Species ~ Month + Year + Site + Age +",
                    "Number + Length*ropey + (Location + segmented)*Mass + ",
                    "flat + scrape")
small_form <- as.formula(small_form)

small_tr_dat <- train_data[, all.vars(small_form)]
small_tr_dat <- small_tr_dat[complete.cases(small_tr_dat),]

###################################################################
## Slide 29: Multinomial Model

ctrl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE)
set.seed(2592) ## locks in the resamples
mnr_tune <- train(small_form, data = small_tr_dat,
                  method = "multinom", 
                  preProc = c("center", "scale"),
                  ## avoid regularization for now
                  tuneGrid = data.frame(decay = 0),
                  trControl = ctrl,
                  ## this next argument is passed to `multinom`
                  trace = FALSE)
###################################################################
## Slide 30: Multinomial Model

mnr_tune
predict(mnr_tune, head(test_data)) ## or type = "prob"

###################################################################
## Slide 31: Multinomial Model Variable Importance

print(varImp(mnr_tune, scale = FALSE), top = 10)

###################################################################
## Slide 34: Multinomial Model -- All Data

full_form <- paste("Species ~ Month + Year + Site + Age + Number + ",
                   "Length*ropey + (Location + segmented)*Mass + ",
                   "flat + scrape + ",
                   "TI + d13C + d15N + CN + Diameter + Taper")
full_form <- as.formula(full_form)
set.seed(2592) 
mnr_impute <- train(full_form, data = train_data,
                    method = "multinom",
                    ## Add imputation to the list of pre-processing steps
                    preProc = c("center", "scale", "knnImpute", "zv"),
                    tuneGrid = data.frame(decay = 0),
                    trControl = ctrl,
                    ## do not remove missing data before modeling
                    na.action = na.pass,
                    trace = FALSE)

mnr_impute

###################################################################
## Slide 36: Multinomial Model -- All Data Variable Importance

print(varImp(mnr_impute, scale = FALSE), top = 10)

###################################################################
## Slide 37: Resampled Confusion Matrix

confusionMatrix(mnr_tune)

###################################################################
## Slide 44: Model Tuning Grid

glmn_grid <- expand.grid(alpha = c(0.05, seq(.1, 1, by = 0.025)),
                         lambda = c(.001, .01, .1))
nrow(glmn_grid)

###################################################################
## Slide 45: Model Tuning 

set.seed(2592) ## use the same resamples as mnr_impute
glmn_tune <- train(full_form, data = train_data,
                   method = "glmnet", 
                   preProc = c("center", "scale", "knnImpute", "zv"),
                   ## pass in the tuning grid
                   tuneGrid = glmn_grid,
                   ## pick the sub-model with the best kappa value
                   metric = "Kappa",
                   na.action = na.pass,
                   trControl = ctrl)
## best sub-model results:
glmn_tune$bestTune
getTrainPerf(glmn_tune)

###################################################################
## Slide 46: Model Tuning Plot

theme_set(theme_bw())
ggplot(glmn_tune) + theme(legend.position = "top")

###################################################################
## Slide 47: Resampled Confusion Matrix

confusionMatrix(glmn_tune)

###################################################################
## Slide 49: Model Comparison

compare_models(glmn_tune, mnr_impute, metric = "Kappa")

###################################################################
## Slide 50: Fitting Other Models

set.seed(2592) 
bagged_tree <- train(Species ~ ., data = train_data,
                     method = "treebag", 
                     metric = "Kappa",
                     na.action = na.pass,
                     trControl = ctrl)
getTrainPerf(bagged_tree)

###################################################################
## Slide 51: Fitting Other Models

set.seed(2592) 
knn_tune <- train(Species ~ ., data = train_data,
                  method = "knn", 
                  preProc = c("center", "scale", "knnImpute", "zv"),
                  ## pass in the tuning grid _size_
                  tuneLength = 20,
                  metric = "Kappa",
                  na.action = na.pass,
                  trControl = ctrl)
getTrainPerf(knn_tune)

###################################################################
## Slide 52: Preprocessing Too

set.seed(2592) 
transformed <- train(full_form, data = train_data,
                   method = "glmnet", 
                   ## Also transform the predictors
                   preProc = c("center", "scale", "knnImpute", 
                               "zv", "YeoJohnson"),
                   tuneGrid = glmn_grid,
                   metric = "Kappa",
                   na.action = na.pass,
                   trControl = ctrl)
getTrainPerf(transformed)

###################################################################
## Slide 53: Collecting the Results

rs <- resamples(list(knn = knn_tune, bagged = bagged_tree,
                     multinomial = mnr_impute, glmnet = glmn_tune,
                     "glmnet + trans" = transformed))
summary(rs, metric = "Kappa")

###################################################################
## Slide 54: Resampling Distributions

bwplot(rs, metric = "Kappa")

###################################################################
## Slide 55: Resampling Distributions

dotplot(rs, metric = "Kappa")

###################################################################
## Slide 56: Test Set Results

test_pred <- predict(glmn_tune, newdata = test_data, na.action = na.pass)
str(test_pred)
test_prob <- predict(glmn_tune, newdata = test_data, 
                     na.action = na.pass, type = "prob")
str(test_prob)

###################################################################
## Slide 57: Test Set Results

confusionMatrix(test_pred, test_data$Species)
