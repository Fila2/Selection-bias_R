## required packages

library(randomForest)
library(ROCR)

#####################
###   FUNCTIONS   ###
#####################


###### random_genes() ######

## This function will generate a matrix with random values for the variable
##'genes'. It needs the arguments 'number of genes' (n_genes) and 'number of 
##'subjects' (n_subjects)

random_genes <- function(n_subj, n_genes){
    set.seed(1)
    genes <- matrix(round(rnorm(n_subj*n_genes, 0, 10), 3), ncol = n_genes)
    return(genes)
    }

###### random_class() ######

## This function will generate a vector with arandom permutation (with 
## replacement) of the 2 possible categories of the variable 'condition' (cond):
## 'Normal' and 'Disease'
    
random_class <- function(n_subj){
    cond <- factor(sample(c('Normal', 'Disease'), n_subj, replace = TRUE))
    return(cond)
}

###### p_filter() ######

## This function will generate a submatrix (sel_genes) filtered by 
## extreme gene values. Requires two arguments: the random matrix ('genes')
## and the threshold for the p-value-based filter ('p_threshold')

p_filter <- function(genes, p_threshold){
    pvalues <- apply(genes, 2, function(x) t.test(x ~ cond) $p.value)
    signif_genes <- which(pvalues < p_threshold)
    sel_genes <- genes[,(signif_genes)]
    return(sel_genes)
}


###### klist() ######

## This function will generate a k-number-based index-vector 
## (Venables & Ripley, "S programming", 2000, Springer, p. 175), using repe

klist <- function(n_subj, knumber){
    index <- sample(rep(1:knumber,
                    length = n_subj),
                n_subj, replace = FALSE)
    return(index)
}

###### train.subset() ######

## This function will define the training subset of the study sample ('dataset'),
## according to the index.select list:
##      subject's index != k number

train.subset <- function(dataset, knumber, index){
    for(sample.number in 1:knumber)
        subset.train <- dataset[index != sample.number,]
    return(subset.train)
}

###### test.subset() ######

## This function will define test subset of the study sample,
## according to the index.select list:
##      subject's index == k number

test.subset <- function(dataset, knumber, index){
    for(sample.number in 1:knumber)
        subset.test <- dataset[index == sample.number,]
    return(subset.test)
}



#################################
###      MAIN SCRIPT          ###
#################################

## Default values for the arguments of the functions:

n_subj <- 50            ## Number of subjects / samples
n_genes <- 1000         ## Number of variables ('genes')
p_threshold <- 0.05     ## p-value for variable filtering
knumber <- 10           ## k-fold value


## Call the functions random_genes() and random_class(). We will get two
## vectors: a matrix and a list of categories (the classes)

genes <- random_genes(n_subj, n_genes)
cond <- random_class(n_subj)

## Call the function p_filter() to generate a submatrix of 'genes'
## filtered by p-value (it requires to define the filtering threshold)

sel_genes <- p_filter(genes, p_threshold)

## Generate (and store) the working data frames:

study_data <- data.frame(cond,
                         genes)


write.table(study_data, file = 'study_data.txt', col.names = TRUE,
            row.names = FALSE, sep = "\t", quote = FALSE)

sel_data <- data.frame(cond,
                sel_genes)

write.table(sel_data, file = 'sel_data.txt', col.names = TRUE,
            row.names = FALSE, sep = "\t", quote = FALSE)



## Generation of train and test subsets for the original and 
## the filtered study samples:

index <- klist(n_subj, knumber)

study_data.train <- train.subset(study_data, knumber, index)
study_data.test <- test.subset(study_data, knumber, index)
sel_data.train <- train.subset(sel_data, knumber, index)
sel_data.test <- test.subset(sel_data, knumber, index)


## Random Forest for training subsets, with default mtry
## (square root of the number of predicting variables) 

sel_data.train.rf <- randomForest(sel_data.train[-1], 
                                  y = sel_data.train$cond, 
                                  ntree=1000, 
### to be discussed later         mtry = bestmtry_data_sel,
                                  keep.forest = TRUE,
                                  importance = TRUE,
                                  do.trace = 100)

study_data.train.rf <- randomForest(study_data.train[-1], 
                                    y = study_data.train$cond, 
                                    ntree=1000, 
### to be discussed later           mtry = bestmtry_data,
                                    importance = TRUE,
                                    do.trace = 100)

## Confusion tables for each of the datasets (pre-filtered and original)

print(sel_data.train.rf$confusion)
print(study_data.train.rf$confusion)

## OOB error rate estimate

oob_err_sel_data <- summary(sel_data.train.rf$err.rate)[3,1]
oob_err_study_data <- summary(study_data.train.rf$err.rate)[3,1]
oob_list_sel_data <- ('')
oob_list_sel_data <- c(oob_list_sel_data, oob_err_sel_data)


## Predicting classes in the tests subsets

sel_data.test.predict_resp <- predict(sel_data.train.rf, sel_data.test, 
                                      'response', norm.votes = TRUE)
sel_data.test.predict_votes <- predict(sel_data.train.rf, sel_data.test, 
                                       'prob', norm.votes = TRUE)

study_data.test.predict_resp <- predict(study_data.train.rf, study_data.test,
                                        'response', norm.votes = TRUE)
study_data.test.predict_votes <- predict(study_data.train.rf, study_data.test,
                                         'prob', norm.votes = TRUE)

## observed-predicted tables

table(sel_data.test$cond, predicted = sel_data.test.predict_resp)
table(study_data.test$cond, predicted = study_data.test.predict_resp)

## Calculation of the AUC score value

study_data.predictions <- as.vector(study_data.train.rf$votes[,2])
study_data.pred <- prediction(study_data.predictions, study_data.train$cond)
sel_data.predictions <- as.vector(sel_data.train.rf$votes[,2])
sel_data.pred <- prediction(sel_data.predictions, study_data.train$cond)

perf_AUC_sel <- performance(sel_data.pred,"auc")
AUC_sel <- perf_AUC_sel@y.values[[1]]
perf_ROC_sel <- performance(sel_data.pred,"tpr","fpr") 

perf_AUC_study <- performance(study_data.pred, 'auc')
AUC_study <- perf_AUC_study@y.values[[1]]
perf_ROC_study <- performance(study_data.pred, 'tpr', 'fpr')

plot(perf_ROC_sel, main="ROC plot for Random Forest (pre-filtering)", 
     col = 2, lwd = 3)
text(0.5,0.5, paste("AUC = ", format(AUC_sel, digits=5, scientific=FALSE)))
abline(a=0,b=1,lwd=2,lty=2,col="gray")

plot(perf_ROC_study, main='ROC plot for Random Forest (no pre-selection)', 
     col = 3, lwd = 3)
text(0.5,0.5, paste("AUC = ", format(AUC_study, digits=5, scientific=FALSE)))
abline(a=0,b=1,lwd=2,lty=2,col="gray")


## Influence of the number of variables

## a) Plotting the importance of the variable according to Giny index

varImpPlot(study_data.train.rf,type=2)


## b) Tune Random Forest to obtain optimal mtry (No. of independent or predictor 
##    variables to be sampled at each split)

bestmtry_data_sel <- tuneRF(sel_data.train[-1], sel_data.train$cond, ntreeTry=1000, 
                            stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE,
                            dobest=TRUE)

bestmtry_data <- tuneRF(study_data.train[-1], study_data.train$cond, ntreeTry=1000, 
                        stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE,
                        dobest=FALSE)


###############  AÑADIR EJEMPLOS ########################
#### (with this information we could simulate 3 experiments, modifying 
#### the mtry value to bestmtry, bestmtry/2 and 2*bestmtry) 


## c) Random Forest cross-validation (assessing error and number of variables)

rf.cv <- rfcv(study_data.train, study_data.train$cond, cv.fold=5, recursive = TRUE)
with(rf.cv, plot(n.var, error.cv, col = 'red', lwd = 2, main = "CV error and 
                 number of variables"))


## CROSS_VALIDATION ERROR VARIABILITY
## (convertir en función!!!)

oob_err_sel_list <-list()
oob_err_study_list <-list()
nrep <- 10  ## number of repetitions

for(i in seq(1:nrep)){
    index <- klist(n_subj, knumber)
    print(index)

    ## Generation of train and test subsets for the original and 
    ## the filtered study samples:
    
    study_data.train <- train.subset(study_data, knumber, index)
    study_data.test <- test.subset(study_data, knumber, index)
    sel_data.train <- train.subset(sel_data, knumber, index)
    sel_data.test <- test.subset(sel_data, knumber, index)
    
    
    ## randomForest() for training subsets, default mtry (square root of
    ## the number of predicting variables) 
    
    sel_data.train.rf <- randomForest(sel_data.train[-1], 
                                      y = sel_data.train$cond, 
                                      ntree=1000, 
                                      keep.forest = TRUE,
                                      importance = TRUE)
    
    study_data.train.rf <- randomForest(study_data.train[-1], 
                                        y = study_data.train$cond, 
                                        ntree=1000, 
                                        importance = TRUE)
    
    ## OOB error rate estimate (median values)
    
    oob_err_sel_data <- median(sel_data.train.rf$err.rate[,1])
    oob_err_study_data <- median(study_data.train.rf$err.rate[,1])
    oob_err_sel_list <- c(oob_err_sel_list, oob_err_sel_data)
    oob_err_study_list <- c(oob_err_study_list, oob_err_study_data)
}

## Generation of a matrix with the output of the iteration (OOB errors)

oob_err_sel <- as.numeric(oob_err_sel_list)
oob_err_study <- as.numeric(oob_err_study_list)
oob_err <- matrix(cbind(oob_err_sel, oob_err_study), ncol = 2, byrow = FALSE)
colnames(oob_err) <- c('Pre-filtering', 'Non pre-filtering')

## Plotting the results

boxplot.matrix(oob_err, use.cols = TRUE, main = 'OOB error estimates variability')

