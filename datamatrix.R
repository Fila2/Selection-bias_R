############################################
#    Data matrix generation ('rawdata')    #
############################################

## Setting up number of rows ('r') and columns ('c'). If these values
## should change we will only need to adjust them here 

r <- 50
c <- 1000

## Generation of matrix labels for rows and files 

collabel_prefix <- 'Gene'
collabel_sufix <- seq(1:c)
collabel <- paste(collabel_prefix, collabel_sufix, sep = "#")
rowlabel_prefix <- 'Subject'
rowlabel_sufix <- seq(1:r)
rowlabel <- paste(rowlabel_prefix, rowlabel_sufix, sep = "#")

## Generation of a random data matrix. 
## The uniform distribution with positive numbers between 0 and 1 
## has been adopted. 

rawdata <- matrix(runif(r*c), nrow = r, ncol = c, dimnames = list(rowlabel, collabel))

