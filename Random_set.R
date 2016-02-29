##################################################
#    Data matrix generation ('random.matrix')    #
##################################################

## Random generation of gene-associated values
## - for n_subj (number of subjects)
## - for n_genes (number of genes)
## Random allocation of 2 classes ('Normal', 'Disease)
## Matrix orientation: columns - subjects

random.set <- function(n_subj, n_genes){
    random.genes <- matrix(rnorm(n_subj*n_genes), ncol = n_subj)                        #Creamos una matriz con numeros sacado al azar de una distribucion normal (rnorm)
    return(random.matrix)   
    
    class <- as.factor(sample(c('Normal', 'Disease'), n_subj, replace = TRUE))
}

