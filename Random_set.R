##################################################
#    Data matrix generation ('random.matrix')    #
##################################################

## Random generation of gene-associated values
## - for n_subj (number of subjects)
## - for n_genes (number of genes)
## Random allocation of 2 conditions ('Normal', 'Disease)
## Matrix orientation: columns - subjects

random.set <- function(n_subj, n_genes){
    random.genes <- matrix(rnorm(n_subj*n_genes), ncol = n_subj)                    
}

