#####################################
#    Gene selection (t test)        #
#####################################

A <- random.set(50,1000)

cond <- as.factor(sample(c('Normal', 'Disease'), 50, replace = TRUE))

pvalues <- apply(A, 1, function(x) t.test(x ~ cond) $p.value)
hist(pvalues)
order(pvalues)
signif_genes <- which(pvalues < 0.05)

A_p_sig <- A[(signif_genes),]
A_p_ns <- A[-(signif_genes),]
