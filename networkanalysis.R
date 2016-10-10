lnks_linear = c()
lnks_subset = c()
lnks_lasso = c()
for (i in 1:40)
{
  lnks_linear[[length(lnks_linear)+1]] <- nnzero(connection_adjusted.matrix[i,])
  lnks_subset[[length(lnks_subset)+1]] <-nnzero(connection_subsetselection.matrix[i,])
  lnks_lasso[[length(lnks_lasso)+1]] <-nnzero(connection_lasso.matrix[i,])
}

var(lnks_linear)
var(lnks_subset)
var(lnks_lasso)