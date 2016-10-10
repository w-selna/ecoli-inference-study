bs <- row.names(base_expr.matrix)
sig <- list()
p_value = 0.0000000000745
listlength <- 1
for (i in 1:nrow(base_expr.matrix))
{
  ret <- t.test(base_expr.matrix[i,1:95],Nx_expr.matrix[i,1:10])
  if(ret$p.value<= p_value){
    sig[[listlength]] <- bs[i]
    listlength = listlength + 1
  }
}

sig2 <- list()
listlength2 <- 1
for (k in 1:nrow(base_expr.matrix))
{
  ret <- t.test(base_expr.matrix[k,1:95],AmNx_expr.matrix[k,1:3])
  if(ret$p.value<= p_value){
    sig2[[listlength2]] <- bs[k]
    listlength2 = listlength2 + 1
  }
}
final <- intersect(sig,sig2)