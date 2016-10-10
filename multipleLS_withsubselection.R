connection_subsetselection.matrix = matrix(nrow = 40,ncol = 40)
colnames(connection_subsetselection.matrix) <- final
rownames(connection_subsetselection.matrix) <- final
for (col in 1:ncol(master_40_t.matrix))
{
  models <- regsubsets(x = master_40_t.matrix[,-col],y = master_40_t.matrix[,col],nvmax = 39)
  ind <- which.min(summary(models,all.best = TRUE)$bic)
  bool_matrix = t(as.matrix(summary(models,all.best = TRUE)$which[ind,-1]))
  coef_matrix = t(t(as.matrix(coef(models,ind)))[,-1])
  for(c in 1:length(bool_matrix))
  {
    for(k in 1:ncol(connection_subsetselection.matrix))
    {
      if (colnames(bool_matrix)[c] == colnames(connection_subsetselection.matrix)[k])
      {
        if (bool_matrix[1,c] == TRUE)
        {
          for (cb in 1:length(coef_matrix))
          {
            if (colnames(coef_matrix)[cb] == colnames(bool_matrix)[c])
            {
              if (sign(coef_matrix[1,cb]) == 1)
              {
                connection_subsetselection.matrix[col,k] = 1
              }else{
                connection_subsetselection.matrix[col,k] = -1
              }
            }
          }
        }else{
          connection_subsetselection.matrix[col,k] = 0
        }  
      }
    }
  }
}
diag(connection_subsetselection.matrix) = diag(connection.matrix)
