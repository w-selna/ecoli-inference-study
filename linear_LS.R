connection_adjusted.matrix = matrix(nrow = 40,ncol = 40)
colnames(connection_adjusted.matrix) <- final
rownames(connection_adjusted.matrix) <- final
problem <- list()
list_lenght = 1
for (x in 1:nrow(master_40.matrix))
{
  for (y in 1:nrow(master_40.matrix))
  {
    if(x == y)
    {
      base_mean = mean(master_40.matrix[x,1:95])
      antiNx_mean = mean(master_40.matrix[x,96:105])
      antiAmNx_mean = mean(master_40.matrix[x,106:108])
      mean_diff_Nx = antiNx_mean - base_mean
      mean_diff_AmNx = antiAmNx_mean - base_mean
      if (mean_diff_Nx*mean_diff_AmNx > 0)
      {
        connection_adjusted.matrix[x,y] <- sign(mean_diff_Nx)
      }else{
        problem[list_lenght] = final[x]
        list_lenght = list_lenght + 1
      }
    }else{
      x_1 = master_40.matrix[x,1:108]
      y_1 = master_40.matrix[y,1:108]
      correlation = cor(x_1,y_1)
      fit = lm(y_1 ~ x_1)
      interval = confint(fit,level = (1-(.05/780)))
      if (interval[2,1]*interval[2*2] > 0)
      {
        connection_adjusted.matrix[x,y] = sign(correlation)
      }else{
        connection_adjusted.matrix[x,y] = 0
      }
    }
  }
}
