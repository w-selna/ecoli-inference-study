connection_lasso.matrix = matrix(nrow = 40,ncol = 40)
colnames(connection_lasso.matrix) <- final
rownames(connection_lasso.matrix) <- final
cvplots = list()#not the best initialization but easier to fill
corrlist = list()
for (r in 1:nrow(master_40.matrix))
{
  x_train = master_40.matrix[-r,c(sort(basetestcols),97,98,99,100,101,102,103,107,108)] #train columns 71 of 108 evenly distributed among control, and the 2 antibiotioc states.
  y_train = master_40.matrix[r,c(sort(basetestcols),97,98,99,100,101,102,103,107,108)] # test on the rest
  x_test = master_40.matrix[-r,-c(sort(basetestcols),97,98,99,100,101,102,103,107,108)]
  y_test = master_40.matrix[r,-c(sort(basetestcols),97,98,99,100,101,102,103,107,108)]
  
  lasfit = glmnet(t(x_train),y_train)# 
  cvfit = cv.glmnet(t(x_train),y_train)
  cvplots[[length(cvplots)+1]] <- plot(cvfit)
  lam_min = cvfit$lambda.1se
  
  coefs_minlam = coef(cvfit, s = lam_min)#put into connection matrix with this
  test_coefs = t(as.matrix(coef(cvfit, s = lam_min)[2:40,]))
  if (r == 1| r == 40)
  {
    if (r == 1){
      actual_coefs = t(rbind(as.matrix(empty_namedmatrix[,1]),as.matrix(test_coefs[,1:39])))
    }else{
      actual_coefs = t(rbind(as.matrix(test_coefs[,1:39]),as.matrix(empty_namedmatrix[,40])))
    }
  }else{
    actual_coefs = t(rbind(as.matrix(test_coefs[,1:r-1]),as.matrix(empty_namedmatrix[,r]),as.matrix(test_coefs[,r:39])))
  }
  links.matrix = matrix(nrow = 1,ncol = 40)
  colnames(links.matrix) <- final
  for (n in 1:ncol(actual_coefs))
  {  
    if (actual_coefs[1,n] != 0)
    {
      if(sign(actual_coefs[1,n]) == 1)
      {
        links.matrix[1,n] = 1
      }else{
        links.matrix[1,n] = -1
      }
    }else{
      links.matrix[1,n] = 0
    }
  }
  links.matrix
  connection_lasso.matrix[r,] = links.matrix
  
  Pred <- predict(cvfit, t(x_test),s = lam_min)
  corrlist[[length(corrlist)+1]] <- cor(Pred,y_test) #go back and print out best one
  #plot(Pred,y_test,xlab = "predicted", ylab = "observed" ) # add line and r^2 to plot with title
  #abline(lne <- lm(y_test~Pred), col="red")
  #legend("bottomright", bty="n", legend=paste("r^2 is",format(summary(lne)$adj.r.squared, digits=3)))
}
diag(connection_lasso.matrix) = diag(connection.matrix)