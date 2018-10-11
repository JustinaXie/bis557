
#' Fit a ridge regression model
#'
#' @description This function passes parameters to the ridge_reg function.
#' @param formula a formula
#' @param data a data.frame
#' @param lambda a penalty value
#' @return An ridge_reg object
#' @importFrom stats model.frame
#' @importFrom stats model.matrix
#' @examples
#' fit <- ridge_reg(Sepal.Length ~Sepal.Width+Petal.Length+Petal.Width , iris,lambda = 1)
#' summary(fit)
#' @export

ridge_reg<-function(formula,lambda,data){
  #Obtain X and y from data
  X<-model.matrix(formula,data)
  y<-model.frame(formula,data)[,1]

  #SVD decomposition of X
  svd_obj<-svd(X)
  U<-svd_obj$u
  V<-svd_obj$v
  svals<-svd_obj$d
  D<-diag(svals/(svals^2+lambda))
  
  #Compute betas
  beta<-V%*%D%*%t(U)%*%y
  rownames(beta)<-colnames(X)
  
  ret<-list(coef = beta)
  
  #Cast class into ridge_reg
  class(ret)<-"ridge_reg"
  ret
}
