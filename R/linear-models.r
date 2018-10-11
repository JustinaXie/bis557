
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats lm
#' @importFrom stats terms
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  #Generate the design matrix of X
  X<-model.matrix(formula, data)
  
  #Find y from the formula
  y<-data[,all.vars(formula)[1]]
  
  #Apply QR method to find beta and change beta to the same form as in lm
  beta<-qr.solve(X,y)
  beta[which(beta==0)] <- NA
  
  #Initiate a model_fit list which has the same class characteristics as lm
  model_fit<-list()
  
  #Parameters in model_fit list
  #Reference: https://www.rdocumentation.org/packages/stats/versions/3.5.1/topics/lm
  model_fit$coefficients <- beta
  model_fit$residuals<- y-X%*%beta
  model_fit$fitted.values<-X%*%beta
  model_fit$rank <- ncol(X) 
  model_fit$weights <- NULL
  model_fit$df.residuals <- nrow(X) - ncol(X) #n-p
  model_fit$qr <- qr(X)
  model_fit$call <- call('lm',formula)
  model_fit$terms <- terms(x = formula,data = data)
  model_fit$contrasts <- NULL
  model_fit$xlevels <- NULL
  model_fit$offset <-NULL
  model_fit$y <- y
  model_fit$x <- X
  model_fit$model = formula
  model_fit$na.action <- NULL
  
  #Cast the class of model_fit to lm
  class(model_fit)<-"lm"
  
  return(model_fit)
}
