
#' S4 Class: sparse.matrix
#'
#' @slot dims dimension
#' @name sparse.matrix
#' @param x value
#' @return a sparse.matrix object
#' @rdname sparse.matrix-methods
#' @importFrom methods new
#' @export sparse.matrix
#' @docType methods
#' 

setClass("sparse.matrix", contains = "data.frame", slots = c(dims = "vector"))
sparse.matrix<-function(i,j,x,dims=NULL){
  if(is.null(dims)){
    dims<-c(max(i),max(j))
  }
  df<-data.frame(i = i, j = j , x = x)
  df<-df[order(df$i),]
  row.names(df)=NULL
  a1<-new("sparse.matrix",df,dims = dims)
  return(a1)
}

#' Transpose method for sparse matrix
#‘
#' @param y,e1,e2 a \code{sparse.matrix} object
#' @docType methods
#' @rdname sparse.matrix-methods
#' @aliases t, sparse.matrix,ANY-method
#' @usage \S4method{t}{sparse.matrix}(x)
#' @inheritParams x from the transpose of matrix
#'


setMethod("t",signature="sparse.matrix",definition=function(x){
    dims<-c(x@dims[2],x@dims[1])
    temp<-x$i
    x$i<-x$j
    x$j<-temp
    x<-x[order(x$i),]
    row.names(x)=NULL
    c<-sparse.matrix(x$i,x$j,x$x,dims)
    return(c)})


#' Multiplication method for sparse matrix
#'
#' @docType methods
#' @rdname sparse.matrix-methods
#' @aliases %*%, sparse.matrix,sparse.matrix-method
#' @usage \S4method{%*%}{sparse.matrix}(x,y)
#' @inheritParams x,y from '%*%' in matrix multiplication
#' 

setMethod("%*%",
          signature(x= "sparse.matrix", y = "sparse.matrix"),
          function(x, y){
            if(x@dims[2] == y@dims[1]){
              dims<-c(x@dims[1],y@dims[2])
              a <- data.frame(i = x$i, j = x$j, x = x$x)
              # index arange of a
              ida<-numeric(a[nrow(a),1]+1)
              ida[1]<-1
              for(ra in 2:nrow(a)){
                if(a[ra-1,1]!=a[ra,1]){
                  ida[a[ra,1]]<- ra
                }
              }
              ida[length(ida)] <- nrow(a)+1
              
              # index range of b
              y<-t(y)
              b<-data.frame(i = y$i, j = y$j, x = y$x)
              idb<-numeric(b[nrow(b),1]+1)
              idb[1]<-1
              for(rb in 2:nrow(b)){
                if(b[rb-1,1]!=b[rb,1]){
                  idb[b[rb,1]]<- rb
                }
              }
              idb[length(idb)] <- nrow(b)+1
              
              df=data.frame()
              for(i in 1:(length(ida)-1)){
                tmp_a=a[ida[i]:(ida[i+1]-1),]
                for(j in 1:(length(idb)-1)){
                  tmp_b=b[idb[j]:(idb[j+1]-1),]
                  c <- merge(subset(tmp_a,select=c("j","x")), subset(tmp_b,select=c("j","x")), by = c("j"), all = TRUE, suffixes = c("1", "2"))
                  c$x1[is.na(c$x1)] <- 0
                  c$x2[is.na(c$x2)] <- 0
                  row_sum <-  sum(c$x1 * c$x2)
                  if (row_sum != 0)
                  {
                    df <- rbind(df, data.frame(i=ida[i],j=idb[j],x=row_sum))
                  }
                }
              }
              row.names(df)=NULL
              sm<-sparse.matrix(df$i,df$j,df$x,dims)
              return(sm)
            }else{
              stop("An error has occurred, please check your input!")
            }
          })

#' Addition method for sparse matrix 
#'
#' @docType methods
#' @rdname sparse.matrix-methods
#' @aliases +, sparse.matrix,  sparse.matrix-method
#' @usage \S4method{+}{sparse.matrix,sparse.matrix}(e1,e2)
#' @inheritParams e1,e2 from '+' in matrix addtion
#' 

setMethod("+",
          signature(e1= "sparse.matrix", e2 = "sparse.matrix"),
          function(e1, e2){
            if(e1@dims[1] == e2@dims[1]& e1@dims[2] ==e2@dims[2]){
              a <- data.frame(i = e1$i, j = e1$j, x = e1$x)
              b <- data.frame(i = e2$i, j = e2$j, x = e2$x)
              c <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
              c$x1[is.na(c$x1)] <- 0
              c$x2[is.na(c$x2)] <- 0
              c$x <- c$x1 + c$x2
              c<-c[, c("i", "j", "x")]
              dims<-e1@dims
              return(sparse.matrix(c$i,c$j,c$x,dims = dims))
            }else{
              stop("An error has occurred, please check your input!")
            }
          })


