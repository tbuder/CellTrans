#' Quasi-optimization of a matrix root to a stochastic matrix.
#'
#' QOM finds a transition matrix that is as close as possible, with respect to the Euclidean distance of the rows, to the fractional root of a given transition matrix. If the argument is a transition matrix, this matrix is returned unchanged.
#' @param A The fractional root of a matrix that shall be regularized.
#' @keywords QOM, matrix root regularization
#' @export




QOM<-function(A) {
  n=nrow(A)
  for (i in 1:n) 
  {
    repeat 
    { 
      
      non_negative_components=sum(A[i,]>0)  
      diff=(sum(A[i,])-1)/non_negative_components
      for (j in 1:n) {
        if (A[i,j]>0) {
          A[i,j]=A[i,j]-diff
        }
      }
      if (all(A[i,]>=0)) {break}
      for (j in 1:n) {
        if (A[i,j]<0) A[i,j]=0 }
    }
  }
  return(A)
}