#' Is a matrix a transition matrix?
#'
#' A function that checks whether a matrix is a stochastic matrix describing a Markov process, i.e. with non-negative entries and row sums equal to one.
#' @param A The matrix that is checked to be a transition matrix.
#' @keywords transition matrix, Matrix process
#' @export

isTrMatrix=function(A) {
  sumisOne=TRUE
  for (i in 1:nrow(A) ) { if (abs(sum(A[i,])-1)>1E-8) {sumisOne=FALSE}
  }
  
  return((all(A>=0)) & (sumisOne))
}
