#' Calculation of the transition matrix.
#'
#' This function derives the transition matrix which contains the transition probabilitites between the distinct cell states.
#' @param M Matrix containing all experimental data: the initial experimental setup matrix and the experimental cell state distribution matrices.
#' @param  t This A vector containing all timepoints at which the cell state distributions have been experimentally measured.
#' @param  used_timepoints A vector with the timepoints utilized to estimate the transition matrix.

#' @keywords transition matrix, Matrix process
#' @export

  calculate_transitionMatrix <- function(M,t,used_timepoints) {
  n=ncol(M)
  transitionMatrix=0
  countQOM=0
  for (i in 1:length(t)) {
    #first submatrix in M contains initial matrix, calculate inverse
    invInitialMatrix=solve(M[1:n,])
    #derive transition matrices beginning with second submatrix in M
  if (t[i] %in% used_timepoints ) {
    if (t[i]>1) {
    Ptemp=expm ( (1/t[i])*logm( (invInitialMatrix%*%M[(i*n+1):((i+1)*n), ]),method="Eigen"))
    } else {
    Ptemp=(invInitialMatrix%*%M[(i*n+1):((i+1)*n), ])%^%(1/t[i])
    }

    if (isTrMatrix(Ptemp)==FALSE) {
      assign( paste("P",t[i],sep=""),QOM(Ptemp)  )
      countQOM=countQOM+1
    }
    else {    assign( paste("P",t[i],sep=""),Ptemp)}

    transitionMatrix=transitionMatrix+get( paste("P",t[i],sep=""))
  }
  }
  transitionMatrix=transitionMatrix/length(used_timepoints)

  return(transitionMatrix)

}
