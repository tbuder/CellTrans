#' Reading all necessary data.
#' 
#' This function opens a dialog box which asks for the number of cell types, the names of the cell types, the time step length and the time points of measurement. Then, the files containing the cell state proportion matrices can be selected. First, the initial experimental setup matrix can either be chosen as identity matrix (for pure initial cell populations) or a custom initial matrix can be provided. Then, the experimental cell proportion matrices are read for each time point of measurement. It is recommended to save the input into a variable for the further analysis, e.g. input <- readExperimentalData().
#' @keywords initial experimental matrix, cell distribution matrices
#' @export


readExperimentalData <- function()  {
  
  dlgMessage("Welcome to CellTrans!\n Please assure that you have prepared appropriate files containing the cell state distribution matrices representing your experimental data.")
  cellnr  <- 	as.integer(dlgInput("Number of cell states")$res)
  #Read cell type names
  cell_types=rep.int(0,cellnr)
  for (i in 1:cellnr)  {
    cell_types[i] <- dlgInput(paste("Name of cell type", i))$res
  }
  #Ask for timeunits and timepoints
  timeunits<-dlgList(title="Time step length",c("minutes","hours","days","weeks","months","cell divisions"))$res
  timenr<-as.integer(dlgInput("Number of time points")$res)
  
  timepoints=rep(0,timenr)
  for (i in 1:timenr)  {
    timepoints[i] <-dlgInput(paste0("Timepoint ",i))$res
  }
  timepoints=as.numeric(timepoints)
  
  
  
  #Create matrix for cell distribution matrices incuding initial experimental matrix
  expData=matrix(0, nrow=cellnr*(timenr+1),ncol=cellnr)
  
  #Ask for initial input matrix
  res=""
  while (res=="") {
    res <- dlgList(title="Initial experimental setup matrix", choices=c("Identity matrix (pure initial cell compositions)", "Individual matrix"))$res
    if (res=="Identity matrix") {
      expData[1:cellnr,]=diag(cellnr)
    } else {
      repeat {
      expData[1:cellnr,]=matrix(scan(dlgOpen(title = "Select initial experimental matrix")$res, n = cellnr*cellnr), cellnr, cellnr, byrow = TRUE)
      while (!isTrMatrix(expData[1:cellnr,])) 
        {
          dlgMessage(paste("Try again! Selected file does not contain an experimental cell state proportion matrix of dimension ",cellnr," !"))
          expData[1:cellnr,]=matrix(scan(dlgOpen(title = " Select initial experimental matrix.")$res, n = cellnr*cellnr), cellnr, cellnr, byrow = TRUE)
          while (det(expData[1:cellnr,])==0) 
          {
            dlgMessage(paste("The experimental setup matrix is not valid (not invertible)!"))
            expData[1:cellnr,]=matrix(scan(dlgOpen(title = " Select initial experimental matrix.")$res, n = cellnr*cellnr), cellnr, cellnr, byrow = TRUE)
          }  
        }
      
      
      break}
      
    }
  }
  
  
  #Ask for cell distribution matrices
  j=0
  for (t in timepoints) {
    j=j+1
    expData[(j*cellnr+1):((j+1)*cellnr),]=matrix(scan(dlgOpen(title = paste0("Select cell distribution matrix at t=",t,"."))$res, n = cellnr*cellnr), cellnr, cellnr, byrow = TRUE)
    while (!isTrMatrix(  expData[(j*cellnr+1):((j+1)*cellnr),]  ) ) {dlgMessage(paste("Try again! Selected file does not contain an initial setup matrix of dimension ",cellnr,"!"))
      expData[(j*cellnr+1):((j+1)*cellnr),]=matrix(scan(dlgOpen(title = paste0("Select cell distribution matrix at t=",t,"."))$res, n = cellnr*cellnr), cellnr, cellnr, byrow = TRUE)
    }
  }
  return(list("cellnr"=cellnr, "cell_types"=cell_types, "timeunits"=timeunits, "timenr"=timenr, "timepoints"=timepoints, "experimentalData"=expData))
  
  
}
