######################### Abundance functions ##################################

# Function to create the interpolation -----------------------------------------

AbundData2interp<-function(dataAbun,
                           npoints=100, # size of the interpolations
                           Var2int      # variable to interpolate
                           ){

  # interpolate real data
  interpData<-with(dataAbun,
                      {interp(x=pR,y=pV,z=get(Var2int),duplicate = "mean",
                              nx=npoints,ny=npoints)})
  # Create structure to transpose interpolated data
  interpDataTrans<-data.table(matrix(0,nrow = npoints*npoints,ncol = 4))
  names(interpDataTrans)<-c("resProb","visProb",Var2int,"notProb")
  # Transpose interpolated data
  for (i in 1:npoints) {
    for (j in 1:npoints) {
      interpDataTrans[(i-1)*npoints+j,resProb:=interpData$x[i]]
      interpDataTrans[(i-1)*npoints+j,visProb:=interpData$y[j]]
      interpDataTrans[(i-1)*npoints+j,eval(Var2int):=interpData$z[i,j]]
    }
  }
  # Complementary probability - "absence"
  interpDataTrans$notProb<-1-interpDataTrans$resProb-interpDataTrans$visProb
  # Select relavant data
  interpDataTrans<-interpDataTrans[resProb+visProb<1]
  return(interpDataTrans)
}




