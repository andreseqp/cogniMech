library("jsonlite")
library("here")

exedir<-'/./ActCrit.exe'

fileName<-"parameters.json"



# Baseline parameter values
param<-list(totRounds=20000,ResReward=1,VisReward=1,
            ResProb=c(0.2),
            VisProb=c(0.2),
            ResProbLeav=0,VisProbLeav=1,negativeRew=-0.5,
            experiment=FALSE,
            inbr=0,outbr=0,trainingRep=100,forRat=0.0,
            alphaT=0.01,printGen=100,seed=1, gammaRange=c(0,0.8),
            netaRange=c(0,1),alphaThRange=c(0.01),
            alphaThNchRange=1,
            folder=here("../Fig3_/"))


# function that creates new folders for simulations results --------------------
check_create.dir<-function(folder,param,values){
  setwd(folder)
  listfolders<-paste(param,values,"_",sep = "")  
  currFolders<-lapply(listfolders,dir.exists)
  if(sum(currFolders>0)){
    warning("At least one of the folders already exists \n Please check",
            immediate. = TRUE)
    print(cbind(listfolders,currFolders))
    ans<-readline("Want to continue?")
    if(substr(ans, 1, 1) == "y"){
      lapply(listfolders,dir.create)
      return(listfolders)
    }
    else{
      return(listfolders)
    }
  }else{
    lapply(listfolders,dir.create)
    return(listfolders)
  }
}

# Arrays with the values of external parameters
rangLeav<-seq(0,1,by = 0.2)
rangAbund<-seq(0,0.9,length=10)
rangScen<-c(0,1,2,3)
rangAlphNC<-c(0,0.5,1)

# General folder for analysis
check_create.dir(simsDir,param = rep("Experiments",1),
                 values = "")

listfolders<-check_create.dir(paste(simsDir,"Experiments_/",sep=""),
                              param = rep("scen",4),
                              values = rangScen)


# Loop through parameter names and values creating JSONs -----------------------
for (i in 1:6) {
    param$VisProbLeav<-rangLeav[i]
    param$folder<-paste("../Fig3_/","Vlp",rangLeav[i],"_/Simul/",sep="")
    outParam<-toJSON(param,auto_unbox = TRUE,pretty = TRUE)
    fileName<-paste("parameters",".json",sep="")
    if(file.exists(paste("./Fig3_/Vlp",rangLeav[i],"_/",
                         fileName,sep = ""))){
      currFile<-fromJSON(paste("./Fig3_/Vlp",rangLeav[i],"_/",
                               fileName,sep = ""))
      if(sum(unlist(currFile)!=unlist(param))>0){
        warning("You are erasing old files!! n\ Check first!!!",immediate. = TRUE)
        print("OLD value")
        print(unlist(currFile)[unlist(currFile)!=unlist(param)])
        print("NEW value")
        print(unlist(param)[unlist(currFile)!=unlist(param)])
        ans<-readline("Want to continue?")
        if(substr(ans, 1, 1) == "y"){
          write(outParam,paste("./Fig3_/Vlp",rangLeav[i],"_/",
                               fileName,sep = ""))
        }
      }
    }
    else{
      write(outParam,paste(param$folder,fileName,sep = "/"))
    }
    # Uncomment for running simulations directly through R:
    # system(paste(exedir,
    #              gsub("\\","/",paste(param$folder,fileName,sep="\\"),fixed=TRUE)
    #              ,sep = " "))
}
