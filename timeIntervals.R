# --------------------------------- Time intervals -------------------------------------------------#

# directory where source files are saved
projDir<-"D:/quinonesa/learning_models_c++/cogniMech/"
  getwd()
alg<-"sarsa"
# directory where simulation output is stored
simsDir<-paste(projDir,alg,sep = "/")


# libraries ---------------------------------------------------------------------------------------
setwd(projDir)
source('d:/quinonesa/Dropbox/R_files/posPlots.R')
source(paste(projDir,"aesth_par.R",sep=""))
source(paste(projDir,"loadData_",alg,".R",sep = ""))
library('plotrix')

# Load Data ---------------------------------------------------------------------------------------
setwd(simsDir)

# Define data to be loaded 

(listPar<-rep("ecol",1))
(listVal<-c(""))
(param<-getParam(simsDir,listparam = listPar,values = listVal))



interv<-1001

# Load interval data for FIA from the raw data
FIAtimeInt<-do.call(
  rbind,lapply(
    getFilelist(simsDir,listPar,listVal)$FAA,
    file2timeInter,interV=interv))



# Load FIA data from processed file

# getFilelist(simsDir,listPar,listVal)$FIA

# FIAtimeInt<-do.call(
#   rbind,lapply(getFilelist(projDir,listPar,listVal)$FIA,fread))



# Load interval data for PIA from the raw data
PIAtimeInt<-do.call(
  rbind,lapply(
    getFilelist(simsDir,listPar,listVal)$PAA,
    file2timeInter,interV=interv))

# Load PIA data from processed file

# PIAtimeInt<-do.call(
#   rbind,lapply(getFilelist(genDir,listPar,listVal)$PIA,fread))

# Load DP data from the raw data
DPdataProb<-do.call(rbind,
                    lapply(getFilelist(simsDir,listPar,listVal)$DP,
                           file2lastDP))

# Load DP data from processed file

# DPdataprob<-do.call(
#   rbind,lapply(getFilelist(genDir,listPar,listVal)$DP,fread))

# Calculate stats --------------------------------------------------------------

# extpar<-listPar[1]

FIAIntstats<-FIAtimeInt[,.(meanProb=mean(Prob.RV.V),
                           upIQR=fivenum(Prob.RV.V)[4],
                           lowIQR=fivenum(Prob.RV.V)[2])
                        ,by=.(Interv,Neta,Outbr,Tau,Gamma)]
# setnames(FIAIntstats,'get',extpar)

PIAIntstats<-PIAtimeInt[,.(meanProb=mean(Prob.RV.V),
                           upIQR=fivenum(Prob.RV.V)[4],
                           lowIQR=fivenum(Prob.RV.V)[2])
                        ,by=.(Interv,Neta,Outbr,Tau,Gamma)]
# setnames(PIAIntstats,'get',extpar)

# Plot the dynamics of VR choice -----------------------------------------------------------

FIAIntstats[,posit:=ifelse(Gamma==0&Neta==0,0,
                           ifelse(Gamma==0.8&Neta==0,0.1,
                                  ifelse(Gamma==0&Neta==1,0.2,0.3)))]
PIAIntstats[,posit:=ifelse(Gamma==0&Neta==0,0,
                           ifelse(Gamma==0.8&Neta==0,0.1,
                                  ifelse(Gamma==0&Neta==1,0.2,0.3)))]


png(filename = paste(simsDir,"FigSup1.png",sep=""),
    width = 1600,height = 800)

par(plt=posPlot(numplotx = 2,idplotx = 1),yaxt='s',las=1)
with(FIAIntstats,{
  plotCI(x=Interv+posit,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',
         col=colboxes[ifelse(Gamma==0.8,
                             ifelse(Neta==1,1,2),
                             ifelse(Neta==1,3,4))],
         sfrac=0.002,cex.axis=1.3,ylim=c(0.4,1))
  mtext(side = 2,text = "frequency of V over R",las=0,cex=2,line=4)
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  text(x=par('usr')[2]*0.1,y=par('usr')[3]+0.9*(par('usr')[4]-par('usr')[3]),
       labels='FAA',cex=1.5)
})

with(DPdataProb,
     {matlines(x = t(matrix(rep(max(FIAtimeInt$Interv)*c(0.75,1),
                                each=length(RV.V)),length(RV.V))),
               y=t(matrix(rep(probRV.V,2),length(RV.V))),
               lwd=2,lty = "dashed",
               col=colboxes[ifelse(Gamma==0.8,
                                   ifelse(Neta==1,1,2),
                                   ifelse(Neta==1,3,4))])})

par(plt=posPlot(numplotx = 2,idplotx = 2),
    new=TRUE,yaxt='s',xpd=TRUE)

with(PIAIntstats,{
  plotCI(x=Interv+posit,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',
         col=colboxes[ifelse(Gamma==0.8,
                             ifelse(Neta==1,1,2),
                             ifelse(Neta==1,3,4))],
         sfrac=0.002,cex.axis=1.3,yaxt='n',ylim=c(0.4,1))
  lines(x=c(0,max(Interv)),y=c(0.5,0.5),col='grey')
  text(x=par('usr')[2]*0.1,y=par('usr')[3]+0.9*(par('usr')[4]-par('usr')[3]),
       labels='PAA',cex=1.5)
})


legend('topright',
       legend=cc("penalty + future reward", "future reward",
                 "penalty","no penalty + no future reward"),
       col=colboxes,pch=15,cex=1.5,ncol=1)

dev.off()


  