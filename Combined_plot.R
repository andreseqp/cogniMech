####### Cobined plot for figure 4 #############################################
# Include in one figure tripleces and dot plots with error bars 

# Load libraries and external functions -----------------------------------
library(here)
# directory where source files are saved

alg<-"ActCrit"


source('posPlots.R')
source(here("aesth_par.R"))
source(here(paste("loadData_",alg,".R",sep = "")))
source(here('ternaryAEQP.R'))
source(here("data2interp.R"))
library('plotrix')
library('akima')
library("vcd")

# Load Data for interpolations --------------------------------------------------------------------------------------


(listPar<-c("abundance"))

(listVal<-c(""))

param<-getParam(here(alg,"Fig4_"),listparam = listPar,values = listVal)



FIAlastQuarData<-do.call(rbind,lapply(
  getFilelist(here(alg,"Fig4_"),listPar,listVal)$FAA,file2lastProp,0.9))


FIA.stats<-FIAlastQuarData[,.(meanProb=mean(Prob.RV.V),
                              upIQR=fivenum(Prob.RV.V)[4],
                              lowIQR=fivenum(Prob.RV.V)[2])
                           ,by=.(Neta,Gamma,pR,pV)]

FIA.stats$notProb<-round(1-FIA.stats$pR-FIA.stats$pV,1)


# Load data for dot plot -------------------------------------------------------


FIAlast<-rbindlist(lapply(getFilelist(here(alg,"Fig4_"),
                                      listPar,listVal)$FAA, 
                              function(x){
                                if(as.numeric(gsub("[[:alpha:]]",
                                                   strsplit(x,"_")[[1]][7],
                                                   replacement=""))==
                                   as.numeric(gsub("[[:alpha:]]",
                                                   strsplit(x,"_")[[1]][8],
                                                   replacement=""))){
                                  file2lastProp(x,0.9)
                                }}))




FIAlast[,pA:=1-pR-pV]

FIA.statsdot<-FIAlast[,.(meanProb=mean(Prob.RV.V),
                      upIQR=fivenum(Prob.RV.V)[4],
                      lowIQR=fivenum(Prob.RV.V)[2])
                   ,by=.(Neta,Gamma,pR,pV,Outbr,pA)]


# Data interpolations ---------------------------------------------------------

FIAinterpData<-AbundData2interp(FIAlastQuarData[Neta==0&Gamma==0.8],
                                Var2int = "Prob.RV.V",npoints = 500)

FIAinterpData.Neg<-AbundData2interp(FIAlastQuarData[Neta==1&Gamma==0],
                                    Var2int = "Prob.RV.V",npoints = 500)

# Panel a ---------------------------------------------------------------------

FIA.statsdot[,posit:=ifelse(Gamma==0&Neta==0,0,
                         ifelse(Gamma==0.8&Neta==0,0.01,
                                ifelse(Gamma==0&Neta==1,0.02,0.03)))]

png(here(alg,"Fig4_panelA.png"),
    width = 700 , height = 1200)

pdf(here(alg,"Fig4_panelA.pdf"),height = 12,width = 7)

par(plt=posPlot(numplotx = 1,numploty = 1,1,1),las=1)
with(FIA.statsdot,{
  plotCI(x = (1-pA)+posit,
         y = meanProb,ui = upIQR
         ,li = lowIQR,
         col=colboxes[ifelse(Gamma==0.8,
                             ifelse(Neta==1,1,2),
                             ifelse(Neta==1,3,4))],
         pch=16,xlab="",ylab="",
         sfrac=0.008,yaxt='s',
         cex.axis=1.3,cex=2,cex.lab=3)
  mtext('Proportion of visitors chosen over residents',2,line = 3, cex=3,las=0)
  mtext('Overall client abundance',1,line = 5,las=1,cex=3)
  # text(x=par('usr')[1]+0.05*(par('usr')[2]-par('usr')[1]),
  #      y=par('usr')[3]+0.95*(par('usr')[4]-par('usr')[3]),
  #      labels='A',cex=2)
})

legend(x=0.6,y=0.65,
       legend=c("penalty + future reward", "future reward",
                "penalty","no penalty + no future reward"),
       col=colboxes,pch=15,cex=1,ncol=1)

dev.off()

# Panel B - future triplex -----------------------------------------------------

png(here(alg,"triplex_panelB.png"),
    width=500,height=500,units ="px")

pdf(here(alg,"triplex_panelB.pdf"),height = 7,width = 7)

cex.lab.par<-1.5

colorbreaksMeans<-seq(0.45,1,length=100)

with(FIAinterpData,{
  ternaryplotAEQP(rbind(cbind(resProb,visProb,notProb),
           cbind(FIA.statsdot[Gamma==0.8&Neta==0]$pR,
                 FIA.statsdot[Gamma==0.8&Neta==0]$pV,
                 FIA.statsdot[Gamma==0.8&Neta==0]$pA)),
                  col = c(paletteMeans(100)[
                    findInterval(Prob.RV.V,colorbreaksMeans)],
                    rep("black",5)),main="",cex=0.4,
                  dimnames = c("Resident","Visitor","Absence")[c(2,3,1)],
                  dimnames_position = "edge",
                  border = "white",labels = "outside",labels_rot = c(0,0,0),
                  cex.lab = cex.lab.par,cex.grid = 1,grid_color = "black",
                  labels_color = "black",
                  numplotx = 1,numploty = 1,idplotx = 1,idploty = 1)
})
dev.off()

# Panel C - triplex negative reward --------------------------------------------

png(here(alg,"triplex_panelC.png"),
    width=500,height=500,units ="px")

pdf(here(alg,"triplex_panelC.pdf"),height = 7,width = 7)

colorbreaksMeans<-seq(0.45,1,length=100)

with(FIAinterpData.Neg,{
  ternaryplotAEQP(rbind(cbind(resProb,visProb,notProb),
                        cbind(FIA.statsdot[Gamma==0&Neta==1]$pR,
                              FIA.statsdot[Gamma==0&Neta==1]$pV,
                              FIA.statsdot[Gamma==0&Neta==1]$pA)),
                  col = c(paletteMeans(100)[
                    findInterval(Prob.RV.V,colorbreaksMeans)],
                    rep("black",5)),main="",cex=0.4,
                  dimnames = c("Resident","Visitor","Absence")[c(2,3,1)],
                  dimnames_position = "edge",
                  border = "white",labels = "outside",labels_rot = c(0,0,0),
                  cex.lab = cex.lab.par,cex.grid = 1,grid_color = "black",
                  labels_color = "black",
                  numplotx = 1,numploty = 1,idplotx = 1,idploty = 1)
})
dev.off()

# ColorScale -------------------------------------------------------------------

png(here(alg,"triplex_panel_colorSca.png"),
    width=400,height=400,units ="px")

pdf(here(alg,"triplex_panel_colorSca.png"),width = 4,height = 4)

plot.new()
with(rbind(FIAinterpData,FIAinterpData.Neg),{
  par(new=FALSE)
  color.bar.aeqp(paletteMeans(100),min =min(colorbreaksMeans),
                 max = max(colorbreaksMeans),nticks = 3,
                 title = "Probability  of Visitors \n chosen over residents",
                 cex.tit = 1.2,
                 numplotx = 2,numploty = 2,idplotx =2,idploty = 1)})
dev.off()
