# ------------------ Effect of leaving probability ------------------------ #
#                         Figure 3

# Directories --------------------------------------------------------------
# Requirements of here:
# install.packages("devtools")
# devtools::install_github("r-lib/rprojroot")
# devtools::install_github("krlmlr/here")
library(here)
# project directory
library('plotrix')
# directory where source files are saved
alg<-"ActCrit"



# libraries ----------------------------------------------------------------

source(paste(here(),'posPlots.R',sep="/"))
source(paste(here(),"aesth_par.R",sep="/"))
source(paste(here(),"/loadData_",alg,".R",sep = ""))


# Load data ------------------------------------------------------------

# Define data to be loaded 

(listPar<-rep("Vlp",6))

(listVal<-seq(0,1,length=6))

FAAlastQuart<-do.call(rbind,lapply(
  getFilelist(here(alg),listPar,listVal)$FAA,file2lastProp,0.75,'Vlp'))

FAA.stats<-FAAlastQuart[,.(meanProb=mean(Prob.RV.V),
                              upIQR=fivenum(Prob.RV.V)[4],
                              lowIQR=fivenum(Prob.RV.V)[2])
                           ,by=.(Neta,Gamma,pR,pV,Outbr,Vlp)]


# Plots -----------------------------------------------------------------------

FAA.stats[,posit:=ifelse(Gamma==0&Neta==0,0,
                         ifelse(Gamma==0.8&Neta==0,0.01,
                                ifelse(Gamma==0&Neta==1,0.02,0.03)))]

# png(here(alg,"/Fig_3.png"),
#     width = 1200,height = 800)

pdf(here("Fig_3.pdf"),width = 12,height = 8,useDingbats = FALSE)

par(plt=posPlot(),las=1)
with(FAA.stats,{
  plotCI(x = Vlp+posit,
         y = meanProb,ui = upIQR
         ,li = lowIQR,
         col=colboxes[ifelse(Gamma==0.8,
                             ifelse(Neta==1,1,2),
                             ifelse(Neta==1,3,4))],
         pch=16,xlab="",ylab="",
         sfrac=0.008,yaxt='s',
         cex.axis=1.3,cex=2,cex.lab=3)
  mtext('Proportion of visitors \n chosen over residents',2,line = 4, cex=3,las=0)
  mtext(expression(l[v]),1,line = 5,las=1,cex=4)
})

legend('topleft',
       c("penalty + future reward", "future reward",
         "penalty","no penalty + no future reward"),
       col=colboxes,pch=15,cex=1,ncol=1)

dev.off()
