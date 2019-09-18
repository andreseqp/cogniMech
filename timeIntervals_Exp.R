# ----Comparison of time intervals for experimental and natural setting------------------------------------------#
#                         Figure 2

# Directories --------------------------------------------------------------
# Requirements of here:
# install.packages("devtools")
# devtools::install_github("r-lib/rprojroot")
# devtools::install_github("krlmlr/here")
library(here)
library('plotrix')

alg<-"ActCrit"
# directory where simulation output is stored


# libraries ---------------------------------------------------------------------------------------
# external file to locate plots in the plotting region
source(paste(here(),'posPlots.R',sep="/"))

# aesthetic parameters 
source(paste(here(),"aesth_par.R",sep="/"))
# funtions to load data
source(paste(here(),"/loadData_",alg,".R",sep = ""))

# Load Data ---------------------------------------------------------------------------------------


# Define data to be loaded 

(listPar<-c("ecol","exper"))
(listVal<-c("",""))


# Load interval data for FAA from the raw data
# natural setting
FAAtimeIntEcol<-do.call(
  rbind,lapply(
    getFilelist(here(alg),listPar[1],listVal[1])$FAA,
    file2timeInter,interV=1001))
#experimental setting
FAAtimeIntExp<-do.call(
  rbind,lapply(
    getFilelist(here(alg),listPar[2],listVal[2])$FAA,
    file2timeInter,interV=1001))


# Load interval data for PAA from the raw data
# natural setting
PAAtimeIntEcol<-do.call(
  rbind,lapply(
    getFilelist(here(alg),listPar[1],listVal[1])$PAA,
    file2timeInter,interV=1001))

#experimental setting
PAAtimeIntExp<-do.call(
  rbind,lapply(
    getFilelist(here(alg),listPar[2],listVal[2])$PAA,
    file2timeInter,interV=1001))

# Calculate statistics on the interval data ------------------------------------


FAAIntstatsEcol<-FAAtimeIntEcol[Gamma!=0.5,.(meanProb=mean(Prob.RV.V),
                           upIQR=fivenum(Prob.RV.V)[4],
                           lowIQR=fivenum(Prob.RV.V)[2])
                        ,by=.(Interv,Neta,Outbr,Gamma)]

FAAIntstatsExp<-FAAtimeIntExp[Gamma!=0.5,.(meanProb=mean(Prob.RV.V),
                                             upIQR=fivenum(Prob.RV.V)[4],
                                             lowIQR=fivenum(Prob.RV.V)[2])
                                ,by=.(Interv,Neta,Outbr,Gamma)]

PAAIntstatsEcol<-PAAtimeIntEcol[Gamma!=0.5,.(meanProb=mean(Prob.RV.V),
                           upIQR=fivenum(Prob.RV.V)[4],
                           lowIQR=fivenum(Prob.RV.V)[2])
                        ,by=.(Interv,Neta,Outbr,Gamma)]


PAAIntstatsExp<-PAAtimeIntExp[Gamma!=0.5,.(meanProb=mean(Prob.RV.V),
                                             upIQR=fivenum(Prob.RV.V)[4],
                                             lowIQR=fivenum(Prob.RV.V)[2])
                                ,by=.(Interv,Neta,Outbr,Gamma)]

# Compute position variable that allows to see the "treatments"

FAAIntstatsEcol[,posit:=ifelse(Gamma==0&Neta==0,0,
                           ifelse(Gamma==0.8&Neta==0,0.1,
                             ifelse(Gamma==0&Neta==1,0.2,0.3)))]
PAAIntstatsEcol[,posit:=ifelse(Gamma==0&Neta==0,0,
                           ifelse(Gamma==0.8&Neta==0,0.1,
                                  ifelse(Gamma==0&Neta==1,0.2,0.3)))]

FAAIntstatsExp[,posit:=ifelse(Gamma==0&Neta==0,0,
                           ifelse(Gamma==0.8&Neta==0,0.1,
                                  ifelse(Gamma==0&Neta==1,0.2,0.3)))]
PAAIntstatsExp[,posit:=ifelse(Gamma==0&Neta==0,0,
                           ifelse(Gamma==0.8&Neta==0,0.1,
                                  ifelse(Gamma==0&Neta==1,0.2,0.3)))]


# Plot the dynamics of VR choice -----------------------------------------------------------

png(paste(simsDir,"/Fig_2.png",sep = ""),
width = 1200,height = 800)

pdf(here("Fig_2.pdf"),width = 12,height = 8)

par(plt=posPlot(numplotx = 2,numploty = 2,idplotx = 1,idploty = 2),
    yaxt='s',las=1)
with(FAAIntstatsEcol,{
  plotCI(x=Interv+posit+1,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',xaxt="n",
         col=colboxes[ifelse(Gamma==0.8,
                             ifelse(Neta==1,1,2),
                             ifelse(Neta==1,3,4))],
         sfrac=0.002,cex.axis=1.3,ylim=c(0.4,1))
  lines(x=c(min(Interv)+1,max(Interv)+1),y=c(0.5,0.5),col='grey')
  text(x=par('usr')[1]+0.05*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.9*(par('usr')[4]-par('usr')[3]),
       labels='A',cex=1.5)
  # lines(x=c(0,max(Interv)),y=c(0.8,0.8),col='grey')
})


par(plt=posPlot(numplotx = 2,numploty = 2,idplotx = 2,idploty = 2),
    new=TRUE,yaxt='s',xpd=TRUE)

with(PAAIntstatsEcol,{
  plotCI(x=Interv+posit+1,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',xaxt="n",
         col=colboxes[ifelse(Gamma==0.8,
                             ifelse(Neta==1,1,2),
                             ifelse(Neta==1,3,4))],
         sfrac=0.002,cex.axis=1.3,yaxt='n',ylim=c(0.4,1))
  lines(x=c(min(Interv)+1,max(Interv)+1),y=c(0.5,0.5),col='grey')
  text(x=par('usr')[1]+0.05*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.9*(par('usr')[4]-par('usr')[3]),
       labels='B',cex=1.5)
})
legend('topright',
       legend=c("penalty + future reward", "future reward",
                "penalty","no penalty + no future reward"),
       col=colboxes,pch=15,cex=1,ncol=1)


par(plt=posPlot(numplotx = 2,numploty = 2,idplotx = 1,idploty = 1),
    yaxt='s',las=1,new=TRUE)
with(FAAIntstatsExp,{
  plotCI(x=Interv+posit+1,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',xaxt="n",
         col=colboxes[ifelse(Gamma==0.8,
                             ifelse(Neta==1,1,2),
                             ifelse(Neta==1,3,4))],
         sfrac=0.002,cex.axis=1.3,ylim=c(0.4,1))
    lines(x=c(min(Interv)+1,max(Interv)+1),y=c(0.5,0.5),col='grey')
  text(x=par('usr')[1]+0.05*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.9*(par('usr')[4]-par('usr')[3]),
       labels='C',cex=1.5)
  axis(1,labels = (Interv+1)*1000,at=Interv+1)
  # lines(x=c(0,max(Interv)),y=c(0.8,0.8),col='grey')
})
text(x= -2,y=1.1,labels = "Proportion of visitors \n chosen over residents",
     srt=90,cex=1.8)


par(plt=posPlot(numplotx = 2,numploty = 2,idplotx = 2,idploty = 1),
    new=TRUE,yaxt='s',xpd=TRUE)

with(PAAIntstatsExp,{
  plotCI(x=Interv+posit+1,y=meanProb,
         ui = upIQR,li=lowIQR,
         pch=16,xlab='',ylab='',xaxt="n",
         col=colboxes[ifelse(Gamma==0.8,
                             ifelse(Neta==1,1,2),
                             ifelse(Neta==1,3,4))],
         sfrac=0.002,cex.axis=1.3,yaxt='n',ylim=c(0.4,1))
  lines(x=c(min(Interv)+1,max(Interv)+1),y=c(0.5,0.5),col='grey')
  text(x=par('usr')[1]+0.05*(par('usr')[2]-par('usr')[1]),
       y=par('usr')[3]+0.9*(par('usr')[4]-par('usr')[3]),
       labels='D',cex=1.5)
  axis(1,labels = (Interv+1)*1000,at=Interv+1)
})
text(x = -0.1,y=0.22,labels = "Trials",cex=2)

dev.off()




  
  