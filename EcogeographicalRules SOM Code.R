##########CODE FOR ECOGEOGRAPHICAL CORRELATES OF MAMMAL COLOURATION
########CREATED: 15/02/22
######LAST EDITED: 22/05/24

#Set the working directory
setwd("C:/Users/natas/Documents/PhD/Data Analysis/")

#Read in the data
x<-read.csv("erdata.csv")
#There should be 4,411 spp.

#Trim out any of the necessary variables with missing data
##ONLY use lines relevant for variables being tested in the model
if(sum(is.na(x$Head.Pattern))>0){x<-x[-which(is.na(x$Head.Pattern)),]}
if(sum(is.na(x$Num.Head.Cols))>0){x<-x[-which(is.na(x$Num.Head.Cols)),]}
if(sum(is.na(x$Pri.Head.Col))>0){x<-x[-which(is.na(x$Pri.Head.Col)),]}
if(sum(is.na(x$Sec.Head.Col))>0){x<-x[-which(is.na(x$Sec.Head.Col)),]}
if(sum(is.na(x$Body.Pattern))>0){x<-x[-which(is.na(x$Body.Pattern)),]}
if(sum(is.na(x$Num.Body.Cols))>0){x<-x[-which(is.na(x$Num.Body.Cols)),]}
if(sum(is.na(x$Pri.Body.Col))>0){x<-x[-which(is.na(x$Pri.Body.Col)),]}
if(sum(is.na(x$Sec.Body.Col))>0){x<-x[-which(is.na(x$Sec.Body.Col)),]}
if(sum(is.na(x$Leg.Pattern))>0){x<-x[-which(is.na(x$Leg.Pattern)),]}
if(sum(is.na(x$Num.Leg.Cols))>0){x<-x[-which(is.na(x$Num.Leg.Cols)),]}
if(sum(is.na(x$Pri.Leg.Col))>0){x<-x[-which(is.na(x$Pri.Leg.Col)),]}
if(sum(is.na(x$Sec.Leg.Col))>0){x<-x[-which(is.na(x$Sec.Leg.Col)),]}
if(sum(is.na(x$Tail.Pattern))>0){x<-x[-which(is.na(x$Tail.Pattern)),]}
if(sum(is.na(x$Num.Tail.Cols))>0){x<-x[-which(is.na(x$Num.Tail.Cols)),]}
if(sum(is.na(x$Pri.Tail.Col))>0){x<-x[-which(is.na(x$Pri.Tail.Col)),]}
if(sum(is.na(x$Sec.Tail.Col))>0){x<-x[-which(is.na(x$Sec.Tail.Col)),]}
if(sum(is.na(x$Mean.Precipitation))>0){x<-x[-which(is.na(x$Mean.Precipitation)),]}
if(sum(is.na(x$Mean.Evapotranspiration))>0){x<-x[-which(is.na(x$Mean.Evapotranspiration)),]}
if(sum(is.na(x$MidRange.Latitude))>0){x<-x[-which(is.na(x$MidRange.Latitude)),]}
if(sum(is.na(x$PC1))>0){x<-x[-which(is.na(x$PC1)),]}

#Check sample size is big enough in focal orders
sum(x$Order=="Afrosoricida")
sum(x$Order=="Carnivora")
sum(x$Order=="Cetartiodactyla")
sum(x$Order=="Dasyuromorphia")
sum(x$Order=="Didelphimorphia")
sum(x$Order=="Diprotodontia")
sum(x$Order=="Eulipotyphla")
sum(x$Order=="Lagomorpha")
sum(x$Order=="Primates")
sum(x$Order=="Rodentia")

#Cut the data down to just the focal taxonomic group(s) - changeable line
x<-x[which(x$Order=="Rodentia"),]

#Re-code the categorical variables
N<-dim(x)[1]
x$HP<-rep(0,N)
x$HP[which(x$Head.Pattern==1)]<-1
x$HP[which(x$Head.Pattern==2)]<-1
x$HP[which(x$Head.Pattern==3)]<-1

x$NHC<-rep(0,N)
x$NHC[which(x$Num.Head.Cols>1)]<-1

x$RedH<-rep(0,N)
x$RedH[which(x$Pri.Head.Col=="F2")]<-1
x$RedH[which(x$Pri.Head.Col=="F3")]<-1
x$RedH[which(x$Pri.Head.Col=="F4")]<-1
x$RedH[which(x$Pri.Head.Col=="F5")]<-1
x$RedH[which(x$Pri.Head.Col=="G2")]<-1
x$RedH[which(x$Pri.Head.Col=="G3")]<-1
x$RedH[which(x$Pri.Head.Col=="G4")]<-1
x$RedH[which(x$Pri.Head.Col=="G5")]<-1

x$DarkH<-rep(0,N)
x$DarkH[which(x$Pri.Head.Col=="B5")]<-1
x$DarkH[which(x$Pri.Head.Col=="C5")]<-1
x$DarkH[which(x$Pri.Head.Col=="D5")]<-1
x$DarkH[which(x$Pri.Head.Col=="E5")]<-1
x$DarkH[which(x$Pri.Head.Col=="F5")]<-1
x$DarkH[which(x$Pri.Head.Col=="G5")]<-1

x$PHcon1<-rep(0,N)
x$PHcon1[which(x$Pri.Head.Col=="A1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="B1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="C1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="D1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="E1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="F1")]<-1
x$PHcon1[which(x$Pri.Head.Col=="G1")]<-1
x$PHcon5<-rep(0,N)
x$PHcon5[which(x$Pri.Head.Col=="B5")]<-1
x$PHcon5[which(x$Pri.Head.Col=="C5")]<-1
x$PHcon5[which(x$Pri.Head.Col=="D5")]<-1
x$PHcon5[which(x$Pri.Head.Col=="E5")]<-1
x$PHcon5[which(x$Pri.Head.Col=="F5")]<-1
x$PHcon5[which(x$Pri.Head.Col=="G5")]<-1
x$SHcon1<-rep(0,N)
x$SHcon1[which(x$Sec.Head.Col=="A1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="B1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="C1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="D1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="E1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="F1")]<-1
x$SHcon1[which(x$Sec.Head.Col=="G1")]<-1
x$SHcon5<-rep(0,N)
x$SHcon5[which(x$Sec.Head.Col=="B5")]<-1
x$SHcon5[which(x$Sec.Head.Col=="C5")]<-1
x$SHcon5[which(x$Sec.Head.Col=="D5")]<-1
x$SHcon5[which(x$Sec.Head.Col=="E5")]<-1
x$SHcon5[which(x$Sec.Head.Col=="F5")]<-1
x$SHcon5[which(x$Sec.Head.Col=="G5")]<-1
x$HCongen<-rep(0,N)
x$HCongen[which(x$PHcon1==1|x$SHcon5==1)]<-1
x$HCongen[which(x$PHcon5==1|x$SHcon1==1)]<-1

x$BP<-rep(0,N)
x$BP[which(x$Body.Pattern==4)]<-1
x$BP[which(x$Body.Pattern==5)]<-1
x$BP[which(x$Body.Pattern==6)]<-1
x$BP[which(x$Body.Pattern==7)]<-1
x$BP[which(x$Body.Pattern==8)]<-1
x$BP[which(x$Body.Pattern==9)]<-1
x$BP[which(x$Body.Pattern=="1, 2")]<-1
x$BP[which(x$Body.Pattern=="1, 4")]<-1
x$BP[which(x$Body.Pattern=="1, 4, 6")]<-1
x$BP[which(x$Body.Pattern=="1, 6")]<-1
x$BP[which(x$Body.Pattern=="2, 4")]<-1
x$BP[which(x$Body.Pattern=="2, 5")]<-1
x$BP[which(x$Body.Pattern=="2, 5, 6")]<-1
x$BP[which(x$Body.Pattern=="3, 4")]<-1
x$BP[which(x$Body.Pattern=="3, 4, 6")]<-1
x$BP[which(x$Body.Pattern=="3, 5")]<-1
x$BP[which(x$Body.Pattern=="3, 6")]<-1
x$BP[which(x$Body.Pattern=="3, 7")]<-1
x$BP[which(x$Body.Pattern=="4, 6")]<-1
x$BP[which(x$Body.Pattern=="4, 6, 8")]<-1
x$BP[which(x$Body.Pattern=="4, 6, 9")]<-1
x$BP[which(x$Body.Pattern=="4, 8")]<-1
x$BP[which(x$Body.Pattern=="4, 9")]<-1
x$BP[which(x$Body.Pattern=="5, 6")]<-1
x$BP[which(x$Body.Pattern=="6, 8")]<-1

x$NBC<-rep(0,N)
x$NBC[which(x$Num.Body.Cols>1)]<-1

x$RedB<-rep(0,N)
x$RedB[which(x$Pri.Body.Col=="F2")]<-1
x$RedB[which(x$Pri.Body.Col=="F3")]<-1
x$RedB[which(x$Pri.Body.Col=="F4")]<-1
x$RedB[which(x$Pri.Body.Col=="F5")]<-1
x$RedB[which(x$Pri.Body.Col=="G2")]<-1
x$RedB[which(x$Pri.Body.Col=="G3")]<-1
x$RedB[which(x$Pri.Body.Col=="G4")]<-1
x$RedB[which(x$Pri.Body.Col=="G5")]<-1

x$DarkB<-rep(0,N)
x$DarkB[which(x$Pri.Body.Col=="B5")]<-1
x$DarkB[which(x$Pri.Body.Col=="C5")]<-1
x$DarkB[which(x$Pri.Body.Col=="D5")]<-1
x$DarkB[which(x$Pri.Body.Col=="E5")]<-1
x$DarkB[which(x$Pri.Body.Col=="F5")]<-1
x$DarkB[which(x$Pri.Body.Col=="G5")]<-1

x$PBcon1<-rep(0,N)
x$PBcon1[which(x$Pri.Body.Col=="A1")]<-1
x$PBcon1[which(x$Pri.Body.Col=="B1")]<-1
x$PBcon1[which(x$Pri.Body.Col=="C1")]<-1
x$PBcon1[which(x$Pri.Body.Col=="D1")]<-1
x$PBcon1[which(x$Pri.Body.Col=="E1")]<-1
x$PBcon1[which(x$Pri.Body.Col=="F1")]<-1
x$PBcon1[which(x$Pri.Body.Col=="G1")]<-1
x$PBcon5<-rep(0,N)
x$PBcon5[which(x$Pri.Body.Col=="B5")]<-1
x$PBcon5[which(x$Pri.Body.Col=="C5")]<-1
x$PBcon5[which(x$Pri.Body.Col=="D5")]<-1
x$PBcon5[which(x$Pri.Body.Col=="E5")]<-1
x$PBcon5[which(x$Pri.Body.Col=="F5")]<-1
x$PBcon5[which(x$Pri.Body.Col=="G5")]<-1
x$SBcon1<-rep(0,N)
x$SBcon1[which(x$Sec.Body.Col=="A1")]<-1
x$SBcon1[which(x$Sec.Body.Col=="B1")]<-1
x$SBcon1[which(x$Sec.Body.Col=="C1")]<-1
x$SBcon1[which(x$Sec.Body.Col=="D1")]<-1
x$SBcon1[which(x$Sec.Body.Col=="E1")]<-1
x$SBcon1[which(x$Sec.Body.Col=="F1")]<-1
x$SBcon1[which(x$Sec.Body.Col=="G1")]<-1
x$SBcon5<-rep(0,N)
x$SBcon5[which(x$Sec.Body.Col=="B5")]<-1
x$SBcon5[which(x$Sec.Body.Col=="C5")]<-1
x$SBcon5[which(x$Sec.Body.Col=="D5")]<-1
x$SBcon5[which(x$Sec.Body.Col=="E5")]<-1
x$SBcon5[which(x$Sec.Body.Col=="F5")]<-1
x$SBcon5[which(x$Sec.Body.Col=="G5")]<-1
x$BCongen<-rep(0,N)
x$BCongen[which(x$PBcon1==1|x$SBcon5==1)]<-1
x$BCongen[which(x$PBcon5==1|x$SBcon1==1)]<-1

x$LP<-rep(0,N)
x$LP[which(x$Leg.Pattern==1)]<-1
x$LP[which(x$Leg.Pattern==2)]<-1
x$LP[which(x$Leg.Pattern==3)]<-1

x$NLC<-rep(0,N)
x$NLC[which(x$Num.Leg.Cols>1)]<-1

x$RedL<-rep(0,N)
x$RedL[which(x$Pri.Leg.Col=="F2")]<-1
x$RedL[which(x$Pri.Leg.Col=="F3")]<-1
x$RedL[which(x$Pri.Leg.Col=="F4")]<-1
x$RedL[which(x$Pri.Leg.Col=="F5")]<-1
x$RedL[which(x$Pri.Leg.Col=="G2")]<-1
x$RedL[which(x$Pri.Leg.Col=="G3")]<-1
x$RedL[which(x$Pri.Leg.Col=="G4")]<-1
x$RedL[which(x$Pri.Leg.Col=="G5")]<-1

x$DarkL<-rep(0,N)
x$DarkL[which(x$Pri.Leg.Col=="B5")]<-1
x$DarkL[which(x$Pri.Leg.Col=="C5")]<-1
x$DarkL[which(x$Pri.Leg.Col=="D5")]<-1
x$DarkL[which(x$Pri.Leg.Col=="E5")]<-1
x$DarkL[which(x$Pri.Leg.Col=="F5")]<-1
x$DarkL[which(x$Pri.Leg.Col=="G5")]<-1

x$PLcon1<-rep(0,N)
x$PLcon1[which(x$Pri.Leg.Col=="A1")]<-1
x$PLcon1[which(x$Pri.Leg.Col=="B1")]<-1
x$PLcon1[which(x$Pri.Leg.Col=="C1")]<-1
x$PLcon1[which(x$Pri.Leg.Col=="D1")]<-1
x$PLcon1[which(x$Pri.Leg.Col=="E1")]<-1
x$PLcon1[which(x$Pri.Leg.Col=="F1")]<-1
x$PLcon1[which(x$Pri.Leg.Col=="G1")]<-1
x$PLcon5<-rep(0,N)
x$PLcon5[which(x$Pri.Leg.Col=="B5")]<-1
x$PLcon5[which(x$Pri.Leg.Col=="C5")]<-1
x$PLcon5[which(x$Pri.Leg.Col=="D5")]<-1
x$PLcon5[which(x$Pri.Leg.Col=="E5")]<-1
x$PLcon5[which(x$Pri.Leg.Col=="F5")]<-1
x$PLcon5[which(x$Pri.Leg.Col=="G5")]<-1
x$SLcon1<-rep(0,N)
x$SLcon1[which(x$Sec.Leg.Col=="A1")]<-1
x$SLcon1[which(x$Sec.Leg.Col=="B1")]<-1
x$SLcon1[which(x$Sec.Leg.Col=="C1")]<-1
x$SLcon1[which(x$Sec.Leg.Col=="D1")]<-1
x$SLcon1[which(x$Sec.Leg.Col=="E1")]<-1
x$SLcon1[which(x$Sec.Leg.Col=="F1")]<-1
x$SLcon1[which(x$Sec.Leg.Col=="G1")]<-1
x$SLcon5<-rep(0,N)
x$SLcon5[which(x$Sec.Leg.Col=="B5")]<-1
x$SLcon5[which(x$Sec.Leg.Col=="C5")]<-1
x$SLcon5[which(x$Sec.Leg.Col=="D5")]<-1
x$SLcon5[which(x$Sec.Leg.Col=="E5")]<-1
x$SLcon5[which(x$Sec.Leg.Col=="F5")]<-1
x$SLcon5[which(x$Sec.Leg.Col=="G5")]<-1
x$LCongen<-rep(0,N)
x$LCongen[which(x$PLcon1==1|x$SLcon5==1)]<-1
x$LCongen[which(x$PLcon5==1|x$SLcon1==1)]<-1

x$TP<-rep(0,N)
x$TP[which(x$Tail.Pattern==1)]<-1
x$TP[which(x$Tail.Pattern==2)]<-1
x$TP[which(x$Tail.Pattern==3)]<-1
x$TP[which(x$Tail.Pattern==4)]<-1
x$TP[which(x$Tail.Pattern==5)]<-1
x$TP[which(x$Tail.Pattern==6)]<-1

x$NTC<-rep(0,N)
x$NTC[which(x$Num.Tail.Cols>1)]<-1

x$RedT<-rep(0,N)
x$RedT[which(x$Pri.Tail.Col=="F2")]<-1
x$RedT[which(x$Pri.Tail.Col=="F3")]<-1
x$RedT[which(x$Pri.Tail.Col=="F4")]<-1
x$RedT[which(x$Pri.Tail.Col=="F5")]<-1
x$RedT[which(x$Pri.Tail.Col=="G2")]<-1
x$RedT[which(x$Pri.Tail.Col=="G3")]<-1
x$RedT[which(x$Pri.Tail.Col=="G4")]<-1
x$RedT[which(x$Pri.Tail.Col=="G5")]<-1

x$DarkT<-rep(0,N)
x$DarkT[which(x$Pri.Tail.Col=="B5")]<-1
x$DarkT[which(x$Pri.Tail.Col=="C5")]<-1
x$DarkT[which(x$Pri.Tail.Col=="D5")]<-1
x$DarkT[which(x$Pri.Tail.Col=="E5")]<-1
x$DarkT[which(x$Pri.Tail.Col=="F5")]<-1
x$DarkT[which(x$Pri.Tail.Col=="G5")]<-1

x$PTcon1<-rep(0,N)
x$PTcon1[which(x$Pri.Tail.Col=="A1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="B1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="C1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="D1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="E1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="F1")]<-1
x$PTcon1[which(x$Pri.Tail.Col=="G1")]<-1
x$PTcon5<-rep(0,N)
x$PTcon5[which(x$Pri.Tail.Col=="B5")]<-1
x$PTcon5[which(x$Pri.Tail.Col=="C5")]<-1
x$PTcon5[which(x$Pri.Tail.Col=="D5")]<-1
x$PTcon5[which(x$Pri.Tail.Col=="E5")]<-1
x$PTcon5[which(x$Pri.Tail.Col=="F5")]<-1
x$PTcon5[which(x$Pri.Tail.Col=="G5")]<-1
x$STcon1<-rep(0,N)
x$STcon1[which(x$Sec.Tail.Col=="A1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="B1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="C1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="D1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="E1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="F1")]<-1
x$STcon1[which(x$Sec.Tail.Col=="G1")]<-1
x$STcon5<-rep(0,N)
x$STcon5[which(x$Sec.Tail.Col=="B5")]<-1
x$STcon5[which(x$Sec.Tail.Col=="C5")]<-1
x$STcon5[which(x$Sec.Tail.Col=="D5")]<-1
x$STcon5[which(x$Sec.Tail.Col=="E5")]<-1
x$STcon5[which(x$Sec.Tail.Col=="F5")]<-1
x$STcon5[which(x$Sec.Tail.Col=="G5")]<-1
x$TCongen<-rep(0,N)
x$TCongen[which(x$PTcon1==1|x$STcon5==1)]<-1
x$TCongen[which(x$PTcon5==1|x$STcon1==1)]<-1

#Install and open the necessary packages
install.packages("MCMCglmm")
install.packages("phangorn")
install.packages("beepr")
library(MCMCglmm)
library(phangorn)
library(beepr)

#Read in 1,000 random tree topologies, cut down to 100, then down to 1
trees<-read.nexus("trees1k.nex")
t100<-trees[1:100]
tree<-t100[[1]]
beep(2)

#Create csv of tip labels so you can see taxonomy of tree
tree$tip.label
treeforcsv<-data.frame(tree$tip.label)
write.csv(treeforcsv, file="tree tips trimmed.csv")

#Make sure the taxonomy of the dataset matches that of the tree
x$NewName<-paste(gsub(" ","_",x$Binomial),toupper(x$Family),toupper(x$Order),sep="_")

#Convert the continuous variables to vectors - changeable line(s)
x$Prec<-as.vector(x$Mean.Precipitation)
x$Evap<-as.vector(x$Mean.Evapotranspiration)
x$PC<-as.vector(x$PC1)
x$MidLat<-as.vector(x$MidRange.Latitude)

#Make all latitude values +ve to judge distance from equator only
x$MidLatPos<-abs(x$MidLat)
x$NewPC<-abs(x$PC)

#Make sure that there isn't a species in the dataset that's missing from the tree
bad<-rep(0,dim(x)[1])
for(i in 1:dim(x)[1]){
  if(sum(x$NewName[i]==tree$tip.label)==0){bad[i]=1}
}
if(sum(bad)>0){x<-x[-which(bad==1),]}

#Trim out species from the tree that are not in the dataset
t100<-lapply(t100,drop.tip,tip=setdiff(tree$tip.label,x$NewName))

#Scale the continuous variables - changeable line(s)
x$PR<-scale(log(as.numeric(x$Prec)))
x$EV<-scale(log(as.numeric(x$Evap)))
x$PCA<-scale(log(as.numeric(x$NewPC)))
x$ML<-scale(log(as.numeric(x$MidLatPos)))

#Force the tree to be ultrametric
i=1
tree<-t100[[i]] 
tree<-nnls.tree(cophenetic(tree),tree,rooted=TRUE)
animalA<-inverseA(tree)$Ainv

#Calculate the priors
##The '9' in 'mu=rep(0,9)' MUST be calculated according to the variables appearing in each model
###Calculation as follows: 1 + (number of continuous variables) + (k-1 for each categorical variable), where 'k' is the number of categories in the variable
####Variables that appear after 'V=gelman.prior(~' can be added/removed depending on what model is being run
prior.Test<-list(B=list(mu=rep(0,3),V=gelman.prior(~PCA+ML, data = x,  scale=1+pi^2/3)),R=list(V=1,fix=1),G=list(G1=list(V=1E-10,nu=-1)))

#Conduct a dummy run to set up the structure of the model and get the starting point
Final.disp<-MCMCglmm(TP~PCA+ML, 
                     random=~NewName, 
                     ginverse=list(NewName=animalA), 
                     prior = prior.Test, 
                     verbose=TRUE,
                     family="categorical", 
                     data = x,
                     nitt=11000,
                     thin=10,
                     burnin=1000,
                     pl=TRUE,
                     pr=TRUE,
                     slice=TRUE)

#Set up the starting point from the dummy run
nsamp.l<-nrow(Final.disp$VCV)
start1.l=list(R=Final.disp$VCV[nsamp.l,"units"], G=list(G1=Final.disp$VCV[nsamp.l,"NewName"]))

#Save the dummy run as a .Rdata file
save(Final.disp,file="RodTP-dummyrun.Rdata")

#Run the true model over 100 tree topologies
for(i in 1:100){
  tree<-t100[[i]]  
  tree<-nnls.tree(cophenetic(tree),tree,rooted=TRUE)
  
  animalA<-inverseA(tree)$Ainv
  
  mod<-MCMCglmm(TP~PCA+ML,  
                random=~NewName, 
                ginverse=list(NewName=animalA), 
                prior = prior.Test, 
                verbose=TRUE,
                family="categorical", 
                start= start1.l,
                data = x,
                nitt=11000,  
                thin=1000, 
                burnin=1000, 
                pl=TRUE, 
                pr=TRUE, 
                slice=TRUE)
  print(i)
  
  Final.disp$VCV[((i-1)*10+1):(i*10), ]<-mod$VCV[1:10,] 
  Final.disp$Sol[((i-1)*10+1):(i*10), ]<-mod$Sol[1:10,] 
  Final.disp$Liab[((i-1)*10+1):(i*10), ]<-mod$Liab[1:10,] 
  
  nsamp.l<-nrow(mod$VCV)
  start1.l=list(R=mod$VCV[nsamp.l,"units"], G=list(G1=mod$VCV[nsamp.l,"NewName"]))
  
  save(Final.disp,file="RodTP.Rdata")
  
}
beep(5)

#See the model summary
summary(Final.disp)

#See plots of the model distribution
par("mar")
par(mar=c(1,1,1,1))
plot(Final.disp)

#Make sure to save the model as a .Rdata file
save(Final.disp,file="RodTP.Rdata")

#Reset the working directory to check model VIFs (if you've moved Rdata file to different location)
setwd("C:/Users/natas/Documents/PhD/Data Analysis/R Outputs/RData Files/Ecogeographical Rules")

#Load in the .Rdata file for the relevant model
load("RodTP.Rdata")

#Create the VIF check function
vif.MCMCglmm <- function (fit, intercept.columns = c(1)) {
  nF <- fit$Fixed$nfl
  v <- cov(as.matrix(fit$X[,1:nF]))
  nam <- colnames(fit$Sol[,1:nF])
  v <- v[-intercept.columns, -intercept.columns, drop = FALSE]
  nam <- nam[-intercept.columns]
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

#Run the VIF check
vif.MCMCglmm(Final.disp)

#If VIFs too high, run PCA and model using PC scores
##Load necessary package
library(tidyverse)

#Select the data of necessary variables (cannot be categorical!)
y<-data.frame(x$PR,x$EV)

#Calculate overall PCs between variables
results<-prcomp(y,scale=TRUE)

#Flip sign of PC scores because R naturally has them in -ve
results$rotation<--1*results$rotation
results$rotation

#Calculate PCs for each species
results$y<--1*results$y
precevapPCs<-results$y

#Save species-level PCs as a csv
write.csv(precevapPCs, file="precevapPCs.csv")

#Plot PCs
biplot(results,scale=0)

#Get overall PC scores
results$sdev^2 / sum(results$sdev^2)
var_explained = results$sdev^2 / sum(results$sdev^2)

#Make a scree plot to see amount of variance explained by each PC
qplot(c(1:2), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#View summary of model when typing up results
summary(Final.disp)