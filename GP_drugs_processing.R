setwd("/Users/francinebennett/Desktop/analysis/NHS_analysis/")

# Load packages
require(ggplot2)
require(plyr)
require(googleVis)
require(RSQLite)
require(gdata)

# List filenames (currently up to July 2012. Present analysis only includes up to May, as after that the Atorva price changes)
file.list<-c("T201109PDP IEXT.csv","T201110PDP IEXT.csv","T201111PDP IEXT.csv","T201112PDP IEXT.csv","T201201PDP IEXT.csv","T201202PDP IEXT.csv","T201203PDP IEXT.csv","T201204PDP IEXT.csv","T201205PDP IEXT.CSV")
             #,"T201206PDP IEXT.csv","T201207PDP IEXT.csv")
write.csv(file.list,"file_list.txt",row.names=FALSE)
addresses<-read.csv("T201204ADD REXT.CSV",header=FALSE)
short.addresses<-addresses[,c(2,3,6,8)]

# Create list of potential problem drugs
GP.drugs <- read.csv("T201109PDP IEXT.csv", header=TRUE)
drug.list<-unique(GP.drugs$BNF.NAME)
drug.list<-drug.list[order(drug.list)]
statins<-drug.list[grep("statin",drug.list)]
statins<-statins[-grep("Nystatin",statins)]
clopidogrel<-c("Clopidogrel")
sartans<-c(
  "Azilsartan Medoxomil",
  "Candesartan Cilexetil",
  "Eprosartan",
  "Irbesartan",
  "Olmesartan Medoxomil",
  "Telmisartan",
  "Valsartan",
  "Losartan Potassium")

problem.drugs<-as.data.frame(rbind(cbind(as.character(statins),"statin"),cbind(clopidogrel,"clopidogrel"),cbind(sartans,"sartan")))
names(problem.drugs)<-c("Drug","category")
problem.drugs$Drug<-trim(problem.drugs$Drug)

# Set up data frames for results
total.problem.spend<-data.frame(matrix(nrow=0,ncol=4))
spend.practice<-data.frame(matrix(nrow=0,ncol=11))
spend.pct<-data.frame(matrix(nrow=0,ncol=6))
spend.practice.total<-data.frame(matrix(nrow=0,ncol=4))

# Loop to load, analyse, and remove large data files
for (i in 1:length(file.list)){
file.name<-file.list[i]
print(file.name)
GP.drugs <- read.csv(file.name, header=TRUE)
GP.drugs$BNF.NAME<-trim(GP.drugs$BNF.NAME)
surgery.subtotal<-aggregate(GP.drugs[,c("ACT.COST","ITEMS")],by=list(GP.drugs$PRACTICE,GP.drugs$PERIOD),FUN=sum)
names(surgery.subtotal)<-c("Practice.code","Month","cost.alldrugs","items.alldrugs")

t<-subset(GP.drugs,BNF.NAME %in% problem.drugs$Drug)
problem.spend<-aggregate(t[,c("ACT.COST","ITEMS")],by=list(t$BNF.NAME,t$PERIOD),FUN=sum)
names(problem.spend)<-c("Drug","Period","Spend","Items")
problem.spend$Spend<-round(problem.spend$Spend,digits=0)
problem.spend$Drug<-as.character(problem.spend$Drug)
total.problem.spend<-rbind(total.problem.spend,problem.spend)

# Calculations by practice
s<-aggregate(t[,c("ACT.COST","ITEMS")],by=list(t$PRACTICE,t$PERIOD,t$BNF.NAME),FUN=sum)
names(s)<-c("Practice.code","Month","Drug","cost.thisdrug","items.thisdrug")
s<-merge(s,surgery.subtotal,all.x=TRUE)
s<-merge(s,short.addresses,by.x="Practice.code",by.y="V2",all.x=TRUE)
spend.practice<-rbind(spend.practice,s)
}  

## Calculate Simvastatin 40mg price and other median actual prices
preparation.level<-read.csv("T201206PDPI+BNFT.csv")
simvastatin<-preparation.level[grep("Simvastatin_Tab 40mg",preparation.level$BNF.NAME),]
simva.price<-median(simvastatin$ACT.COST/simvastatin$ITEMS)
write.csv(simva.price,"simva_price.csv",row.names=FALSE)
atorvastatin<-spend.practice[grep("Atorvastatin",spend.practice$Drug),]
atorva.price<-median(atorvastatin$cost.thisdrug/atorvastatin$items.thisdrug)
rosuvastatin<-spend.practice[grep("Rosuvastatin Calcium",spend.practice$Drug),]
rosuva.price<-median(rosuvastatin$cost.thisdrug/rosuvastatin$items.thisdrug)

## Calculate and file savings figures
problem.drugs$saving<-as.numeric(0)
problem.drugs[problem.drugs$Drug=="Rosuvastatin Calcium",]$saving<-1-(simva.price/rosuva.price)
problem.drugs[problem.drugs$Drug=="Atorvastatin",]$saving<-1-(simva.price/atorva.price)

## Write out summary files for main analysis process
write.csv(spend.practice,"spend_practice.csv",row.names=FALSE,quote=FALSE)
write.csv(problem.drugs,"problem_drugs.csv",row.names=FALSE,quote=FALSE)
write.csv(problem.spend,"problem_spend.csv",row.names=FALSE,quote=FALSE)
write.csv(total.problem.spend,"total_problem_spend.csv",row.names=FALSE,quote=FALSE)