setwd("/Users/francinebennett/Desktop/analysis/NHS_analysis/")

# Load packages
require(ggplot2)
require(plyr)
require(googleVis)
require(gdata)

# load aggregate.data
file.list<-read.csv("file_list.txt")$x
spend.practice<-read.csv("spend_practice.csv")
problem.drugs<-read.csv("problem_drugs.csv")
problem.spend<-read.csv("problem_spend.csv")
total.problem.spend<-read.csv("total_problem_spend.csv")
simva.price<-read.csv("simva_price.csv")
simva.price<-simva.price[1,1]

spend.practice$item.pct<-spend.practice$items.thisdrug/spend.practice$items.alldrugs
total.problem.spend<-merge(total.problem.spend,problem.drugs,all.x=TRUE)
total.problem.spend$amount.wasted<-total.problem.spend$Spend*total.problem.spend$saving
wasted.totals<-aggregate(total.problem.spend[,c("Spend","Items","amount.wasted")],by=list("Drug"=total.problem.spend$Drug,"category"=total.problem.spend$category),FUN=sum)

## Calculate waste per practice
spend.practice<-merge(spend.practice,problem.drugs,all.x=TRUE)
spend.practice$amount.wasted<-spend.practice$cost.thisdrug*as.numeric(spend.practice$saving)

totals.headings<- list("Drug"=spend.practice$Drug,
                      "Practice.name"=spend.practice$V3,
                      "Practice.code"=spend.practice$Practice.code,
                      "Postcode"=spend.practice$V8,
                      "category"=spend.practice$category)
spend.practice.totals<-
  aggregate(spend.practice[,c("cost.thisdrug","items.thisdrug",
                              "amount.wasted")],
            by=totals.headings,
            FUN=sum)

temp.totals<-spend.practice[,c("Practice.code","Month","cost.alldrugs","items.alldrugs")]
temp.totals<-temp.totals[!duplicated(temp.totals),]
temp.totals<-aggregate(temp.totals[,c("cost.alldrugs","items.alldrugs")],by=list("Practice.code"=temp.totals$Practice.code),FUN=sum)
spend.practice.totals<-merge(spend.practice.totals,temp.totals,all.x=TRUE)
spend.practice.totals$item.pct<-spend.practice.totals$items.thisdrug/spend.practice.totals$items.alldrugs

# Switching over time
statins<-subset(spend.practice,category=="statin")
statins<-subset(statins,Drug!="Simvastatin")
statin.totals<-aggregate(statins[,"items.thisdrug"],by=list(statins$V3,statins$V6,statins$V8,statins$Practice.code,statins$Month),FUN=sum)
names(statin.totals)[4]<-"Practice.code"
names(statin.totals)[5]<-"Month"
statins<-subset(statins,Drug=="Atorvastatin")
statins<-merge(statins,statin.totals,all.x=TRUE)
statins<-statins[,c("Practice.code","Month","items.thisdrug","Group.1","Group.2","Group.3","x")]
statins$pct.prop.atorva<-statins$items.thisdrug/statins$x
statins<-subset(statins,x>50) # Only include clinics with >50 statin prescriptions/month
statins$Month<-as.Date(paste(statins$Month,"01"),"%Y%m%d")
boxplot(statins$pct.prop.atorva~statins$Month,ylim=c(0.35,1),main="Percent statin items which are Atorvastatin, by month")
boxplot(statins$items.thisdrug~statins$Month,ylim=c(0,300),main="Number of Atorvastatin items prescribed, by month")

## Looking for connections between overprescription by drug

# Create data frame of % of category and % of total prescriptions that are potentially problematic
practice.indicator<-spend.practice.totals[,c("Practice.name","Practice.code","Postcode","items.alldrugs")]
practice.indicator<-practice.indicator[!duplicated(practice.indicator),]
t<-subset(spend.practice.totals,category=="statin")
statin.total<-aggregate(t$items.thisdrug,by=list("Practice.name"=t$Practice.name,"Practice.code"=t$Practice.code),FUN=sum)
names(statin.total)[3]<-"items.statins"
t<-subset(spend.practice.totals,Drug=="Atorvastatin")
atorva<-aggregate(t$items.thisdrug,by=list("Practice.name"=t$Practice.name,"Practice.code"=t$Practice.code),FUN=sum)
names(atorva)[3]<-"items.atorva"
t<-subset(spend.practice.totals,Drug=="Rosuvastatin Calcium")
rosuva<-aggregate(t$items.thisdrug,by=list("Practice.name"=t$Practice.name,"Practice.code"=t$Practice.code),FUN=sum)
names(rosuva)[3]<-"items.rosuva"

practice.indicator<-merge(practice.indicator,statin.total,all.x=TRUE)
practice.indicator<-merge(practice.indicator,atorva,all.x=TRUE)
practice.indicator<-merge(practice.indicator,rosuva,all.x=TRUE)

practice.indicator$pct.statin.atorva<-practice.indicator$items.atorva/practice.indicator$items.statins
practice.indicator$pct.statin.rosuva<-practice.indicator$items.rosuva/practice.indicator$items.statins
practice.indicator[is.na(practice.indicator)]<-0

# Check for correlations between bad prescribing
summary(lm(practice.indicator$pct.statin.rosuva~practice.indicator$pct.statin.atorva))

# Plot different types of bad prescribing
ggplot(practice.indicator, aes(pct.statin.rosuva,pct.statin.atorva))+geom_point(aes(colour=log(items.alldrugs)))

# Basis for funnel plot
ggplot(practice.indicator, aes(items.alldrugs,pct.statin.atorva))+
  geom_point()+opts(title="Atorva funnel")
ggplot(practice.indicator, aes(items.alldrugs,pct.statin.rosuva))+
  geom_point()+opts(title="Rosuva funnel")

# Roll up to CCG level
ccg.rollup<-read.xls("list-of-proposed-practices-ccg.xls",sheet="Practice list")
spend.ccg.totals<-merge(spend.practice.totals,ccg.rollup,by.x="Practice.code",by.y="Practice.code",all.x=TRUE)
ccg.totals.headings<- list("Drug"=spend.ccg.totals$Drug,
                       "Proposed.CCG.name"=spend.ccg.totals$Proposed.CCG.name,
                       "Interim.CCG.code"=spend.ccg.totals$Interim.CCG.code,
                       "category"=spend.ccg.totals$category)
spend.ccg.totals<-
  aggregate(spend.ccg.totals[,c("cost.thisdrug","items.thisdrug",
                              "amount.wasted")],
            by=ccg.totals.headings,
            FUN=sum)

## Timeseries plots
t<-subset(spend.practice,Drug %in% c("Atorvastatin","Rosuvastatin Calcium","Simvastatin"))
statin.timeseries<-aggregate(t$cost.thisdrug,by=list(t$Month,t$Drug),FUN=sum)
names(statin.timeseries)<-c("Month","Drug","Spend")
statin.timeseries$Month<-as.Date(paste(statin.timeseries$Month,"01",sep=""),"%Y%m%d")
p <- ggplot(statin.timeseries, aes(Month, Spend)) + geom_line()+
  scale_y_continuous(limits = c(0, 3e7))+
  opts(strip.text.y=theme_text(size=15,angle=270))
p + facet_grid(Drug ~ .)

t<-subset(spend.practice,Drug %in% c("Atorvastatin","Rosuvastatin Calcium","Simvastatin"))
statin.item.timeseries<-aggregate(t$items.thisdrug,by=list(t$Month,t$Drug),FUN=sum)
names(statin.item.timeseries)<-c("Month","Drug","Items")
statin.item.timeseries$Month<-as.Date(paste(statin.item.timeseries$Month,"01",sep=""),"%Y%m%d")
p <- ggplot(statin.item.timeseries, aes(Month, Items)) + geom_line()
p + facet_grid(Drug ~ .)

## PCT mapping
pct.lookup<-read.csv("epcmem.csv",header=FALSE)
names(pct.lookup)<-c("Practice.code","PCT.code","Org.type","Join.date","Leave.date","Amended.record")
pct.lookup<-subset(pct.lookup,is.na(Leave.date))
pct.totals<-merge(spend.practice.totals,pct.lookup,all.x=TRUE)
pct.totals<-subset(pct.totals,category=="statin")
pct.totals$item.bad<-FALSE
pct.totals[pct.totals$Drug %in% c("Atorvastatin","Rosuvastatin Calcium"),]$item.bad<-TRUE
pct.totals<-aggregate(pct.totals$items.thisdrug,by=list(pct.totals$item.bad,pct.totals$PCT.code),FUN=sum)
pct.totals<-cast(pct.totals,Group.2~Group.1)
names(pct.totals)<-c("PCT.code","ok.drugs","problem.drugs")
pct.totals$pct.problem<-pct.totals$problem.drugs/(pct.totals$problem.drugs+pct.totals$ok.drugs)
pct.totals$total.items.month<-(pct.totals$ok.drugs+pct.totals$problem.drugs)/length(file.list)
pct.totals<-pct.totals[,c("PCT.code","total.items.month","pct.problem")]
pct.totals$pct.problem<-round(pct.totals$pct.problem,3)
pct.totals$total.items.month<-round(pct.totals$total.items.month,0)
write.csv(pct.totals,"pct_statin_totals.csv",row.names=FALSE)

## Savings figures
median(subset(statin.timeseries,Drug=="Atorvastatin")$Spend)*problem.drugs[problem.drugs$Drug=="Atorvastatin",]$saving
median(subset(statin.timeseries,Drug=="Rosuvastatin Calcium")$Spend)*problem.drugs[problem.drugs$Drug=="Rosuvastatin Calcium",]$saving

# JSON format for time series charts
require(RJSONIO)
toJSONarray <- function(dtf){
  clnms <- colnames(dtf)
  name.value <- function(i){
    quote <- '';
    if(class(dtf[, i])!='numeric'){
      quote <- '"';
    }
    paste('"', i, '" : ', quote, dtf[,i], quote, sep='')
  }
  objs <- apply(sapply(clnms, name.value), 1, function(x){paste(x, collapse=', ')})
  objs <- paste('{', objs, '}')
  res <- paste('[', paste(objs, collapse=', '), ']')
  return(res)
}
atorva<-subset(statin.timeseries,Drug=="Atorvastatin")[,c("Month","Spend")]
names(atorva)<-c("x","y")
atorva$x<-as.numeric(as.Date(atorva$x))*24*60*60
toJSONarray(atorva)

simva<-subset(statin.timeseries,Drug=="Simvastatin")[,c("Month","Spend")]
names(simva)<-c("x","y")
simva$x<-as.numeric(as.Date(simva$x))*24*60*60
toJSONarray(simva)

rosuva<-subset(statin.timeseries,Drug=="Rosuvastatin Calcium")[,c("Month","Spend")]
names(rosuva)<-c("x","y")
rosuva$x<-as.numeric(as.Date(rosuva$x))*24*60*60
toJSONarray(rosuva)



