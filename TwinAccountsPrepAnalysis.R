#Twin Accounts analysis
#Data sources: TA client list, TA credit score tracking (done outside of ETO), ETO data
#Create dataset combining TA participation & score information with general FOC population (including TA clients) score info
#Create matched comparison group(s) from non-TA participants in broader FOC population, compare outcomes

library("stringr", lib.loc="C:/Program Files/R/R-3.1.1/library")

#functions####
nF<-function(x) {ifelse(is.na(x), F,x)}
countN<-function(x) {as.integer(sum(!is.na(x)))}
rmean<-function(x){mean(x,na.rm=T)}
rmeanr<-function(x){round(mean(x,na.rm=T),2)}
rmedian<-function(x){median(x,na.rm=T)}
rsum<-function(x) {sum(x,na.rm=T)}

#function to get filepath depending on whether on Mac or Windows
pathFOCData<-
function(directory,filename){
  if(Sys.info()["sysname"]=="Darwin"){
    paste("/Users/srhrnkn/Dropbox/LISC/FOC/FOCData/",directory,"/",filename,sep="")
  }
  else{
    paste("C:\\\\Users\\\\SRankin\\\\Dropbox\\\\LISC\\\\FOC\\\\FOCData\\\\",directory,"\\\\",filename,sep="")
  }
}

#get data: TA client list, TA credit score tracking sheet, ETO data from FOC4yr####
read.csv(pathFOCData("Twin Accounts","Twin Accts client list_no names.csv"))->ta.clients
read.csv(pathFOCData("Twin Accounts","TrackingSheet40.csv"))->ta.tracking
load(pathFOCData("FOC4yr","ScoresSortedFICO")
load(pathFOCData("FOC4yr","IndividualOutcomes")
load(pathFOCData("FOC4yr","AllVars")


#clean up formats####
names(ta.clients)<-str_replace_all(names(ta.clients),"\\.\\.",".")
names(ta.tracking)<-str_replace_all(names(ta.tracking),"\\.\\.",".")
#fix scores coming in as factors - make new, set 0s to NA
ta.tracking$ScoreEntry<-ta.tracking$Credit.Score.At.Entry
ta.tracking$ScoreEntry[ta.tracking$ScoreEntry==0]<-NA
ta.tracking$Score6mo<-as.numeric(as.character(ta.tracking$Credit.Score.6.months))
ta.tracking$Score6mo[ta.tracking$Score6mo==0]<-NA
ta.tracking$Score12mo<-as.numeric(as.character(ta.tracking$Credit.Score.12.months))
ta.tracking$Score12mo[ta.tracking$Score12mo==0]<-NA
#fix dates
ta.tracking$Loan.Closing.Date<-as.Date(ta.tracking$Loan.Closing.Date,"%m/%d/%Y")
ta.tracking$Loan.Ending.Date<-as.Date(ta.tracking$Loan.Ending.Date,"%m/%d/%Y")

#additional fields####

#calc 6 mo and 12 mo
ta.tracking$Date6mo<-ta.tracking$Loan.Closing.Date+365/2
ta.tracking$Date12mo<-ta.tracking$Loan.Closing.Date+365

#add standardized site names
read.csv(pathFOCData("Twin Accounts","sitematchingTA.csv"))->ta.sitematch
names(ta.sitematch)<-c("agency","city","SITE","Notes")
ta.clients<-merge(ta.clients,ta.sitematch[,c("agency","SITE")],by="agency",all.x=T)
ta.clients$SitePartic<-paste(ta.clients$SITE,ta.clients$ID.number,sep="")
ta.tracking.id<-merge(ta.tracking,ta.clients[,c("SITE","SitePartic","acct.")],by.x="Acct.",by.y="acct.",all.x=T)
#add account statuses
ta.tracking.id$had.ta<-!is.na(ta.tracking.id$Loan.Status)
ta.tracking.id$completed.ta<-ta.tracking.id$Loan.Status=="Completed"
ta.tracking.id$ta.group<-"No Account"
ta.tracking.id$ta.group[ta.tracking.id$Loan.Status=="Completed"]<-"Completed"
ta.tracking.id$ta.group[ta.tracking.id$Loan.Status=="Open"]<-"Open"
ta.tracking.id$ta.group[grepl("Closed",ta.tracking.id$Loan.Status)]<-"Closed Early"
ta.tracking.id$ta.group<-factor(ta.tracking.id$ta.group,levels=c("No Account","Open","Completed","Closed Early"))


#Scores########
#get more recent scores for participants with missing credit status/score
#issue is that score status is blank or got score but FICO is missing.
#first reshape ta.tracking.id - there's prob a better way to do this but:
ta.entry.scores<-ta.tracking.id[,c("SITE","SitePartic","Loan.Closing.Date","ScoreEntry")]
ta.6mo.scores<-ta.tracking.id[,c("SITE","SitePartic","Date6mo","Score6mo")]
ta.12mo.scores<-ta.tracking.id[,c("SITE","SitePartic","Date12mo","Score12mo")]
names(ta.entry.scores)<-c("SITE","SitePartic","DateCreditReport","FICOScore")
names(ta.6mo.scores)<-c("SITE","SitePartic","DateCreditReport","FICOScore")
names(ta.12mo.scores)<-c("SITE","SitePartic","DateCreditReport","FICOScore")
tascoresstacked<-rbind(ta.entry.scores,ta.6mo.scores,ta.12mo.scores)
#get rid on unfound ids
tascoresstacked<-tascoresstacked[!is.na(tascoresstacked$SitePartic)&!grepl("Not included",tascoresstacked$SITE),]
tascoresstacked<-data.frame(tascoresstacked,DateCreditReportNums=as.integer(tascoresstacked$DateCreditReport),ScoreStatusFICO=ifelse(is.na(tascoresstacked$FICOScore),"Unscored","Scored"))
tascoresstacked$SITE<-factor(tascoresstacked$SITE,levels=levels(ScoresSortedFICO$SITE))

#stack ETO and tracking score data
ScoresSortedFICO.ta<-rbind(data.frame(ScoresSortedFICO[,names(ScoresSortedFICO) %in% names(tascoresstacked)],DataSource="ETO"),data.frame(tascoresstacked,DataSource="TATracking"))

#now re-do ranking etc
ScoresSortedFICO.ta$DateCreditReportRank<-ave(ScoresSortedFICO.ta$DateCreditReportNums,ScoresSortedFICO.ta$SitePartic,FUN = function(x) rank(x, ties.method = "first"))
ScoresSortedFICO.ta$MaxCreditReportCount<-ave(ScoresSortedFICO.ta$DateCreditReportRank,ScoresSortedFICO.ta$SitePartic,FUN = max)
ScoresSortedFICO.ta$DateCreditReportRank<-as.factor(ScoresSortedFICO.ta$DateCreditReportRank)
#sort by date
ScoresSortedFICO.ta[order(ScoresSortedFICO.ta$SitePartic,ScoresSortedFICO.ta$DateCreditReportRank),]->ScoresSortedFICO.ta

#Calc first date
ScoresSortedFICO.ta$FirstCr<-as.Date(ave(ScoresSortedFICO.ta$DateCreditReportNums,ScoresSortedFICO.ta$SitePartic,FUN = min),origin="1970-01-01")
#calc days from first credit report for each subsequent report
ScoresSortedFICO.ta$DaysFromFirst<-as.numeric(ScoresSortedFICO.ta$DateCreditReport-ScoresSortedFICO.ta$FirstCr)
#Add credit category
ScoresSortedFICO.ta$cr.cat<-as.factor(with(ScoresSortedFICO.ta,(ifelse(ScoreStatusFICO=="Scored",ifelse(FICOScore>=620,"Hi","Lo"),"Unscored"))))
#Add days from enrollment (this kicks out anyone not in Demo but that's ok, it will happen later anyway)
ScoresSortedFICO.ta<-merge(ScoresSortedFICO.ta,Demo[,c("SitePartic","EarliestEnrolled")],by="SitePartic")
ScoresSortedFICO.ta$DaysFromEnrolled<-as.numeric(ScoresSortedFICO.ta$DateCreditReport-ScoresSortedFICO.ta$EarliestEnrolled)
#(some of these are earlier than first enrollment date - presumably because they are pulling old credit reports? or should I be suspicious that these folks were previously enrolled?)

#first calc distance from 6monthly markers
ScoresSortedFICO.ta$diffto6mo<-ScoresSortedFICO.ta$DaysFromEnrolled-182.5
ScoresSortedFICO.ta$diffto12mo<-ScoresSortedFICO.ta$DaysFromEnrolled-365
ScoresSortedFICO.ta$diffto18mo<-ScoresSortedFICO.ta$DaysFromEnrolled-547.5
ScoresSortedFICO.ta$diffto24mo<-ScoresSortedFICO.ta$DaysFromEnrolled-730
ScoresSortedFICO.ta$diffto30mo<-ScoresSortedFICO.ta$DaysFromEnrolled-912.5
ScoresSortedFICO.ta$diffto36mo<-ScoresSortedFICO.ta$DaysFromEnrolled-1095
ScoresSortedFICO.ta$diffto42mo<-ScoresSortedFICO.ta$DaysFromEnrolled-1277.5
ScoresSortedFICO.ta$diffto48mo<-ScoresSortedFICO.ta$DaysFromEnrolled-1460

#6month scores - take the min of abs val diff to monthly, keep if it's in the 30-day range
ScoresSortedFICO.ta.6mo<-aggregate(abs(diffto6mo)~SitePartic,ScoresSortedFICO.ta,min)
names(ScoresSortedFICO.ta.6mo)[2]<-'diffto6mo'
ScoresSortedFICO.ta.6mo<-ScoresSortedFICO.ta.6mo[which(ScoresSortedFICO.ta.6mo$diffto6mo<=30),]
ScoresSortedFICO.ta.12mo<-aggregate(abs(diffto12mo)~SitePartic,ScoresSortedFICO.ta,min)
names(ScoresSortedFICO.ta.12mo)[2]<-'diffto12mo'
ScoresSortedFICO.ta.12mo<-ScoresSortedFICO.ta.12mo[which(ScoresSortedFICO.ta.12mo$diffto12mo<=30),]
ScoresSortedFICO.ta.18mo<-aggregate(abs(diffto18mo)~SitePartic,ScoresSortedFICO.ta,min)
names(ScoresSortedFICO.ta.18mo)[2]<-'diffto18mo'
ScoresSortedFICO.ta.18mo<-ScoresSortedFICO.ta.18mo[which(ScoresSortedFICO.ta.18mo$diffto18mo<=30),]
ScoresSortedFICO.ta.24mo<-aggregate(abs(diffto24mo)~SitePartic,ScoresSortedFICO.ta,min)
names(ScoresSortedFICO.ta.24mo)[2]<-'diffto24mo'
ScoresSortedFICO.ta.24mo<-ScoresSortedFICO.ta.24mo[which(ScoresSortedFICO.ta.24mo$diffto24mo<=30),]
ScoresSortedFICO.ta.30mo<-aggregate(abs(diffto30mo)~SitePartic,ScoresSortedFICO.ta,min)
names(ScoresSortedFICO.ta.30mo)[2]<-'diffto30mo'
ScoresSortedFICO.ta.30mo<-ScoresSortedFICO.ta.30mo[which(ScoresSortedFICO.ta.30mo$diffto30mo<=30),]
ScoresSortedFICO.ta.36mo<-aggregate(abs(diffto36mo)~SitePartic,ScoresSortedFICO.ta,min)
names(ScoresSortedFICO.ta.36mo)[2]<-'diffto36mo'
ScoresSortedFICO.ta.36mo<-ScoresSortedFICO.ta.36mo[which(ScoresSortedFICO.ta.36mo$diffto36mo<=30),]
ScoresSortedFICO.ta.42mo<-aggregate(abs(diffto42mo)~SitePartic,ScoresSortedFICO.ta,min)
names(ScoresSortedFICO.ta.42mo)[2]<-'diffto42mo'
ScoresSortedFICO.ta.42mo<-ScoresSortedFICO.ta.42mo[which(ScoresSortedFICO.ta.42mo$diffto42mo<=30),]
ScoresSortedFICO.ta.48mo<-aggregate(abs(diffto48mo)~SitePartic,ScoresSortedFICO.ta,min)
names(ScoresSortedFICO.ta.48mo)[2]<-'diffto48mo'
ScoresSortedFICO.ta.48mo<-ScoresSortedFICO.ta.48mo[which(ScoresSortedFICO.ta.48mo$diffto48mo<=30),]

#assign the flags
ScoresSortedFICO.ta$appr6moFICO<-paste0(ScoresSortedFICO.ta$SitePartic,abs(ScoresSortedFICO.ta$diffto6mo)) %in% paste0(ScoresSortedFICO.ta.6mo$SitePartic,ScoresSortedFICO.ta.6mo$diffto6mo)
ScoresSortedFICO.ta$appr12moFICO<-paste0(ScoresSortedFICO.ta$SitePartic,abs(ScoresSortedFICO.ta$diffto12mo)) %in% paste0(ScoresSortedFICO.ta.12mo$SitePartic,ScoresSortedFICO.ta.12mo$diffto12mo)
ScoresSortedFICO.ta$appr18moFICO<-paste0(ScoresSortedFICO.ta$SitePartic,abs(ScoresSortedFICO.ta$diffto18mo)) %in% paste0(ScoresSortedFICO.ta.18mo$SitePartic,ScoresSortedFICO.ta.18mo$diffto18mo)
ScoresSortedFICO.ta$appr24moFICO<-paste0(ScoresSortedFICO.ta$SitePartic,abs(ScoresSortedFICO.ta$diffto24mo)) %in% paste0(ScoresSortedFICO.ta.24mo$SitePartic,ScoresSortedFICO.ta.24mo$diffto24mo)
ScoresSortedFICO.ta$appr30moFICO<-paste0(ScoresSortedFICO.ta$SitePartic,abs(ScoresSortedFICO.ta$diffto30mo)) %in% paste0(ScoresSortedFICO.ta.30mo$SitePartic,ScoresSortedFICO.ta.30mo$diffto30mo)
ScoresSortedFICO.ta$appr36moFICO<-paste0(ScoresSortedFICO.ta$SitePartic,abs(ScoresSortedFICO.ta$diffto36mo)) %in% paste0(ScoresSortedFICO.ta.36mo$SitePartic,ScoresSortedFICO.ta.36mo$diffto36mo)
ScoresSortedFICO.ta$appr42moFICO<-paste0(ScoresSortedFICO.ta$SitePartic,abs(ScoresSortedFICO.ta$diffto42mo)) %in% paste0(ScoresSortedFICO.ta.42mo$SitePartic,ScoresSortedFICO.ta.42mo$diffto42mo)
ScoresSortedFICO.ta$appr48moFICO<-paste0(ScoresSortedFICO.ta$SitePartic,abs(ScoresSortedFICO.ta$diffto48mo)) %in% paste0(ScoresSortedFICO.ta.48mo$SitePartic,ScoresSortedFICO.ta.48mo$diffto48mo)

#remove temp objs
rm(ScoresSortedFICO.ta.6mo)
rm(ScoresSortedFICO.ta.12mo)
rm(ScoresSortedFICO.ta.18mo)
rm(ScoresSortedFICO.ta.24mo)
rm(ScoresSortedFICO.ta.30mo)
rm(ScoresSortedFICO.ta.36mo)
rm(ScoresSortedFICO.ta.42mo)
rm(ScoresSortedFICO.ta.48mo)


#could also add thin or thick here but would have to go back to Credit to grab it

#redo first/last. or figure out when the TA usuall falls after prog enrollment and get that one?? or??
#calc first and last
taincl.ind<-ScoresSortedFICO.ta[ScoresSortedFICO.ta$DateCreditReportRank==1,c(1:4,6:7,12)]
names(taincl.ind)[3:7]<-c("FirstCr","FirstFICOScore","FirstScoreStatusFICO","FirstDataSource","First.cr.cat")
taincl.ind<-merge(taincl.ind,ScoresSortedFICO.ta[ScoresSortedFICO.ta$DateCreditReportRank==ScoresSortedFICO.ta$MaxCreditReportCount&ScoresSortedFICO.ta$MaxCreditReportCount!=1,c(2:4,6:7,12,9,11)],by="SitePartic",all.x=T)
names(taincl.ind)[8:length(names(taincl.ind))]<-c("LastCr","LastFICOScore","LastScoreStatusFICO","LastDataSource","Last.cr.cat","LastCreditReportCount","LastDaysFromFirst")
#rename these so they can be distinguished from the IndividualOutcomes vars
names(taincl.ind)[3:length(names(taincl.ind))]<-paste("taincl",names(taincl.ind)[3:length(names(taincl.ind))],sep=".")
#add sixmonthly fields: date, Score status, score
taincl.ind<-merge(taincl.ind,unique(ScoresSortedFICO.ta[ScoresSortedFICO.ta$appr6moFICO==T,c(1,3,6,4)]),by='SitePartic',all.x=T)
names(taincl.ind)[(length(names(taincl.ind))-2):length(names(taincl.ind))]<-c('taincl.Date6mo','taincl.ScoreStatus6mo','taincl.FICOScore6mo')
taincl.ind<-merge(taincl.ind,unique(ScoresSortedFICO.ta[ScoresSortedFICO.ta$appr12moFICO==T,c(1,3,6,4)]),by='SitePartic',all.x=T)
names(taincl.ind)[(length(names(taincl.ind))-2):length(names(taincl.ind))]<-c('taincl.Date12mo','taincl.ScoreStatus12mo','taincl.FICOScore12mo')
taincl.ind<-merge(taincl.ind,unique(ScoresSortedFICO.ta[ScoresSortedFICO.ta$appr18moFICO==T,c(1,3,6,4)]),by='SitePartic',all.x=T)
names(taincl.ind)[(length(names(taincl.ind))-2):length(names(taincl.ind))]<-c('taincl.Date18mo','taincl.ScoreStatus18mo','taincl.FICOScore18mo')
taincl.ind<-merge(taincl.ind,unique(ScoresSortedFICO.ta[ScoresSortedFICO.ta$appr24moFICO==T,c(1,3,6,4)]),by='SitePartic',all.x=T)
names(taincl.ind)[(length(names(taincl.ind))-2):length(names(taincl.ind))]<-c('taincl.Date24mo','taincl.ScoreStatus24mo','taincl.FICOScore24mo')
taincl.ind<-merge(taincl.ind,unique(ScoresSortedFICO.ta[ScoresSortedFICO.ta$appr30moFICO==T,c(1,3,6,4)]),by='SitePartic',all.x=T)
names(taincl.ind)[(length(names(taincl.ind))-2):length(names(taincl.ind))]<-c('taincl.Date30mo','taincl.ScoreStatus30mo','taincl.FICOScore30mo')
taincl.ind<-merge(taincl.ind,unique(ScoresSortedFICO.ta[ScoresSortedFICO.ta$appr36moFICO==T,c(1,3,6,4)]),by='SitePartic',all.x=T)
names(taincl.ind)[(length(names(taincl.ind))-2):length(names(taincl.ind))]<-c('taincl.Date36mo','taincl.ScoreStatus36mo','taincl.FICOScore36mo')
taincl.ind<-merge(taincl.ind,unique(ScoresSortedFICO.ta[ScoresSortedFICO.ta$appr42moFICO==T,c(1,3,6,4)]),by='SitePartic',all.x=T)
names(taincl.ind)[(length(names(taincl.ind))-2):length(names(taincl.ind))]<-c('taincl.Date42mo','taincl.ScoreStatus42mo','taincl.FICOScore42mo')
taincl.ind<-merge(taincl.ind,unique(ScoresSortedFICO.ta[ScoresSortedFICO.ta$appr48moFICO==T,c(1,3,6,4)]),by='SitePartic',all.x=T)
names(taincl.ind)[(length(names(taincl.ind))-2):length(names(taincl.ind))]<-c('taincl.Date48mo','taincl.ScoreStatus48mo','taincl.FICOScore48mo')

#manually kill two duped records for now (taking the later in cases where there are symetrically timed reports)
taincl.ind<-taincl.ind[-c(30190,13250),]

#Now do the same for the TA group based on days from TA opening

#merge ScoresSortedFICO.ta with ta.tracking.id fields to get account opening dates

#three accounts have duped records - all because they have multiple accounts - leave these out
#Emerge124516 has two accounts - one closed early, one completed
#same with Metro107555 but both closed early
#NLEN145111 one closed early, one open
doubleTA<-data.frame(SitePartic=c("Emerge124516","Metro107555","NLEN145111"),DateEarlierTA=as.Date(c("2014-02-05","2013-03-25","2014-09-16")))

ScoresSortedFICO.ta.holders<-merge(ta.tracking.id[!(paste0(ta.tracking.id$SitePartic,ta.tracking.id$Loan.Closing.Date) %in% paste0(doubleTA$SitePartic,doubleTA$DateEarlierTA)),c("SitePartic","Loan.Closing.Date","Loan.Ending.Date","had.ta","completed.ta","ta.group")],ScoresSortedFICO.ta)

ScoresSortedFICO.ta.holders$DaysFromTA<-as.numeric(ScoresSortedFICO.ta.holders$DateCreditReport-ScoresSortedFICO.ta.holders$Loan.Closing.Date)


#first calc distance from 6monthly markers
ScoresSortedFICO.ta.holders$TAdiffto6mo<-ScoresSortedFICO.ta.holders$DaysFromTA-182.5
ScoresSortedFICO.ta.holders$TAdiffto12mo<-ScoresSortedFICO.ta.holders$DaysFromTA-365
ScoresSortedFICO.ta.holders$TAdiffto18mo<-ScoresSortedFICO.ta.holders$DaysFromTA-547.5
ScoresSortedFICO.ta.holders$TAdiffto24mo<-ScoresSortedFICO.ta.holders$DaysFromTA-730
ScoresSortedFICO.ta.holders$TAdiffto30mo<-ScoresSortedFICO.ta.holders$DaysFromTA-912.5
ScoresSortedFICO.ta.holders$TAdiffto36mo<-ScoresSortedFICO.ta.holders$DaysFromTA-1095
ScoresSortedFICO.ta.holders$TAdiffto42mo<-ScoresSortedFICO.ta.holders$DaysFromTA-1277.5
ScoresSortedFICO.ta.holders$TAdiffto48mo<-ScoresSortedFICO.ta.holders$DaysFromTA-1460

#6month scores - take the min of abs val TAdiff to monthly, keep if it's in the 30-day range
ScoresSortedFICO.ta.holders.6mo<-aggregate(abs(TAdiffto6mo)~SitePartic,ScoresSortedFICO.ta.holders,min)
names(ScoresSortedFICO.ta.holders.6mo)[2]<-'TAdiffto6mo'
ScoresSortedFICO.ta.holders.6mo<-ScoresSortedFICO.ta.holders.6mo[which(ScoresSortedFICO.ta.holders.6mo$TAdiffto6mo<=30),]
ScoresSortedFICO.ta.holders.12mo<-aggregate(abs(TAdiffto12mo)~SitePartic,ScoresSortedFICO.ta.holders,min)
names(ScoresSortedFICO.ta.holders.12mo)[2]<-'TAdiffto12mo'
ScoresSortedFICO.ta.holders.12mo<-ScoresSortedFICO.ta.holders.12mo[which(ScoresSortedFICO.ta.holders.12mo$TAdiffto12mo<=30),]
ScoresSortedFICO.ta.holders.18mo<-aggregate(abs(TAdiffto18mo)~SitePartic,ScoresSortedFICO.ta.holders,min)
names(ScoresSortedFICO.ta.holders.18mo)[2]<-'TAdiffto18mo'
ScoresSortedFICO.ta.holders.18mo<-ScoresSortedFICO.ta.holders.18mo[which(ScoresSortedFICO.ta.holders.18mo$TAdiffto18mo<=30),]
ScoresSortedFICO.ta.holders.24mo<-aggregate(abs(TAdiffto24mo)~SitePartic,ScoresSortedFICO.ta.holders,min)
names(ScoresSortedFICO.ta.holders.24mo)[2]<-'TAdiffto24mo'
ScoresSortedFICO.ta.holders.24mo<-ScoresSortedFICO.ta.holders.24mo[which(ScoresSortedFICO.ta.holders.24mo$TAdiffto24mo<=30),]
ScoresSortedFICO.ta.holders.30mo<-aggregate(abs(TAdiffto30mo)~SitePartic,ScoresSortedFICO.ta.holders,min)
names(ScoresSortedFICO.ta.holders.30mo)[2]<-'TAdiffto30mo'
ScoresSortedFICO.ta.holders.30mo<-ScoresSortedFICO.ta.holders.30mo[which(ScoresSortedFICO.ta.holders.30mo$TAdiffto30mo<=30),]
ScoresSortedFICO.ta.holders.36mo<-aggregate(abs(TAdiffto36mo)~SitePartic,ScoresSortedFICO.ta.holders,min)
names(ScoresSortedFICO.ta.holders.36mo)[2]<-'TAdiffto36mo'
ScoresSortedFICO.ta.holders.36mo<-ScoresSortedFICO.ta.holders.36mo[which(ScoresSortedFICO.ta.holders.36mo$TAdiffto36mo<=30),]
ScoresSortedFICO.ta.holders.42mo<-aggregate(abs(TAdiffto42mo)~SitePartic,ScoresSortedFICO.ta.holders,min)
names(ScoresSortedFICO.ta.holders.42mo)[2]<-'TAdiffto42mo'
ScoresSortedFICO.ta.holders.42mo<-ScoresSortedFICO.ta.holders.42mo[which(ScoresSortedFICO.ta.holders.42mo$TAdiffto42mo<=30),]
ScoresSortedFICO.ta.holders.48mo<-aggregate(abs(TAdiffto48mo)~SitePartic,ScoresSortedFICO.ta.holders,min)
names(ScoresSortedFICO.ta.holders.48mo)[2]<-'TAdiffto48mo'
ScoresSortedFICO.ta.holders.48mo<-ScoresSortedFICO.ta.holders.48mo[which(ScoresSortedFICO.ta.holders.48mo$TAdiffto48mo<=30),]

#assign the flags
ScoresSortedFICO.ta.holders$TAappr6moFICO<-paste0(ScoresSortedFICO.ta.holders$SitePartic,abs(ScoresSortedFICO.ta.holders$TAdiffto6mo)) %in% paste0(ScoresSortedFICO.ta.holders.6mo$SitePartic,ScoresSortedFICO.ta.holders.6mo$TAdiffto6mo)
ScoresSortedFICO.ta.holders$TAappr12moFICO<-paste0(ScoresSortedFICO.ta.holders$SitePartic,abs(ScoresSortedFICO.ta.holders$TAdiffto12mo)) %in% paste0(ScoresSortedFICO.ta.holders.12mo$SitePartic,ScoresSortedFICO.ta.holders.12mo$TAdiffto12mo)
ScoresSortedFICO.ta.holders$TAappr18moFICO<-paste0(ScoresSortedFICO.ta.holders$SitePartic,abs(ScoresSortedFICO.ta.holders$TAdiffto18mo)) %in% paste0(ScoresSortedFICO.ta.holders.18mo$SitePartic,ScoresSortedFICO.ta.holders.18mo$TAdiffto18mo)
ScoresSortedFICO.ta.holders$TAappr24moFICO<-paste0(ScoresSortedFICO.ta.holders$SitePartic,abs(ScoresSortedFICO.ta.holders$TAdiffto24mo)) %in% paste0(ScoresSortedFICO.ta.holders.24mo$SitePartic,ScoresSortedFICO.ta.holders.24mo$TAdiffto24mo)
ScoresSortedFICO.ta.holders$TAappr30moFICO<-paste0(ScoresSortedFICO.ta.holders$SitePartic,abs(ScoresSortedFICO.ta.holders$TAdiffto30mo)) %in% paste0(ScoresSortedFICO.ta.holders.30mo$SitePartic,ScoresSortedFICO.ta.holders.30mo$TAdiffto30mo)
ScoresSortedFICO.ta.holders$TAappr36moFICO<-paste0(ScoresSortedFICO.ta.holders$SitePartic,abs(ScoresSortedFICO.ta.holders$TAdiffto36mo)) %in% paste0(ScoresSortedFICO.ta.holders.36mo$SitePartic,ScoresSortedFICO.ta.holders.36mo$TAdiffto36mo)
ScoresSortedFICO.ta.holders$TAappr42moFICO<-paste0(ScoresSortedFICO.ta.holders$SitePartic,abs(ScoresSortedFICO.ta.holders$TAdiffto42mo)) %in% paste0(ScoresSortedFICO.ta.holders.42mo$SitePartic,ScoresSortedFICO.ta.holders.42mo$TAdiffto42mo)
ScoresSortedFICO.ta.holders$TAappr48moFICO<-paste0(ScoresSortedFICO.ta.holders$SitePartic,abs(ScoresSortedFICO.ta.holders$TAdiffto48mo)) %in% paste0(ScoresSortedFICO.ta.holders.48mo$SitePartic,ScoresSortedFICO.ta.holders.48mo$TAdiffto48mo)

#remove temp objs
rm(ScoresSortedFICO.ta.holders.6mo)
rm(ScoresSortedFICO.ta.holders.12mo)
rm(ScoresSortedFICO.ta.holders.18mo)
rm(ScoresSortedFICO.ta.holders.24mo)
rm(ScoresSortedFICO.ta.holders.30mo)
rm(ScoresSortedFICO.ta.holders.36mo)
rm(ScoresSortedFICO.ta.holders.42mo)
rm(ScoresSortedFICO.ta.holders.48mo)

#wrap up to ind level: date, Score status, score for each 6mo

scores.ta.holders.ind<-ta.tracking.id[!(paste0(ta.tracking.id$SitePartic,ta.tracking.id$Loan.Closing.Date) %in% paste0(doubleTA$SitePartic,doubleTA$DateEarlierTA))&(ta.tracking.id$SitePartic %in% ScoresSortedFICO.ta.holders$SitePartic),c("SitePartic","Loan.Closing.Date","Loan.Ending.Date","had.ta","completed.ta","ta.group")]

scores.ta.holders.ind<-merge(scores.ta.holders.ind,unique(ScoresSortedFICO.ta.holders[ScoresSortedFICO.ta.holders$TAappr6moFICO==T,c(1,8,11,9)]),by='SitePartic',all.x=T)
names(scores.ta.holders.ind)[(length(names(scores.ta.holders.ind))-2):length(names(scores.ta.holders.ind))]<-c('taincl.Date6mo.TA','taincl.ScoreStatus6mo.TA','taincl.FICOScore6mo.TA')
scores.ta.holders.ind<-merge(scores.ta.holders.ind,unique(ScoresSortedFICO.ta.holders[ScoresSortedFICO.ta.holders$TAappr12moFICO==T,c(1,8,11,9)]),by='SitePartic',all.x=T)
names(scores.ta.holders.ind)[(length(names(scores.ta.holders.ind))-2):length(names(scores.ta.holders.ind))]<-c('taincl.Date12mo.TA','taincl.ScoreStatus12mo.TA','taincl.FICOScore12mo.TA')
scores.ta.holders.ind<-merge(scores.ta.holders.ind,unique(ScoresSortedFICO.ta.holders[ScoresSortedFICO.ta.holders$TAappr18moFICO==T,c(1,8,11,9)]),by='SitePartic',all.x=T)
names(scores.ta.holders.ind)[(length(names(scores.ta.holders.ind))-2):length(names(scores.ta.holders.ind))]<-c('taincl.Date18mo.TA','taincl.ScoreStatus18mo.TA','taincl.FICOScore18mo.TA')
scores.ta.holders.ind<-merge(scores.ta.holders.ind,unique(ScoresSortedFICO.ta.holders[ScoresSortedFICO.ta.holders$TAappr24moFICO==T,c(1,8,11,9)]),by='SitePartic',all.x=T)
names(scores.ta.holders.ind)[(length(names(scores.ta.holders.ind))-2):length(names(scores.ta.holders.ind))]<-c('taincl.Date24mo.TA','taincl.ScoreStatus24mo.TA','taincl.FICOScore24mo.TA')
scores.ta.holders.ind<-merge(scores.ta.holders.ind,unique(ScoresSortedFICO.ta.holders[ScoresSortedFICO.ta.holders$TAappr30moFICO==T,c(1,8,11,9)]),by='SitePartic',all.x=T)
names(scores.ta.holders.ind)[(length(names(scores.ta.holders.ind))-2):length(names(scores.ta.holders.ind))]<-c('taincl.Date30mo.TA','taincl.ScoreStatus30mo.TA','taincl.FICOScore30mo.TA')
scores.ta.holders.ind<-merge(scores.ta.holders.ind,unique(ScoresSortedFICO.ta.holders[ScoresSortedFICO.ta.holders$TAappr36moFICO==T,c(1,8,11,9)]),by='SitePartic',all.x=T)
names(scores.ta.holders.ind)[(length(names(scores.ta.holders.ind))-2):length(names(scores.ta.holders.ind))]<-c('taincl.Date36mo.TA','taincl.ScoreStatus36mo.TA','taincl.FICOScore36mo.TA')
scores.ta.holders.ind<-merge(scores.ta.holders.ind,unique(ScoresSortedFICO.ta.holders[ScoresSortedFICO.ta.holders$TAappr42moFICO==T,c(1,8,11,9)]),by='SitePartic',all.x=T)
names(scores.ta.holders.ind)[(length(names(scores.ta.holders.ind))-2):length(names(scores.ta.holders.ind))]<-c('taincl.Date42mo.TA','taincl.ScoreStatus42mo.TA','taincl.FICOScore42mo.TA')
scores.ta.holders.ind<-merge(scores.ta.holders.ind,unique(ScoresSortedFICO.ta.holders[ScoresSortedFICO.ta.holders$TAappr48moFICO==T,c(1,8,11,9)]),by='SitePartic',all.x=T)
names(scores.ta.holders.ind)[(length(names(scores.ta.holders.ind))-2):length(names(scores.ta.holders.ind))]<-c('taincl.Date48mo.TA','taincl.ScoreStatus48mo.TA','taincl.FICOScore48mo.TA')



#ta.ch merges all the Individual outcomes data with the ta tracking data
ta.ch<-merge(IndividualOutcomes[,as.character(AllVars$Name[AllVars$in.IndOut==T])],ta.tracking.id,by=c("SITE","SitePartic"),all.x=T)
#and then also add the taincl version of the credit vars
ta.ch<-merge(ta.ch,taincl.ind, by=c("SitePartic","SITE"),all.x=T)
#temp line to add the new sixmonthly vars - starting over this would be included in above
#ta.ch<-merge(ta.ch,taincl.ind[,c("SitePartic",names(taincl.ind)[!names(taincl.ind) %in% names(ta.ch)])],by="SitePartic",all.x=T)
#add the 6monthly TA vars
ta.ch<-merge(ta.ch,scores.ta.holders.ind[,c("SitePartic",names(scores.ta.holders.ind)[!names(scores.ta.holders.ind) %in% names(ta.ch)])],by="SitePartic",all.x=T)
#everyone not in the ta.tracking group has no account
ta.ch$ta.group[is.na(ta.ch$ta.group)]<-"No Account"
#create boolean for whether they completed and whether they have an account at all
ta.ch$is.completed<-ta.ch$ta.group=="Completed"
ta.ch$is.ta<-ta.ch$ta.group!="No Account"
#create group for closed early. can't have NAs because using for match.
ta.ch$is.closed.early.ind<-ta.ch$ta.group=="Closed Early"
ta.ch$is.closed.early.ind[is.na(ta.ch$is.closed.early.ind)]<-F

#create final sixmonthly vars that take the .TA version for TA folks and the non .TA version for non-TA
ta.ch$taincl.FICOScore6mo.byista<-ifelse(ta.ch[,"is.ta"],ta.ch$taincl.FICOScore6mo.TA,ta.ch$taincl.FICOScore6mo)
ta.ch$taincl.FICOScore12mo.byista<-ifelse(ta.ch[,"is.ta"],ta.ch$taincl.FICOScore12mo.TA,ta.ch$taincl.FICOScore12mo)
ta.ch$taincl.FICOScore18mo.byista<-ifelse(ta.ch[,"is.ta"],ta.ch$taincl.FICOScore18mo.TA,ta.ch$taincl.FICOScore18mo)
ta.ch$taincl.FICOScore24mo.byista<-ifelse(ta.ch[,"is.ta"],ta.ch$taincl.FICOScore24mo.TA,ta.ch$taincl.FICOScore24mo)
ta.ch$taincl.FICOScore30mo.byista<-ifelse(ta.ch[,"is.ta"],ta.ch$taincl.FICOScore30mo.TA,ta.ch$taincl.FICOScore30mo)
ta.ch$taincl.FICOScore36mo.byista<-ifelse(ta.ch[,"is.ta"],ta.ch$taincl.FICOScore36mo.TA,ta.ch$taincl.FICOScore36mo)
ta.ch$taincl.FICOScore42mo.byista<-ifelse(ta.ch[,"is.ta"],ta.ch$taincl.FICOScore42mo.TA,ta.ch$taincl.FICOScore42mo)
ta.ch$taincl.FICOScore48mo.byista<-ifelse(ta.ch[,"is.ta"],ta.ch$taincl.FICOScore48mo.TA,ta.ch$taincl.FICOScore48mo)



#create credit category
ta.ch$First.cr.cat<-as.factor(with(ta.ch,(ifelse(FirstScoreStatus=="No score available (insufficient credit history)","Unscored",as.character(FirstFICOCat)))))
ta.ch$ta.start.cr.cat<-factor(NA,levels = levels(ta.ch$First.cr.cat))
ta.ch$ta.start.cr.cat[ta.ch$is.ta==T]<-as.factor(with(ta.ch[ta.ch$is.ta==T,],(ifelse(is.na(ScoreEntry),"Unscored",ifelse(ScoreEntry>=620,"Hi","Lo")))))
ta.ch$FirstCrCloseLag<-as.numeric(ta.ch$Loan.Closing.Date-ta.ch$FirstCr)
ta.ch$LoanEndLastCrLag<-as.numeric(ta.ch$LastCr-ta.ch$Loan.Ending.Date)
ta.ch$CloseLastCrLag<-as.numeric(ta.ch$LastCr-ta.ch$Loan.Closing.Date)
#add indicator to ta.ch for cases where the TA is completed AND the last Credit date is after the ending date
ta.ch$is.compl.timingok<-ta.ch$is.completed&ta.ch$LoanEndLastCrLag>=0&ta.ch$FirstCrCloseLag>=0
ta.ch$is.compl.timingok[is.na(ta.ch$is.compl.timingok)]<-F

#redo this for taincl versions of vars
#taincl.First.cr.cat already exists
ta.ch$taincl.FirstCrCloseLag<-as.numeric(ta.ch$Loan.Closing.Date-ta.ch$taincl.FirstCr)
ta.ch$taincl.LoanEndLastCrLag<-as.numeric(ta.ch$taincl.LastCr-ta.ch$Loan.Ending.Date)
ta.ch$taincl.CloseLastCrLag<-as.numeric(ta.ch$taincl.LastCr-ta.ch$Loan.Closing.Date)
#add indicator to ta.ch for cases where the TA is completed AND the last Credit date is after the ending date. But maybe we don't care about this now - the only cases where this test doesn't come out positive are the ones where the loan ending date is later than a year after the opening.
ta.ch$taincl.is.compl.timingok<-ta.ch$is.completed&ta.ch$taincl.LoanEndLastCrLag>=&ta.ch$taincl.FirstCrCloseLag>=0
ta.ch$taincl.is.compl.timingok[is.na(ta.ch$taincl.is.compl.timingok)]<-F

#compute changes for taincl versions
ta.ch$taincl.FICOCh<-ta.ch$taincl.LastFICOScore-ta.ch$taincl.FirstFICOScore
ta.ch$taincl.FICOInc<-ta.ch$taincl.FICOCh>0
ta.ch$taincl.BecameScored<-ta.ch$taincl.FirstScoreStatusFICO=="Unscored"&ta.ch$taincl.LastScoreStatusFICO!="Unscored"
ta.ch$taincl.BecameScored[ta.ch$taincl.FirstScoreStatusFICO=="Scored"]<-NA
ta.ch$taincl.First.has.score<-ifelse(ta.ch$taincl.FirstScoreStatusFICO=="Scored",1,0)
ta.ch$taincl.Last.has.score<-ifelse(ta.ch$taincl.LastScoreStatusFICO=="Scored",1,0)


#look at dates for completed
ta.datef<-ta.ch[ta.ch$is.completed,c("SitePartic","FirstCr","LastCr","First.cr.cat","FirstFICOClean","LastFICOClean","FICOCh","Loan.Closing.Date","Date12mo","Loan.Ending.Date","ScoreEntry","Score12mo","LastDateCreditReportRank")]
ta.datef$FirstCrCloseLag<-as.numeric(ta.datef$Loan.Closing.Date-ta.datef$FirstCr)
ta.datef$LoanEndLastCrLag<-as.numeric(ta.datef$LastCr-ta.datef$Loan.Ending.Date)

#Add first last lag
ta.ch$taincl.FirstLastFICOLag<-as.numeric(ta.ch$taincl.LastCr-ta.ch$taincl.FirstCr)

#add additional vars needed for match
ta.ch$BankrBase<-ta.ch$SitePartic %in% Baseline$SitePartic[Baseline$InBankruptcy=="Yes"]


#note: FICOCh calcs are showing zero for cases where there is only ever one FICO pull so it should be null -  taincl version of FICO Ch handles this ok, just not FICOCh itself, don't use



      

#Analysis ####
#Analyze pre and post outcomes for TA account holders & compare to matched comparison group. 

library("MatchIt", lib.loc="C:/Program Files/R/R-3.1.1/library")

#declare function for comparing outcome vars. df is the data frame to analyze. depvars is a character vector of names of dependent variables. deptypes is a vector with the dependent variable types - logical (boolean) or numeric. indvar is the independent variable. Returns a data frame with the means, medians, and Ns for dependent variables grouped by independent variable, and the result of either an anova or chi sq sig test for each dependent variable (depending on data type). To be used after matching is performed. 
OutcomeDF<-function(df,depvars,deptypes,indvar){
  tempby<-by(data = df,INDICES = df[,indvar],FUN = function(x){rMnMedNr(x,depvars)})
  tempsigs<-numeric()
  for(i in 1:length(depvars)){
    if(deptypes[i]=="boolean"){
      tempsigs<-c(tempsigs,chisq.test(table(df[,indvar],df[,depvars[i]]))$p.value)
    }
    else{
      tempsigs<-c(tempsigs,summary(aov(as.formula(paste0(depvars[i],"~",indvar)),df))[[1]][[5]][[1]][[1]])
    }
  }
  tempdf<-cbind(as.data.frame(tempby[[1]]),as.data.frame(tempby[[2]]),Sigs=round(tempsigs,4))
  names(tempdf)<-c(paste0(names(as.data.frame(tempby[[1]])),".Comp"),paste0(names(as.data.frame(tempby[[2]])),".Tr"),"Sig")
  tempdf
}

#Analysis prior to matching####

#Get list of TA analysis outcome vars. Can always change/add to this. 

first.last.ta.outcome.vars<-c("taincl.FirstFICOScore","taincl.LastFICOScore","taincl.First.has.score","taincl.Last.has.score","taincl.FICOCh","taincl.FICOInc","taincl.BecameScored") 
first.last.ta.outcome.vartypes<-c("numeric","numeric","boolean","boolean","numeric","boolean","boolean") 
sixmonthly.ta.outcomevars<-c("taincl.FICOScore6mo.byista" , "taincl.FICOScore12mo.byista" ,"taincl.FICOScore18mo.byista" ,"taincl.FICOScore24mo.byista","taincl.FICOScore30mo.byista", "taincl.FICOScore36mo.byista" ,"taincl.FICOScore42mo.byista", "taincl.FICOScore48mo.byista")
sixmonthly.ta.outcomevartypes<-rep("numeric",8)

#outcome vars by has TA - all data
firstlast.ta.ch.byallTA<-OutcomeDF(ta.ch,first.last.ta.outcome.vars,first.last.ta.outcome.vartypes,"is.ta")
firstlast.ta.ch.bycomplTA<-OutcomeDF(ta.ch,first.last.ta.outcome.vars,first.last.ta.outcome.vartypes,"is.compl.timingok")

sixmonthly.ta.ch.byallTA<-OutcomeDF(ta.ch,sixmonthly.ta.outcomevars,sixmonthly.ta.outcomevartypes,"is.ta")
sixmonthly.ta.ch.bycomplTA<-OutcomeDF(ta.ch,sixmonthly.ta.outcomevars,sixmonthly.ta.outcomevartypes,"is.compl.timingok")


##Create matched comparison groups####
#start counter - this is so can store results & their iteration number - these are in object called matchiter - can always check back to see what filter, ind var, dep vars each iteration used. (All of this would prob be easier with broom!)
#matchiternum<-1
#start data frame
#matchiter<-data.frame(Iter=matchiternum,Vars=paste("'",ta.matchvars,collapse="','","'"),Treatment="is.completed",Notes="",Filter="")

#write function to revert to previous iter and rerun code
##this is all goofy because of string issues
# ta.setmatch<-function(x){
#   ta.matchvars<-as.character(matchiter$Vars[matchiter$Iter==x])
#   ta.treatment<-matchiter$Treatment[matchiter$Iter==x]
#   ta.filter<-matchiter$Filter[matchiter$Iter==x]
# }


#increment counter up
matchiternum<-matchiternum + 1
#set match vars and treatment group (edit this by hand for each new model) 
ta.matchvars<-c("Age","RaceEthnicity","CriminalConvictions","Education1","Gender","taincl.First.cr.cat","AnnualHHIncome")
ta.treatment<-"taincl.is.compl.timingok"
ta.filter<-'ta.ch$SITE!="FocusHope"&!grepl("Indy",ta.ch$SITE)&(ta.ch$is.ta==T|ta.ch$BankrBase==F)&(ta.ch$is.ta==F|ta.ch[,ta.treatment]==T)&(ta.ch$SitePartic %in% IndividualOutcomes$SitePartic)&ta.ch$taincl.FirstLastFICOLag>365'
#record matchvars
matchiter<-rbind(matchiter,data.frame(Iter=matchiternum,Vars=paste("'",ta.matchvars,collapse="','","'"),Treatment=ta.treatment,Filter=ta.filter,Notes="same as 27 but AnnualHHIncome instead of First Net Income"))


#create dataset which doesn't have any NAs for any match vars for matchit to work
ta.match<-ta.ch[eval(parse(text=ta.filter)),][apply(ta.ch[eval(parse(text=ta.filter)),ta.matchvars],MARGIN = 1,FUN=function(x){sum(is.na(x))==0}),c(ta.matchvars,ta.treatment,"SitePartic")]

match.data(matchit(as.formula(paste(ta.treatment,"~",paste(ta.matchvars,collapse = "+"),sep="")),ta.match,method="nearest",ratio=10))->matchdata

#merge outcome data in

merge(matchdata,ta.ch[,c("SitePartic",names(ta.ch)[!names(ta.ch) %in% names(matchdata)])],by="SitePartic")->ta.postmatch

#archive postmatch
assign(paste0("ta.postmatch",matchiternum),value = ta.postmatch)

#repeat analysis with match data

#outcome vars - match data
firstlast.ta.postmatch.byTr<-OutcomeDF(ta.postmatch,first.last.ta.outcome.vars,first.last.ta.outcome.vartypes,ta.treatment)
sixmonthly.ta.postmatch.byTr<-OutcomeDF(ta.postmatch,sixmonthly.ta.outcomevars,sixmonthly.ta.outcomevartypes,ta.treatment)

#archive this comparison
assign(paste0("firstlast.ta.postmatch.byTr",matchiternum),firstlast.ta.postmatch.byTr)
assign(paste0("sixmonthly.ta.postmatch.byTr",matchiternum),sixmonthly.ta.postmatch.byTr)





#figure out some way to document the model here and incorporate it in the file name?
#write.csv(ta.postmatch.scores.6mo.byTr,file=pathFOCData("FOC4yr",paste("TAbyTrPostmatch",matchiternum,".csv",sep="")))
#write(paste("Treatment def:",ta.treatment,"\n","Matching vars:",paste(ta.matchvars,collapse=",")),file=pathFOCData("FOC4yr",paste("TAbyTrPostmatchDesc",matchiternum,".txt",sep="")))



#try controlling for stuff rather than just doing comparison of means - doesn't change anything
#summary(glm(Placement~VocOrBridge+Age+RaceEthnicity+Education1,Bridgepostmatch,family="binomial"))
#summary(glm(Ret180DayUnSub~VocOrBridge+Age+RaceEthnicity+Education1,Bridgepostmatch,family="binomial"))
#summary(lm(lastwagemultjobs~VocOrBridge+Age+RaceEthnicity+Education1,Bridgepostmatch))
#summary(lm(wagediffmultjobs~VocOrBridge+Age+RaceEthnicity+Education1,Bridgepostmatch))


