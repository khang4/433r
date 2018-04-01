library(dplyr,warn.conflicts=FALSE);
library(ggplot2,warn.conflicts=FALSE);
options(width=2000);
pdf(NULL);

bars<-function(alldata)
{
    relevantdata<-data.frame(count(select(alldata,clean_test),clean_test));
    # colnames(relevantdata)<-c("dubious","men","notalk","nowomen","ok");
    # relevantdata<-relevantdata[2,];
    # print(relevantdata);

    ggplot(data=relevantdata,aes(x=clean_test,y=n,fill=c("#332d44","#854d81","#506684","#7083a4","#397694"),width=1))+geom_bar(stat="identity")+labs(y="amount of movies",x="status",title="amounts of movies with certain statuses")+theme(panel.background=element_rect(fill="white",colour="white"),panel.grid.major.y=element_line(colour="#26272a",size=.08),axis.line.x=element_line(colour="black"),axis.ticks=element_blank(),panel.grid.major.x=element_blank(),plot.title=element_text(hjust=.5))+scale_fill_manual(guide=FALSE,values=c("#332d44","#854d81","#506684","#7083a4","#397694"));
    ggsave("graph1.png");
}

violin<-function(alldata)
{
    relevantdata<-select(alldata,binary,code);
    relevantdata$code<-as.numeric(gsub("PASS|FAIL","",relevantdata$code));
    # relevantdata$binary<-gsub("PASS",1,relevantdata$binary);
    # relevantdata$binary<-gsub("FAIL",0,relevantdata$binary);

    # relevantdata<-data.frame(count(relevantdata,code,binary));

    # print(relevantdata);

    ggplot(data=relevantdata,aes(binary,code))+geom_violin(color="#f5b0be",fill=c("#f5b0be")+labs(y="year",x="pass or fail",title="movies that passed or failed vs time")+theme(panel.background=element_rect(fill="white",colour="white"),panel.grid.major.y=element_line(colour="#26272a",size=.08),axis.line.x=element_line(colour="black"),axis.ticks=element_blank(),panel.grid.major.x=element_blank(),plot.title=element_text(hjust=.5));
    ggsave("graph2.png");
}

glowworms<-function(alldata)
{
    rdata<-select(alldata,budget_2013.,binary,code);
    rdata$code<-as.numeric(gsub("PASS|FAIL","",rdata$code));
    rdata$budget_2013.=strtoi(rdata$budget_2013.);
    rdata$code=strtoi(rdata$code);

    # print(rdata);

    ggplot(data=rdata,aes(x=code,y=budget_2013.,color=binary))+geom_smooth();
    ggsave("graph5.png");
}

scattuh<-function(alldata)
{
    rdata<-select(alldata,clean_test,budget_2013.,domgross_2013.);
    rdata$budget_2013.=strtoi(rdata$budget_2013.);
    rdata$domgross_2013.=strtoi(rdata$domgross_2013.);
    # print(rdata);
    # print(alldata);

    ggplot(data=rdata,aes(x=budget_2013.,y=domgross_2013.,colour=clean_test))+geom_point();
    ggsave("graph4.png");
}

alldata<-read.csv(file="data/movies.csv",header=TRUE);

bars(alldata);
# violin(alldata);
# scattuh(alldata);