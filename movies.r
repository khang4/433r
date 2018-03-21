library(dplyr,warn.conflicts=FALSE);
library(ggplot2,warn.conflicts=FALSE);
options(width=2000);

bars<-function(alldata)
{
    relevantdata<-data.frame(count(select(alldata,clean_test),clean_test));
    # colnames(relevantdata)<-c("dubious","men","notalk","nowomen","ok");
    # relevantdata<-relevantdata[2,];
    # print(relevantdata);

    ggplot(data=relevantdata,aes(x=clean_test,y=n))+geom_bar(stat="identity");
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

    ggplot(data=relevantdata,aes(binary,code))+geom_violin();
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