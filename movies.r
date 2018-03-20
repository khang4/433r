library(dplyr,warn.conflicts=FALSE);
library(ggplot2,warn.conflicts=FALSE);
options(width=2000);

failreasons<-function(alldata)
{
    relevantdata<-data.frame(count(select(alldata,clean_test),clean_test));
    # colnames(relevantdata)<-c("dubious","men","notalk","nowomen","ok");
    # relevantdata<-relevantdata[2,];
    print(relevantdata);

    ggplot(data=relevantdata,aes(x=clean_test,y=n))+geom_bar(stat="identity");
    ggsave("graph1.png");
}

alldata<-read.csv(file="data/movies.csv",header=TRUE);

relevantdata<-select(alldata,binary,code);
relevantdata$code<-as.numeric(gsub("PASS|FAIL","",relevantdata$code));
# relevantdata$binary<-gsub("PASS",1,relevantdata$binary);
# relevantdata$binary<-gsub("FAIL",0,relevantdata$binary);

# relevantdata<-data.frame(count(relevantdata,code,binary));

print(relevantdata);

ggplot(data=relevantdata,aes(binary,code))+geom_violin();
ggsave("graph2.png");