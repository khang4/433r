library(dplyr,warn.conflicts=FALSE);
library(ggplot2,warn.conflicts=FALSE);
options(width=2000);

alldata<-read.csv(file="data/movies.csv",header=TRUE);

relevantdata<-data.frame(count(select(alldata,clean_test),clean_test));
# colnames(relevantdata)<-c("dubious","men","notalk","nowomen","ok");
# relevantdata<-relevantdata[2,];
print(relevantdata);

ggplot(data=relevantdata,aes(x=clean_test,y=n))+geom_bar(stat="identity");
ggsave("graph.png");