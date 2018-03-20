library(dplyr,warn.conflicts=FALSE);
options(width=2000);

alldata<-read.csv(file="data/movies.csv",header=TRUE);

relevantdata<-count(select(alldata,clean_test),clean_test);
print(relevantdata);