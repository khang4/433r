library(dplyr,warn.conflicts=FALSE);
options(width=2000);

alldata<-read.csv(file="data/USM2017",header=TRUE);
# print(x);

relevantData<-select(alldata,"Institution","Program.Area","Bachelor.s");
# print(data1);

searchSchoolDataSize<-28;
searchSchoolData<-data.frame(matrix(ncol=2,nrow=searchSchoolDataSize));
colnames(searchSchoolData)<-c("programarea","bachelors");
searchSchoolDataIndex<-1;

searchSchool<-"University of Maryland, Baltimore County";
onsearchschool<-0;

invisible(apply(relevantData,1,function(row){
    if (row[[1]]==searchSchool || onsearchschool==1)
    {
        if (row[[1]]!="" && row[[1]]!=searchSchool)
        {
            onsearchschool<<-0;
            return();
        } else
        {
            onsearchschool<<-1;
        }

        searchSchoolData[searchSchoolDataIndex,]<<-c(row[[2]],row[[3]]);
        searchSchoolDataIndex<<-searchSchoolDataIndex+1;

        if (searchSchoolDataIndex>searchSchoolDataSize)
        {
            searchSchoolDataSize<<-searchSchoolDataSize+10;
            expandrows<-data.frame(matrix(ncol=2,nrow=10));
            colnames(expandrows)<-colnames(searchSchoolData);
            searchSchoolData<<-rbind(searchSchoolData,expandrows);
        }

        # cat(sprintf("%30s %8s\n",row[[2]],row[[3]]));
    }
}));

searchSchoolData1<-searchSchoolData;

alldata<-read.csv(file="data/USM1986",header=TRUE);

relevantData<-select(alldata,"Institution","Program.Area","Bachelor.s");

searchSchoolDataSize<-28;
searchSchoolData<-data.frame(matrix(ncol=2,nrow=searchSchoolDataSize));
colnames(searchSchoolData)<-c("programarea","bachelors2");
searchSchoolDataIndex<-1;

onsearchschool<-0;

invisible(apply(relevantData,1,function(row){
    if (row[[1]]==searchSchool || onsearchschool==1)
    {
        if (row[[1]]!="" && row[[1]]!=searchSchool)
        {
            onsearchschool<<-0;
            return();
        } else
        {
            onsearchschool<<-1;
        }

        searchSchoolData[searchSchoolDataIndex,]<<-c(row[[2]],row[[3]]);
        searchSchoolDataIndex<<-searchSchoolDataIndex+1;

        if (searchSchoolDataIndex>searchSchoolDataSize)
        {
            searchSchoolDataSize<<-searchSchoolDataSize+10;
            expandrows<-data.frame(matrix(ncol=2,nrow=10));
            colnames(expandrows)<-colnames(searchSchoolData);
            searchSchoolData<<-rbind(searchSchoolData,expandrows);
        }

        # cat(sprintf("%30s %8s\n",row[[2]],row[[3]]));
    }
}));

# print(searchSchoolData1);
# print(searchSchoolData);

searchSchoolData3<-merge(searchSchoolData1,searchSchoolData);
searchSchoolData3$diff<-as.numeric(gsub(",","",searchSchoolData3$bachelors))-as.numeric(gsub(",","",searchSchoolData3$bachelors2));
searchSchoolData3<-searchSchoolData3[!is.na(searchSchoolData3$programarea),];
print(searchSchoolData3);