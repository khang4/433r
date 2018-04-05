library(dplyr,warn.conflicts=FALSE);
# options(width=2000);

filterData<-function(datafile,searchSchool,bachelorRowName)
{
    alldata<-read.csv(file=datafile,header=TRUE);

    relevantData<-select(alldata,"Institution","Program.Area","Bachelor.s");

    searchSchoolDataSize<-28;
    searchSchoolData<-data.frame(matrix(ncol=2,nrow=searchSchoolDataSize));
    colnames(searchSchoolData)<-c("programarea",bachelorRowName);
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
        }
    }));

    return(searchSchoolData);
}

degreeChange<-function(file1,file2,searchSchool)
{
    if (!file.exists(file1))
    {
        cat(sprintf("file %s does not seem to be accessible\n",file1));
        return(NULL);
    }

    if (!file.exists(file2))
    {
        cat(sprintf("file %s does not seem to be accessible\n",file2));
        return(NULL);
    }

    searchSchoolData<-filterData(file1,searchSchool,"bachelors");
    searchSchoolData2<-filterData(file2,searchSchool,"bachelors2");

    searchSchoolData3<-merge(searchSchoolData2,searchSchoolData);
    searchSchoolData3$diff<-as.numeric(gsub(",","",searchSchoolData3$bachelors))-as.numeric(gsub(",","",searchSchoolData3$bachelors2));
    searchSchoolData3<-searchSchoolData3[!is.na(searchSchoolData3$programarea),];
    searchSchoolData3<-searchSchoolData3[c("programarea","diff")];
    colnames(searchSchoolData3)<-c("","Bachelors");
    return(searchSchoolData3);
}

# degreeChange("data/USM1986","data/USM2017","University of Maryland, College Park");