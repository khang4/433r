library(dplyr,warn.conflicts=FALSE);
options(width=2000);

alldata<-read.csv(file="data/USM2017",header=TRUE);
# print(x);

relevantData<-select(alldata,"Institution","Program.Area","Bachelor.s");
# print(data1);

searchSchool<-"Salisbury University";
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

        cat(sprintf("%30s %8s\n",row[[2]],row[[3]]));
    }
}));