library(dplyr,warn.conflicts=FALSE);
options(width=2000);

alldata<-read.csv(file="data/USM2017",header=TRUE);
# print(x);

data1<-select(alldata,"Institution","Program.Area","Bachelor.s");
# print(data1);

searchSchool<-"Salisbury University";

insts<-data.frame("hey");
names(insts)<-c("Institution");

invisible(apply(data1,1,function(row){
    if (row["Institution"]!="")
    {
        insts<<-rbind(insts,row["Institution"]);
    }
}));

print(insts);