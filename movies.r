library(dplyr,warn.conflicts=FALSE);
library(ggplot2,warn.conflicts=FALSE);
library(scales,warn.conflicts=FALSE);
options(width=2000);
pdf(NULL);

bars<-function(alldata)
{
    relevantdata<-data.frame(count(select(alldata,clean_test),clean_test));
    # colnames(relevantdata)<-c("dubious","men","notalk","nowomen","ok");
    # relevantdata<-relevantdata[2,];
    # print(relevantdata);

    ggplot(data=relevantdata,aes(x=clean_test,y=n,fill=c("#332d44","#854d81","#506684","#7083a4","#397694"),width=1))+geom_bar(stat="identity")+labs(y="amount of movies",x="status",title="amounts of movies with certain statuses")+theme(panel.background=element_rect(fill="white",colour="white"),panel.grid.major.y=element_line(colour="#26272a",size=.08),axis.line.x=element_line(colour="black"),axis.ticks=element_blank(),panel.grid.major.x=element_blank(),plot.title=element_text(hjust=.5))+scale_fill_manual(guide=FALSE,values=c("#332d44","#854d81","#506684","#7083a4","#397694"));
    ggsave("plot1.png");
}

violin<-function(alldata)
{
    relevantdata<-select(alldata,binary,code);
    relevantdata$code<-as.numeric(gsub("PASS|FAIL","",relevantdata$code));
    # relevantdata$binary<-gsub("PASS",1,relevantdata$binary);
    # relevantdata$binary<-gsub("FAIL",0,relevantdata$binary);

    # relevantdata<-data.frame(count(relevantdata,code,binary));

    # print(relevantdata);

    ggplot(data=relevantdata,aes(binary,code))+geom_violin(color="#f5b0be",fill="#f5b0be")+labs(y="year",x="pass or fail",title="movies that passed or failed vs time")+theme(panel.background=element_rect(fill="white",colour="white"),panel.grid.major.y=element_line(colour="#26272a",size=.08),axis.line.x=element_line(colour="black"),axis.ticks=element_blank(),panel.grid.major.x=element_blank(),plot.title=element_text(hjust=.5));
    ggsave("plot2.png");
}

glowworms<-function(alldata)
{
    rdata<-select(alldata,budget_2013.,binary,code);
    rdata$code<-as.numeric(gsub("PASS|FAIL","",rdata$code));
    rdata$budget_2013.=strtoi(rdata$budget_2013.);
    rdata$code=strtoi(rdata$code);

    # print(rdata);

    ggplot(data=rdata,aes(x=code,y=budget_2013.,color=binary,fill=binary))+geom_smooth()+labs(y="average budget ($)",x="year",title="average budget and number of movies released per year",colour="movie status",fill="nope")+theme(panel.background=element_rect(fill="white",colour="white"),panel.grid.major.y=element_line(colour="#26272a",size=.08),axis.line.x=element_line(colour="black"),axis.ticks=element_blank(),panel.grid.major.x=element_blank(),plot.title=element_text(hjust=.5))+scale_colour_manual(values=c("#928b63","#504c70"))+scale_fill_manual(values=c("#928b63","#504c70"),guide="none")+scale_y_continuous(breaks=seq(0,80000000,20000000),labels=comma);
    ggsave("plot3.png");
}

scattuh<-function(alldata)
{
    rdata<-select(alldata,clean_test,budget_2013.,domgross_2013.);
    rdata$budget_2013.=strtoi(rdata$budget_2013.);
    rdata$domgross_2013.=strtoi(rdata$domgross_2013.);
    # print(rdata);
    # print(alldata);

    ggplot(data=rdata,aes(x=budget_2013.,y=domgross_2013.,colour=clean_test))+geom_point()+labs(y="gross ($)",x="budget ($)",title="movies budget vs domestic gross and their statuses",colour="movie status")+theme(panel.background=element_rect(fill="white",colour="white"),panel.grid.major.y=element_line(colour="#26272a",size=.08),axis.line.x=element_line(colour="black"),axis.ticks=element_blank(),panel.grid.major.x=element_blank(),plot.title=element_text(hjust=.5))+scale_y_continuous(breaks=seq(0,1800000000,187500000),labels=comma)+scale_x_continuous(breaks=seq(0,400000000,100000000),labels=comma)+scale_colour_manual(values=c("#928b63","#504c70","#798d96","#ad685e","#ce44a5"));
    ggsave("plot4.png");
}

makePlots<-function()
{
    if (!file.exists("movies.csv"))
    {
        cat(sprintf("movies.csv must be in the same directory as the program\n"));
        return();
    }

    alldata<-read.csv(file="movies.csv",header=TRUE);

    bars(alldata);
    violin(alldata);
    glowworms(alldata);
    scattuh(alldata);
}

makePlots();