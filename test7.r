library(plyr,warn.conflicts=FALSE);

bob1<-data.frame(a=c("hey","no"),b=c(2,3));
bob2<-data.frame(a=c("hey","nop"),b=c(5,10));

print(bob1);
print(bob2);
bob3<-rbind(bob1,bob2);
ddply(bob3,"a",numcolwise(sum));
