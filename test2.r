library(dplyr,warn.conflicts=FALSE);

#testing how to init data frames and add things to them
bob<-data.frame(matrix(ncol=3,nrow=3));
colnames(bob)<-c("hey","there","how");
bobindex<-1;

bob[bobindex,]<-c(1,2,3);
bobindex=bobindex+1;

bob2<-data.frame(matrix(ncol=3,nrow=3));
colnames(bob2)<-c("hey","there","how");
bob<-rbind(bob,bob2);

print(bob);

#how to printf...i feel like theres a better way
cat(sprintf("%i\n",bob[1,1]));

#so numbers can be used as bools
testbool<-0;

if (!testbool)
{
    print("naw");
}