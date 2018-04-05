bill<-data.frame(count=integer());

addword<-"hey";

if (is.na(bill[addword,]))
{
    bill[addword,]<-1;
} else {
    bill[addword,]<-bill[addword,]+1;
}

print(bill);