options(width=2000);

document<-function(name)
{
    return(structure(list(name=name),class="document"));
}

printname<-function(doc)
{
    UseMethod("printname");
}

printname.document<-function(doc)
{
    cat(sprintf("name is %s\n",doc[["name"]]));
}

bob<-document("hey there");
printname(bob);

bob[["name"]]<-"nop";
printname(bob);

bobosize<-10;
bobo<-data.frame(matrix(ncol=1,nrow=bobosize));
colnames(bobo)<-c("count");
boboindex<-1;

bobo[1,]<-1;
rownames(bobo)[1]<-"hey";
boboindex<-boboindex+1;

addword<-"heya";
if (!is.na(bobo[addword,]))
{
    bobo[addword,]<-bobo[addword,]+1;
} else {
    bobo[boboindex,]<-1;
    rownames(bobo)[boboindex]<-addword;
    boboindex<-boboindex+1;
}

print(bobo);