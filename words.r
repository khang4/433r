library(readr,warn.conflicts=FALSE);
library(dplyr,warn.conflicts=FALSE);
options(width=2000);

# text<-scan("data/victorious.txt",character(),quote=NULL,fileEncoding="UTF-8");
invisible(text<-strsplit(read_file("data/hey.txt"),"\\s",perl=TRUE)[[1]]);

# cat(sprintf("length: %s\n",length(text)));
# cat(sprintf("max length: %s\n",max(nchar(text))));

wordcounts<-table(unlist(text));
wordcounts<-cbind.data.frame(names(wordcounts),strtoi(wordcounts));
names(wordcounts)<-c("word","count");
wordcounts<-wordcounts[order(-wordcounts$count),,drop=FALSE];
wordcounts<-cbind.data.frame(wordcounts,nchar(as.vector(wordcounts$word)));
names(wordcounts)<-c("word","count","wordsize");


print(wordcounts);