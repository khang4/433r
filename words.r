library(readr,warn.conflicts=FALSE);
library(dplyr,warn.conflicts=FALSE);
options(width=2000);

# text<-scan("data/victorious.txt",character(),quote=NULL,fileEncoding="UTF-8");
invisible(text<-strsplit(read_file("data/hey.txt"),"\\s",perl=TRUE)[[1]]);

wordcounts<-table(unlist(text));
wordcounts<-cbind.data.frame(names(wordcounts),strtoi(wordcounts));
names(wordcounts)<-c("word","count");
wordcounts<-wordcounts[order(-wordcounts$count),,drop=FALSE];
wordsizes<-nchar(as.vector(wordcounts$word));
wordcounts<-cbind.data.frame(wordcounts,wordsizes);
names(wordcounts)<-c("word","count","wordsize");

lettercount<-sum(wordsizes);
maxwordsize<-max(wordsizes);
cat(sprintf("longest word length: %s\n",maxwordsize));
cat(sprintf("total letters: %s\n",lettercount));
cat(sprintf("total words: %s\n",nrow(wordcounts)));
cat(sprintf("longest word: %s\n",filter(wordcounts,wordsize==maxwordsize)[1,][["word"]]));

print(wordcounts);