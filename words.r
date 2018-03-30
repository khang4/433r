library(readr,warn.conflicts=FALSE);
library(dplyr,warn.conflicts=FALSE);
options(width=2000);

document<-function(filename)
{
    # text<-scan(filename,character(),quote=NULL,fileEncoding="UTF-8");
    invisible(text<-strsplit(read_file(filename),"\\s",perl=TRUE)[[1]]);

    wordcounts<-table(unlist(text));
    wordcounts<-cbind.data.frame(names(wordcounts),strtoi(wordcounts));
    names(wordcounts)<-c("word","count");
    wordcounts<-wordcounts[order(-wordcounts$count),,drop=FALSE];
    wordsizes<-nchar(as.vector(wordcounts$word)); #array of all sizes of words
    wordcounts<-cbind.data.frame(wordcounts,wordsizes); #table of word counts
    names(wordcounts)<-c("word","count","wordsize");

    maxwordsize<-max(wordsizes); #letter count of longest word
    totalwords<-nrow(wordcounts); #total number of words
    averageletters=sum(wordsizes)/totalwords; #average letters per word
    longestword<-toString(filter(wordcounts,wordsize==maxwordsize)[1,][["word"]]); #the longest word

    rownames(wordcounts)<-NULL;

    return(structure(list(
        wordcounts=wordcounts[c("word","count")],
        longestword=longestword,
        averageletters=averageletters,
        totalwords=totalwords
    ),class="document"));
}

summary<-function(doc){UseMethod("summary")}
summary.document<-function(doc)
{
    cat(sprintf("word count: %s\n",doc$totalwords));
    cat(sprintf("average letters per word: %s\n",doc$averageletters));
    cat(sprintf("longest word: %s\n",doc$longestword));
}

most_common<-function(doc,rows){UseMethod("most_common")}
most_common.document<-function(doc,rows=5)
{
    print(doc$wordcounts[1:rows,]);
}

doc<-document("data/hey.txt");
summary(doc);
most_common(doc,20);