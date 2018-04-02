library(readr,warn.conflicts=FALSE);
library(plyr,warn.conflicts=FALSE);
library(dplyr,warn.conflicts=FALSE);
options(width=100);

document<-function(filename)
{
    invisible(text<-scan(filename,character(),quote=NULL,fileEncoding="UTF-8",quiet=TRUE));
    # invisible(text<-strsplit(read_file(filename),"\\s",perl=TRUE)[[1]]);

    wordcounts<-table(unlist(text));
    wordcounts<-cbind.data.frame(names(wordcounts),strtoi(wordcounts));
    names(wordcounts)<-c("word","count");
    wordcounts<-wordcounts[order(-wordcounts$count),,drop=FALSE];
    wordsizes<-nchar(as.vector(wordcounts$word)); #array of all sizes of words
    wordcounts<-cbind.data.frame(wordcounts,wordsizes); #table of word counts
    names(wordcounts)<-c("word","count","wordsize");

    maxwordsize<-max(wordsizes); #letter count of longest word
    totalwords<-length(text); #total number of words
    lettercount<-sum(wordcounts$count*wordcounts$wordsize); #total number of letters

    rownames(wordcounts)<-NULL;

    previewlength<-50;
    if (length(text)<50)
    {
        previewlength<-length(text);
    }

    return(structure(list(
        wordcounts=wordcounts[c("word","count")],
        longestword=toString(filter(wordcounts,wordsize==maxwordsize)[1,][["word"]]), #the longest word
        averageletters=lettercount/totalwords, #average letters per word
        totalwords=totalwords,
        lettercount=lettercount,
        previewwords=text[1:previewlength]
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
    return(doc$wordcounts[1:rows,]);
}

preview<-function(doc){UseMethod("preview")}
preview.document<-function(doc)
{
    return(doc$previewwords);
}

corpus<-function(filepath)
{
    textfiles<-dir(filepath,pattern="\\.txt$",full.names=TRUE,recursive=TRUE);
    docs<-lapply(textfiles,document);

    maxwords<-0;
    minwords<--1;
    totalwords<-0;
    totalletters<-0;
    longestwordSize<-0;

    allwordset<-FALSE;
    allwordcounts<-NULL;

    allpreviewset<-FALSE;
    allpreview<-NULL;

    lapply(docs,function(doc){
        totalwords<<-totalwords+doc$totalwords;
        totalletters<<-totalletters+doc$lettercount;
        maxwords<<-max(maxwords,doc$totalwords);

        if (minwords<0)
        {
            minwords<<-doc$totalwords;
        } else {
            minwords<<-min(minwords,doc$totalwords);
        }

        if (nchar(doc$longestword)>longestwordSize)
        {
            longestwordSize<<-nchar(doc$longestword);
            longestword<<-doc$longestword;
        }

        if (!allwordset)
        {
            allwordset<<-TRUE;
            allwordcounts<<-doc$wordcounts;
        } else {
            allwordcounts<<-ddply(rbind(allwordcounts,doc$wordcounts),"word",numcolwise(sum));
        }

        if (!allpreviewset)
        {
            allpreviewset<<-TRUE;
            allpreview<<-doc$previewwords;
        } else {
            allpreview<<-append(allpreview,doc$previewwords);
        }
    });

    allwordcounts<-allwordcounts[order(-allwordcounts$count),,drop=FALSE];
    rownames(allwordcounts)<-NULL;

    return(structure(list(
        minwords=minwords,
        maxwords=maxwords,
        averageletters=totalletters/totalwords,
        totalwords=totalwords,
        averagewords=totalwords/length(textfiles),
        longestword=longestword,
        allwordcounts=allwordcounts,
        allpreview=allpreview
    ),class="corpus"));
}

summary<-function(corpse){UseMethod("summary")}
summary.corpus<-function(corpse)
{
    cat(sprintf("corpus total words: %s\n",corpse$totalwords));
    cat(sprintf("average words per document: %s\n",corpse$averagewords));
    cat(sprintf("max words in a document: %s\n",corpse$maxwords));
    cat(sprintf("min words in a document: %s\n",corpse$minwords));
    cat(sprintf("average letters per word: %s\n",corpse$averageletters));
    cat(sprintf("longest word in corpus: %s\n",corpse$longestword));
}

most_common<-function(corp,rows){UseMethod("most_common")}
most_common.corpus<-function(corp,rows=10)
{
    return(corp$allwordcounts[1:rows,]);
}

preview<-function(corp){UseMethod("preview")}
preview.corpus<-function(corp)
{
    return(corp$allpreview);
}

# doc<-document("data/testdata/hey2.txt");
# summary(doc);
# most_common(doc,20);
# preview(doc);

corp<-corpus("data/testdata");
summary(corp);
most_common(corp,20);
preview(corp);