library(readr,warn.conflicts=FALSE);
options(width=2000);

ddict<-function(initsize,increment=10)
{
    dictdata<-data.frame(matrix(ncol=1,nrow=initsize));
    colnames(dictdata)<-c("count");

    return(structure(list(
        dictdata=dictdata,
        size=initsize,
        index=1,
        increment=increment
    ),class="ddict"));
}

addword<-function(dict,word){UseMethod("addword");}
addword.ddict<-function(dict,word)
{
    if (is.na(dict[["dictdata"]][word,]))
    {
        dict[["dictdata"]][dict[["index"]],]<-1;
        rownames(dict[["dictdata"]])[dict[["index"]]]<-word;
        dict[["index"]]<-dict[["index"]]+1;
    } else {
        dict[["dictdata"]][word,]<-dict[["dictdata"]][word,]+1;
    }

    if (dict[["index"]]>=dict[["size"]])
    {
        expanddict<-data.frame(matrix(ncol=1,nrow=dict[["increment"]]),row.names=c((dict$size+1):(dict$size+dict$increment)));
        colnames(expanddict)<-c("count");
        dict[["dictdata"]]<-rbind(dict[["dictdata"]],expanddict);
        dict$size<-dict$size+dict$increment;
    }

    return(dict);
}

printdict<-function(dict){UseMethod("printdict");}
printdict.ddict<-function(dict)
{
    print(dict[["dictdata"]]);
}

sortdict<-function(dict){UseMethod("sortdict");}
sortdict.ddict<-function(dict)
{
    dict$dictdata<-dict$dictdata[order(-dict$dictdata$count),,drop=FALSE];
    return(dict);
}

text<-scan("data/hounds.txt",character(),quote=NULL);

letters<-0;

longestwordlen<-0;
longestword<-"";

wordcount<-ddict(500,300);

invisible(lapply(text,function(word){
    word<-gsub("[,.]","",word);

    letters<<-letters+nchar(word);

    if (nchar(word)>longestwordlen)
    {
        longestwordlen<<-nchar(word);
        longestword<<-word;
    }

    wordcount<<-addword(wordcount,word);
}));

print(length(text));
print(letters);
print(longestword);

wordcount<-sortdict(wordcount);
printdict(wordcount);