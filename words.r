library(readr,warn.conflicts=FALSE);
options(width=2000);

ddict<-function()
{
    dictdata<-data.frame(count=integer());

    return(structure(list(
        dictdata=dictdata
    ),class="ddict"));
}

addword<-function(dict,word){UseMethod("addword");}
addword.ddict<-function(dict,word)
{
    if (is.na(dict$dictdata[word,]))
    {
        dict$dictdata[word,]<-1;
    } else {
        dict$dictdata[word,]<-dict$dictdata[word,]+1;
    }

    return(dict);
}

printdict<-function(dict){UseMethod("printdict");}
printdict.ddict<-function(dict)
{
    print(dict$dictdata);
}

sortdict<-function(dict){UseMethod("sortdict");}
sortdict.ddict<-function(dict)
{
    dict$dictdata<-dict$dictdata[order(-dict$dictdata$count),,drop=FALSE];
    return(dict);
}

text<-scan("data/les_mis.txt",character(),quote=NULL,fileEncoding="UTF-8");
print("hey");
letters<-0;

longestwordlen<-0;
longestword<-"";

wordcount<-ddict();

invisible(lapply(text,function(word){
    # word<-gsub("[,.\"]","",word);
    # word<-gsub("[\"]","",word);

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