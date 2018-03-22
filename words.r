library(readr,warn.conflicts=FALSE);
options(width=2000);

text<-scan("data/victorious.txt",character());

letters<-0;

longestwordlen<-0;
longestword<-"";

invisible(lapply(text,function(word){
    word<-gsub("[,.]","",word);

    letters<<-letters+nchar(word);

    if (nchar(word)>longestwordlen)
    {
        longestwordlen<<-nchar(word);
        longestword<<-word;
    }
}));

print(length(text));
print(letters);
print(longestword);