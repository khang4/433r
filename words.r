library(readr,warn.conflicts=FALSE);
options(width=2000);

# document<-function(filename)
# {


#     return(structure(list(filename=filename),class="document"));
# }

text<-scan("data/victorious.txt",character());

wordcount<-list();

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

    if (is.null(wordcount[[word]]))
    {
        wordcount[[word]]<<-wordcount[[word]]+1;
    } else
    {
        wordcount[[word]]<<-1;
    }
}));

print(length(text));
print(letters);
print(longestword);
cat(sprintf("%i\n",wordcount[["due"]]));
print(wordcount[["due"]]);