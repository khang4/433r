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
        expanddict<-data.frame(matrix(ncol=1,nrow=dict[["increment"]]));
        colnames(expanddict)<-c("count");
        dict[["dictdata"]]<-rbind(dict[["dictdata"]],expanddict);
        dict[["size"]]<-dict[["size"]]+dict[["increment"]];
    }

    return(dict);
}

printdict<-function(dict){UseMethod("printdict");}
printdict.ddict<-function(dict)
{
    print(dict[["dictdata"]]);
}

bob<-ddict(3,10);

bob<-addword(bob,"hey");
bob<-addword(bob,"hey");
bob<-addword(bob,"no");
bob<-addword(bob,"noa");
printdict(bob);