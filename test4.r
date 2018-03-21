options(width=2000);

document<-function(name)
{
    return(structure(list(m_name=name),class="document"));
}

printname<-function(doc)
{
    UseMethod("printname");
}

printname.document<-function(doc)
{
    cat(sprintf("name is %s\n",doc[["m_name"]]));
}

bob<-document("hey there");
printname(bob);