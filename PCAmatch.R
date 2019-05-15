
arg=commandArgs(trailingOnly = T)
DF=fread(arg[1], header = T)# 
names(DF)[1:3] = c('ID1', 'ID2', 'DX')
Ncontrols = arg[2]
Outfile = paste0(arg[3])
PackList = rownames(installed.packages())
## check for reqd for packages else install if not found
if('data.table' %in%  PackList & 'ggplot2' %in% PackList){
  require(data.table)
  require(ggplot2)
} else {
  install.packages('data.table')
  install.packages('ggplot2')
}

### this function should calculate the EUCL Distance expects a single query and many subjects
Eucl.Dist=function(Query.DF, Subject.DF){
  dist.one=function(x1, x2){
    return((x1 - x2) ^ 2)
  }
  Query.Dim = dim(Query.DF)
  Subject.Dim = dim(Subject.DF)
  if(Query.Dim[1] == 1 & Subject.Dim[2] ==  Query.Dim[2]){
    Sum.List=sapply(seq(1, Query.Dim[2]), FUN = function(x) dist.one(Query.DF[,x], Subject.DF[,x]), simplify = T)
    Sum.Mat=t(Sum.List)
    Sum.Dist=apply(Sum.Mat, 2, FUN = function(x) sqrt(sum(x)))
    return(Sum.Dist)
  } else{
    stop('Query dataframe should only have one subject Or Column numbers differ between subject and query')
  }
}


## Run the function ##
Main.Call = function(DF, Ncontrols){
  DF.Cases=DF[DX == 1, -c(2:3)]
  DF.Controls = DF[DX != 1, -c(2:3)]
  Case.Control.Master = NULL
  cat('Identified ', dim(DF.Cases)[1] , ' cases and ', dim(DF.Controls)[1])
  for( Subj in DF.Cases$ID1){
    cat('Now Performing dist on ', Subj)
    test.query=as.data.frame(DF.Cases[ID1 == Subj, -1])
    test.subject = as.data.frame(DF.Controls[,-1])
    out=Eucl.Dist(Query.DF = test.query, Subject.DF = test.subject)
    dist.test=data.frame(IID = test.subject$ID1, dist=out)
    setDT(dist.test)
    dist.test=dist.test[order(dist)]
    dist.test$IID = as.character(dist.test$IID)
    n = Ncontrols
    Control.IDS = dist.test$IID[1:n]
    temp.df = data.frame(row.names = NULL, CASEID = rep(Subj, n), CONTROLID= Control.IDS)
    Case.Control.Master = rbind.data.frame(temp.df, Case.Control.Master)
  }
  return(Case.Control.Master)
}

## Aggregate the results
Case.Control.Master=apply(Case.Control.Master, 2, FUN=function(x) as.character(x))
Case.Control.Master = as.data.frame(Case.Control.Master)
Case.Control.Master

