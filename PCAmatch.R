PackList = rownames(installed.packages())
## check for reqd for packages else install if not found
if('data.table' %in%  PackList & 'ggplot2' %in% PackList & 'stringr' %in% PackList ){
  require(data.table)
  require(ggplot2)
  require(stringr)
} else {
  install.packages('data.table')
  install.packages('ggplot2')
  install.packages('stringr')
}
arg=commandArgs(trailingOnly = T)
DF=fread(arg[1], header = T)# 
names(DF)[1:5] = c('ID1', 'ID2','DX', 'C1', 'C2')
DF$ID1 = paste0(DF$ID1, ",", DF$ID2)
DF[, ID2 := NULL]
Ncontrols = arg[2]
Outfile = paste0(arg[3])
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
  DF.Cases=DF[DX == 1, -c(2)]
  DF.Controls = DF[DX != 1, -c(2)]
  Case.Control.Master = NULL
  cat('Identified ', dim(DF.Cases)[1] , ' cases and ', dim(DF.Controls)[1])
  for( Subj in DF.Cases$ID1){
    cat('Now Performing dist on ', Subj, sep = "\n")
    test.query=as.data.frame(DF.Cases[ID1 == Subj, -1])
    test.subject = as.data.frame(DF.Controls[,-1])
    out=Eucl.Dist(Query.DF = test.query, Subject.DF = test.subject)
    dist.test=data.frame(IID = DF.Controls$ID1, dist=out)
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
### run the fucntion
Case.Control.Master = Main.Call(DF=DF, Ncontrols = Ncontrols)
## Aggregate the results
Case.Control.Master=apply(Case.Control.Master, 2, FUN=function(x) as.character(x))
Case.Control.Master = as.data.frame(Case.Control.Master)
setDT(Case.Control.Master)

## create a plot of the true controls and cases
DF$DX2 = ifelse(DF$ID1 %in%  unique(Case.Control.Master$CONTROLID), 'MATCHED.CONTROL', ifelse(DF$ID1 %in% unique(Case.Control.Master$CASEID), 'CASE', NA))
p=ggplot(DF[!is.na(DX2)], aes(C1, C2, color = DX2))+geom_point(alpha=0.6)
ggsave(p, filename = paste0(Outfile, ".png"), dpi = 400, device = "png", units = "in", width = 8, height = 5)
## write the file to disk
IDs=as.data.frame(str_split(DF$ID1, pattern = ","))
names(IDs) = c('ID1', 'ID2')
DF[, ID1:=NULL]
IDS_DF=cbind.data.frame(IDs, DF)
                                               
fwrite(IDS_DF[!is.na(DX2)], file = paste0(Outfile, ".csv"))

