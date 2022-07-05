# 20181214 added more FAratio for Hayato
# 20180816 stupid single column multiplication fixed
# 20180505 updated for single column of sample name
# 20170831 updated so that I can run chol only without FA
# 2017.4.8-22: changed several JPA related functions
# 2016.9.12 adding  functions: update quant and raw file according to prep file change
# 2016.9.7 small update. fixing Aggregate.by.name, make <NA> names "-"
# 2016.8.31 small update. fixing map_and_datafile to look at distribution; add tk_choose.files to Mac to end a long-lasting nightmare!
# 2016.8.17 small update. fixing labeling for barplot
# 2016.7.5 big update to fit JPA's need ; add several JPA required functions and some basic functions in the first part
# 2016.5.9: big revision
# this script is used for MS/GC data analysis for small scale experiment
# update 2016.6.13 combine fa and chol file: missing one file case



# input: map, cell count, std composition
# input: famesAUC->prep file, chol AUC
# input: Result_Model1_fames,Result_Model1_chol

### Begin: for PLATFORM COMPATIBILITY  ##############
#WhereAmI=getwd()
#if ( grepl("C:/",WhereAmI)){
#  Platform="Windows"
#} else {
#  Platform="Mac"
#}

# update 20220606, Gino
# get OS name. avoid using "C:/" to identify windows
Platform = Sys.info()[["sysname"]]


if( Platform=="Windows"){
  TOOLPATH="C:\\Users\\Bensinger-Analysis\\Dylan\\[protocol]data analysis pipeline\\GCMS pipeline\\"
}

if( Platform=="Mac"){
  TOOLPATH="/Users/dylandrow/Documents/Bensinger Lab/For Joe Argus/GCMS pipeline for Joe/"
  library(tcltk2)
  choose.files<-function(){
   return(tk_choose.files()) 
  }
}
###End: for PLATFORM COMPATIBILITY  ##############


library(xlsx)
####  FUNCTION GROUP: reading and writing ######
xlsx2list<-function(file,excludesheet=c("confirmation")){
  wb <- loadWorkbook(file)
  sheets <- getSheets(wb)
  sheetnames=names(sheets)
  mylist=list()
  for ( name in sheetnames){
    if (name %in% excludesheet) {next}
    mylist[[name]]=read.xlsx(file,sheetName=name,check.names=F,row.names=T)  
  }
  return(mylist)
}

Show.list.structure<-function(data,showrows=1:5){
  temp=data
  depth=1
  while(class(temp) == "list"){
    #show names and go further
    temp.names=matrix(names(temp),1,)
    rownames(temp.names)=paste("Layer #",as.character(depth))
    print(temp.names)
    temp=temp[[1]]
    depth=depth+1
  }
  print(temp[showrows,])
  print(dim(temp))
}

InsertColumn<-function(columns, data,method="before",col_2_insert){
  pos=which(colnames(data)==col_2_insert)
  if (method=="before"){
    result= cbind( data[,1:(pos-1)],columns, data[,pos:ncol(data)]  )
  }
  if (method=="after"){
    result= cbind( data[,1:(pos)],columns, data[,(pos+1):ncol(data)]  )
  }
  return(result)
}

Read.file.multiformat<-function(file,sheetdelete=NULL){
  if (grepl("rda",tolower(file))){
    result=readRDS(file)
  } 
  if (grepl(".xls", file)){
    result=xlsx2list(file = file)
  }
  result[sheetdelete]=NULL
  return(result)
}

RDA2xlsx<-function(rdafile){
  rdafile=file.choose()
  filename=gsub(".Rda","",basename(rdafile))
  dir=dirname(rdafile)
  setwd(dir)
  data=readRDS(rdafile)
  outputname=paste0(filename,".xlsx")
  for (sheetname in names(data)){
    write.xlsx(data[[sheetname]],outputname,sheetName=sheetname,append=T)
  }
}

list2xlsx<-function(list,outputname,delete=NULL){
  for (sheetname in names(list)){
    if(is.null(delete)){
      data=data.frame(list[[sheetname]])
    }    else { 
        data=data.frame(list[[sheetname]])[-delete,] 
        }
    write.xlsx(data,outputname,sheetName=sheetname,append=T,col.names = T)    
    print(paste0("writing ",sheetname))    
  }
}
#### end of FUNCTION GROUP: reading and writing #####

#### FUNCTION GROUP: Converting xlsx files #####

# add names to quants file, so that we can look at the result better
AddSampleNames<-function(samplenames,quants){
  # quants=abs_quants
  sample_quants=quants
  if(!is.list(quants)){
    sample_quants=list(quants=quants)
  }
  for(pa in names(quants)){
    matrix=as.matrix(quants[[pa]])
    rownames(matrix)=rownames(quants[[pa]])
    colnames(matrix)=colnames(quants[[pa]])
    temp=as.matrix(matrix[match(rownames(samplenames), rownames(matrix)),])
    temp=apply(temp,1:2,as.numeric)
    colnames(temp)=colnames(quants[[pa]])
    sample_quants[[pa]]=cbind.data.frame(samplenames,temp)    
  }
  return(sample_quants)
}

#combine any file with their name
map_and_data_file<-function(outname=NULL,mapfile,datafile,sheetdelete = "standard",write=T,Normalise=T){
  
  if (is.null(outname)) {outname=gsub(".xls","_SAMPLENAMES.xls",datafile)}
  
  basename(mapfile)
  basename(datafile)
  
  # get sample names from MAP file
  if (F) {
    map_raw=read.xlsx(file=mapfile,sheetName="map",check.names=F,startRow=2)
    NArow=which(is.na(map_raw$Col.ms))
    if (length(NArow)>0) {map_raw=map_raw[-NArow,]}
    for (i.col in colnames(map_raw)){
      if ( length (na.omit(map_raw[,i.col]))==0) {map_raw[,i.col]=NULL}
    } 
    rownames(map_raw)=Pos2SampNumber("MS_",map_raw$Row.ms,map_raw$Col.ms)  
    samplenames=map_raw[,which(grepl("Name",colnames(map_raw)))]  
  }
  
  map_raw=readMAPfile(mapfile)
  samplenames=map_raw[,which(grepl("Name",colnames(map_raw)))]  
  
  data.raw=Read.file.multiformat(file = datafile,sheetdelete =sheetdelete)  
  if( !is.list(data.raw)){
    data.raw=list(data=data.raw)
  }
  
    # debug: map might have more names.
  data.sort=list()
   data.sort.norm=list()
  for (name in names(data.raw)){
    data=as.matrix(data.raw[[name]]) 
    if(length(data)==0){next}    
    oldname=rownames(data)
    wordlength=nchar(oldname[1])
    rownames(data)=paste0("MS_",substr(oldname,start = wordlength-1,stop = wordlength))
    
    s=rownames(samplenames); 
    data_names=s[s %in% rownames(data)]
    m=as.matrix(  data[data_names,]   );  
    if("Code" %in% colnames(m)){m=m[,-ncol(m)]}
    # colnames(m)=paste0("M+",as.character(0:(ncol(data)-1)))
    data.sort[[name]]=m
    
    #### for normalizing 
    m.sum=apply(m,1,sum); m.norm=m/m.sum
    sheetname.norm=paste0(name,"_norm")
    data.sort.norm[[sheetname.norm]]=m.norm
    ### normalising
  }
  result=AddSampleNames(samplenames = samplenames,quants = data.sort)
  if (write){list2xlsx(list = result,outputname = outname)}
  to_return=result
  
  result.norm=AddSampleNames(samplenames = samplenames,quants = data.sort.norm)
  if (write & Normalise){
    list2xlsx(list = result.norm,outputname = outname);
    to_return=c(result,result.norm)
  }
  

  return(to_return)
}


#INPUT: matrix sorted by ROW
# OUTPUT: matrix sorted by Col
Sort_By_Column.xlsx<-function(inputfile,SheetDelete=NULL,outputfile=NULL){
  library(xlsx)
  wb<-loadWorkbook(inputfile)
  sheets=getSheets(wb)
  sheetnames=names(sheets)
  if( !is.null(SheetDelete)) {    sheetnames= sheetnames[-c(SheetDelete)]  }
  if ( is.null(outputfile)) {outputfile=gsub( ".xls", "_SortByColumn.xls",inputfile)}
  
  for ( sheetname in sheetnames){
    
    raw=read.xlsx(inputfile,sheetname,header=T,row.names=T)
    if (is.null(raw)) { next}
    resort=Sort_By_Column(raw)
    write.xlsx(resort,sheetName = sheetname,outputfile,append=T)
  } 
}
# sort by column rather than rows, for single matrix
Sort_By_Column<-function(INPUT){  
  samplenames=rownames(INPUT)
    n=nchar(samplenames)
  tmp=as.integer(substr(samplenames,n-1,n))
  num_by_col=Num2RowNCol(tmp)[,3]
  OUTPUT=INPUT[order(num_by_col),]
  return(OUTPUT)
}


Num2RowNCol<-function(number,WELL=96){  
  number=as.matrix(number)
  row=number %/%12 +1
  col=number%%12; col[which(col==0)]=12
  num_by_col=(col-1)*8+row
  result=cbind(row,col,num_by_col)
  colnames(result)=c("row","col","num_by_col"); 
  rownames(result)=paste0("sample_",as.character(number))
  return(result)
}
############
####### End of FUNCTION GROUP: Converting xlsx files



########yhahs################################
ChangeRownames<-function(list){
  for (i in 1:length(list)){
    mat=list[[i]]
    rownames(mat)=gsub("F","MS_",rownames(mat))
    rownames(mat)=gsub("C","MS_",rownames(mat))
    list[[i]]=mat
  }
  return(list)
}

Aggregate.by.replicate<-function(data,REPLICATE=4,FUN){
  FUN.name=FUN
  FUN<-match.fun(FUN)
  result=c()
  for (i in 1:ncol(data)){
    temp=matrix(data[,i],REPLICATE,)
    result.col=apply(temp,2,FUN,na.rm=T)
    result=cbind(result,result.col)
  }
  colnames(result)=paste0(colnames(data),".",FUN.name)
  return(result)
}

# updated for NA names
Aggregate.by.name<-function(name_data,FUN,namekey=c("Name")){
  
  FUN.name=FUN
  FUN<-match.fun(FUN)
  
  data=parseName_data(Name_data = name_data,namekey = namekey)$data
  name=parseName_data(Name_data = name_data,namekey=namekey)$samplenames
  
  temp=as.matrix(name); temp[which(is.na(temp))]="-"; temp=as.data.frame(temp);name=temp
  
  by=as.list(name)
  data.fun=aggregate.data.frame(data,by=by,FUN,na.rm=T)
  
  order.after.aggregate=aggregate.data.frame(1:nrow(data),by=by,min,na.rm=T)
  data.result=data.fun[order(order.after.aggregate[,"x"]),]
  return(data.result)
}


parseName_data<-function(Name_data,namekey=c("Name","bad")){
  name.col=which( grepl( paste(namekey,collapse = "|")  ,colnames(Name_data),ignore.case = T) )
  
  Name=Name_data[,name.col]
  Name=as.matrix(Name)
  rownames(Name)=rownames(Name_data)
  colnames(Name)=colnames(Name_data)[name.col]
  
  data=as.matrix(Name_data[,-name.col])
  data=apply(as.matrix(Name_data[,-name.col]),1:2, as.numeric, na.rm=T)
  rownames(data)=rownames(Name_data)
  colnames(data)=colnames(Name_data)[-name.col]
  return(list(samplenames=Name,data=data))
}


Merge.mean.sd<-function(data.list,annotation=c("mean","sd")){
  n=length(data.list)  
  data.cbind=c()
  for (i in 1:n){
    data=data.list[[i]]
    colnames(data)=paste0(colnames(data),".",annotation[i])
    data.cbind=cbind(data.cbind,data)
  }
    
  col.order=matrix(1:ncol(data.cbind),,n)
  col.order=c(t(col.order))
  result=data.cbind[,col.order]
  return(result)
}


ListExtract_old<-function(lists,rows=NULL,hidesheet="standard"){
  n=length(lists)  
  for (i in 1:length(hidesheet)){
    lists[[hidesheet[i]]]=NULL
  }
  newlist=list()
  
  # get the overlapping sheetnames
  for ( i in 1:length(lists)){
    temp=names(lists[[i]])
    if(i==1) {pas=temp}
    else { pas=intersect(pas,temp) }
  }  
  pas
  
  for ( i in 1:length(lists)){
    for (pa in pas){
      if(is.null(rows)){
        temp=lists[[i]][[pa]]
      } else {
        temp=lists[[i]][[pa]][rows[[i]],]
      }
      newlist[[pa]]=rbind(newlist[[pa]],temp)
    }
  }
  return(newlist)
}


ListExtract<-function(lists,rows=NULL,hidesheet="standard"){
  n=length(lists)  
  for (i in 1:length(hidesheet)){
    lists[[hidesheet[i]]]=NULL
  }

  
  # get the overlapping sheetnames
  for ( i in 1:length(lists)){
    temp=names(lists[[i]])
    if(i==1) {pas=temp}
    else { pas=intersect(pas,temp) }
  }  
  pas
  
  newlist=list()
  for (pa in pas){
    for ( i in 1:length(lists)){
      if(is.null(rows)){
        temp=lists[[i]][[pa]]
      } else {
        temp=lists[[i]][[pa]][rows[[i]],]
      }
      newlist[[pa]]=rbind(newlist[[pa]],temp)
    }
  }
  return(newlist)
}


# here we bind to rownames=a|b
# things are combined to be numeric
CombineMatrix<-function(a,b,resort_alphabet=T){
  ab.name=unique(c(rownames(a),rownames(b)))
  shell=cbind(ab.name,1)
  rownames(shell)=ab.name
  
  result=cbind(shell,a[match(rownames(shell), rownames(a)),],
               b[match(rownames(shell), rownames(b)),])
  t=result[,-c(1:2)]
  t=apply(t,1:2,as.numeric,na.rm=T)
  colnames(t)=c(colnames(a),colnames(b))
  
  if( resort_alphabet) {
    return(t[order(rownames(t)),])
  } else {
    return(t)
  }
}

# I am only combining common sheets for fa and chol
combine_FA_chol<-function(FA_plate,chol_plate,method="Kevin"){
  FA_pa=names(FA_plate)
  chol_pa=names(chol_plate)
  both_pa=intersect(FA_pa,chol_pa)
  
  if (method=="Kevin"){
    both_plate=list()
    for (pa in both_pa){
      a=FA_plate[[pa]]
      b=chol_plate[[pa]]
      rownames(a)=gsub("\\D","MS_",rownames(a))
      rownames(b)=gsub("\\D","MS_",rownames(b))
      temp=CombineMatrix(a,b)
      both_plate[[pa]]=temp 
    }
  }
    
  if (method=="Joe"){
    # New case: with Model 3
    if (  ("Pcalc" %in% FA_pa)  && ("p" %in% chol_pa) ) {
      names(chol_plate)[chol_pa=="p"]="Pcalc"; chol_pa=names(chol_plate)
      print("Hybrid: Fames=Model 3, Chol=Model 1!")
    }
    
    both_plate=list()
    for (pa in FA_pa){
      if ( !pa %in% chol_pa ){  both_plate[[pa]]=FA_plate[[pa]] ; next} 
      a=FA_plate[[pa]]
      b=chol_plate[[pa]]
      if ( length( intersect(rownames(a),rownames(b) )  )==0) {
        rownames(a)=gsub("\\D","MS_",rownames(a))
        rownames(b)=gsub("\\D","MS_",rownames(b))
      }
      temp=CombineMatrix(a,b,resort_alphabet=F)
      both_plate[[pa]]=temp 
    }
  }
  return(both_plate)
}

combine_FA_chol_file_concise<-function(famesfile=NULL,cholfile=NULL,write2xlsx=F,method="Kevin"){
  FA_plate=readRDS(famesfile)
  chol_plate=readRDS(cholfile)
  
  x=combine_FA_chol(FA_plate,chol_plate,method = method)
  output=gsub("fames_","fames+chol_",tolower(basename(famesfile)) )
  if (write2xlsx){
    list2xlsx(list = x,outputname = paste0("All_nocell_",output,".xlsx"))
  }
  #outfile=paste0(output,".Rda")
  outfile=output
  saveRDS(x,outfile)
  print(paste0("Output: ", basename(outfile)))
  return(outfile)
}

combine_FA_chol_file<-function(myfolder,write2xlsx=F,method="Kevin"){
  setwd(myfolder)
  print("I'm going to combine FAMES_quant.Rda with chol_quant.Rda file")
  print("Please select a FAMES_quant.Rda file")
    famesfile=file.choose()
    print(basename(famesfile))
  print("Please select a CHOL_quant.Rda file")
    cholfile=file.choose()
    print(basename(cholfile))
    if (!exists("cholfile")){
      print("[chol_quant.Rda] not selected!")
      return(famesfile)
    }
    if (!exists("famesfile")){
      print("[fames_quant.Rda] not selected!")
      return(cholfile)
    }
    
    FA_plate=readRDS(famesfile)
    chol_plate=readRDS(cholfile)
  
  x=combine_FA_chol(FA_plate,chol_plate,method = method)
  output=gsub("fames_","fames+chol_",tolower(basename(famesfile)) )
  if (write2xlsx){
    list2xlsx(list = x,outputname = paste0("All_nocell_",output,".xlsx"))
  }
  #outfile=paste0(output,".Rda")
  outfile=output
  saveRDS(x,outfile)
  print(paste0("Output: ", basename(outfile)))
  return(outfile)
}


# this is for reading in RAW cellcount file. Never used
cellcount<-function(inputfile,skip=5,COVERAGE=1){
  print( "for 24 well plate, resolution and coverage pairs: [2x, 0.935], [4x, 0.9278], [10x, 0.2517]")  
  raw=read.table(inputfile,skip=skip,check.names=F,header=T)
  raw.extract=raw[,c("Well Name","Total Cells (CellScoring)")]
  cellcount=aggregate(x = raw.extract[,2],list(well=raw.extract[,1]),sum,na.rm=T)
  r=substr(cellcount[,1],1,1)
  c=as.integer(substr(cellcount[,1],2,3))
  result=cbind(r,c,cellcount)
  colnames(result)=c("cell.row","cell.col","cell.well","cellcount")
  result$cellcount=result$cellcount/COVERAGE
  return(result)
}

FillBlank<-function(shell,matrix){
  a=cbind(shell,1)
  rownames(a)=shell
  result=cbind(a,matrix[match(rownames(a), rownames(matrix)),])
  t=as.matrix(result[,-c(1:2)])
  t=apply(t,1:2,as.numeric,na.rm=T)
  t=as.matrix(t[order(rownames(t)),])
  return(t)
}
# add every possible axes thanks to that we have cell count now.
AddCellAxes<-function(abs_quants,cellcount=NULL){
  if ( !"cell_count" %in% names(abs_quants) ){
    abs_quants[["cell_count"]]=FillBlank(rownames(abs_quants[[1]]),cellcount)
    abs_quants[["cell_count"]]=as.matrix(abs_quants[["cell_count"]])
      colnames(abs_quants[["cell_count"]])="cell_count"
  }  
  
  regular_sheets=c("quant","quant_norm","quant_syn","quant_scav")
  for (pa in intersect(regular_sheets,names(abs_quants))){
    newpa=paste0(pa,".cell")
    temp=abs_quants[[pa]]/as.numeric(abs_quants$cell_count)
    abs_quants[[newpa]]=temp*1e6
  }

  if ( "160" %in% colnames(abs_quants$quant_norm)){
    temp1=abs_quants$quant_norm[,"160"]/abs_quants$quant_norm[,"161"]
    temp4=abs_quants$quant_norm[,"161"] / abs_quants$quant_norm[,"160"]
    
    if ("181" %in% colnames(abs_quants$quant_norm)){
      temp2=abs_quants$quant_norm[,"180"]/abs_quants$quant_norm[,"181"]
      temp5=abs_quants$quant_norm[,"181"]/abs_quants$quant_norm[,"180"]
    } else {
      temp2=abs_quants$quant_norm[,"180"]/(abs_quants$quant_norm[,"181a"]+abs_quants$quant_norm[,"181b"])
      temp5=(abs_quants$quant_norm[,"181a"]+abs_quants$quant_norm[,"181b"]) /abs_quants$quant_norm[,"180"]
    }
    temp3=abs_quants$quant_norm[,"182"]/abs_quants$quant_norm[,"180"]
    
    FAratio=cbind(temp1,temp2,temp3,temp4,temp5)
    colnames(FAratio)=c("160.161","180.181","182.180","161.160","181.180")
    abs_quants[["FAratio"]]=FAratio
  }
    
  if ( "200" %in% colnames(abs_quants$quant_norm)){
    temp4=abs_quants$quant_norm[,"200"]/abs_quants$quant_norm[,"180"]
    FAratio=cbind(temp1,temp2,temp3,temp4)
    colnames(FAratio)=c("160.161","180.181","182.180","200.180")
    abs_quants[["FAratio"]]=FAratio
  }
  return(abs_quants)
}

# this is to recalculate everything for abs_quants
# you should prepare [cell_count,quant,s,p.real,abs_quant] for it
# it will calculate 
AbsQuants.Update<-function(abs_quants){
  cellcount=as.numeric(abs_quants$cell_count) / 1e6
  abs_quants[["Abs_total_mil.cell"]]=abs_quants$Abs_total/cellcount
    print("updated: Abs_total_mil.cell ")
  
  ### for syn, we need intersect
  if (( "s" %in% names(abs_quants)) && length(abs_quants$s)){
    s.lipid=colnames(abs_quants$s)
    quant.lipid=colnames(abs_quants$quant)
    syn.lipid=intersect(s.lipid,quant.lipid)
    temp.quant=abs_quants$Abs_total_mil.cell[,syn.lipid]
      temp.quant=as.matrix(temp.quant)
    temp.s=abs_quants$s[,syn.lipid]
      temp.s=as.matrix(temp.s)
    abs_quants[["Abs_syn_mil.cell"]]=temp.quant * temp.s
    print("updated: Abs_syn_mil.cell ")
    # add a paramter: abs_quant * s= abs_syn
    temp.quant.bulk=abs_quants$Abs_total[,syn.lipid]
    abs_quants[["Abs_syn"]]=temp.quant.bulk * temp.s

    
    print("updated: Abs_syn ")
    
    abs_quants[["Abs_scav_mil.cell"]]=temp.quant * (1-temp.s)
    print("updated: Abs_scav_mil.cell ")
  }
 
  # the FA ratios
  if ( "160" %in% colnames(abs_quants$quant_norm)){
    temp1=abs_quants$quant_norm[,"160"]/abs_quants$quant_norm[,"161"]
    temp2=abs_quants$quant_norm[,"180"]/abs_quants$quant_norm[,"181"]
    temp3=abs_quants$quant_norm[,"182"]/abs_quants$quant_norm[,"180"]
    temp4=abs_quants$quant_norm[,"161"]/abs_quants$quant_norm[,"160"]
    temp5=abs_quants$quant_norm[,"181"]/abs_quants$quant_norm[,"180"]
    
    FAratio=cbind(temp1,temp2,temp3,temp4,temp5)
    colnames(FAratio)=c("160.161","180.181","182.180","161.160","181.180")
    abs_quants[["FAratio"]]=FAratio
    print("updated: Fatty acid ratios from quant_norm data")
  }

  if ( ! "p.real" %in% names(abs_quants) ){
    warning("p not updated!")
  }  
  return(abs_quants) 
}

FAsheets_pa_extract<-function(FAsheets,pa){
  pa.bind=c()
  for ( FA in names(FAsheets)){
    temp=FAsheets[[FA]][,pa]; 
    temp=as.matrix(temp);
    rownames(temp)=rownames(FAsheets[[FA]]); colnames(temp)=FA
    pa.bind=cbind(pa.bind,temp)
  }
  return(pa.bind)
}
######## some real functions ########


# Function 1: change the colnames and rownames to sample coding.
asc <- function(x) { strtoi(charToRaw(x),16L) }
chr <- function(n) { rawToChar(as.raw(n)) }
Pos2SampNumber<-function(char=NULL,row,col){
  row=as.character(toupper(row))
  col=as.integer(col)
  samplenumber=(sapply(row,asc)-65)*12+col
  result=paste0(char,as.character(samplenumber))
  for (i in 1:length(row)){
    if(samplenumber[i]<10){
      result[i]=paste0(char,"0",as.character(samplenumber[i]))
    }
  }
  return(result)
}

# Function 2: calculating abs quant according to std sample
Quant.abs<-function(std.y,std.x,sample.x,title=NULL,saveFig=F,write_slope=F){
  fit=lm(std.y~std.x)
  k=fit$coefficients[[2]]
  b=fit$coefficients[[1]]
  sample.y=k*sample.x+b  
  formula=paste0("y=",fit$coefficients[[2]],"x + ",fit$coefficients[[1]])
  print(formula)
  xrange=c(min(sample.x,std.x,na.rm=T),max(sample.x,std.x,na.rm=T))
  yrange=c(min(sample.y,std.y,na.rm=T),max(sample.y,std.y,na.rm=T))
  if(saveFig){
    png(filename = paste0("Standard Curve_",title,".png"))
  }
  
  par(mar=c(8,5,3,3) )
  plot(sample.x,sample.y,col=6, main=title,
       xlab="AUC quant normalized by internal control",
       ylab="Abs Quant (nmol)",
       xlim=xrange,ylim=yrange,
       cex.main=1.5,cex.lab=1.5,cex.axis=1.5)
  legend("topleft", cex =1.2,  bty = "n",  
         #legend = paste0(formula,"  r2=",summary(fit)$r.squared))
         legend = paste0("  r2=",summary(fit)$r.squared))
  points(std.x,std.y,col=1,pch="S")  
  abline(fit)
  if(summary(fit)$r.squared<=0.8) {
    warning(paste0("really bad fit for lipid: ",title))
  }
  
  if(saveFig){
    dev.off()
  }
  return(sample.y)
}

# now the preference is to do sample and standard separately

if(F){ # yes this is for debugging, love myself!
  standard_abs = part3.match[,-which(is.na(colnames(part3.match) ) )];
sample.quant_norm = abs_quants$quant_norm;
saveFig = T; title_comment = "not checked";
standard.quant_norm = part2

# for Kevin
standard_abs = standard_nmol;
 sample.quant_norm = abs_quants$quant_norm;
 saveFig = T;title_comment = " checked";
 standard.quant_norm = standard_quant_norm
}


# for debug
if (F){
  standard_abs = standard_abs;
   sample.quant_norm = abs_quants$quant_norm;
   standard.quant_norm=NULL;
   saveFig = T;
   standard.quant_norm = standard_quant_norm;
   title_comment = "checked";
}
Quant.abs.plate<-function(standard_abs,
                          sample.quant_norm,
                          standard.quant_norm=NULL,
                          standard_name=NULL,
                          saveFig=T,
                          title_comment=NULL){
  lipid.cbind=c()
  abs.lipids=intersect(colnames(standard_abs),colnames(sample.quant_norm))
  
  if ( saveFig){
    png(filename = paste0("Std Curve _",title_comment,".png"),
        width = 1600,height = 400*ceiling(length(abs.lipids)/4))
    par(mfrow=c( ceiling(length(abs.lipids)/4),4 ))  
  }
  
  bad.lipid=c()
  for ( i.lipid in 1:length(abs.lipids)){
    lipid=abs.lipids[i.lipid];
    print(lipid)
    ###### abs quant
    data=as.matrix(sample.quant_norm[,lipid])
    std.y = standard_abs[,lipid]
    if ( ! is.null(standard.quant_norm) ){
       std.x=standard.quant_norm[,lipid]
    } else {
      if(is.null(standard_name)){
        warning("no standard quant")
        return(NULL)
      }
      std.x = data[standard_name,]
    }
    
    if ( sum(!is.na(std.x+std.y)) <3){  # if matched pairs of std <3: you need 3 points to draw a descent line
      bad.lipid=c(bad.lipid,i.lipid)
      next
    }
    if ( sum(std.x,na.rm=T)==0){      
      bad.lipid=c(bad.lipid,i.lipid);
      next
    } #if there is no data....
    
    sample.x = data     #data[which(! (rownames(data) %in% standard_name)),]
    title=paste(lipid, "AbsQuant (",title_comment, ")")
    temp=Quant.abs(std.y = std.y,std.x = std.x,sample.x = sample.x,title = title,saveFig=F)  
    lipid.cbind=cbind(lipid.cbind,temp)
  }
  
  bad.lipid=setdiff(bad.lipid, "170")
  
  if (is.null(bad.lipid)){
    colnames(lipid.cbind)=abs.lipids
  } else {
    colnames(lipid.cbind)=abs.lipids[-bad.lipid]
    bad.lipid.names=abs.lipids[bad.lipid]
    warning("bad std points for lipid:" , paste(bad.lipid.names, collapse = ", "))
  }
  
  if(saveFig){
    dev.off()
  }
  return(lipid.cbind)
}


# Function 3: plot out all the bar plots
# xx includes rownames and mean and std
PlotBar<-function(xx,title){
  par(mar=c(8,5,3,3)+10)
  ymax=max(xx[,1]+xx[,2],na.rm=T)*4/3
  print(ymax)
  ylab=NULL
  if( grepl("Abs",title) ){
    if (grepl("mil.cell",title)){ 
      ylab="nmol per mil cell"
    } else {
      ylab="nmol per well"
    }
    
  }
  
  xbar=barplot(xx[,1],ylim=c(0,ymax),space=1,cex.names  = 1.5,
               ylab=ylab,axis.lty=1,las=2,
               main=title, cex.main=2)
  arrows(xbar,xx[,1]+xx[,2],xbar,xx[,1]-xx[,2],angle=90,code=3,length=0.1)
}

#quant=standard_quant;FACon=FACon; leaveIC = T
Quant_norm<-function(quant,FACon,CholCon="STIG",leaveIC=F){
  colnames(quant)=gsub("\\X","",colnames(quant))
  if ( !("chol" %in% colnames(quant))){quant=cbind(quant,0,0)}
  
  n=ncol(quant)
  
  # if there is FA data
  fa.quant_norm=c()
  if ( FACon %in% colnames(quant)){
    FACon.c=which(colnames(quant) == FACon) 
    if(leaveIC){
      fa.quant=quant[,-c(n,n-1)]
    } else {
      fa.quant=quant[,-c(n,n-1,FACon.c)]  
    }
    fa.quant_norm=fa.quant/quant[,FACon]
  }
  result=fa.quant_norm
  
  if ( "chol" %in% colnames(quant)){
    chol.quant_norm=as.matrix(quant[,"chol"])/quant[,CholCon]
    if (leaveIC){chol.quant_norm=as.matrix(quant[,c(n-1,n)])/quant[,CholCon]}
    result=cbind(fa.quant_norm,chol.quant_norm)
    colnames(result)[ncol(result)]="chol"
    if(leaveIC){colnames(result)[ncol(result)]="STIG"}
  }
  rownames(result)=gsub("_quant","_normalized",rownames(result))
  return(result)  
}

GetCellCount<-function(cell.files,skip=5){  
  n=length(cell.files)
  cellcount=list()
  for (i.file in 1:n){  
    cell.raw=read.table(cell.files[i.file],skip = skip, header=T)
    cell.sum=aggregate(cell.raw[,"Total.Cells..CellScoring."], list(well=cell.raw[,"Well.Name"] ), sum,na.rm=T)
    
    temp=strsplit(gsub(".txt","",basename(cell.files[i.file])),split = "_")[[1]]
    platename=temp[length(temp)]
    name=cell.sum[,1]
    cell.sum=as.matrix(cell.sum[,-1])
    rownames(cell.sum)=name    
    cellcount[[platename]]=cell.sum
  }
  return(cellcount)
}

LipidSheets<-function(list, format="list"){ # all sheetnames named after lipids
  sheets=names(list)
  lipidsheets=c()
  for ( sheet in sheets){
    if ((substr(sheet,1,1) %in% c( "1","2")  || tolower(sheet)=="chol" ) 
        && sheet!="1-e0" && !grepl("tot",sheet)) {
      lipidsheets=c(lipidsheets,sheet)
    }
  }
  
  if(format=="list"){
    return(lipidsheets)
  } else {return (names(lipidsheets))}

}

# update 20170408 adding "1-e0" and abs columns to FAsheets
# the function will add Abs_total, cell_count, and calculate elongation series 
# output: FAsheets only, with calculated values
FAsheets_add_abs<-function(Abs_total,Model3Result,cellcount=NULL){
  if (F){ # debugging
    Abs_total=abs_quants$Abs_total
    Model3Result=abs_quants[LipidSheets(abs_quants)]
    cellcount=1
  }
  FAsheets=Model3Result
  for ( FA in names(FAsheets)){
    FAsheets[[FA]]=as.data.frame(FAsheets[[FA]])
    FAsheets[[FA]]$quant_syn=NULL
    data=FAsheets[[FA]]
    # add 1-e0 column to the sheet
    temp=matrix(1-FAsheets[[FA]]$e0,,1); temp[which(temp==2)]=NA
      colnames(temp)="1-e0"
    data=InsertColumn(columns = temp,data = data,method = "before", col_2_insert = "e0" )
    
    if ( FA %in% colnames(Abs_total) ) {  temp=as.matrix(Abs_total[,FA]) } else {temp=matrix(NA,nrow(FAsheets[[FA]]),1)}
      colnames(temp)="Abs_total";temp=as.data.frame(temp)
      
    for ( pa in c("s","1-e0","e0","e1","e2","e3","e4","e5"))  {
      data[which(data==-1,arr.ind = T)]=NA      #
      temp[,paste0(pa,"_abs")]=data[,pa]*temp$Abs_total
    }
    FAsheets[[FA]]=InsertColumn(columns = temp,data = data,method = "before", col_2_insert = "log_lik" )
    
    if ( length(cellcount>0) ){
      temp.cell=temp/cellcount*1e6; colnames(temp.cell)=paste0(colnames(temp),".cell")
      temp.cell=cbind(cellcount,temp.cell) ; colnames(temp.cell)[1]="cell_count"
      FAsheets[[FA]]=InsertColumn(columns = temp.cell,data = FAsheets[[FA]],
                                  method = "before", col_2_insert = "log_lik" )
    }
  }
  return (FAsheets)
}

LipidClass<-function(lipid_names){
  lipid_names=gsub("X","",lipid_names)
  classify=as.integer(substr(lipid_names,3,3));
    names(classify)=lipid_names
  SAFA_col=which(classify==0);
  MUFA_col=which(classify==1);
  PUFA_col=which(classify>1);
  SAMUFA_col=which(classify<2);
  lipid_class=list(classify=classify,
                   SAFA_col=SAFA_col,MUFA_col=MUFA_col,PUFA_col=PUFA_col,
                   SAMUFA_col=SAMUFA_col);
  return(lipid_class)
}

ElongationActivity.LipidClass<-function(FAsheets, LipidClassName){
  lipid_class=LipidClass(names(FAsheets))
  if(LipidClassName == "FASN"){
    FASN=c()
    for (FA in names(FAsheets)){
      temp=as.matrix(FAsheets[[FA]]$s_abs); 
        rownames(temp)=rownames(FAsheets[[FA]]);colnames(temp)=paste0(FA)
      FASN=cbind(FASN,temp)
    }
    FASN.sum=as.matrix(apply(FASN,1,sum,na.rm=T)); colnames(FASN.sum)="Total"
    FASN=cbind(FASN.sum,FASN)
    FASN=list("FASN tot"=FASN)
    return(FASN)
  }
  
  if (LipidClassName == "SAFA+MUFA"){
    FAsheets.SAMUFA=FAsheets[lipid_class$SAMUFA_col]
    SAMUFA_elong_list=list()
    for (FAelongated in c(16,18,20,22)){
      sheetname=paste0(as.character(FAelongated), "_", as.character(FAelongated+2)," SAFA+MUFA tot")
      
      from=as.character(FAelongated); to=as.character(FAelongated+2)
      print(paste0( "SAFA&MUFA",from, "=>", to, " : "))
      
      CurrentElongation=c()
      for (currentFA in names(FAsheets.SAMUFA)){
        currentFAlength=as.integer(substr(currentFA,1,2)); 
          if (currentFAlength<=FAelongated) {next}
        jump_current=( currentFAlength - FAelongated)/2; 
        jump_140= (currentFAlength - 14)/2;
        e_small=jump_current
        e_big=jump_140;
        e_big_colname=paste0("e",as.character(e_big),"_abs");
        e_small_colname=paste0("e",as.character(e_small),"_abs");
        
        data=FAsheets[[currentFA]]
        e_abs_col=which(colnames(data) %in% paste0("e",as.character(1:5),"_abs"))
        data_e=data[,e_abs_col]
        temp=data$s_abs + apply ( data_e[,e_small:e_big], 1, sum, na.rm=T)
          temp=as.matrix(temp); rownames(temp)=rownames(data);    colnames(temp)=currentFA
        CurrentElongation=cbind(CurrentElongation,temp)
        
       print(paste0(currentFA, ": s, ","e",as.character(e_small),"...",as.character(e_big) ))
      }
      if(is.null(CurrentElongation)){ next} # some samples don't even have 24:0...
      Sum = as.matrix(apply(CurrentElongation,1,sum,na.rm=T)); 
        colnames(Sum)="Total"
      CurrentElongation=cbind(Sum,CurrentElongation)
      
      SAMUFA_elong_list[[sheetname]] = CurrentElongation
    }
    return(SAMUFA_elong_list)
  }
  
  if (LipidClassName == "PUFA"){
    FAsheets.SAMUFA=FAsheets[lipid_class$PUFA_col]
    if(length(FAsheets.SAMUFA)==0){
      return (list())
    }
    PUFA_elong_list=list()
    for (FAelongated in c(16,18,20)){
      sheetname=paste0(as.character(FAelongated), "_", as.character(FAelongated+2)," PUFA tot")
      
      from=as.character(FAelongated); to=as.character(FAelongated+2)
      print(paste0( "PUFA",from, "=>", to, " : "))
      
      CurrentElongation=c()
      for (currentFA in names(FAsheets.SAMUFA)){
        currentFAlength=as.integer(substr(currentFA,1,2)); if (currentFAlength<=FAelongated) {next}
        jump_current=( currentFAlength - FAelongated)/2; 
        jump_162=( currentFAlength - 16)/2
        e_small=jump_current
        e_big=jump_162
        
        data=FAsheets[[currentFA]]
        e_abs_col=which(colnames(data) %in% paste0("e",as.character(1:5),"_abs"))
        data_e=data[,e_abs_col]
        if(e_small==e_big){
          temp=data_e[,e_small:e_big]
        } else {
          temp=apply ( data_e[,e_small:e_big], 1, sum, na.rm=T)
        }
        temp=as.matrix(temp); rownames(temp)=rownames(data);    colnames(temp)=currentFA
        CurrentElongation=cbind(CurrentElongation,temp)
        
        print(paste0(currentFA, ": e",as.character(e_small),"...",as.character(e_big) ))
        
      }
      Sum = as.matrix(apply(CurrentElongation,1,sum,na.rm=T)); 
      colnames(Sum)="Total"
      CurrentElongation=cbind(Sum,CurrentElongation)
      
      PUFA_elong_list[[sheetname]] = CurrentElongation
    }
    return(PUFA_elong_list)
  }
}
  
# old function
# the function will calculate FASN, 16=>18, 18=>20 etc
ElongationActivity<-function(FAsheets,cellcount=NULL){
  # FASN
  FASN=c()
  for (FA in names(FAsheets)){
    temp=as.matrix(FAsheets[[FA]]$s_abs); rownames(temp)=rownames(FAsheets[[FA]]);colnames(temp)=paste0(FA,"_s_abs")
    FASN=cbind(FASN,temp)
  }
  FASN.sum=as.matrix(apply(FASN,1,sum,na.rm=T)); 
    rownames(FASN.sum)=rownames(FASN); colnames(FASN.sum)="FASN"
  
  # 16=>18 etc
  # nFA=18,20,22. when nFA=18,
  E.activity=FASN.sum
  for ( FAelongated in c(16,18,20,22)){
    elongation=paste0(as.character(FAelongated), "=>", as.character(FAelongated+2)  )  # if FAelongated=16, then elongation="16=>18"
    E=c()
    for (FA in names(FAsheets)){
      nFA=as.integer(substr(FA,1,2)); if (nFA<=FAelongated) {next}
      step=nFA/2; 
      e_big=step-FAelongated/2; e_small=e_big+1;
      e_big_colname=paste0("e",as.character(e_big),"_abs");
      e_small_colname=paste0("e",as.character(e_small),"_abs");
      data=FAsheets[[FA]]
      temp=data$s_abs+data[,e_big_colname]-data[,e_small_colname]; temp=as.matrix(temp)
        rownames(temp)=rownames(data);      colnames(temp)=FA
      E=cbind(E,temp)
    }
    E.sum=as.matrix(apply(E,1,sum,na.rm=T)); rownames(E.sum)=rownames(E);colnames(E.sum)=elongation
    E.activity=cbind(E.activity,E.sum)
  }
  
  if (!is.null(cellcount)){
    E.cell=E.activity*1e6/c(cellcount);
    colnames(E.cell)=paste0(colnames(E.activity),".cell")
  }
  return(cbind(E.activity,E.cell))
}


# update, add abs to FAsheets, and other weird sheets e.g. SYNTH and elongation activity
# where is everything updated from:
# user will delete values in standard and quant and s 
AbsQuants.Update_JPA<-function(abs_quants){
  
  cellcount=NULL; if ("cell_count" %in% names(abs_quants)){cellcount=abs_quants$cell_count}
  
  FAsheets=abs_quants[LipidSheets(abs_quants)]
  
  for (FA in names(FAsheets)){
    FAsheets[[FA]]=as.data.frame(FAsheets[[FA]])
    FAsheets[[FA]]$quant=abs_quants$quant[,FA]
    FAsheets[[FA]]$quant_norm=abs_quants$quant_norm[,FA] # just updating 
  }
  FAsheets=FAsheets_add_abs(Abs_total=abs_quants$Abs_total,
                            Model3Result=FAsheets,
                            cellcount=cellcount)
  
  
  #####################
  # mol% calculation
  mol_list=list(); 
    mol_list[["mol%"]]=1;
    mol_list[["mol% SAFA"]]=2;
    mol_list[["mol% MUFA"]]=3;
    mol_list[["mol% PUFA"]]=4;
    mol_list[["mol% SAFA+MUFA"]]=5;
  
  input=abs_quants$Abs_total[,colnames(abs_quants$Abs_total)]; 
    input=data.frame(input)
  lipid_class=LipidClass(colnames(input))
  #LipidClass=list("classify"   "SAFA_col"   "MUFA_col"   "PUFA_col"   "SAMUFA_col")
  # gives you the index of SAFA lipid etc
    SAFA_col=lipid_class$SAFA_col; 
    MUFA_col=lipid_class$MUFA_col;
    PUFA_col=lipid_class$PUFA_col;
    SAMUFA_col=lipid_class$SAMUFA_col;
  length=as.integer( substr ( colnames(input), 2,3 )  ); 
  input$"Avg Length" =as.matrix( input) %*% length
  input$Total=apply(input[,1:(ncol(input)-1)],1,sum,na.rm=T)
  input$SAFA=apply (input[,SAFA_col] , 1, sum, na.rm=T )
  input$MUFA=apply (input[,MUFA_col] , 1, sum, na.rm=T )
  input$PUFA=apply (input[,PUFA_col] , 1, sum, na.rm=T )
  mol_list[["mol%"]]=as.matrix(input ) / input$Total

  # Mol% breakdown calculation 
  for (i.mol in 2:4){
    # get lipid mols
    temp=input[,lipid_class[[i.mol]]]
    mol_list[[i.mol]] = temp / apply(temp,1,sum,na.rm=T)
    # get length of lipid
    length=as.integer( substr ( colnames(temp), 2,3 )  ); 
      names(length)=colnames(temp)
    # calculate average length
    mol_list[[i.mol]]$"Avg Length"= as.matrix(mol_list[[i.mol]]) %*% length
    mol_list[[i.mol]]=as.matrix(mol_list[[i.mol]])
  }
  # i.mol=5: mol% SAFA+MUFA
  SAMUFA_input=input[,lipid_class$SAMUFA_col]
  length=as.integer( substr ( colnames(SAMUFA_input), 2,3 )  ); names(length)=colnames(SAMUFA_input)
  SAMUFA_temp=c()
  for ( i.length in unique(length)){ # adding 180+181 etc
    col=  which( as.integer( substr( colnames(SAMUFA_input) ,2,3)  ) == i.length )
    temp=as.data.frame (SAMUFA_input[,col])
    sum = as.matrix(apply(temp,1,sum,na.rm=T)); colnames(sum)=as.character(i.length)
    SAMUFA_temp =  cbind(SAMUFA_temp, sum) 
  }
  SAMUFA_temp=as.data.frame(SAMUFA_temp)
  mol_list$`mol% SAFA+MUFA`= SAMUFA_temp / apply(SAMUFA_temp ,1,  sum, na.rm=T)
  mol_list$`mol% SAFA+MUFA`$"Avg Length" = as.matrix( mol_list$`mol% SAFA+MUFA` ) %*% unique(length)
  output=mol_list
  # End of Mol% breakdown calculation 
  #####################
  
  ################
  # Abs_total add columns
  abs_quants$Abs_total=input[,-c(which(colnames(input)=="Avg Length"))]
  abs_quants$Abs_total.cell=abs_quants$Abs_total/ abs_quants$cell_count *1e6
  
  #ElongationActivity=ElongationActivity(FAsheets = FAsheets,cellcount = cellcount)
  FASN=ElongationActivity.LipidClass(FAsheets = FAsheets,LipidClassName = "FASN");
  SAMUFA_elong_list=ElongationActivity.LipidClass(FAsheets = FAsheets,LipidClassName = "SAFA+MUFA");
  PUFA_elong_list=ElongationActivity.LipidClass(FAsheets = FAsheets,LipidClassName = "PUFA");
  FASNandElongation=c(FASN, SAMUFA_elong_list,PUFA_elong_list)
    Enzymesheets_name_nocell=names(FASNandElongation);
    Enzymesheets_name_cell=paste0(names(FASNandElongation),".cell")
  for (i.list in 1:length(FASNandElongation)){
    cellcount=abs_quants$cell_count
    sheetname=names(FASNandElongation)[i.list]
    newsheetname=paste0(sheetname,".cell")
    FASNandElongation[[newsheetname]]=FASNandElongation[[sheetname]] / c(cellcount) *1e6
  }
  
  EnzymeActivity=c()
  for ( sheetname in Enzymesheets_name_nocell){
    temp=FASNandElongation[[sheetname]][,"Total"]
    temp=as.matrix(temp)
    rownames(temp)=rownames(FASNandElongation[[sheetname]])
    colnames(temp)=gsub(" tot","",sheetname)
    EnzymeActivity=cbind(EnzymeActivity,temp)
  }
  
  EnzymeActivity.cell=c()
  for ( sheetname in Enzymesheets_name_cell){
    temp=FASNandElongation[[sheetname]][,"Total"]
    temp=as.matrix(temp)
    rownames(temp)=rownames(FASNandElongation[[sheetname]])
    colnames(temp)=gsub(" tot.cell","",sheetname)
    EnzymeActivity.cell=cbind(EnzymeActivity.cell,temp)
  }
  
  # sheet SYNTH= 1-e0
  SYNTH=FAsheets_pa_extract(FAsheets = FAsheets,pa = "e0"); 
    SYNTH[which(SYNTH==-1)]=NA; SYNTH=1-SYNTH;
  
  # sheet P0_1_2
  P0_1_2=c()
  for (pa in c("p0","p1","p2")){
    temp=FAsheets_pa_extract(FAsheets = FAsheets,pa = pa);colnames(temp)=paste0(pa,"_",colnames(temp))
    if (pa=="p0"){P0_1_2=temp}
    if (pa!="p0") {P0_1_2=cbind(P0_1_2," ",temp)}
  }
  if(!is.null(cellcount)){
    abs_quants$Abs_total.cell=abs_quants$Abs_total *1e6/ c(abs_quants$cell_count) 
  }
  
  # sheet: FA ratios
  FAratio_key= cbind(from= c( "140",  "160",  "180",  "200",  "220",  "161",  "181a", "201",  
                        "221",  "182",  "203",  "204",  "182",  "160",  "180",  "240" ),
               to =  c("160",  "180",  "200",  "220",  "240",  "181b", "201",  "221",
                       "241",  "203",  "204",  "224",  "224",  "161",  "181t", "241"))
  input=abs_quants$quant; 
  X181t=as.matrix(input[,"181a"]+input[,"181b"]); colnames(X181t)="181t"
  input=cbind(input,X181t)
  FAratio=c()
  for ( i.key in 1:nrow(FAratio_key)){
    from=FAratio_key[i.key,"from"]
    to=FAratio_key[i.key,"to"]
    tempname=paste0(to,".",from)
    
    if ( !(from %in% colnames(input))  || !(to %in% colnames(input))  ){ next}
    temp=as.matrix(input[,to] / input[,from]); colnames(temp)=tempname
    FAratio=cbind(FAratio,temp)
  }
  # end of FAratio
  if (F) { # old version
    temp1=abs_quants$quant_norm[,"160"]/abs_quants$quant_norm[,"161"]
    if ("181" %in% colnames(abs_quants$quant_norm)){
      temp2=abs_quants$quant_norm[,"180"]/abs_quants$quant_norm[,"181"]
    } else {
      temp2=abs_quants$quant_norm[,"180"]/(abs_quants$quant_norm[,"181a"]+abs_quants$quant_norm[,"181b"])
    }
    temp3=abs_quants$quant_norm[,"182"]/abs_quants$quant_norm[,"180"]
    FAratio=cbind(temp1,temp2,temp3)
    colnames(FAratio)=c("160.161","180.181","182.180")
    abs_quants[["FAratio"]]=FAratio
    if ( "200" %in% colnames(abs_quants$quant_norm)){
      temp4=abs_quants$quant_norm[,"200"]/abs_quants$quant_norm[,"180"]
      FAratio=cbind(temp1,temp2,temp3,temp4)
      colnames(FAratio)=c("160.161","180.181","182.180","200.180")
      abs_quants[["FAratio"]]=FAratio
    }
  }
  # end of FAratio
  # end of new Sheets
  
  # rearrange abs_quants
  abs_quants_reorganize=c(abs_quants[c("cell_count","quant","quant_norm", "Abs_total","Abs_total.cell")],
                          mol_list,
                          list(FAratio=FAratio),
                          list(P0_1_2=P0_1_2),
                          abs_quants[c("Pcalc.real","s")],
                          list("1-e0"=SYNTH),
                          list("EnzymeActivity"=EnzymeActivity, "EnzymeActivity.cell"=EnzymeActivity.cell),
                          FASNandElongation,
                          FAsheets)
                       
  return(abs_quants_reorganize) 
}

# update 2016.9.12 for dirty samples. update RAW and quant.Rda according to change in prep file
Update_RAW.Rda_2_prepfile<-function(prepfile,RAW.Rda.file){
  prep.new=xlsx2list(file = prepfile)
  RAW.Rda=readRDS(RAW.Rda.file)
  
  for ( name in names(RAW.Rda)){
    RAW.Rda[[name]]=as.matrix(RAW.Rda[[name]],rownames.force);
  }
  
  for ( lipid in names(RAW.Rda)){
    if (! lipid %in% names(prep.new)){ next}
    ions=ncol(RAW.Rda[[lipid]])
    newdata=prep.new[[lipid]][,1:ions]; newdata=as.matrix(newdata); rownames(newdata)=rownames(prep.new[[lipid]])
    
    RAW.Rda[[lipid]]=newdata[rownames(RAW.Rda[[lipid]]),]
  }
  saveRDS(object = RAW.Rda,file = RAW.Rda.file)
  return(RAW.Rda.file)
}

Update_quant.Rda_2_RAW.Rdafile<-function(RAW.Rda.file,quant.Rda.file,InControl="190",cutoff_control= 10000){
  quant.Rda=readRDS(quant.Rda.file)
  RAW.Rda=readRDS(RAW.Rda.file)
  
  new_quant=c()
  for ( lipid in names(RAW.Rda)){
    data=as.matrix(RAW.Rda[[lipid]],rownames.force = T)
    temp_quant=apply(data,1,sum,na.rm=T);temp_quant=as.matrix(temp_quant); rownames(temp_quant)=rownames(data);colnames(temp_quant)=lipid
    new_quant=cbind(new_quant,temp_quant)
  }
  # now we have new quant
  for (col in colnames(quant.Rda$quant)){
    if ( ! col %in% colnames(new_quant)){next}
    temp=quant.Rda$quant
    quant.Rda$quant[,col]=new_quant[rownames(temp),col]
  }
  
  quant=quant.Rda$quant
  quant.Rda$quant_norm=quant/quant[,InControl]
  quant.Rda$quant_norm[which(quant[,InControl]< cutoff_control),]=NA # filter
  
  saveRDS(quant.Rda,file=quant.Rda.file)  
  return(quant.Rda.file)
}

Update_due2_prepfile<-function(prepfile,InControl="190",cutoff_control= 10000){
  temp=gsub("prep_","",prepfile); temp=gsub(".xls","",temp)
  RAW.Rda.file=paste0(temp,"_rawAUC.Rda")
  quant.Rda.file=paste0(temp,"_quant.Rda")
  Update_RAW.Rda_2_prepfile(prepfile = prepfile,RAW.Rda.file = RAW.Rda.file)
  Update_quant.Rda_2_RAW.Rdafile(RAW.Rda.file = RAW.Rda.file,quant.Rda.file = quant.Rda.file,InControl = InControl,cutoff_control = cutoff_control)
  print("Now files are updated")
  print(RAW.Rda.file);print(quant.Rda.file)
}

readMAPfile<-function(mapfile,format="Kevin"){
  map_raw=read.xlsx(file=mapfile,sheetName="map",check.names=F,startRow=2)
  #map_raw=read.xlsx(mapfile,sheet="map",check.names=F,startRow=2) #update 20220606, Gino, change format to match new openxlsx library
  
  #delete empty rows
  NArow=which(is.na(map_raw$SampleName1)) # delete extra empty rows
  if (length(NArow)>0) {map_raw=map_raw[-NArow,]}
  # delete empty columns
  for (i.col in colnames(map_raw)){
    if ( length (na.omit(map_raw[,i.col]))==0) {map_raw[,i.col]=NULL}
  }  
  
  #if SampleID has all the names, then use SampleID as the rownames for mapraw, otherwise, create SampleID
  if ( sum(is.na(map_raw$SampleID))==0){
    rownames(map_raw)=map_raw$SampleID
  } else {
    rownames(map_raw)=Pos2SampNumber("MS_",map_raw$Row.ms,map_raw$Col.ms)
  }
  head(map_raw)
  return(map_raw)
}