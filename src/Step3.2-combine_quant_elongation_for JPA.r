# Author: dylan
# this script is for merge all the possible data for model 3 and the original data
# including p0/1/2 and e0/1/2/3 from modeling, quants, normalized quants, and quant*(1-s) and quant*s for all lipids

# input: model data (manual)
# input: quant file, in Rda

### Begin: for PLATFORM COMPATIBILITY  ##############
#WhereAmI=getwd()
#if ( grepl("C:/Users",WhereAmI)){
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

source(paste0(TOOLPATH,"TOOL pipeline functions.r"))

##### USER INTERFACE #####
if (T){
  
  ENRICH = 0.99
  ########################
  
  p_Threshold=0.002
  s_Threshold=0.002
  
  p_Threshold=0
  s_Threshold=0
  write2xlsx=F
  
  print("please select all files: Result_Model3_.....xls")
  inputfiles=choose.files() 
  
}




for ( inputfile in inputfiles){
  # inputfile=inputfiles
  dir=dirname(inputfile)
  setwd(dir)
  filename=basename(inputfile)
  filename=gsub("Results-Model3-prep_","",filename)
  filename=gsub(".xlsx","",filename); filename=gsub(".xls","",filename)
  
  isFA=grepl("fames",tolower(filename))
  isChol=grepl("chol",tolower(inputfile))
  
  raw.list=xlsx2list(file = inputfile)
  sheetnames=names(raw.list); print("sheets in file: "); print(sheetnames)
  sheetnames=gsub("x","",sheetnames); names(raw.list)=sheetnames; lipids=sheetnames
  
  print(paste0("Merging results for file: ",filename))
  # filter 
  for ( sheet in sheetnames){
    raw=raw.list[[sheet]]
    # quality control: if either s or p < threshol,then they are both 0      
    for ( pa in c("p0","p1","p2","s") ){
      raw[[pa]][which(raw$s < s_Threshold)]=NA
      raw[[pa]][which(raw$p2 < p_Threshold)]=NA
    }
    raw.list[[sheet]]=raw
  }
  
  # calculate Pcalc
  for ( sheet in sheetnames){
    raw=raw.list[[sheet]]
    raw$Pcalc= (raw$p2 + 0.5*raw$p1 -raw$q)/(ENRICH-raw$q)
    # rearrange
    n=ncol(raw);     raw=raw[,c(1:4,n,5:(n-1))]  
    raw.list[[sheet]]=raw
    #p = (p2 + .5p1 - q)/(e - q)  ;e = label glucose enrichment
  }
  
  # Now the QUANT data!
  Quant=readRDS(paste0(filename,"_quant.Rda")); print(colnames(Quant[[1]]))
  
  # write to a new list 
  data.list=list()
  for ( sheet in intersect(sheetnames,colnames(Quant[[1]]))){
    raw=raw.list[[sheet]]
    raw$quant=Quant$quant[,sheet]  # [] match rownames! to be debugged
    raw$quant_norm=Quant$quant_norm[,sheet] 
    raw$quant_syn=raw$quant_norm * raw$s
      n=ncol(raw); raw=raw[,c(n-2,n-1,n,1:(n-3))]
    data.list[[sheet]]=raw
  }
  
  data.list$quant=Quant$quant; data.list$quant_norm=Quant$quant_norm

  for ( pa in c("s","Pcalc","quant_syn")){
    pa.extract=c()
    sheets=intersect(sheetnames,colnames(Quant[[1]]))
    for ( sheet in sheets){
     temp = data.list[[sheet]][[pa]]
     pa.extract=cbind(pa.extract,temp)
    }
    colnames(pa.extract)=sheets; rownames(pa.extract)=rownames(data.list[[sheet]])
    data.list[[pa]]=pa.extract
  }
  
  saveRDS(data.list,paste0(filename,"_quant_s.Rda"))
  
  outputname=paste0("Temp-Summary-",filename,".xlsx")
  if (write2xlsx ){
    list2xlsx(data.list,outputname)
  }
  
  print(paste0("end of Merging file: ",filename))
}
print("Now the quant_s.Rda files are updated with p and s info:)")
print("End of Step 3.2. Please run Step 5.2")





# -------------------Update----------------------- #
# update 20220606, Gino
# get OS name with:
# Platform = Sys.info()[["sysname"]] 
# avoid using "C:/" to identify windows