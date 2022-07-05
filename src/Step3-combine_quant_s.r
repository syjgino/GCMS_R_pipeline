# 2018-02-21 file bug fixed
# 08-17-2016 changing inputfiles according to platforms
# 10-02-2015 changing folder. making user interface clearer
# last updated: 06-16-2015
# Author: dylan
# this script is for merge all the possible data
# including p, s from modeling, quants, normalized quants, and quant*(1-s) and quant*s for all lipids

# input: model data (manual)
# input: quant file, in Rda


##### USER INTERFACE #####
if (T){
  myfolder=getwd()
  
  p_Threshold=0.002
  s_Threshold=0.002
  step3write2xlsx=T
  
  quant_Rda_files=c()
  print("please select all files: Result_Model1_.....xls")
  inputfiles=choose.files()
}
##########################
FA=c("140","160","161","180","181")
FA=FattyAcids
library(xlsx)
setwd(myfolder)

for ( inputfile in inputfiles){
  dir=dirname(inputfile)
  setwd(dir)
  filename=basename(inputfile)
  filename=gsub("Results-Model1-prep_","",filename)
  filename=gsub(".xls","",filename)
  
  print(paste0("Merging results for file: ",filename))
  
  isFA=grepl("fames",tolower(basename(filename)))
  isChol=grepl("chol",tolower(basename(inputfile)))
  if(isFA){
    lipids=FA
    sheetnames=paste0("x",lipids)
  }
  if(isChol){
    lipids="chol"
    sheetnames=c("chol")
  }
  
  p_values=c()
  s_values=c()
  for (sheetname in sheetnames){
    raw=read.xlsx(inputfile,sheetName=sheetname,row.names=T,check.names=F)
    
    # weired scenario: there's no data for a certain lipid
    if ( length(raw)==0 ){
      fake=matrix(NA,nrow(raw),1)
      rownames(fake)=rownames(raw)
      p=fake
      s=fake
    } else {
      p=as.matrix(raw$p)
      s=as.matrix(raw$s)
    }
    p_values=cbind(p_values,p)
    s_values=cbind(s_values,s)
  }
  colnames(p_values)=lipids
  rownames(p_values)=row.names(raw)
  colnames(s_values)=lipids
  rownames(s_values)=row.names(raw)
  
  # quality control: if either s or p < threshol,then they are both 0        
  s_values[which(s_values<s_Threshold)]=NA
  p_values[which(p_values<p_Threshold)]=NA
  p_values[which(s_values<s_Threshold)]=NA # delete p while s is too small
  
  Quant=readRDS(paste0(filename,"_quant.Rda"))
  quant_norm=Quant$quant_norm
  
  ### calculte s*quant and scav*quant
  s_lipids=colnames(s_values)
  quant_lipids=colnames(quant_norm)
  quant_s_lipids=intersect(s_lipids,quant_lipids)
  col=quant_s_lipids
  
  quant_s=s_values[,col]*quant_norm[,col]
  quant_scav=(1-s_values[,col])*quant_norm[,col]
  quant_s=as.matrix(quant_s)
  colnames(quant_s)=quant_s_lipids
  quant_scav=as.matrix(quant_scav)
  colnames(quant_scav)=quant_s_lipids
  
  Merge=list(quant=Quant$quant,quant_norm=quant_norm,s=s_values,p=p_values,quant_syn=quant_s,quant_scav=quant_scav)
  quant_Rda_file=paste0(filename,"_quant.Rda")
  saveRDS(Merge,quant_Rda_file)
  quant_Rda_files=c(quant_Rda_files,quant_Rda_file)
  

  outputname=paste0("All-Results-",filename,".xlsx")
  if (step3write2xlsx ){
    list2xlsx(Merge,outputname)
  }
  
  print(paste0("end of Merging file: ",filename))
}

print("Now the .Rda files are updated with p and s info:)")



