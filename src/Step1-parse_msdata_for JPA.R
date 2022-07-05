#######################
# this script is designed for multi-file processing in the same folder
# input format: .xlsx file from Eve
# 1st column with sample name, 2nd column with code {-1, 0, 1}
# mission 1: write to Moses xls: won't write 170 or 190 to the xls for modeling
# mission 2: save to list:AUC_lipids, for further quant analysis


#function to detect M+0 for all the FA
#input is the colnames converted to double data type, which are M/Z actually
#output gives 3 information as in { 16:0,M+4 }
#possible bugs: 
#1.isoform,  manually change the lipid name
#2.M+13 will cause trouble: doesn't usually happen

# if code is not predefined, then this script will use the 2nd column in raw file to define code

#this function is to parse the different fatty acid according to their molecular weight
# Algorithm for Joe: Same as kevin's, but due to M-2 and M-1, 240 gives 14:1, so for even number LC, desat-1 fixes it.




######################
cutoff_control= 10000## because of atune in chol for lib 7/8
prep_write=T
InControl.fix=T
InControl="190"
Sort_by_Column=F
######################

### Begin: for PLATFORM COMPATIBILITY  ##############
#WhereAmI=getwd()
#if ( grepl("C:/Users",WhereAmI)){
#  Platform="Windows"
#} else {
#  Platform="Mac"
#}


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

inputfiles=choose.files()

Sort_By_Column<-function(INPUT){  # copied from tool.r
  samplenames=rownames(INPUT)
  n=nchar(samplenames)
  tmp=as.integer(substr(samplenames,n-1,n))
  num_by_col=Num2RowNCol(tmp)[,3]
  OUTPUT=INPUT[order(num_by_col),]
  return(OUTPUT)
}
Num2RowNCol<-function(number,WELL=96){  # copied from tool.r
  number=as.matrix(number)
  row=number %/%12 +1
  col=number%%12; col[which(col==0)]=12
  num_by_col=(col-1)*8+row
  result=cbind(row,col,num_by_col)
  colnames(result)=c("row","col","num_by_col"); 
  rownames(result)=paste0("sample_",as.character(number))
  return(result)
}

FattyAcidClassify <- function(MassVector){
  if (MassVector[1]!=242) {"error: data not starting with 14:0 fatty acid!"}
  
  dMassVector = MassVector[2:length(MassVector)]-MassVector[1:length(MassVector)-1]
  dMassVector
  dMassVector = c(10,dMassVector) #d means the difference, for the same fatty acid, the next weight always increase by 1. or it's other fatty acid
  FAVector_raw = MassVector*(dMassVector!=1) #pick all the M+0 molecule weight
  FAbegin_left=which(FAVector_raw>0)  #left and right is the offset for one fatty acid
  FAbegin_right=FAbegin_left[2:length(FAbegin_left)]-1
  FAbegin_right=c(FAbegin_right,length(FAVector_raw))
  FAVector = FAVector_raw[FAVector_raw>0] #FAVector= 242 270 268 284 298 296 294
  FAVector

  FattyLength = ceiling((FAVector-242+14*14)/14)  ##potential bug in highly desaturated FA eg 16:4##
  FattyLength
  FattyDesaturated = (FAVector-242) %% 14 # =  0  0 12  0  0 12 10
  FattyDesaturated = (FattyDesaturated!=0)*((14-FattyDesaturated) / 2 ) # = 0 0 1 0 0 1 2
  FattyDesaturated
  FattyAcidID=cbind(FattyLength,FattyDesaturated,FAbegin_left,FAbegin_right)
  colnames(FattyAcidID)=c("length","desat","left","right")
  FattyAcidID=as.data.frame(FattyAcidID)
  return(FattyAcidID)
}

name2code<-function(sampleID){
  notrun=(0:7)*12+1
  
  ID=as.numeric(gsub("\\D", "", sampleID))
  
  for ( i in 1:length(ID) ){
    if(ID[i]==12){ID[i]=0}
    else if(ID[i] %in% notrun){ID[i]=-1}
    else {ID[i]=1}    
  }    
  
  return(ID)
}

###############
library(xlsx)

if (F){
  KEY_file="160411 LC VLC Ion Key.xlsx"
  KEY_1=read.xlsx(file = KEY_file,sheetIndex = 1,startRow = 7)
  KEY_1=KEY_1[,1:3]
  KEY_2=read.xlsx(file=KEY_file,sheetIndex = 2, startRow=7)
  KEY_2=KEY_2[,1:3]
  KEY=rbind(KEY_1,KEY_2) 
  KEY=readRDS(file="JPA_KEY.Rda")
}

print("please select all your raw mass hunter files:)")
for (inputfile in inputfiles){
  # get filename, use it for output
  filename=basename(inputfile)
  print(paste0("now processing ",filename))
  
  filename=gsub(".xlsx","",filename)
  filepath=dirname(inputfile)
  isFA=grepl("fames",tolower(filename))
  isChol=grepl("chol",tolower(inputfile))
  if (! (isFA || isChol)) {warning("Filename should end with \"FAMES\" or \"CHOL\"!")}
  setwd(filepath)
  output_xls=paste0("prep_",filename,".xls")
 
  raw = as.matrix(read.xlsx(inputfile,sheetIndex=1,check.names=F))
    # rearrange the data in raw file
  
  # rearrange the data in raw file
  raw_modified=raw[-1,-c(1,5,6,7),drop=F] 
  rownames(raw_modified)=gsub(".D","",raw_modified[,3])
    # resort rows
  if(Sort_by_Column){
    raw_modified=Sort_By_Column(INPUT = raw_modified)
    warning("samples re-ordered by Columns on MS plate")
  }
  code=as.matrix(raw_modified[,1]) # prepare code before it's too late
  raw_modified=raw_modified[,-(1:3)]
   # resort columns
  ion_order=substr(colnames(raw_modified),1,3)
  raw_modified=raw_modified[,order(ion_order)]
  colnames(raw_modified)=gsub("\\..*","",colnames(raw_modified)) #\\. means ".", . means any character
  colnames(raw_modified)=gsub(".*_","",colnames(raw_modified))  #get rid of the junk in the name

  AUCdata=apply(raw_modified,1:2,as.numeric)
  rm(raw_modified)  
  
  # get the code of the data {-1,0,1}, NA will be regarded as "1"  
  code.num=length(which(!is.na(code)))
  if( code.num > 0) {
    code[is.na(code)]=1
    Code=as.matrix(as.numeric(code))
  } else {
    Code=name2code(row.names(AUCdata))
    Code=as.matrix(Code)
    warning("I'm using 1st column as standard curve!")
  }
  colnames(Code)="Code"
  
  if (isChol){
    lipids=c("chol","STIG")
    Chol_AUC=matrix(0,nrow(AUCdata),30)
    colnames(Chol_AUC)=paste0("M+",as.character(c(-2:27)))
    rownames(Chol_AUC)=rownames(AUCdata)
    
    Chol_AUC[,1:(ncol(AUCdata)-1)]=AUCdata[,1:(ncol(AUCdata)-1)]
    Chol_AUC=cbind(Chol_AUC,Code)
    
    Chol_AUC=apply(Chol_AUC,1:2, as.numeric)
    
    AUC_lipids=list()
    AUC_lipids[["chol"]]=AUCdata[,1:(ncol(AUCdata)-1)]
    AUC_lipids[["STIG"]]=AUCdata[,ncol(AUCdata)]
    
    write.xlsx( Chol_AUC,output_xls,sheetName="chol",append=T,col.names=T,row.names=T)
    write.xlsx(AUC_lipids[["STIG"]],output_xls,sheetName="STIG",append=T,col.names=T,row.names=T)
  } # end of processing chol file
  
  if (isFA) {
    # detect all the lipids in the data
    MassVector=as.integer(colnames(AUCdata)) 
    FA_ID=FattyAcidClassify(MassVector)
    even=which((FA_ID$length) %% 2 ==0)
    if (FA_ID$desat[even][1]>0){  # Joe starts with M-2
      warning("Hello Joe!")
      FA_ID$desat[even] = FA_ID$desat[even]-1
      FA_ID$left[even] = FA_ID$left[even]+2
    }
     print (FA_ID)
    lipids=paste0(as.character(FA_ID[,1]),as.character(FA_ID[,2]))
      n.isoform=sum(table(lipids)>1)
      if ( n.isoform >0){
        for( itemp in 1:n.isoform) {
          t=table(lipids)
          FA_isos=names(t)[which(t>1)[itemp]]
          lipids_isos_pos=which(lipids==FA_isos)
            ntemp=length(lipids_isos_pos)
            lipids[lipids_isos_pos]=paste0(lipids[lipids_isos_pos],letters[1:ntemp])
        }
      }
    print(lipids)
    warning("Make sure lipid names are correct.")
    if ( length(lipids) > length(unique(lipids))){
      warning("Replicating lipids, will use the second one with the same lipid name!")
    }
    
    ### MISSION 1: calculate p and s value with MOSES script ###
    ### prepare for data input, parse the file, put code #######
    ### write separately to different sheets, caution: data type matters!###
    
    for ( i in 1:length(lipids)){
      carbons=FA_ID$length[i]
      if (carbons %% 2 == 1){next}
      FattyAcid=lipids[i]
      
      FA_AUC=matrix(0,nrow(AUCdata),FA_ID$length[i]+2)  
      # create a zero matrix samplenumber*(carbon number +2) for MOSES script
      colnames(FA_AUC)=paste0("M+",as.character(0:(carbons+1)))
      rownames(FA_AUC)=rownames(AUCdata)
      
      FA_AUC[,1:(FA_ID$right[i]-FA_ID$left[i]+1)] = AUCdata[,FA_ID$left[i]:FA_ID$right[i]]  
      FA_AUC[is.na(FA_AUC)]=0
      
      FA_AUC=cbind(FA_AUC,Code) ##change here
      colnames(FA_AUC)[ncol(FA_AUC)] = "Code"
      
      FA_AUC=apply(FA_AUC,1:2,as.numeric)
      #give the label in the left upper grid
      # corner=paste(FA_ID$length[i],FA_ID$desat[i],sep=":")
      #FA_AUC_corner=AddCorner(FA_AUC,corner)
      if (prep_write) {
        write.xlsx(FA_AUC,output_xls,sheetName=FattyAcid,append=T,col.names=T)
      }
    }  
    
    AUC_lipids=list()
    for ( i in 1:length(lipids)){
      temp=AUCdata[,FA_ID$left[i]:FA_ID$right[i]] 
      AUC_lipids[[lipids[i]]]=temp
    }
  } # end of processing Fatty Acid file
  
  ### MISSION 2: calculate quantitive value ###
  ### prepare for data input
  saveRDS(AUC_lipids,file=paste0(filename,"_rawAUC.Rda")) 
  
  #####get quantS
  nlipids=length(AUC_lipids)
  quant=matrix(0,nrow(AUCdata),nlipids)
  colnames(quant)=names((AUC_lipids))
  rownames(quant)=rownames(AUCdata)
  quant_norm=quant
  
  if(isFA && !InControl.fix){
    InControl="170"
    if ( "190" %in% lipids ) {InControl="190"}
  }
  if(isChol){InControl="STIG"}
  
  for (i in 1:nlipids){
    temp=as.matrix(AUC_lipids[[i]])
    quant[,i]=apply(temp,1,sum,na.rm=T)
  }
  
  quant_norm=quant/quant[,InControl]
  
  quant_norm[which(quant[,InControl]< cutoff_control),]=NA
  
  Quant=list(quant=quant,quant_norm=quant_norm)
  saveRDS(Quant,file=paste0(filename,"_quant.Rda"))
  
  ###this is related with the final results
  # before running the model, we can have the quant first
  # sample names not added!!!!!!!!!!!!!!!
  outputall=paste0("[temp]All-Results-",filename,".xlsx")
  quant_sample=quant[Code==1,]
  quant_norm_sample=quant_norm[Code==1,]
  
 # write.xlsx(quant_sample,outputall,sheetName="quant_raw",append=T)
 # write.xlsx(quant_norm_sample,outputall,sheetName="quant_normalized",append=T)
  
  #Quant_sample=list(quant=quant_sample,norm=quant_norm_sample)
  #saveRDS(Quant_sample,file=paste0("temp view",filename,"_quant_sample.Rda")) 
  print(paste0("Converted to: ",output_xls))
}




# -------------------Update----------------------- #
# 10-2-2015: no more stupid code.txt, use first column as code=-1 if not indicated
# last updated: 4-11-2016: to enable more complicated fatty acid file conversion 
# Author: Quan Dylan Zhou
#
#
# update 20220606, Gino
# get OS name with:
# Platform = Sys.info()[["sysname"]] 
# avoid using "C:/" to identify windows