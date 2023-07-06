# last updated: 2017.05.26 minor update
# last updated: 2016.08.28: resort the columns by colnames (Masshunter sometimes arranges poorly)
# last updated: 05-11-2016: F12 code =1 in default; and sort by column
# last updated: 10-2-2015: no more stupid code.txt, use first column as code=-1 if not indicated
# Author: dylan

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

#######USER INTERFACE ##########
cutoff_control= 1000## for quality control, if AUC<cutoff_control, it's treated as bad injection, [cutoff, because of atune in chol for lib 7/8]
InControl="190"
InControl.fix=F
  # if you fix, the program will listen to InControl that's set. 
  #if you don't fix, the program will recognize InControl automatically: if there is 190, use 190, if not, use 170.
  # this is only for InControl for FA data.
########END OF USER INTERFACE########
# Other parameters
prep_write=T
temp_quant_write=F
Sort_by_Column=T # this is to convert the ms data by column instead of by row.
############

#### laoding functions #####
###############
install.packages("openxlsx")
library(openxlsx)
library(xlsx)
if (T){
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
  
  # this function is the default setting of code. 
  # first column S1-S6 --> code=-1
  name2code<-function(sampleID){
    notrun=(0:7)*12+1
    
    ID=as.numeric(gsub("\\D", "", sampleID))
    
    for ( i in 1:length(ID) ){
      #if(ID[i]==12){ID[i]=0}
      if(ID[i] %in% notrun){ID[i]=-1}
      else {ID[i]=1}    
    }    
    
    return(ID)
  }
}



if ( substr(getwd(),2,6)=="Users"  ) {
  inputfiles=file.choose()
} else {
  setwd("C:\\Users\\Bensinger-Analysis\\Kevin")
  cat("select all raw AUC data files:\
..._CHOL.xlsx    ..._FAMES.xlsx")
  inputfiles=choose.files()
}

for (inputfile in inputfiles){
  raw = as.matrix(read.xlsx(inputfile,sheetIndex=1,check.names=F))
  # get filename, use it for output
  filename=basename(inputfile)
  print(paste0("now processing ",filename))
  
  filename=gsub(".xlsx","",filename)
  filepath=dirname(inputfile)
  isFA=grepl("fames",tolower(filename))
  #isChol=grepl("chol",tolower(inputfile))
  isChol=grepl("chol",tolower(filename)) #gino update
  if (! (isFA || isChol)) {warning("error in filename! must contain FAMES or CHOL!"); next}
  setwd(filepath)
  output_xls=paste0("prep_",filename,".xls")
  
  # rearrange the data in raw file
  raw_modified=raw[-1,-c(1,2,4,5,6,7),drop=F] 
    rownames(raw_modified)=raw_modified[,1]
    raw_modified=raw_modified[,-1]
  #
    ion_order=substr(colnames(raw_modified),1,3)
    raw_modified=raw_modified[,order(ion_order)]
    colnames(raw_modified)=gsub("\\..*","",colnames(raw_modified)) #\\. means ".", . means any character
    colnames(raw_modified)=gsub(".*_","",colnames(raw_modified))  #get rid of the junk in the name



  AUCdata=apply(raw_modified,1:2,as.numeric)
   rm(raw_modified)  
  AUCdata[is.na(AUCdata)]=0
  if (Sort_by_Column){
    AUCdata=Sort_By_Column(INPUT = AUCdata)
    print("Samples sorted by Column on GCMS")
  }
  
  # get the code of the data {-1,0,1}, NA will be regarded as "1"  
  code=as.matrix(raw[,2])
  code=as.matrix(code[-1,])  
  code.num=length(which(!is.na(code)))
  if( code.num > 0) {
    code[is.na(code)]=1
    Code=as.matrix(as.numeric(code))
  } else {
    Code=name2code(row.names(AUCdata))
    Code=as.matrix(Code)
    warning("code=-1 for first column ! Or please add code to raw file!")
  }
  colnames(Code)="Code"
  
  # INPUT: AUCdata
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
    
    if (prep_write){
      write.xlsx( Chol_AUC,output_xls,sheetName="chol",append=T,col.names=T,row.names=T)   
    }
 
  } # end of processing chol file
  
  if (isFA) {
    # detect all the lipids in the data
    MassVector=as.integer(colnames(AUCdata)) 
    FA_ID=FattyAcidClassify(MassVector)
    lipids=paste0(as.character(FA_ID[,1]),as.character(FA_ID[,2]))
    print(lipids)
    warning("Check lipid names: ", paste(lipids,collapse = ", "))
    
    ### MISSION 1: calculate p and s value with MOSES script ###
    ### prepare for data input, parse the file, put code #######
    ### write separately to different sheets, caution: data type matters!###
    
    for ( i in 1:length(lipids)){
      carbons=FA_ID$length[i]
      if (carbons==17){next}
      if (carbons==19){next}
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
        write.xlsx(FA_AUC,output_xls,sheetName=FattyAcid,append=T,col.names=T,row.names=T)
      }
    }  
    
    AUC_lipids=list()
    for ( i in 1:length(lipids)){
      temp=AUCdata[,FA_ID$left[i]:FA_ID$right[i]] 
      AUC_lipids[[lipids[i]]]=temp
    }
  } # end of processing Fatty Acid file
  
  saveRDS(AUC_lipids,file=paste0(filename,"_rawAUC.Rda")) 
  if(isFA && !InControl.fix){
    InControl="170"
    if ( "190" %in% lipids ) {InControl="190"}
  }
  if(isChol){InControl="STIG"}
  if (prep_write){
    write.xlsx(AUC_lipids[[InControl]],output_xls,sheetName=InControl,append=T,col.names=T,row.names=T)
  }

####### MISSION 1 completed
  
  ### MISSION 2: calculate quantitive value ###
  ### prepare for data input
  #####get quantS
  quant=matrix(0,nrow(AUCdata),length(lipids))
  colnames(quant)=lipids
  rownames(quant)=rownames(AUCdata)
  quant_norm=quant
  

  for (i in 1:length(lipids)){
    temp=as.matrix(AUC_lipids[[i]])
    quant[,i]=apply(temp,1,sum,na.rm=T)
  }
  
  quant_norm=quant/quant[,InControl]
  
  quant_norm[which(quant[,InControl]< cutoff_control),]=NA # filter
  
  Quant=list(quant=quant,quant_norm=quant_norm)
  saveRDS(Quant,file=paste0(filename,"_quant.Rda"))  
  
  ###this is related with the final results
  # before running the model, we can have the quant first
  # sample names not added!!!!!!!!!!!!!!!
  temp_quant.xlsx=paste0("[quant]",filename,".xlsx")
  quant_sample=quant
  quant_norm_sample=quant_norm
  
if(temp_quant_write){
   write.xlsx(quant_sample,temp_quant.xlsx,sheetName="quant_raw",append=T)
   write.xlsx(quant_norm_sample,temp_quant.xlsx,sheetName="quant_normalized",append=T)
}

  #Quant_sample=list(quant=quant_sample,norm=quant_norm_sample)
  #saveRDS(Quant_sample,file=paste0("temp view",filename,"_quant_sample.Rda")) 
  print(paste0("end of processing ",filename))
}