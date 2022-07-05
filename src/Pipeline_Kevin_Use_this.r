#----------------------------------------------------------------#
########################## User Interface #############################
Missing_Lipid=F #If you are running both chol and FAMES, then =F, if you don't have both, then T
FACon="190"      # internal control for Fatty Acid. change it to "190" for macrophage experiments!
PERCENT_LABEL=0.5  # % of glucose that's labeled. 
                  # will be overwritten if MAP file has percentage information
FattyAcids=c("140","160","161","180","181")
p_Threshold=0.002  # Quality Control: if p<threshold, then it will be excluded
s_Threshold=0.002  # Quality Control: if s<threshold, then it will be excluded
I_Want_to_Look_At_Distribution=F # For side-by-side comparison of distributions 
                                #pre and after modeling

############### End of User Interface, don't change anything below ###############
# to Wayne: if you want to re-draw the figures, update your [Summary xxx.Update.xlsx], 
# select the code in the {} below and press Ctrl+Enter
# {source(paste0(TOOLPATH,"Step8-stats and graph.r"))}
#----------------------------------------------------------------#


# select all inputfiles here

print("please select ALL POSSIBLE FAMES and CHOL results, it's fine you only have FAMES or CHOL available ")
print("they look like this: [Result_Model1_....xls]")
inputfiles=choose.files();basename(inputfiles)

print("please select your Map file. the map file starts with [ Map_xxxx.xlsx ]")
cat("please make sure all duplicates use the same sample name")
mapfile=file.choose(); basename(mapfile)

myfolder=getwd()



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
#TOOLPATH="C:\\Users\\Bensinger-Analysis\\Dylan\\[protocol]data analysis pipeline\\GCMS pipeline\\"
source(paste0(TOOLPATH,"TOOL pipeline functions.r"))

##### USER HANDLERS
user_outname=NULL
# Step 3. Incorporate s and p into quant data file, and combine chol and fames data together
####### USER HANDLE 1 #######
########  END of USER HANDLE 1##########
step3write2xlsx=F

quant_Rda_files=c()
source(paste0(TOOLPATH,"Step3-combine_quant_s.r")) # caution: this is for Model1 only
# quant_Rda_files is now updated 

# sometimes Wayne only has CHOL or FAMES ready.
Result_lipid=c()
fames_chol_file=NULL
for (i in 1:length(inputfiles)){
  temp=unlist(strsplit(basename(toupper(inputfiles[i])),split = "_"))
  temp=temp[length(temp)]
  temp=unlist(strsplit(temp,split = "\\."))[1]
  Result_lipid=c(Result_lipid,temp)
}
if(length(unique(Result_lipid))==1){
  Missing_Lipid=T
  to_print=paste0("only ", Result_lipid[1], " available")
  print(to_print)
}

if (Missing_Lipid==T){
  fames_chol_file=quant_Rda_files
  print(paste(basename(quant_Rda_files),"selected"))
  #fames_chol_file=file.choose()
} else {
  famesfile=quant_Rda_files[which(grepl("fames",tolower(quant_Rda_files)))];
  cholfile=quant_Rda_files[which(grepl("chol",tolower(quant_Rda_files)))];
  fames_chol_file=combine_FA_chol_file_concise(famesfile = famesfile,cholfile = cholfile,write2xlsx = F,method = "Kevin")
}

# Step 5. Use Map file and fame+chol.Rda file to calculate everything
######### USER HANDLE 2#########################
check.RAW.xlsx=T
######### END of USER HANDLE 2################
source(paste0(TOOLPATH,"Step5-Calculate Absolute Quants with Standards.r"))
Summary_unchecked_file=user_outname

# Step 6. Update after manually checking data 
RecalculateAbs=T
sheet2show=c( "standard",
              "cell_count","quant","s","p.real", "Abs_total",
              "Abs_total_mil.cell", "Abs_syn","Abs_syn_mil.cell",#"Abs_scav_mil.cell"  ,
              "FAratio" )
cat ("After manually checking the standard Curve, data and MAP file, Press [enter] to continue")
line=NULL
line <- readline()
if ( line=="") {
  raw.files=Summary_unchecked_file
  basename(raw.files)
}

if (check.RAW.xlsx==T){
  source(paste0(TOOLPATH,"Step6-Update Quant file.r"))
}
updatefile=updatefile

# Step 7.rearrange the data if needed 
# Get Dylan to help

# Step 8.Plot and calculate means and std
########### USER HANDLE ##########
plot=T
write2xlsx=T
width=1600
height=700
sheet2plot=c( "cell_count","quant","s","p.real", "Abs_total",
              "Abs_total_mil.cell","Abs_syn", "Abs_syn_mil.cell","FAratio" )
sheet2ignore=c("contents","standard", "Summary",
               "140", "160","161", "180", "181a", "181b","182", "200", "203", "204", "220", "224", "240", "241")
name2plot=NULL # this can be the column number of the names you want to show
delete_col=NULL

map=readMAPfile(mapfile,format = "Kevin")
exclude_samples=map$SampleID[which(map$Exclude>0)]
Rda.files=updatefile
filefolder=myfolder
source(paste0(TOOLPATH,"Step8-stats and graph.r"))

if (I_Want_to_Look_At_Distribution){
  outname="Distribution.xlsx"
  mapfile=mapfile
  print("please select the data files you want to merge with MAP")
  datafiles=choose.files()
  
  for ( i in 1:length(datafiles)){
    map_and_data_file(outname = outname,mapfile=mapfile,datafile = datafiles[i],write = T)
  }
  
}

#map_and_data_file(outname = "temp.xlsx",mapfile=file.choose(),datafile = file.choose(),write = T,Normalise = F)




# -------------------Update----------------------- #
# updated: 2017.05.06 to help Wayne run chol only
# updated: 2017.05.01 to end poor Wayne's user complaint
# updated: 2016.
# updated: 20180505 fixed samplename bug
#
# update 20220606, Gino
# get OS name with:
# Platform = Sys.info()[["sysname"]] 
# avoid using "C:/" to identify windows
