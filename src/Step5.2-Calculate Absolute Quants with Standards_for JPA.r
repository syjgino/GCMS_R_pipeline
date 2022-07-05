# update 16.7.4 only fits Model3 output files 
#update 16.6.13 samplenames, special case: only one column of sampleName
# update 16.4.13, deleted all standard in the sample sheets
# update 170630, map file names and raw file names

  # fa acid only , no s or p values
library(xlsx)
################################################
if (T){
  user_outname=NULL # if NULL, the final summary file name will be the same as MAP file name
  PERCENT_LABEL=1 # this will be replaced by MAP label info
  FACon="190"
  check.RAW.xlsx=T
  print("please select your [xxxx_quant_s.Rda] file")
  fames_chol_file=file.choose()
  print("please select your Map file. the map file starts with [ Map_xxxx.xlsx ]")
  mapfile=file.choose()
}


setwd(dirname(mapfile))
quants.Rda=fames_chol_file
if ( is.null(fames_chol_file)){
  print("Please select _fames+chol.Rda file")
  quants.Rda=file.choose()
}
print(basename(mapfile))
print(basename(quants.Rda))

if (is.null(user_outname)){
  temp=gsub("MAP_","",basename(mapfile))
  temp=gsub("Map_","",basename(temp))
  temp=gsub("map.xls",".xls",basename(temp))
  user_outname=paste0("Summary_",temp)
    print(user_outname)
}
################################################
# input: MAP plate information
map_raw=read.xlsx(file=mapfile,sheetName="map",check.names=F,startRow=2)
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

# input: quant/s data
quants=readRDS(quants.Rda)
name_compare= (rownames(quants[[1]]) %in% as.matrix(map_raw$SampleID) )

if (F){  # older Joe samples comes with Fs
  if (sum(name_compare==F) >1) { # this means you are using MS plate ID rather than sample ID
    if(substr(rownames(quants[[1]])[1],1,1) %in% c("F","C")) {quants=ChangeRownames(quants)}
    for ( name in names(quants)){ 
      currentnames=rownames(quants[[name]])
      rownames(quants[[name]])=as.matrix(map_raw[currentnames,"SampleID"])
    }
  }
}

for ( name in names(quants)){ 
  quants[[name]]=as.matrix(quants[[name]])
  quants[[name]]=quants[[name]][rownames(map_raw),]
}
########################

#check sample IDs
quants_ID=rownames(quants[[1]])
missing_sample_ID=setdiff(as.matrix(map_raw$SampleID),quants_ID); 
if(length(missing_sample_ID)>0) { print("missing these samples in MS data!"); print(missing_sample_ID)}

if ( "Pcalc" %in% names(quants)){
  quants[["Pcalc.real"]]=quants$Pcalc/PERCENT_LABEL
  print("Pcalc --> Pcalc.real")
  quants$Pcalc=NULL
}


##### Combine cell numbers to the quant file
cellcount=c()
cellcount=as.matrix(map_raw[,"CellCount"])
  cellcount=apply(cellcount,1:2,as.numeric,na.rm=T); rownames(cellcount)=map_raw$SampleID ; colnames(cellcount)="cell_count"
## TASK # never used 
if ( length(which(!is.na(cellcount)))==0){  
  print("Please select cellcount.txt file")
  cell.files=choose.files()
  basename(cell.files)
  cellcounts.raw=GetCellCount(cell.files)
  
  for ( ir in 1:nrow(map_raw)){
    platename=as.character(map_raw$PlateName[ir])
    well.col= map_raw$Col.cell[ir]
    well.row=map_raw$Row.cell[ir]
    if( is.na(well.col) | is.na(well.row)){
      cellcount[ir]=NA
      next
    }
    if(as.numeric(well.col)<10){ 
      well = paste0(well.row,"0",as.character(well.col))
    } else {
      well = paste0(well.row,as.character(well.col))
    }
    if ( ! well %in% rownames(cellcounts.raw[[platename]])){
      cellcount[ir]=NA
    } else {
    cellcount[ir]=cellcounts.raw[[platename]][well,]
    }
  }  
}
print("Cellcounts ready")
quants[["cell_count"]]=cellcount
abs_quants=quants

########### Prepare standard information and output ###############
if( !("Standard" %in% colnames(map_raw))){ map_raw=cbind(map_raw,NA); colnames(map_raw)[ncol(map_raw)]="Standard" }
temp=map_raw[which(map_raw$Standard>=0),]  
standard_name=temp[order(temp$Standard),"SampleID"] ; standard_name=as.matrix(standard_name)
standard_name_code=temp[,"Standard"]; names(standard_name_code)=standard_name
# get the names to delete, but delete later!
if (length(standard_name) ==0){
  print("no standard in this plate")
  StandardInPlate=F
} else {
  StandardInPlate=T
  standard_abs.raw=read.xlsx(file=mapfile,sheetIndex="SC MM",check.names=F,startRow=99,endRow=111)
  Num_Standard=max( grep("S",standard_abs.raw[,4]) )
  standard_abs=standard_abs.raw[1:Num_Standard,c(5:24,26:27)]; 
    rownames(standard_abs)=standard_abs.raw[1:Num_Standard,4]
  colnames(standard_abs)=tolower ( gsub(":","",colnames(standard_abs))  );
  colnames(standard_abs)=tolower ( gsub("161d9","161",colnames(standard_abs))  );
  colnames(standard_abs)=tolower ( gsub("181d9","181a",colnames(standard_abs))  );
  colnames(standard_abs)=tolower ( gsub("181d11","181b",colnames(standard_abs))  );
  if(sum(standard_abs[,"181b"])==0){standard_abs[,"181b"]=standard_abs[,"181a"]; print("no 181b in standard curve, using 181a instead")}
  standard_abs=standard_abs[,order(colnames(standard_abs))]
  standard_abs
}
 ## ISOLATE standard_quant_norm
# if stand is on plate, extract, if not, get it from other file!
if ( StandardInPlate ){
  standard_quant=abs_quants$quant[standard_name,]
  standard_quant_norm= abs_quants$quant_norm[standard_name,]

  Neg=NULL
  ## Part 1
  temp=standard_quant[which(standard_name_code==0),]
  if(length(temp)) {Neg=apply(temp,2,mean,na.rm=T)}
  part1=rbind(Neg,standard_quant[which(standard_name_code>0),])
  rownames(part1)=paste0(rownames(part1),"_quant")
  ## Part 2
  temp=standard_quant_norm[which(standard_name_code==0),]
  if(length(temp)) {Neg=apply(temp,2,mean,na.rm=T)}
  part2=rbind(Neg,standard_quant_norm[which(standard_name_code>0),])
  rownames(part2)=paste0(rownames(part2),"_normalized")
  ## Part 3
  if ( !is.null(Neg)) { Neg=0 }
  part3=rbind(Neg, as.matrix(standard_abs))
  part3=part3[1:nrow(part1),]
  rownames(part3)=gsub("_quant","_nmol",rownames(part1))
  part3.match=part3[, match(colnames(part1), colnames(part3))]
  std.write=rbind(part1,part2,part3.match)
} 
write.xlsx(std.write,file = user_outname,sheetName = "standard",append=T)

################## Get MAP Sample ready and calculate the abs total
#delete unnecessary rows
delete_name=map_raw[which(map_raw$Standard>=0 | map_raw$Standard<0 ),"SampleID"];  # so in the future we can do delete=-1
  delete_name=as.matrix(delete_name)
map.delete=map_raw
if (length(delete_name)>0){
  
  for (sheet in names(abs_quants)){
    if ( sheet == "standard" || is.na(sheet)){next}  
    delete=which(rownames(abs_quants[[sheet]]) %in% delete_name)
    colnames=colnames(abs_quants[[sheet]])
    abs_quants[[sheet]]=as.matrix(abs_quants[[sheet]][-delete,]); colnames(abs_quants[[sheet]])=colnames
  }
  map.delete=map_raw[-which(map_raw$SampleID %in% delete_name),]
}

abs_quants[["Abs_total"]]=Quant.abs.plate(standard_abs = part3.match[,-which(is.na(colnames(part3.match) ) )],
                                          sample.quant_norm = abs_quants$quant_norm,
                                          saveFig = T,title_comment = "not checked",
                                          standard.quant_norm = part2)
if (check.RAW.xlsx==F){# if you choose not to check, then I calculate everything for you
  abs_quants=AbsQuants.Update(abs_quants)
  warning("data not checked!")
}

#############################################################
rownames(map.delete)=as.matrix(map.delete$SampleID)

PlateName=map.delete$PlateName

samplename.col=which(grepl("SampleName",colnames(map.delete))  )  
samplenames=map.delete[,samplename.col]; rownames(samplenames)=map.delete$SampleID
# bind names
if ( is.null(PlateName) ){  
  #map.delete$PlateName=NULL
  #abs_quants_name=AddSampleNames(samplenames = samplenames,quants = abs_quants)
  names2bind=as.matrix(samplenames);
    rownames(names2bind)=rownames(map.delete);colnames(names2bind)=colnames(map.delete)[samplename.col]
  names2bind=as.data.frame(names2bind)
}  else {  
  #abs_quants_name=AddSampleNames(samplenames = cbind(PlateName,samplenames),quants = abs_quants)
  #names2bind=cbind(PlateName,samplenames)
  names2bind=cbind.data.frame(PlateName,samplenames) # 
} 

list.na=which(is.na(names(abs_quants))); 
  if(length(list.na)){abs_quants[[list.na]]=NULL}; abs_quants$quant_syn=NULL

sheets=names(abs_quants); sheets_head=c("cell_count","quant","quant_norm","Abs_total","s" ,"Pcalc.real");
  sheets_change=c(sheets_head,setdiff(sheets,sheets_head));sheets_change
abs_quants=abs_quants[sheets_change]
abs_quants_name=AddSampleNames(samplenames = names2bind,quants = abs_quants)

list2xlsx(abs_quants_name,user_outname)
print(paste0("Summary file is in file [ ",user_outname," ]"))
print( paste0("please check \" Standard Curve.png\" and delete bad data in \"",user_outname,"\"") )
print("End of Step 5.2. Please run Step 6.2")
