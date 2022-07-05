library(xlsx)
################################################
if (F){
  user_outname=".xlsx"
  PERCENT_LABEL=0.5 ##this will be overwritten if there's data in the "%Label" Column in MAP file.
  FACon="190"
  check.RAW.xlsx=T
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
map_raw=readMAPfile(mapfile,format = "Kevin")

# input: quant/s data
quants=readRDS(quants.Rda)
quants=ChangeRownames(quants)
for ( name in names(quants)){ quants[[name]]=as.matrix(quants[[name]])}

#check sample IDs
map_ID=rownames(map_raw)
quants_ID=rownames(quants[[1]])
  missing_sample_ID=setdiff(map_ID,quants_ID); 
  if(length(missing_sample_ID)>0) {     
    to_print=paste(missing_sample_ID, ": missing from the MS data!\n")
    #print("missing these samples in MS data!"); print(missing_sample_ID)
    warning(to_print)
  }
  
# p input
if ( "p" %in% names(quants)){
  if ( ! ("%Label" %in% colnames(map_raw)  )){
    quants[["p.real"]]=quants$p/PERCENT_LABEL
  }
  
  ### 16.8.23
  if ("%Label" %in% colnames(map_raw)){
    if ( "Standard" %in% colnames(map_raw) ){
      Percent_Label=map_raw[rownames(quants$p),c("%Label","Standard")]; 
      Percent_Label[which(Percent_Label$Standard>=0),"%Label"]=NA; 
      # in case people do 0,5 instead of 0.5  ...fpr Wayne....
      tmp=as.matrix(Percent_Label)
        tmp[,1]=gsub(",",".",tmp[,1])
        tmp=apply(tmp,1:2,as.numeric)
        Percent_Label=tmp
      ## end of the error-friendly codes
      label.temp=as.numeric(Percent_Label[,1])
      temp=quants$p/label.temp; 
        temp[which(temp==Inf)]=NA; 
        quants[["p.real"]]=temp  ###
    } else {
      Percent_Label=map_raw[rownames(quants$p),c("%Label")]; 
      temp=quants$p/Percent_Label; temp[which(temp==Inf)]=NA; quants[["p.real"]]=temp
    }
    
    Percent_Label=as.matrix(Percent_Label)[,1]
        
    quants$s[which(Percent_Label==0),]=NA
  }
    
  # filter bad p where p>1
  p.real= quants[["p.real"]]; quants[["p.real"]][which(p.real>1)]=NA;
  quants$s[which(p.real>1)]=NA;
  print("p --> p.real")
  quants$p=NULL
}
##### Combine cell numbers to the quant file
cellcount=c()
cellcount=as.matrix(map_raw[,"CellCount"])
  cellcount=apply(cellcount,1:2,as.numeric,na.rm=T)
rownames(cellcount)=rownames(map_raw)
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
quants.cell=AddCellAxes(quants,cellcount = cellcount)
  sheet=intersect(c("cell_count","quant","quant_norm","s","p.real"),names(quants.cell))
abs_quants=quants.cell[sheet] # abs_quants is only selected sheets
#saveRDS(abs_quants,gsub(".xlsx",".noname.Rda",user_outname))

########### Prepare standard information and output ###############
if( !("Standard" %in% colnames(map_raw))){ map_raw=cbind(map_raw,NA); colnames(map_raw)[ncol(map_raw)]="Standard" }
temp=map_raw[which(map_raw$Standard>0),]  
standard_name=rownames(temp)[order(temp$Standard)] 
  names(standard_name)=as.character(temp$Standard)
# get the names to delete, but delete later!
if (length(standard_name) ==0){
  print("no standard in this plate")
  StandardInPlate=F
} else {
  StandardInPlate=T
  
  standard_abs.raw=read.xlsx(file=mapfile,sheetIndex="standard",check.names=F,startRow=2,endRow=10)
  standard_abs=t(standard_abs.raw[,17:22])
    rownames(standard_abs)=as.character(1:nrow(standard_abs))
    colnames(standard_abs)=as.character(standard_abs.raw[,16]); 
  standard_abs = standard_abs[names(standard_name),]
  standard_abs
}
 ## ISOLATE standard_quant_norm
# if stand is on plate, extract, if not, get it from other file!
if ( StandardInPlate ){
  standard_quant=abs_quants$quant[standard_name,]
  standard_quant_norm= abs_quants$quant_norm[standard_name,]
  
  part1=standard_quant
  rownames(part1)=paste0(rownames(part1),"_quant")
  part2=standard_quant_norm
  rownames(part2)=paste0(rownames(part2),"_normalized")
  part3=standard_abs
  rownames(part3)=gsub("_quant","_nmol",rownames(part1)) ## bug here
  part3.match=part3[, match(colnames(part1),colnames(part3))]
  std.write=rbind(part1,part2,part3.match)
} else { # if there is no standard curve on this plate
  print("please choose a  [summary.....xlsx] file with standard tab")
  std.file=file.choose()  
  standard=read.xlsx(std.file,sheetName="standard",header=T,row.names=T)
    colnames(standard)=gsub("\\X","",colnames(standard))
  standard_quant=standard[ which(grepl("quant",rownames(standard)))  ,]
  standard_nmol=standard[ which(grepl("nmol",rownames(standard)))  ,]
  standard_quant_norm=Quant_norm(standard_quant,FACon=FACon,leaveIC = T)
  rownames(standard_quant_norm)=gsub("_quant","_normalized",rownames(standard_quant_norm))
  standard.new=rbind(standard_quant,standard_quant_norm,standard_nmol)  
  std.write=standard.new
  standard_abs=standard_nmol
}
write.xlsx(std.write,file = user_outname,sheetName = "standard",append=T)


################## Get Sample ready and calculate the abs total
#delete unnecessary rows
delete_name=rownames(map_raw)[which(map_raw$Standard>=0 | map_raw$Standard<0 )] # so in the future we can do delete=-1
map.delete=map_raw
if (length(delete_name)>0){
  
  for (sheet in names(abs_quants)){
    if ( sheet == "standard" || is.na(sheet)){next}  
    delete=which(rownames(abs_quants[[sheet]]) %in% delete_name)
    colnames=colnames(abs_quants[[sheet]])
    abs_quants[[sheet]]=as.matrix(abs_quants[[sheet]][-delete,]); colnames(abs_quants[[sheet]])=colnames
  }
  map.delete=map_raw[-which(rownames(map_raw) %in% delete_name),]
}

abs_quants[["Abs_total"]]=Quant.abs.plate(standard_abs = standard_abs,
                                          sample.quant_norm = abs_quants$quant_norm,
                                          saveFig = T,title_comment = "not checked",
                                          standard.quant_norm = standard_quant_norm)
if (check.RAW.xlsx==F){# if you choose not to check, then I calculate everything for you
  abs_quants=AbsQuants.Update(abs_quants)
  warning("data not checked!")
}

#############################################################
PlateName=map.delete$PlateName

samplename.col=which(grepl("SampleName",colnames(map.delete))  )  
samplenames=map.delete[,samplename.col]
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
  if(length(list.na)){abs_quants[[list.na]]=NULL}
abs_quants_name=AddSampleNames(samplenames = names2bind,quants = abs_quants)

list2xlsx(abs_quants_name,user_outname)
Summary_unchecked_file=user_outname
print(paste0("Summary file is in file [ ",user_outname," ]"))
print( paste0("please check \" Standard Curve.png\" and delete bad data in \"",user_outname,"\"") )







# updated 180802 Dylan, the "0,5" issue
# update 18.5.20 minor change
# update 17.8.29 debugging for missing a certain standard in the sample
# update 16.8.23 Add a new column in MAP: %Label. for different labeling across samples(e.g. 0 for T=0)
# update 16.6.13 samplenames, special case: only one column of sampleName
# update 16.4.13, deleted all standard in the sample sheets; check sample ID between quants file and MAP
# fa acid only , no s or p values