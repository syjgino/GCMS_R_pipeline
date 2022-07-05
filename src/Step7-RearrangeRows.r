###  User handle ###
####################
if (F){
  # set names for output files
  rearranged_files_names=c("160412 STAT KO.xlsx",
                           "160412 CH25H KO.xlsx")
  rearranged_file_rows=list(file1.rows=list(c(1:8,17:56),9:48),
                            file2.rows=list(c(9:16,57:88),c(1:8,49:88)))


  rearranged_files_names=c("Summary_180414efflux_Mean and Sd.xlsx")
  rearranged_file_rows="all"
  rearranged_file_rows.sort=F  
}

print("please select all plates file to be rearranged")
files=choose.files()
basename(files)

plate_files=list()
for ( i.file in 1:length(files)){
  file=files[i.file]
  if (grepl(".Rda",file)){
    plate_files[[i.file]]=readRDS(file)
  } 
  if (grepl(".xls", file)){
    plate_files[[i.file]]=xlsx2list(file = file)
  }
  plate_files[[i.file]]$standard=NULL
}

if (rearranged_file_rows=="all"){
  outfile=ListExtract(lists = plate_files )
  if (rearranged_file_rows.sort==T){
    for (sheet in names(outfile)){
      name_data=outfile[[sheet]]
      name.col=which(grepl("SampleName",colnames(name_data)))
      #df[order(df[,1],df[,2],decreasing=TRUE),]
      #mat[do.call(order, as.data.frame(mat)),]  
      temp=name_data[do.call(order,as.data.frame(name_data[,name.col])), ]
      outfile[[sheet]]=temp
    }    
  }  
  list2xlsx(list = outfile,outputname = rearranged_files_names[1] )
} else {  
  for ( i.file in 1:length(rearranged_files_names)){
    outfile= ListExtract(lists = plate_files,rows = rearranged_file_rows[[i.file]])
    list2xlsx(list = outfile,outputname = rearranged_files_names[i.file] )
    #saveRDS(outfile,file = gsub(".xlsx",".Rda", rearranged_files_names[i.file])
  }  
}

print("Rows are rearranged to files:")
print(rearranged_files_names)
