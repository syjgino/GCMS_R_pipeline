# 20170427 now sheet2plot and sheet2 mean and average can be separate
# average and std
####### USER HANDLES #####
if (F) {
  library(xlsx)
  # Step 8.0: choose file
  Rda.files=NULL
  
  # step 8.1 : The script will calculate mean and std for all sheets except for sheet2ignore,
  #             and plot only sheet2plot
  if (F) {
    sheet2plot=c( "cell_count","quant","s","p.real", "Abs_total",
                  "Abs_total_mil.cell","Abs_syn", "Abs_syn_mil.cell","FAratio" )
    sheet2plot="as_it_is"    # if "as_it_is", then I'll plot all the parameters
    name2plot=2:4 # this is for plotting only  # by default, will show SampleName_x only
  }
  sheet2plot=c("cell_count", "quant_norm",
             "Abs_total","Abs_total.cell",
             "mol%","mol% SAFA", "mol% MUFA" , "mol% PUFA","mol% SAFA+MUFA",
             "FAratio", "Pcalc.real","s","1-e0",
             "EnzymeActivity", "EnzymeActivity.cell") # for Joe 20170425
  sheet2ignore=c("contents","standard", "Summary",
                "140", "160","161", "180", "181a", "181b","182", "200", "203", "204", "220", "224", "240", "241")
  # step 8.2: names to show 
  delete_col=NULL # this is for stat calc and xlsx output 
  name2plot=NULL
  # step 8.3: samples to exclude
  # exclude_samples=rownames(map_raw)[which(map_raw$Standard==-1)]  
  exclude_samples=c()
  # step 8.4: plotting settings
  plot=F
  width=1600
  height=700  
  # step 8.5: write the stat to .xlsx
  write2xlsx=T
}

print("now let's plot it!")
if ( is.null(Rda.files)){  
  print("please select your .xlsx file with to plot")
  Rda.files=choose.files()
  filefolder=dirname(Rda.files[1])
  setwd(filefolder)
  basename(Rda.files)
}

#######################

for ( i.file in 1:length(Rda.files) ) {
  #     i.file =1
  
  ######## calculate the means and std for one file
 
  ##### Read Raw data from either Rda or .xlsx file
  file=basename(Rda.files[i.file])
  Fig.foldername=paste0("Figures for ",file)
  if (grepl("Rda",file)){
    quants.formal=readRDS(file)
  } 
  if (grepl(".xls", file)){
      quants.formal=xlsx2list(file = file)
  }
  
  sheet2plot=intersect(sheet2plot,names(quants.formal))
  ################ INPUT: quants.formal
  ################ OUTPUT: quants.formal.stat
  quants.formal.stat=list()
  if (sheet2plot[1]=="as_it_is"){
    sheet2plot=names(quants.formal)
  }
  dir.create(Fig.foldername,showWarnings=F)
  setwd(Fig.foldername)
  for ( pa in names(quants.formal)){
    if( pa %in% sheet2ignore) {next}
        
    # delete extra name columns and bad samples
    if (is.null(delete_col)){
      name_data=quants.formal[[pa]]
    } else {
     name_data=quants.formal[[pa]][,-delete_col]
    }
    if(!is.null(exclude_samples)) { 
      name_data=name_data[!(rownames(name_data) %in% exclude_samples),] 
    }
    
    namekey="Name"
    name_data.mean=Aggregate.by.name(name_data = name_data,FUN = "mean",namekey = namekey)
    name_data.sd=Aggregate.by.name(name_data = name_data,FUN = "sd",namekey = namekey)
    
    mean=parseName_data(Name_data = name_data.mean,namekey)$data
    sd=parseName_data(Name_data = name_data.sd,namekey)$data
    data.mean_sd=Merge.mean.sd(data.list = list(mean,sd),annotation = c("mean","sd"))
    samplenames=parseName_data(Name_data = name_data.sd,namekey)$samplenames    
    data.stat=cbind.data.frame(samplenames,as.matrix(data.mean_sd))
    
    #write.xlsx(data.stat,"test.xlsx")  
    quants.formal.stat[[pa]]=NULL
    quants.formal.stat[[pa]]=data.stat
    
    # now we have
    # mean,sd, samplenames ready
    # we can generate the graph :)
    if (is.null(name2plot)){
      c=colnames(samplenames)
      name2plot=which(grepl("SampleName",c,ignore.case = T))
    }
    s=as.matrix(samplenames[,name2plot])
    plotnames=s[,1]
    if (ncol(s)>=2){
      for ( icol.s in 2:ncol(s)){
        plotnames=paste0(plotnames,"_",s[,icol.s])
      }
    }

    #if(plot & ( !pa %in% c("quant","quant_norm")) ){
    if(plot & ( pa %in% sheet2plot)) {
      for (i.plot in 1:ncol(mean)){
        xx=cbind(mean[,i.plot],sd[,i.plot])
        if (all(is.na(xx[,1]))) { next}
        rownames(xx)=plotnames      
        title=paste0(pa," for ",colnames(mean)[i.plot])
        title=gsub(".sd","",title)
        title=gsub(".mean","",title)
        title=gsub("\\X","",title)
        title=gsub("%"," percent",title)
        png(filename = paste0(title,".png"),width = width,height = height)
        PlotBar(xx,title = title)
        dev.off()
      }
    }
  }
  
  setwd(filefolder)
  if(write2xlsx) { 
    outname=gsub(".Rda","",file)
    outname=gsub(".xlsx","",outname)
    outname=paste0(outname,"Mean and Sd.xlsx")
    list2xlsx(list = quants.formal.stat,outputname = outname) 
  }
} #end of all files

if (write2xlsx){
  print(paste0("Figures are in the folder: ",Fig.foldername))
  print(paste0("Mean and Sd are in file [",outname,"]"))
}
Rda.files=NULL
