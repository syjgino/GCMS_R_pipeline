#### USER INPUT:
if (T ){
  Save2Rda=T
  WRITE=T
  p_Threshold=0.002
  s_Threshold=0.002
  p_Threshold=0
  s_Threshold=0
  RecalculateAbs=T
  FACon="190"
  
  if (F){
    sheet2show=c( "standard",
                "cell_count","quant","quant_norm","s","p.real", "Abs_total",
                "Abs_total_mil.cell", "Abs_syn_mil.cell",#"Abs_scav_mil.cell"  ,
                "FAratio" )
  }
  
  if(F){
    cat ("After manually checking the data, Press [enter] to continue")
    line=NULL
    line <- readline()
    if ( line=="") {
      print("please select your .xlsx file with (standard,quant,s, p.real) to be updated")
      raw.files=file.choose()
      basename(raw.files)
    }
  }
}
#################
print("please select your [Summaryxxxx.xlsx] file with (standard,quant,s, p.real) to be updated")
raw.files=file.choose()
basename(raw.files)

######

for ( i.file in 1:length(raw.files)){
  #  i.file =1
  raw.file=raw.files[i.file]
  
  file.list=xlsx2list(file = raw.file)
  Show.list.structure(file.list)
  
  
  ### for the rest of sheets, store data in abs_quants, store names in sample_names
  
  samplenames=parseName_data(Name_data = file.list$quant)$samplenames
  abs_quants=list()
  for (name in names(file.list)){
    if (grepl("standard", name)) {next}
    abs_quants[[name]]=parseName_data(Name_data = file.list[[name]])$data
    colnames(abs_quants[[name]])=gsub("\\X","",colnames(abs_quants[[name]]))
  }
  
  if (RecalculateAbs){
    ### read stand info ####
    standard=file.list[[  which(grepl( "standard",names(file.list)) )]]
    colnames(standard)=gsub("\\X","",colnames(standard))
    standard_quant=standard[ which(grepl("quant",rownames(standard)))  ,]
    standard_nmol=standard[ which(grepl("nmol",rownames(standard)))  ,]
    standard_quant_norm=Quant_norm(standard_quant,FACon=FACon,leaveIC = T)
    rownames(standard_quant_norm)=gsub("_quant","_normalized",rownames(standard_quant_norm))
    standard.new=rbind(standard_quant,standard_quant_norm,standard_nmol)  
    
    abs_quants$quant_norm=Quant_norm(abs_quants$quant,FACon = FACon,leaveIC=T) # norm is updated!
      quant_norm_for_calc=Quant_norm(abs_quants$quant,FACon = FACon,leaveIC=F)
    standard_quant_norm=Quant_norm(standard_quant,FACon=FACon,leaveIC = F)
    # udate Abs_total
    abs_quants[["Abs_total"]]=Quant.abs.plate(standard_abs = standard_nmol,
                                                  sample.quant_norm = quant_norm_for_calc,
                                                  saveFig = T,title_comment = " checked",
                                              standard.quant_norm = standard_quant_norm)
    
  }
  
  bad.s=which(abs_quants$s<s_Threshold,arr.ind = T)
  abs_quants$s[bad.s]=NA
  bad.p=which(abs_quants$p.real<p_Threshold,arr.ind = T)
  abs_quants$p.real[bad.p]=NA
  abs_quants$p.real[bad.s]=NA # when s<threshold, it means the p.real is not accurate, so delete p as well

  # recalculate
  if(F){ # for debugging
    BACKUP=abs_quants
    abs_quants=BACKUP
  }
  abs_quants=AbsQuants.Update_JPA(abs_quants = abs_quants)  ## lipid sheets are updated from the previous sheets
  for (sheetname in names(abs_quants)){ # wash the data
    colnames(abs_quants[[sheetname]])=gsub("X","",colnames(abs_quants[[sheetname]]))
    temp=as.matrix(abs_quants[[sheetname]])
    temp[which(is.infinite(temp))]=NA
    abs_quants[[sheetname]]=temp
  }
  
  # add some empty columns for Joe
  Enzymesheets_name=names(abs_quants)[which(grepl (" tot" ,names(abs_quants)) )]
  template_lipids=c(  "140", "160","161", "180", "181a", "181b","182", 
                      "200", "203", "204", "220", "224", "240", "241")
  
  if (F) { # Joe wants all sheets to have all lipid names
    
    sheets2insert=c(Enzymesheets_name,"Pcalc.real", "s","1-e0")
    for (sheetname in sheets2insert){
      emptycol_name=setdiff(template_lipids,colnames(abs_quants[[sheetname]]))
      emptycol=matrix(NA,nrow(abs_quants[[sheetname]]),length(emptycol_name));
        colnames(emptycol)=emptycol_name
      temp=abs_quants[[sheetname]]
      temp=cbind(emptycol,temp)
      
      notlipids_col=setdiff(colnames(temp),template_lipids)
      temp=temp[,c(template_lipids,notlipids)]
      
      abs_quants[[sheetname]]=temp
    }
    
    for (sheetname in setdiff(names(abs_quants),template_lipids) ){
      print(" ")
      print(sheetname)
      print(colnames(abs_quants[[sheetname]]))
    } 
    
    if(!("201" %in% colnames(abs_quants$Abs_total)) ){ # Joes wants to add x201 and x221 to all possible sheets...
      # insert empty columns
      # 1. for sheets like Abs_total
      sheets2insert=c(Enzymesheets_name,"Abs_total","Abs_total.cell","mol%","Pcalc.real","s","1-e0")
      for (sheetname in sheets2insert){
        emptycol=matrix(NA,nrow(abs_quants[[sheetname]]),1); colnames(emptycol)="201"
        temp=abs_quants[[sheetname]]
        temp=InsertColumn(emptycol,temp,method = "before",col_2_insert = "203")
        emptycol=matrix(NA,nrow(abs_quants[[sheetname]]),1); colnames(emptycol)="221"
        temp=InsertColumn(emptycol,temp,method = "before",col_2_insert = "224")
        abs_quants[[sheetname]]=temp
      }
      
      sheets2insert=c("mol% MUFA")
      for (sheetname in sheets2insert){
        emptycol=matrix(NA,nrow(abs_quants[[sheetname]]),2); colnames(emptycol)=c("201","221")
        temp=abs_quants[[sheetname]]
        temp=InsertColumn(emptycol,temp,method = "before",col_2_insert = "241")
        abs_quants[[sheetname]]=temp
      }
      
      temp=abs_quants[["P0_1_2"]]
      for ( i in c(0,1,2)){
        emptycol=matrix(NA,nrow(abs_quants[[sheetname]]),1); 
        
        colnames(emptycol)=paste0("p",as.character(i),"_201")
        temp=InsertColumn(emptycol,temp,method = "before",col_2_insert = paste0("p",as.character(i),"_203"))
        
        colnames(emptycol)=paste0("p",as.character(i),"_221")
        temp=InsertColumn(emptycol,temp,method = "before",col_2_insert = paste0("p",as.character(i),"_224"))
      }
      abs_quants[["P0_1_2"]]=temp
    }
    
    for (sheetname in Enzymesheets_name){
      temp=abs_quants[[sheetname]]
      last=ncol(temp)
      temp=temp[,c(last,1:(last-1))]
      abs_quants[[sheetname]]=temp
    }
  }
  
  ###### Export to xlsx!
  abs_quants_name=AddSampleNames(samplenames = samplenames,quants = abs_quants)
  if (RecalculateAbs) {abs_quants_name[["standard"]]=standard.new}
  names(abs_quants)
  if (Save2Rda){
    saveRDS(abs_quants_name, file = gsub(".xlsx",".Update.Master.Rda",raw.file))
  }
  if(WRITE){
    updatefile = gsub(".xlsx",".Update.Master.xlsx",raw.file)
    list2xlsx(list = abs_quants_name,outputname =updatefile)
  }
  if(F){
    Enzymesheets_number=which(grepl (" tot" ,names(abs_quants))  )
    others_number=setdiff(c(1:length(abs_quants)),Enzymesheets_number)
  
    updatefile = gsub(".xlsx",".Update.xlsx",raw.file)
    updatefile_enzyme=gsub(".xlsx",".Update.Enzyme.xlsx",raw.file)
  #  sheet2show=intersect(sheet2show,names(abs_quants_name))
    list2xlsx(list = abs_quants_name[others_number],outputname =updatefile )
    list2xlsx(list = abs_quants_name[Enzymesheets_number],outputname =updatefile_enzyme )
  }
  
  write.xlsx(x = as.matrix(names(abs_quants)),sheetName = "contents",file=updatefile,append=T)
  print(paste0("file is updated in [",basename(updatefile),"]"))
}
print("End of Step 6.2. Please run Step 8.2")
