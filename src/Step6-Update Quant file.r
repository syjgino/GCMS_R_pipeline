#### USER INPUT:
if (F ){
  p_Threshold=0.002
  s_Threshold=0.002
  RecalculateAbs=T
  FACon="190"
  sheet2show=c( "standard",
                "cell_count","quant","quant_norm","s","p.real", "Abs_total",
                "Abs_total_mil.cell", "Abs_syn_mil.cell",#"Abs_scav_mil.cell"  ,
                "FAratio" )

  cat ("After manually checking the data, Press [enter] to continue")
  line=NULL
  line <- readline()
  if ( line=="") {
    print("please select your Summary_xxxx.xlsx file with (standard,quant,s, p.real) to be updated")
    raw.files=choose.files()
    basename(raw.files)
  }
  
}
#################

######

for ( i.file in 1:length(raw.files)){
  #  i.file=1
  raw.file=raw.files[i.file]
  
  file.list=xlsx2list(file = raw.file)
  names(file.list)
  
  
  ### for the rest of sheets, store data in abs_quants, store names in sample_names
  
  samplenames=parseName_data(Name_data = file.list$quant)$samplenames;
 
    
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
    
    abs_quants$quant_norm=Quant_norm(abs_quants$quant,FACon = FACon)
    standard_quant_norm=Quant_norm(standard_quant,FACon=FACon,leaveIC = F)

    abs_quants[["Abs_total"]]=Quant.abs.plate(standard_abs = standard_nmol,
                                                  sample.quant_norm = abs_quants$quant_norm,
                                                  saveFig = T,title_comment = " checked",
                                              standard.quant_norm = standard_quant_norm)
  }
  
  bad.s=which(abs_quants$s<s_Threshold,arr.ind = T)
  abs_quants$s[bad.s]=NA
  bad.p=which(abs_quants$p.real<p_Threshold,arr.ind = T)
  abs_quants$p.real[bad.p]=NA
  abs_quants$p.real[bad.s]=NA # when s<threshold, it means the p.real is not accurate, so delete p as well
  
  if(F){
    BACKUP=abs_quants
    abs_quants=BACKUP
  }
  
  abs_quants=AbsQuants.Update(abs_quants = abs_quants)
  abs_quants_name=AddSampleNames(samplenames = samplenames,quants = abs_quants)
  if (RecalculateAbs) {abs_quants_name[["standard"]]=standard.new}
  updatefile = gsub(".xlsx",".Update.xlsx",raw.file)
  sheet2show=intersect(sheet2show,names(abs_quants_name)) # smart!
  list2xlsx(list = abs_quants_name[sheet2show],outputname =updatefile )
  #saveRDS(abs_quants_name, gsub(".xlsx",".Update .Rda",raw.file) )  
  print(paste0("file is updated in [",basename(updatefile),"]"))
}
