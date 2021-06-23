
#' Create EDA file
#' 
#' The function is used to calculate the CV of the supply and the growth rates.
#' @param data: The data frame on which the basic EDA needs to be done.
#' @param group_by_element: the variables on which the stats var will be calculated. For example: for country, group_by_var="occupation_code", for state occupation granularity, it will be c(state_id, occupation_code).
#' Note the table should be in same granularity level as the group_by_elements.
#' @param eda_var: The attribute name on which eda needs to be performed
#' @param year: Attribute that has year. 
#' @export

eda_file_create=function(data, group_by_elements, eda_var, year)
{
  if(!is.numeric(data$year))
  {
    winDialog(type = c("ok"), "Year was not numeric, changing to numeric")
    data$year=as.numeric(data$year)
  }
  if(year %in% group_by_elements)
  {
    group_by_elements=group_by_elements[-grep(year,group_by_elements)]
  }
  eda_file=data %>%
    mutate_(eda_var=eda_var) %>%
    group_by_at(vars(one_of(group_by_elements))) %>%
    mutate(cv=sd(eda_var, na.rm=T)/mean(eda_var, na.rm=T)) %>%
    mutate(Yyear=paste0("Y", year))
  eda_file=data.table(eda_file)
  eda_file=dcast(eda_file, as.formula(paste0(paste(group_by_elements, collapse="+"),"+cv~Yyear")) , value.var = eda_var)
  
  
  ## calculating the growth rates
  for(i in (min(data$year)+1):max(data$year))
  {
    eda_file= eda_file %>%
      mutate(!!paste0("gr_",i):=round(abs(eval(parse(text = paste0("Y",i)))-
                                            eval(parse(text = paste0("Y",i-1))))/eval(parse(text = paste0("Y",i-1))),4))
  }
  return(eda_file)
}

#' Job/State/MSA contribution
#' 
#' The function is used to calculate contribution of any attribute based on its supply.
#' @param data: The data frame on which the basic EDA needs to be done.
#' @param group_by_element: the variables on which the stats var will be calculated. For example: for country, group_by_var="", for state occupation granularity, it will be c(state_id).
#' Note the table should be in same granularity level as the group_by_elements.
#' @param eda_var: The attribute name on which eda needs to be performed
#' @param year: Attribute that has year. 
#' @param contri_of: This variable tells about the attribute for which the contribution needs to calculated. 
#' @export
contri_file_create=function(data, group_by_elements=NA, eda_var, year, contri_of)
{
  if(!is.numeric(data$year))
  {
    winDialog(type = c("ok"), "Year was not numeric, changing to numeric")
    data$year=as.numeric(data$year)
  }
  if(year %in% group_by_elements)
  {
    group_by_elements=group_by_elements[-grep(year,group_by_elements)]
  }
  contri_file=data %>%
    mutate_(eda_var=eda_var) %>%
    group_by_at(vars(one_of(c(group_by_elements, year)))) %>%
    mutate(job_contri=eda_var/sum(eda_var, na.rm=T)) %>%
    mutate(Yyear=paste0("Y", year))
  contri_file=data.table(contri_file)
  contri_file=dcast(contri_file, as.formula(paste0(paste(group_by_elements, collapse="+"),"+",contri_of,"~Yyear")) , value.var = c("eda_var", "job_contri"))
  
  colnames(contri_file)=gsub("eda_var_","",colnames(contri_file))
  colnames(contri_file)=gsub("job_contri_Y","contribution_",colnames(contri_file))
  return(contri_file)
}

#' Save excel function
#' 
#' A function to save the excel workbook onto the local system
#' @param eb: workbook object
#' @param geog: The geography for which the file is saved
#' @export
save_file=function(wb, geog)
{
  path=choose.files(caption=paste0("Save the ",geog," Validation file"),  multi=F)
  if(file.exists(path))
  {
    l=file.remove(path)
  }
  saveWorkbook(wb, path)
}

#' Validation file template creation function
#' 
#' The function will help to get a template of the validation file for any supply project.
#' NOTE: This needs to be analysed with secondary research.
#' If the raw data is not given, then the analysis with theraw data will be ignored.
#' @param country_fin: Final country file with occupation_code, year and tot_emp as required column headers.
#' @param country_raw: Raw country file with occupation_code, year and raw_tot_emp as required column headers.
#' @param state_fin: Final state file with state_id, occupation_code, year and tot_emp as required column headers.
#' @param state_raw: Raw state file with msa_id, occupation_code, year and raw_tot_emp as required column headers.
#' @param msa_fin: Final msa file with msa_id, occupation_code, year and tot_emp as required column headers.
#' @param msa_raw: Raw msa file with msa_id, occupation_code, year and raw_tot_emp as required column headers.
#' @param msa_shared_in_state_map: This will be a mapping file of MSA to state mapping with the percetage of MSA falling into that state. If the file is not yearwise then please create yearwise data.
#' NOTE: If this parameter is missing then the reconcilation test of MSA to state will be ignored.
#' @param state_prop_in_msa: This file will give the proportion of state population which falls in its MSAs. Yearly data is preferred.
#' @export
#' 
validation=function(country_fin=NA, country_raw=NA, 
                    state_fin=NA, state_raw=NA,
                    msa_fin=NA, msa_raw=NA,
                    msa_shared_in_state_map=NA,
                    state_prop_in_msa=NA)
{
  pct = createStyle(numFmt="0.00%")
  #################Country validation file creation
  if(is.data.frame(country_fin))
  {
    print("Country Validation started")
    if("occupation_code" %in% colnames(country_fin) &
       "year" %in% colnames(country_fin) &
       "tot_emp" %in% colnames(country_fin))
    {
      country_fin=country_fin%>%
        select(occupation_code, year, tot_emp)
      
      
      fin_eda=eda_file_create(data=country_fin, group_by_elements = c("occupation_code"), eda_var = "tot_emp", year="year")
      wb = createWorkbook()
      
      if(is.data.frame(country_raw))
      {
        if("occupation_code" %in% colnames(country_raw) &
           "year" %in% colnames(country_raw) &
           "raw_tot_emp" %in% colnames(country_raw))
        {
          country_raw=country_raw%>%
            select(occupation_code, year, raw_tot_emp)
          country_raw_diff=left_join(country_fin, country_raw,
                                     by=c("occupation_code", "year")) %>%
            mutate(diff=abs(raw_tot_emp-tot_emp),
                   diff_perc=diff/raw_tot_emp)
          
          
          raw_eda=eda_file_create(data=country_raw, group_by_elements = c("occupation_code"), eda_var = "raw_tot_emp", year="year")
          
          sht = addWorksheet(wb, "raw data")
          writeData(wb, sht, country_raw)
          sht = addWorksheet(wb, "CV & GR on raw data")
          writeData(wb, sht, raw_eda)
          addStyle(wb, sht, style=pct, cols=c(grep("cv", colnames(raw_eda)), grep("gr_", colnames(raw_eda))), 
                   rows=2:(nrow(raw_eda)+1), gridExpand=TRUE)
          
          sht = addWorksheet(wb, "final data")
          writeData(wb, sht, country_fin)
          sht = addWorksheet(wb, "Raw and final data analysis")
          writeData(wb, sht, country_raw_diff)
          addStyle(wb, sht, style=pct, cols=c(grep("_perc", colnames(country_raw_diff))), 
                   rows=2:(nrow(country_raw_diff)+1), gridExpand=TRUE)
          sht = addWorksheet(wb, "CV & GR on final data")
          writeData(wb, sht, fin_eda)
          addStyle(wb, sht, style=pct, cols=c(grep("cv", colnames(fin_eda)), grep("gr_", colnames(fin_eda))), 
                   rows=2:(nrow(fin_eda)+1), gridExpand=TRUE)
          
          
          
        }else
        {
          stop("There should be a column named occupation_code, year and raw_tot_emp in country raw file")
        }
      }else
      {
        winDialog(type = c("ok"), "Raw file not inputed, no validation based on raw data will be done")
        sht = addWorksheet(wb, "final data")
        writeData(wb, sht, country_fin)
        sht = addWorksheet(wb, "CV & GR on final data")
        writeData(wb, sht, fin_eda)
        addStyle(wb, sht, style=pct, cols=c(grep("cv", colnames(fin_eda)), grep("gr_", colnames(fin_eda))), 
                 rows=2:(nrow(fin_eda)+1), gridExpand=TRUE)
      }
    }else
    {
      stop("There should be a column named occupation_code, year and tot_emp in country final file")
    }
    fin_jc=contri_file_create(data=country_fin, eda_var = "tot_emp", year="year", contri_of = "occupation_code")
    sht = addWorksheet(wb, "Job contribution")
    writeData(wb, sht, fin_jc)
    addStyle(wb, sht, style=pct, cols=c(grep("contribution_", colnames(fin_jc))), 
             rows=2:(nrow(fin_jc)+1), gridExpand=TRUE)
    save_file(wb, "Country")
    print("Country Validation ends")
  }
  
  ###################State validation file creation
  if(is.data.frame(state_fin))
  {
    print("State Validation started")
    if("occupation_code" %in% colnames(state_fin) &
       "year" %in% colnames(state_fin) &
       "tot_emp" %in% colnames(state_fin) &
       "state_id" %in% colnames(state_fin))
    {
      state_fin=state_fin%>%
        select(state_id, occupation_code, year, tot_emp)
      
      fin_eda=eda_file_create(data=state_fin, group_by_elements = c("state_id","occupation_code"), eda_var = "tot_emp", year="year")
      wb = createWorkbook()
      
      if(is.data.frame(state_raw))
      {
        if("occupation_code" %in% colnames(state_raw) &
           "year" %in% colnames(state_raw) &
           "raw_tot_emp" %in% colnames(state_raw) &
           "state_id" %in% colnames(state_raw))
        {
          state_raw=state_raw%>%
            select(state_id, occupation_code, year, raw_tot_emp)
          state_raw_diff=left_join(state_fin, state_raw,
                                   by=c("state_id","occupation_code", "year")) %>%
            mutate(diff=abs(raw_tot_emp-tot_emp),
                   diff_perc=diff/raw_tot_emp)
          
          
          raw_eda=eda_file_create(data=state_raw, group_by_elements = c("state_id","occupation_code"), 
                                  eda_var = "raw_tot_emp", year="year")
          
          sht = addWorksheet(wb, "raw data")
          writeData(wb, sht, state_raw)
          sht = addWorksheet(wb, "CV & GR on raw data")
          writeData(wb, sht, raw_eda)
          addStyle(wb, sht, style=pct, cols=c(grep("cv", colnames(raw_eda)), grep("gr_", colnames(raw_eda))), 
                   rows=2:(nrow(raw_eda)+1), gridExpand=TRUE)
          
          sht = addWorksheet(wb, "final data")
          writeData(wb, sht, state_fin)
          sht = addWorksheet(wb, "Raw and final data analysis")
          writeData(wb, sht, state_raw_diff)
          addStyle(wb, sht, style=pct, cols=c(grep("_perc", colnames(state_raw_diff))), 
                   rows=2:(nrow(state_raw_diff)+1), gridExpand=TRUE)
          sht = addWorksheet(wb, "CV & GR on final data")
          writeData(wb, sht, fin_eda)
          addStyle(wb, sht, style=pct, cols=c(grep("cv", colnames(fin_eda)), grep("gr_", colnames(fin_eda))), 
                   rows=2:(nrow(fin_eda)+1), gridExpand=TRUE)
          
        }else
        {
          stop("There should be a column named state_id, occupation_code, year and raw_tot_emp in state raw file")
        }
      }else
      {
        winDialog(type = c("ok"), "Raw file not inputed, no validation based on raw data will be done")
        sht = addWorksheet(wb, "final data")
        writeData(wb, sht, state_fin)
        sht = addWorksheet(wb, "CV & GR on final data")
        writeData(wb, sht, fin_eda)
        addStyle(wb, sht, style=pct, cols=c(grep("cv", colnames(fin_eda)), grep("gr_", colnames(fin_eda))), 
                 rows=2:(nrow(fin_eda)+1), gridExpand=TRUE)
      }
    }else
    {
      stop("There should be a column named state_id, occupation_code, year and tot_emp in state final file")
    }
    
    ########State aggreagated data to get the validation at only state granuarity
    state_data= state_fin %>%
      group_by(state_id, year) %>%
      summarise(tot_emp=sum(tot_emp, na.rm=T))
    
    ###Getting state level CV and GR
    state_eda=eda_file_create(data=state_data, group_by_elements = c("state_id"), eda_var = "tot_emp", year="year")
    sht = addWorksheet(wb, "CV & GR at state level")
    writeData(wb, sht, state_eda)
    addStyle(wb, sht, style=pct, cols=c(grep("cv", colnames(state_eda)), grep("gr_", colnames(state_eda))), 
             rows=2:(nrow(state_eda)+1), gridExpand=TRUE)
    
    ########Getting job contribution
    fin_jc=contri_file_create(data=state_fin, group_by_element="state_id", eda_var = "tot_emp", year="year", 
                              contri_of = "occupation_code")
    sht = addWorksheet(wb, "Job contribution")
    writeData(wb, sht, fin_jc)
    addStyle(wb, sht, style=pct, cols=c(grep("contribution_", colnames(fin_jc))), 
             rows=2:(nrow(fin_jc)+1), gridExpand=TRUE)
    
    ########Getting state contribution
    state_contri=contri_file_create(data=state_data, eda_var = "tot_emp", year="year", 
                                    contri_of = "state_id")
    sht = addWorksheet(wb, "State contribution")
    writeData(wb, sht, state_contri)
    addStyle(wb, sht, style=pct, cols=c(grep("contribution_", colnames(state_contri))), 
             rows=2:(nrow(state_contri)+1), gridExpand=TRUE)
    
    ###########Reconcilation check
    ## Input country file if does not exist
    repeat
    {
      if(is.data.frame(country_fin))
      {
        break
      }
      else
      {
        country_fin_path=choose.files(caption = "Enter country final file for reconcilation check", multi = F)
        if(substr(country_fin_path,nchar(country_fin_path)-2,nchar(country_fin_path))!="csv")
        {
          winDialog(type = c("ok"), "Please input CSV file")
        }else
        {
          country_fin=read.csv(country_fin_path)
          if("occupation_code" %in% colnames(country_fin) &
             "year" %in% colnames(country_fin) &
             "tot_emp" %in% colnames(country_fin))
          {
            if(nrow(country_fin %>%
                    group_by(occupation_code, year) %>%
                    summarise(n=n()) %>%
                    filter(n>1))>0)
            {
              rm(country_fin)
              winDialog(type = c("ok"), "Input file is not country file since there is duplication in occupation code and year")
            }else
            {
              country_fin=country_fin %>%
                select(occupation_code, year, tot_emp)
              
              break
            }
          }else
          {
            winDialog(type = c("ok"), "There should be a column named occupation_code, year and tot_emp in country final file")
          }
        }
      }
    }
    ###reconcilation check
    state_recon=state_fin %>% 
      group_by(occupation_code, year) %>%
      summarise(state_sum=sum(tot_emp, na.rm=T))
    
    state_recon=full_join(state_recon, country_fin %>%
                            rename(country_tot_emp=tot_emp), by=c("occupation_code", "year"))
    
    state_recon=state_recon %>%
      mutate(diff_perc=abs(ifelse(is.na(state_sum),0,state_sum)- ifelse(is.na(country_tot_emp),0,country_tot_emp))/
               ifelse(is.na(country_tot_emp),ifelse(is.na(state_sum),1,state_sum),country_tot_emp)) %>%
      mutate(check=ifelse(diff_perc>=0.02,"check",""))
    
    sht = addWorksheet(wb, "State country reconcile")
    writeData(wb, sht, state_recon)
    addStyle(wb, sht, style=pct, cols=c(grep("_perc", colnames(state_recon))), 
             rows=2:(nrow(state_recon)+1), gridExpand=TRUE)
    
    save_file(wb, "State")
    print("State Validation ends")
  }
  
  
  ###################MSA validation file creation
  if(is.data.frame(msa_fin))
  {
    print("MSA Validation started")
    if("occupation_code" %in% colnames(msa_fin) &
       "year" %in% colnames(msa_fin) &
       "tot_emp" %in% colnames(msa_fin) &
       "msa_id" %in% colnames(msa_fin))
    {
      msa_fin=msa_fin%>%
        select(msa_id, occupation_code, year, tot_emp)
      
      fin_eda=eda_file_create(data=msa_fin, group_by_elements = c("msa_id","occupation_code"), eda_var = "tot_emp", year="year")
      wb = createWorkbook()
      
      if(is.data.frame(msa_raw))
      {
        if("occupation_code" %in% colnames(msa_raw) &
           "year" %in% colnames(msa_raw) &
           "raw_tot_emp" %in% colnames(msa_raw) &
           "msa_id" %in% colnames(msa_raw))
        {
          msa_raw=msa_raw%>%
            select(msa_id, occupation_code, year, raw_tot_emp)
          msa_raw_diff=left_join(msa_fin, msa_raw,
                                 by=c("msa_id","occupation_code", "year")) %>%
            mutate(diff=abs(raw_tot_emp-tot_emp),
                   diff_perc=diff/raw_tot_emp)
          
          
          raw_eda=eda_file_create(data=msa_raw, group_by_elements = c("msa_id","occupation_code"), 
                                  eda_var = "raw_tot_emp", year="year")
          
          sht = addWorksheet(wb, "raw data")
          writeData(wb, sht, msa_raw)
          sht = addWorksheet(wb, "CV & GR on raw data")
          writeData(wb, sht, raw_eda)
          addStyle(wb, sht, style=pct, cols=c(grep("cv", colnames(raw_eda)), grep("gr_", colnames(raw_eda))), 
                   rows=2:(nrow(raw_eda)+1), gridExpand=TRUE)
          
          sht = addWorksheet(wb, "final data")
          writeData(wb, sht, msa_fin)
          sht = addWorksheet(wb, "Raw and final data analysis")
          writeData(wb, sht, msa_raw_diff)
          addStyle(wb, sht, style=pct, cols=c(grep("_perc", colnames(msa_raw_diff))), 
                   rows=2:(nrow(msa_raw_diff)+1), gridExpand=TRUE)
          sht = addWorksheet(wb, "CV & GR on final data")
          writeData(wb, sht, fin_eda)
          addStyle(wb, sht, style=pct, cols=c(grep("cv", colnames(fin_eda)), grep("gr_", colnames(fin_eda))), 
                   rows=2:(nrow(fin_eda)+1), gridExpand=TRUE)
          
        }else
        {
          stop("There should be a column named msa_id, occupation_code, year and raw_tot_emp in msa raw file")
        }
      }else
      {
        winDialog(type = c("ok"), "Raw file not inputed, no validation based on raw data will be done")
        sht = addWorksheet(wb, "final data")
        writeData(wb, sht, msa_fin)
        sht = addWorksheet(wb, "CV & GR on final data")
        writeData(wb, sht, fin_eda)
        addStyle(wb, sht, style=pct, cols=c(grep("cv", colnames(fin_eda)), grep("gr_", colnames(fin_eda))), 
                 rows=2:(nrow(fin_eda)+1), gridExpand=TRUE)
      }
    }else
    {
      stop("There should be a column named msa_id, occupation_code, year and tot_emp in msa final file")
    }
    
    ########msa aggreagated data to get the validation at only msa granuarity
    msa_data= msa_fin %>%
      group_by(msa_id, year) %>%
      summarise(tot_emp=sum(tot_emp, na.rm=T))
    
    ###Getting msa level CV and GR
    msa_eda=eda_file_create(data=msa_data, group_by_elements = c("msa_id"), eda_var = "tot_emp", year="year")
    sht = addWorksheet(wb, "CV & GR at msa level")
    writeData(wb, sht, msa_eda)
    addStyle(wb, sht, style=pct, cols=c(grep("cv", colnames(msa_eda)), grep("gr_", colnames(msa_eda))), 
             rows=2:(nrow(msa_eda)+1), gridExpand=TRUE)
    
    ########Getting job contribution
    fin_jc=contri_file_create(data=msa_fin, group_by_element="msa_id", eda_var = "tot_emp", year="year", 
                              contri_of = "occupation_code")
    sht = addWorksheet(wb, "Job contribution")
    writeData(wb, sht, fin_jc)
    addStyle(wb, sht, style=pct, cols=c(grep("contribution_", colnames(fin_jc))), 
             rows=2:(nrow(fin_jc)+1), gridExpand=TRUE)
    
    ########Getting msa contribution
    msa_contri=contri_file_create(data=msa_data, eda_var = "tot_emp", year="year", 
                                  contri_of = "msa_id")
    sht = addWorksheet(wb, "msa contribution")
    writeData(wb, sht, msa_contri)
    addStyle(wb, sht, style=pct, cols=c(grep("contribution_", colnames(msa_contri))), 
             rows=2:(nrow(msa_contri)+1), gridExpand=TRUE)
    
    #######################Reconcilation check for MSA
    
    ### test msa shared in state mapping file
    if(!is.data.frame(msa_shared_in_state_map))
    {
      winDialog(type="ok", "MSA to state mapping is not given, there will not be reconcilation check done for MSA")
    }else
    {
      #### enter only when the column names are as per specified
      if("state_id" %in% colnames(msa_shared_in_state_map) &
         "msa_id" %in% colnames(msa_shared_in_state_map) &
         "year" %in% colnames(msa_shared_in_state_map) &
         "rate" %in% colnames(msa_shared_in_state_map))
      {
        msa_shared_in_state_map= msa_shared_in_state_map %>%
          select(msa_id, state_id, year, rate)
        
        ####CHecking the summation of the rate for all MSA
        msa_shared_in_state_test= msa_shared_in_state_map %>%
          group_by(msa_id, year) %>%
          summarise(rate_sum=round(sum(rate, na.rm=T),1)) %>%
          filter(rate_sum!=1)
        if(nrow(msa_shared_in_state_test)>0)
        {
          stop(paste("Summation of the rate for an MSA should be equal to 0. Please check MSA: ", 
                     paste(msa_shared_in_state_test$msa_id, collapse = ","), 
                     "for year",paste(msa_shared_in_state_test$year, collapse = ",")))
        }
        
        #### check state_prop_in_msa
        if(!is.data.frame(state_prop_in_msa))
        {
          winDialog(type="ok", "The file which gives the proportion of state population falling in its MSAs is missing.
                    It will be assumed all the state is exhastive of its MSAs")
          
          state_prop_in_msa=msa_shared_in_state_map %>%
            select(state_id, year) %>% unique() %>%
            mutate(state_prop_in_msa=1)
        }
        #####enter only when the column names make sense and there is no duplication by state id
        if("state_id" %in% colnames(state_prop_in_msa) &
           "year" %in% colnames(state_prop_in_msa) &
           "state_prop_in_msa" %in% colnames(state_prop_in_msa))
        {
          if(nrow(state_prop_in_msa %>%
                  group_by(state_id, year) %>%
                  summarise(n=n()) %>%
                  filter(n>1))>0)
          {
            stop("State propotion file can not have duplication on state id and year. Please check and try again.")
          }else
          {
            state_prop_in_msa=state_prop_in_msa %>%
              select(state_id, year, state_prop_in_msa)
            msa_recon=left_join(msa_fin, msa_shared_in_state_map,
                                by=c("msa_id", "year"))
            msa_recon=msa_recon %>%
              mutate(tot_emp_in_state=tot_emp*rate)
            
            msa_recon=msa_recon %>% 
              group_by(state_id, occupation_code, year) %>%
              summarise(msa_sum=sum(tot_emp_in_state, na.rm=T))
            
            
            ############checking for state_fin file
            repeat
            {
              if(is.data.frame(state_fin))
              {
                break
              }
              else
              {
                state_fin_path=choose.files(caption = "Enter state final file for reconcilation check", multi = F)
                if(substr(state_fin_path,nchar(state_fin_path)-2,nchar(state_fin_path))!="csv")
                {
                  winDialog(type = c("ok"), "Please input CSV file")
                }else
                {
                  state_fin=read.csv(state_fin_path)
                  if("occupation_code" %in% colnames(state_fin) &
                     "year" %in% colnames(state_fin) &
                     "tot_emp" %in% colnames(state_fin) &
                     "state_id" %in% colnames(state_fin))
                  {
                    state_fin=state_fin %>%
                      select(state_id, occupation_code, year, tot_emp)
                    break
                  }else
                  {
                    winDialog(type = c("ok"), "There should be a column named occupation_code, year and tot_emp in state final file")
                  }
                }
              }
            }
            
            state_prop_tot_emp=left_join(state_fin, state_prop_in_msa, by=c("state_id","year"))
            state_prop_tot_emp=state_prop_tot_emp %>%
              rename(state_tot_emp=tot_emp) %>%
              mutate(state_prop_tot_emp= round(state_tot_emp*state_prop_in_msa,0))
            
            msa_recon=full_join(msa_recon, state_prop_tot_emp, by=c("state_id","occupation_code", "year"))
            msa_recon=msa_recon %>%
              mutate(diff_perc=abs(ifelse(is.na(msa_sum),0,msa_sum)- ifelse(is.na(state_prop_tot_emp),0,state_prop_tot_emp))/
                       ifelse(is.na(state_prop_tot_emp),ifelse(is.na(msa_sum),1,msa_sum),state_prop_tot_emp)) %>%
              mutate(check=ifelse(diff_perc>0.04,"check",""))
            
            sht = addWorksheet(wb, "MSA state reconcile")
            writeData(wb, sht, msa_recon)
            addStyle(wb, sht, style=pct, cols=c(grep("_perc", colnames(msa_recon)), grep("rate", colnames(msa_recon)), 
                                                grep("state_prop_in_", colnames(msa_recon))),
                     rows=2:(nrow(msa_recon)+1), gridExpand=TRUE)
          }
          
        }else
        {
          stop("State propotion file should have columns state_id, state_prop_in_msa and year.
               This file should give the proportion of state population in all its MSA.
               For example: State S1 has 3 MSAs M1, M2, M3. Th epopulation of each is like:
               S1: 1000
               M1:300
               M2:200
               m3:100
               then a row will be there in the table with stte_id as S1 and
               state_prop_in_msa=(300+200+100)/1000=0.6")
        }
      }else
      {
        stop("MSA shared between different state mapping files should have columns msa_id, state_id, year and rate.
             If an MSA is mapped in more than one state, then rate should give by what percentage the part of the MSA is in that state.
             NOTE: Sum of rate for an MSA should always be equal to 1.")
      }
    }
    
    save_file(wb, "MSA")
    print("MSA Validation ends")
  }
}

