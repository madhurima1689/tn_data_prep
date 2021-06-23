
#' Supply Transition function to convert the LOCAL CODE counts into ONET counts
#' 
#' This function helps to convert the counts for LOCAL CODE into the ONET counts.
#' If the count at soc 6 level is same, 
#' then it is redistributed on the basis of the supply or demand file for the Benchmark country.
#' Here as a default value to redistribute SOC 6 is given, This can be changed in the function call.\cr\cr
#' 
#' All the required files will be asked to be inputed in the function.
#' Please read all the window titles to input correct files.
#' 
#' MSA adjustment can be optionally done.
#' @export
#' @param county_supply_file: Counts for local code at country level. There should be a column named occupation_code, country_id, year and tot_emp.
#' @param state_supply_file: Counts for local code at state level. There should be a column named occupation_code, state_id, year and tot_emp.
#' @param msa_supply_file: Counts for local code at msa level. There should be a column named occupation_code, msa_id, year and tot_emp.
#' @param redist_SOC: The variable to facilitate redistributing uing demand data in case of same values at higher hierarchy. DEFAULT to 6
#' @param output_path: The path where all the output files will be saved.
#' @param country_id: The country_id of the country to get the geographical mapping from the database.
#' @param sum_up_msa: If MSAs cover the whole territory of a country then "yes". DEFAULT to "no"
#' @param threshold_msa: If the sum of the msa should not exceed state values then "yes". DEFAULT to "yes"
#' @param username: Gartner username
#' @param password: Workbench database password
#' @param host: The connection string to connect to the host
#' @return All the files yearwise and otherwise will be wirtten on the output folder specified. A list will be returned with all the data too.\cr
#' final_list[[1]][[1]]: Country final files
#' final_list[[1]][[2]]: Country Rate
#' final_list[[2]][[1]]: State final files
#' final_list[[2]][[2]]: State Rate
#' final_list[[3]][[1]]: MSA final files
#' final_list[[3]][[2]]: MSA Rate


supply_transition=function(country_supply_file=NA, state_supply_file=NA, msa_supply_file=NA, redist_SOC=6, output_path="",
                           country_id=NA, sum_up_msa="no", threshold_msa="yes",
                           username=NA, password=NA, host=NA)
{
  final_list=list()
  
  if(is.na(username) | is.na(password) | is.na(host))
  {
    stop("Please input username, password and the host for the connetion of the database.")
  }
  
  if(is.na(country_id))
  {
    stop("Please input country_id as in database")
  }
  
  if(!sum_up_msa %in% c("yes", "no"))
  {
    stop("sum_up_msa should be either 'yes' or 'no'")
  }
  
  if(!threshold_msa %in% c("yes", "no"))
  {
    stop("sum_up_msa should be either 'yes' or 'no'")
  }
  
  if(is.data.frame(country_supply_file))
  {
    if(!"occupation_code" %in% colnames(country_supply_file) |
       !"country_id" %in% colnames(country_supply_file) |
       !"tot_emp" %in% colnames(country_supply_file) |
       !"year" %in% colnames(country_supply_file))
    {
      stop("There should be a column named occupation_code, country_id, year and tot_emp in country supply file")
    }
    else
    {
      colnames(country_supply_file)[colnames(country_supply_file)=="occupation_code"]="local_occupation_code"
    }
  }
  
  if(is.data.frame(state_supply_file))
  {
    if(!"occupation_code" %in% colnames(state_supply_file) |
       !"state_id" %in% colnames(state_supply_file) |
       !"tot_emp" %in% colnames(state_supply_file) |
       !"year" %in% colnames(state_supply_file))
    {
      stop("There should be a column named occupation_code, state_id, year and tot_emp in state supply file")
    }
    else
    {
      colnames(state_supply_file)[colnames(state_supply_file)=="occupation_code"]="local_occupation_code"
    }
  }
  
  if(is.data.frame(msa_supply_file))
  {
    if(!"occupation_code" %in% colnames(msa_supply_file) |
       !"msa_id" %in% colnames(msa_supply_file) |
       !"tot_emp" %in% colnames(msa_supply_file) |
       !"year" %in% colnames(msa_supply_file))
    {
      stop("There should be a column named occupation_code, msa_id, year and tot_emp in msa supply file")
    }
    else
    {
      colnames(msa_supply_file)[colnames(msa_supply_file)=="occupation_code"]="local_occupation_code"
    }
  }
  
  ##############Getting the geo mapping
  print("Making db connection")
  mydb = dbConnect(MySQL(), 
                   user=username, password=password, 
                   host=host)
  
  query = paste0("select s.country_id, s.id as state_id, a.msa_id, a.primary_state from norm_prod.states_2016 s left join norm_prod.msas_states_2016 a on s.id=a.state_id where s.country_id=",country_id,";")
  ## Run the query
  rs = dbSendQuery(mydb, query)
  ## Fetch the data
  geo_map_raw = fetch(rs, n=-1)
  all_cons <- dbListConnections(MySQL())
  for(con in all_cons){dbDisconnect(con)}
  
  geo_map_raw= geo_map_raw %>%
    group_by(country_id, state_id) %>%
    mutate(sum_ps=sum(primary_state))
  
  geo_map=geo_map_raw %>%
    filter(primary_state ==1 | is.na(primary_state) | sum_ps==0)
  
  geo_map=as.data.frame(geo_map)
  print("DB connection successful")
  
  ##################### Input occ mapping
  repeat
  {
    occ_name=choose.files(caption="Select the Occupational Mapping file with 2 columns: onet_code, local_occupation_code", 
                          multi=F)
    if(substr(occ_name,nchar(occ_name)-2,nchar(occ_name))!="csv")
    {
      winDialog(type = c("ok"), "Please input CSV file")
    }
    else
    {
      occ_map=read.csv(occ_name)
      if("onet_code" %in% colnames(occ_map) &
         "local_occupation_code" %in% colnames(occ_map))
      {
        occ_map= occ_map %>%
          filter((substr(onet_code,1,2)==99 & nchar(onet_code)==max(nchar(onet_code))) | substr(onet_code,1,2)!=99)
        break
      }else
      {
        winDialog(type = c("ok"), "The Occupational mapping file should have columns name:
                  onet_code and local_occupation_code")
      }
    }
  }
  
  repeat
  {
    bm_supply_name=choose.files(
      caption="Select the benchmark country supply file with onet_code and count to correct the supply values", 
      multi=F)
    if(substr(bm_supply_name,nchar(bm_supply_name)-2,nchar(bm_supply_name))!="csv")
    {
      winDialog(type = c("ok"), "Please input CSV file")
    }
    else
    {
      bm_supply=read.csv(bm_supply_name)
      if("onet_code" %in% colnames(bm_supply) &
         "count" %in% colnames(bm_supply))
      {
        bm_test=bm_supply %>%
          group_by(onet_code) %>%
          summarise(cnt=n()) %>%
          filter(cnt>1)
        if(nrow(bm_test)>0)
        {
          winDialog(type = c("ok"), "The benchmark file should be Supply at COUNTRY level. It can not have more than one row of an onet code.")
        }else
        {
          bm_supply$count <- as.numeric(as.character(gsub(",","",bm_supply$count)))
          break
        }
      }else
      {
        winDialog(type = c("ok"), "The bm_supply file should have columns name:
                  onet_code and
                  count")
      }
    }
  }
  
  if(is.data.frame(country_supply_file))
  {
    repeat
    {
      demand_name=choose.files(caption=paste0("Select the Demand file for COUNTRY with country_id, ONET_code, local_code and country_count"), 
                               multi=F)
      if(substr(demand_name,nchar(demand_name)-2,nchar(demand_name))!="csv")
      {
        winDialog(type = c("ok"), "Please input CSV file")
      }
      else
      {
        demand_country=read.csv(demand_name)
        if("ONET_code" %in% colnames(demand_country) &
           "local_code" %in% colnames(demand_country) &
           "country_count" %in% colnames(demand_country) &
           "country_id" %in% colnames(demand_country))
        {
          bm_test=demand_country %>%
            group_by(ONET_code, local_code) %>%
            summarise(cnt=n()) %>%
            filter(cnt>1)
          if(nrow(bm_test)>0)
          {
            winDialog(type = c("ok"), "The demand file should not have more than one row for a combination of onet code and local occupation code.")
          }else
          {
            colnames(demand_country)[colnames(demand_country)=="ONET_code"]="onet_code"
            colnames(demand_country)[colnames(demand_country)=="local_code"]="local_occupation_code"
            colnames(demand_country)[colnames(demand_country)=="country_count"]="count"
            break
          }
        }else
        {
          winDialog(type = c("ok"), "The Demand file should have columns name:
                    country_id,
                    ONET_code, 
                    local_code and
                    country_count")
        }
      }
    }

    ###########################################Country Calculations Starts
    
    print("Processing file for Country")
    country_supply_file$local_occupation_code=as.character(country_supply_file$local_occupation_code)
    country_supply_file$country_id=as.numeric(as.character(country_supply_file$country_id))
    occ_map$local_occupation_code=as.character(occ_map$local_occupation_code)
    occ_map$onet_code=as.character(occ_map$onet_code)
    geo_map$country_id=as.numeric(as.character(geo_map$country_id))
    demand_country$local_occupation_code=as.character(demand_country$local_occupation_code)
    demand_country$onet_code=as.character(demand_country$onet_code)
    demand_country$country_id=as.numeric(as.character(demand_country$country_id))
    country=left_join(
      inner_join(
        full_join(geo_map %>% select(country_id) %>% unique() %>% mutate(fake="1"),
                  occ_map %>% mutate(fake="1"),
                  by="fake") %>% select(-fake),
        country_supply_file, 
        by=c("country_id", "local_occupation_code")),
      demand_country, 
      by=c("country_id", "local_occupation_code", "onet_code")
    ) %>%
      rename(demand_count=count)
    
    country=country %>%
      group_by(country_id, local_occupation_code, year) %>%
      mutate(demand_count_sum_by_local=sum(demand_count, na.rm=T)) %>%
      mutate(Rate_local_Onet=ifelse(is.na(demand_count/demand_count_sum_by_local),0,demand_count/demand_count_sum_by_local)) %>%
      mutate(threshold_rate=ifelse(Rate_local_Onet<0.05,0.05,Rate_local_Onet))
    country= country %>%
      group_by(country_id, local_occupation_code, year) %>%
      mutate(sum_wd_threshold=sum(threshold_rate, na.rm=T)) %>%
      mutate(adjusted_rate=threshold_rate/sum_wd_threshold) %>%
      mutate(onet_count=adjusted_rate*tot_emp)
    
    ##############################Adjusting the values with bench mark countries demand
    
    country_adjust=country %>%
      group_by(country_id, onet_occupation_type_id, local_occupation_code, onet_code, year, onet_count) %>%
      summarise(n=n()) %>%
      select(-n) %>%
      group_by(country_id, onet_occupation_type_id, onet_code, year) %>%
      summarise(onet_supply=sum(onet_count, na.rm=T))
    
    bm_supply$onet_code=as.character(bm_supply$onet_code)
    country_bm=left_join(country_adjust, 
                         bm_supply %>% select(onet_code, count), by="onet_code")
    
    country_bm$count[country_bm$count==0]=NA
    
    if(sum(is.na(country_bm$count))>0)
    {
      winDialog(type = c("ok"), "Reference count has few NULLS or 0s and will be ignored.")
    }
    
    
    country_final=country_bm %>%
      mutate(soc=substr(onet_code,1,redist_SOC)) %>%
      group_by(soc, year, onet_supply) %>%
      mutate(sum=sum(count, na.rm=T),
             cnt=n(),
             cnt_na=sum(is.na(count))) %>%
      mutate(onet_final_supply=ifelse(is.na(count), onet_supply,count/sum*onet_supply*(cnt-cnt_na))) %>%
      group_by(country_id, onet_occupation_type_id, onet_code, year, onet_final_supply) %>%
      summarise(n=n()) %>%
      select(-n) %>%
      rename(occupation_type_id=onet_occupation_type_id,
             country_onet_supply=onet_final_supply,
             tot_emp=onet_final_supply,
             occupation_code=onet_code) %>%
      mutate(tot_emp=round(tot_emp,0)) %>%
      select(occupation_type_id, country_id, year, occupation_code, tot_emp)
    
    country_final$tot_emp[is.na(country_final$tot_emp)]=0
    for(i in min(country_final$year):max(country_final$year))
    {
      temp=country_final %>%
        filter(year==i)
      write.csv(temp, paste0(output_path,"/country_supply_output_",i,".csv"), row.names = F)
    }
    
    country_rate=left_join(country %>% 
                             select(country_id, onet_code, local_occupation_code, year, adjusted_rate) %>%
                             rename(country_rate=adjusted_rate),
                           country_final %>%
                             rename(onet_code=occupation_code,
                                    onet_occupation_type_id=occupation_type_id,
                                    country_onet_supply=tot_emp),
                           by=c("country_id", "onet_code", "year")) 
    
    write.csv(country_final, paste0(output_path,"/country_supply_output.csv"), row.names = F)
    write.csv(country_rate, paste0(output_path,"/ONET Country Rates and Supply.csv"), row.names = F)
    
    ###########warning message if LOC and SOC number doesnot match
    
    country_loc_aggr=country_supply_file %>%
      group_by(country_id, year) %>%
      summarise(sum_loc_tot_emp=sum(tot_emp, na.rm=T))
    
    country_soc_aggr= country_final %>%
      group_by(country_id, year) %>%
      summarise(sum_soc_tot_emp=sum(tot_emp, na.rm=T))
    
    country_aggr= left_join( country_loc_aggr, country_soc_aggr, by=c("country_id", "year"))
    
    country_aggr= country_aggr %>%
      mutate(diff=abs(sum_loc_tot_emp-sum_soc_tot_emp)) %>%
      mutate(diff_perc=diff/sum_loc_tot_emp*100)
    
    if(max(country_aggr$diff_perc)>5)
    {
      winDialog(type = c("ok"), "There is difference between LOC and SOC for country, please validate thoroughly the country file.")
      print("There is difference between LOC and SOC for country, please validate thoroughly country file.")
    }
    ################################################################
    
    
    final_list[[length(final_list)+1]]=list(data.frame(country_final), data.frame(country_rate))
    names(final_list)[length(final_list)]="Country"
    
  }
  
  ##################################################################################################################
  ##################################################################################################################
  ##################################################################################################################
  ##################################################################################################################
  ##################################################################################################################
  
  if(is.data.frame(state_supply_file))
  {
    repeat
    {
      demand_name=choose.files(caption=paste0("Select the Demand file for STATE with state_id, ONET_code, local_code and state_count"), 
                               multi=F)
      if(substr(demand_name,nchar(demand_name)-2,nchar(demand_name))!="csv")
      {
        winDialog(type = c("ok"), "Please input CSV file")
      }
      else
      {
        demand_state=read.csv(demand_name)
        if("ONET_code" %in% colnames(demand_state) &
           "local_code" %in% colnames(demand_state) &
           "state_count" %in% colnames(demand_state) &
           "state_id" %in% colnames(demand_state))
        {
          colnames(demand_state)[colnames(demand_state)=="ONET_code"]="onet_code"
          colnames(demand_state)[colnames(demand_state)=="local_code"]="local_occupation_code"
          colnames(demand_state)[colnames(demand_state)=="state_count"]="count"
          break
        }else
        {
          winDialog(type = c("ok"), "The Demand file should have columns name:
                    state_id,
                    ONET_code, 
                    local_code and
                    state_count")
        }
      }
    }
    
    if(!exists("country_rate"))
    {
      repeat
      {
        country_rate_name=choose.files(caption=paste0("Select the Country Rates with onet_code, local_occupation_code, year, country rate and onet_supply"), 
                                       multi=F)
        if(substr(country_rate_name,nchar(country_rate_name)-2,nchar(country_rate_name))!="csv")
        {
          winDialog(type = c("ok"), "Please input CSV file")
        }
        else
        {
          country_rate=read.csv(country_rate_name)
          if("onet_code" %in% colnames(country_rate) &
             "local_occupation_code" %in% colnames(country_rate) &
             "year" %in% colnames(country_rate)&
             "country_rate" %in% colnames(country_rate)&
             "country_onet_supply" %in% colnames(country_rate))
          {
            break
          }else
          {
            winDialog(type = c("ok"), "The Country Rate file should have columns name:
                      onet_code, 
                      local_occupation_code,
                      year,
                      country_rate and 
                      country_onet_supply")
          }
        }
      }
    }
    
    ###########################################State Calculations Starts
    
    print("Processing file for State")
    state_supply_file$local_occupation_code=as.character(state_supply_file$local_occupation_code)
    state_supply_file$state_id=as.numeric(as.character(state_supply_file$state_id))
    occ_map$local_occupation_code=as.character(occ_map$local_occupation_code)
    occ_map$onet_code=as.character(occ_map$onet_code)
    geo_map$state_id=as.numeric(as.character(geo_map$state_id))
    demand_state$local_occupation_code=as.character(demand_state$local_occupation_code)
    demand_state$onet_code=as.character(demand_state$onet_code)
    demand_state$state_id=as.numeric(as.character(demand_state$state_id))
    country_rate$onet_occupation_type_id=as.numeric(as.character(country_rate$onet_occupation_type_id))
    country_rate$local_occupation_code=as.character(country_rate$local_occupation_code)
    country_rate$onet_code=as.character(country_rate$onet_code)
    country_rate$year=as.numeric(as.character(country_rate$year))
    state=left_join(
      inner_join(
        full_join(geo_map %>% select(state_id) %>% unique() %>% mutate(fake="1"),
                  occ_map %>% mutate(fake="1"),
                  by="fake") %>% select(-fake),
        state_supply_file, 
        by=c("state_id", "local_occupation_code")),
      demand_state, 
      by=c("state_id", "local_occupation_code", "onet_code")
    ) %>%
      rename(demand_count=count)
    
    state=left_join(state, country_rate, by=c("onet_occupation_type_id",
                                              "local_occupation_code", "onet_code", "year"))
    
    state=state %>%
      group_by(state_id, local_occupation_code, year) %>%
      mutate(demand_count_sum_by_local=sum(demand_count, na.rm=T)) %>%
      mutate(Rate_local_Onet=ifelse(is.na(demand_count/demand_count_sum_by_local),0,demand_count/demand_count_sum_by_local)) %>%
      mutate(Rate_local_Onet=ifelse(is.na(demand_count), country_rate, Rate_local_Onet )) %>%
      mutate(inter_Rate_local_Onet=Rate_local_Onet/sum(Rate_local_Onet, na.rm = T)) %>%
      mutate(threshold_rate=ifelse(inter_Rate_local_Onet<0.05 | is.na(inter_Rate_local_Onet),0.05,inter_Rate_local_Onet))
    state= state %>%
      group_by(state_id, local_occupation_code, year) %>%
      mutate(sum_wd_threshold=sum(threshold_rate, na.rm=T)) %>%
      mutate(adjusted_rate=threshold_rate/sum_wd_threshold) %>%
      mutate(onet_count=adjusted_rate*tot_emp)
    
    ##############################Adjusting the values with bench mark countries demand
    
    state_adjust=state %>%
      group_by(state_id, onet_occupation_type_id, onet_code, year, local_occupation_code, onet_count, country_onet_supply) %>%
      summarise(n=n()) %>%
      select(-n) %>%
      group_by(state_id, onet_occupation_type_id, onet_code, year,country_onet_supply) %>%
      summarise(onet_supply=sum(onet_count, na.rm=T))
    
    bm_supply$onet_code=as.character(bm_supply$onet_code)
    state_bm=left_join(state_adjust, 
                       bm_supply %>% select(onet_code, count), by="onet_code")
    
    state_bm$count[state_bm$count==0]=NA
    
    if(sum(is.na(state_bm$count))>0)
    {
      winDialog(type = c("ok"), "Reference count has few NULLS or 0s and will be ignored.")
    }
    
    state_onet_supply=state_bm %>%
      mutate(soc=substr(onet_code,1,redist_SOC)) %>%
      group_by(state_id, soc, year, onet_supply) %>%
      mutate(cnt_na=sum(is.na(count))) 
    
    state_onet_supply= state_onet_supply %>%
      group_by(state_id, soc, year, onet_supply) %>%
      mutate(cnt=n(),
             sum=sum(count, na.rm=T)) %>%
      mutate(onet_final_supply=ifelse(is.na(count), onet_supply,count/sum*onet_supply*(cnt-cnt_na))) 
    
    state_onet_supply=data.table(state_onet_supply)
    state_onet_supply[, `:=` (state_onet_sum = sum(onet_final_supply, na.rm=T)), 
                      by = c("onet_code", "year")]
    
    
    state_final= state_onet_supply %>%
      mutate(state_onet_supply=onet_final_supply/state_onet_sum*country_onet_supply) %>%
      select(state_id, onet_occupation_type_id, onet_code, year, state_onet_supply) %>%
      rename(occupation_type_id=onet_occupation_type_id,
             tot_emp=state_onet_supply,
             occupation_code=onet_code) %>%
      mutate(tot_emp=round(tot_emp,0)) %>%
      select(occupation_type_id, state_id, year, occupation_code, tot_emp)
    
    state_final$tot_emp[is.na(state_final$tot_emp)]=0
    for(i in min(state_final$year):max(state_final$year))
    {
      temp=state_final %>%
        filter(year==i)
      write.csv(temp, paste0(output_path,"/state_supply_output_",i,".csv"), row.names = F)
    }
    
    state_rate=left_join(state %>% 
                           select(state_id, onet_code, local_occupation_code, year, adjusted_rate) %>%
                           rename(state_rate=adjusted_rate),
                         state_final %>%
                           rename(onet_code=occupation_code,
                                  onet_occupation_type_id=occupation_type_id,
                                  state_onet_supply=tot_emp),
                         by=c("state_id", "onet_code", "year"))

    ###########warning message if LOC and SOC number doesnot match
    
    state_loc_aggr=state_supply_file %>%
      group_by(state_id, year) %>%
      summarise(sum_loc_tot_emp=sum(tot_emp, na.rm=T))
    
    state_soc_aggr= state_final %>%
      group_by(state_id, year) %>%
      summarise(sum_soc_tot_emp=sum(tot_emp, na.rm=T))
    
    state_aggr= left_join( state_loc_aggr, state_soc_aggr, by=c("state_id", "year"))
    
    state_aggr= state_aggr %>%
      mutate(diff=abs(sum_loc_tot_emp-sum_soc_tot_emp)) %>%
      mutate(diff_perc=diff/sum_loc_tot_emp*100)
    
    if(max(state_aggr$diff_perc)>5)
    {
      winDialog(type = c("ok"), "There is difference between LOC and SOC for state, please validate thoroughly the state file.")
      print("There is difference between LOC and SOC for state, please validate thoroughly state file.")
    }
    ################################################################
    
    
    write.csv(state_final, paste0(output_path,"/state_supply_output.csv"), row.names = F)
    write.csv(state_rate, paste0(output_path,"/ONET State Rates and Supply.csv"), row.names = F)
    
    final_list[[length(final_list)+1]]=list(data.frame(state_final), data.frame(state_rate))
    names(final_list)[length(final_list)]="State"
    
  }
  
  ##################################################################################################################
  ##################################################################################################################
  ##################################################################################################################
  ##################################################################################################################
  ##################################################################################################################
  
  if(is.data.frame(msa_supply_file))
  {
    repeat
    {
      demand_name=choose.files(caption=paste0("Select the Demand file for MSA with ONET_code, local_code and msa_count"), 
                               multi=F)
      if(substr(demand_name,nchar(demand_name)-2,nchar(demand_name))!="csv")
      {
        winDialog(type = c("ok"), "Please input CSV file")
      }
      else
      {
        demand_msa=read.csv(demand_name)
        if("ONET_code" %in% colnames(demand_msa) &
           "local_code" %in% colnames(demand_msa) &
           "msa_count" %in% colnames(demand_msa))
        {
          colnames(demand_msa)[colnames(demand_msa)=="ONET_code"]="onet_code"
          colnames(demand_msa)[colnames(demand_msa)=="local_code"]="local_occupation_code"
          colnames(demand_msa)[colnames(demand_msa)=="msa_count"]="count"
          break
        }else
        {
          winDialog(type = c("ok"), "The Demand file should have columns name:
                    ONET_code, 
                    local_code and
                    msa_count")
        }
      }
    }
    
    if(!exists("state_rate"))
    {
      repeat
      {
        state_rate_name=choose.files(caption=paste0("Select the state Rates with onet_code, local_occupation_code, year, state rate and onet_supply"), 
                                     multi=F)
        if(substr(state_rate_name,nchar(state_rate_name)-2,nchar(state_rate_name))!="csv")
        {
          winDialog(type = c("ok"), "Please input CSV file")
        }
        else
        {
          state_rate=read.csv(state_rate_name)
          if("onet_code" %in% colnames(state_rate) &
             "local_occupation_code" %in% colnames(state_rate) &
             "year" %in% colnames(state_rate)&
             "state_rate" %in% colnames(state_rate)&
             "state_onet_supply" %in% colnames(state_rate))
          {
            break
          }else
          {
            winDialog(type = c("ok"), "The state Rate file should have columns name:
                      onet_code, 
                      local_occupation_code,
                      year,
                      state_rate and 
                      state_onet_supply")
          }
        }
      }
    }
    
    ###########################################msa Calculations Starts
    
    print("Processing file for MSA")
    msa_supply_file$local_occupation_code=as.character(msa_supply_file$local_occupation_code)
    msa_supply_file$msa_id=as.numeric(as.character(msa_supply_file$msa_id))
    occ_map$local_occupation_code=as.character(occ_map$local_occupation_code)
    occ_map$onet_code=as.character(occ_map$onet_code)
    geo_map$msa_id=as.numeric(as.character(geo_map$msa_id))
    demand_msa$local_occupation_code=as.character(demand_msa$local_occupation_code)
    demand_msa$onet_code=as.character(demand_msa$onet_code)
    demand_msa$msa_id=as.numeric(as.character(demand_msa$msa_id))
    state_rate$onet_occupation_type_id=as.numeric(as.character(state_rate$onet_occupation_type_id))
    state_rate$local_occupation_code=as.character(state_rate$local_occupation_code)
    state_rate$onet_code=as.character(state_rate$onet_code)
    state_rate$year=as.numeric(as.character(state_rate$year))
    msa=left_join(
      inner_join(
        full_join(geo_map %>% select(msa_id, state_id, primary_state) %>% filter(primary_state==1) %>% unique() %>% mutate(fake="1"),
                  occ_map %>% mutate(fake="1"),
                  by="fake") %>% select(-fake),
        msa_supply_file, 
        by=c("msa_id", "local_occupation_code")),
      demand_msa, 
      by=c("msa_id", "local_occupation_code", "onet_code")
    ) %>%
      rename(demand_count=count)
    
    msa=left_join(msa, state_rate, by=c("onet_occupation_type_id", "state_id",
                                        "local_occupation_code", "onet_code", "year"))
    
    msa= msa %>%
      group_by(state_id, onet_code, year) %>%
      mutate(cnt=n(),
             cnt_na=sum(is.na(state_onet_supply))) %>%
      mutate(state_onet_supply=ifelse(cnt==cnt_na,0,na.locf(state_onet_supply)))
    
    
    msa=msa %>%
      group_by(msa_id, state_id, local_occupation_code, year) %>%
      mutate(demand_count_sum_by_local=sum(demand_count, na.rm=T)) %>%
      mutate(Rate_local_Onet=ifelse(is.na(demand_count/demand_count_sum_by_local),0,demand_count/demand_count_sum_by_local)) %>%
      mutate(Rate_local_Onet=ifelse(is.na(demand_count), state_rate, Rate_local_Onet )) %>%
      mutate(inter_Rate_local_Onet=Rate_local_Onet/sum(Rate_local_Onet, na.rm = T)) %>%
      mutate(threshold_rate=ifelse(inter_Rate_local_Onet<0.05 | is.na(inter_Rate_local_Onet),0.05,inter_Rate_local_Onet))
    msa= msa %>%
      group_by(msa_id, state_id, local_occupation_code, year) %>%
      mutate(sum_wd_threshold=sum(threshold_rate, na.rm=T)) %>%
      mutate(adjusted_rate=threshold_rate/sum_wd_threshold) %>%
      mutate(onet_count=adjusted_rate*tot_emp)
    
    ##############################Adjusting the values with bench mark countries demand
    
    msa_adjust=msa %>%
      group_by(msa_id, state_id, onet_occupation_type_id, local_occupation_code, onet_code, year, onet_count, state_onet_supply) %>%
      summarise(n=n()) %>%
      select(-n) %>%
      group_by(msa_id, state_id, onet_occupation_type_id, onet_code, year,state_onet_supply) %>%
      summarise(onet_supply=sum(onet_count, na.rm=T))
    
    bm_supply$onet_code=as.character(bm_supply$onet_code)
    msa_bm=left_join(msa_adjust, 
                     bm_supply %>% select(onet_code, count), by="onet_code")
    
    msa_bm$count[msa_bm$count==0]=NA
    
    if(sum(is.na(msa_bm$count))>0)
    {
      winDialog(type = c("ok"), "Reference count has few NULLS or 0s and will be ignored.")
    }
    
    msa_onet_supply=msa_bm %>%
      mutate(soc=substr(onet_code,1,redist_SOC)) %>%
      group_by(msa_id, state_id, soc, year, onet_supply) %>%
      mutate(sum=sum(count, na.rm=T),
             cnt=n()) 
    
    msa_onet_supply= msa_onet_supply %>%
      mutate(cnt_na=ifelse(is.na(count),1,0)) %>%
      group_by(msa_id, state_id, soc, year, onet_supply) %>%
      mutate(cnt_na=sum(cnt_na)) 
    
    msa_onet_supply= msa_onet_supply %>%
      mutate(onet_final_supply=ifelse(is.na(count), onet_supply, count/sum*onet_supply*(cnt-cnt_na)))
    
    if(sum_up_msa=="yes")
    {
      
      msa_onet_supply=data.table(msa_onet_supply)
      msa_onet_supply[, `:=` (msa_onet_sum = sum(onet_final_supply, na.rm=T)), 
                      by = c("state_id", "onet_code", "year")]
      
      
      msa_final= msa_onet_supply %>%
        mutate(msa_onet_supply=onet_final_supply/msa_onet_sum*state_onet_supply) %>%
        select(msa_id, onet_occupation_type_id, onet_code, year, msa_onet_supply) %>%
        rename(occupation_type_id=onet_occupation_type_id,
               tot_emp=msa_onet_supply,
               occupation_code=onet_code) %>%
        mutate(tot_emp=round(tot_emp,0)) %>%
        select(occupation_type_id, msa_id, year, occupation_code, tot_emp)
    }else
    {
      if(threshold_msa=="yes")
      {
        msa_onet_supply=data.table(msa_onet_supply)
        msa_onet_supply[, `:=` (msa_onet_sum = sum(onet_final_supply, na.rm=T)), 
                        by = c("state_id", "onet_code", "year")]
        
        
        msa_final= msa_onet_supply %>%
          mutate(msa_onet_supply=ifelse(state_onet_supply<msa_onet_sum, 
                                        onet_final_supply/msa_onet_sum*state_onet_supply,
                                        onet_final_supply)) %>%
          select(msa_id, onet_occupation_type_id, onet_code, year, msa_onet_supply) %>%
          rename(occupation_type_id=onet_occupation_type_id,
                 tot_emp=msa_onet_supply,
                 occupation_code=onet_code) %>%
          mutate(tot_emp=round(tot_emp,0)) %>%
          select(occupation_type_id, msa_id, year, occupation_code, tot_emp)
      }else
      {
        msa_onet_supply=data.table(msa_onet_supply)
        msa_final=msa_onet_supply %>%
          rename(occupation_type_id=onet_occupation_type_id,
                 tot_emp=onet_final_supply,
                 occupation_code=onet_code) %>%
          mutate(tot_emp=round(tot_emp,0)) %>%
          select(occupation_type_id, msa_id, year, occupation_code, tot_emp) 
      }
    }
    
    msa_final$tot_emp[is.na(msa_final$tot_emp)]=0
    for(i in min(msa_final$year):max(msa_final$year))
    {
      temp=msa_final %>%
        filter(year==i)
      write.csv(temp, paste0(output_path,"/msa_supply_output_",i,".csv"), row.names = F)
    }
    
    msa_rate=left_join(msa %>% 
                         group_by(msa_id, onet_code, local_occupation_code, year, adjusted_rate) %>%
                         summarise(n=n()) %>% select(-n) %>%
                         rename(msa_rate=adjusted_rate),
                       msa_final %>%
                         rename(onet_code=occupation_code,
                                onet_occupation_type_id=occupation_type_id,
                                msa_onet_supply=tot_emp),
                       by=c("msa_id", "onet_code", "year"))
    

    ###########warning message if LOC and SOC number doesnot match
    
    msa_loc_aggr=msa_supply_file %>%
      group_by(msa_id, year) %>%
      summarise(sum_loc_tot_emp=sum(tot_emp, na.rm=T))
    
    msa_soc_aggr= msa_final %>%
      group_by(msa_id, year) %>%
      summarise(sum_soc_tot_emp=sum(tot_emp, na.rm=T))
    
    msa_aggr= left_join( msa_loc_aggr, msa_soc_aggr, by=c("msa_id", "year"))
    
    msa_aggr= msa_aggr %>%
      mutate(diff=abs(sum_loc_tot_emp-sum_soc_tot_emp)) %>%
      mutate(diff_perc=diff/sum_loc_tot_emp*100)
    
    if(max(msa_aggr$diff_perc)>5)
    {
      winDialog(type = c("ok"), "There is difference between LOC and SOC for msa, please validate thoroughly the msa file.")
      print("There is difference between LOC and SOC for msa, please validate thoroughly msa file.")
    }
    ################################################################
    
    write.csv(msa_final, paste0(output_path,"/msa_supply_output.csv"), row.names = F)
    write.csv(msa_rate, paste0(output_path,"/ONET msa Rates and Supply.csv"), row.names = F)
    
    final_list[[length(final_list)+1]]=list(data.frame(msa_final), data.frame(msa_rate))
    names(final_list)[length(final_list)]="MSA"
    
    
  }
  
    final_list
}

