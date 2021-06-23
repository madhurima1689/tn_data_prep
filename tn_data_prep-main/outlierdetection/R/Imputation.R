#########################################################################
#########################################################################
######################## Missing Valu Imputation  ########################
#########################################################################

#' Missing value imputation for the dataset with sparse data
#' 
#' In this function, missing data will be imputated for the sparse dataset using the growth rate.
#' The dataset on which outlier detection is done should be imputed using ts impute. 
#' That will serve as the complete dataset for the imputation of the next dataset.
#' @export
#' @param complete: Th complete data frame with the imputated values from ts impute
#' @param incomp: The data that needs to be imputed.
#' @param digit_coding: A dataset that will contain all th elevel of job occupation. 
#' If NA, then the function will assume that 1level code is substring of 1 digit in occupation code, 
#' 2 level code is substring of first 2 digits and so on. DEFAULT to NA
#' @param digit_colnames: The column name of the 1 level code, 2 level code and so on in digit_coding dataset. 
#' Please maintain the chronological order
#' @param occ_code: Attribute that contains occupational code
#' @param key: Attributes that act as the key. Note: No need to add occupational code in this.
#' @param compl_supply: The column name of the supply column in complete dataset
#' @param incomp_supply: The column name of the supply column in incomplete dataset that needs to be imputed
#' @param qtr: Attribute with quarter value
#' @param year: Attribute with year value
#' @return The dataset with both the datasets and the supply columns imputed.



imputation=function(complete, incomp, digit_coding=NA, digit_colnames=NA, occ_code, key, compl_supply, incomp_supply, qtr, year)
{
  if(!(qtr %in% colnames(complete) & qtr %in% colnames(incomp)))
  {
    stop("Please make sure Quarter column name is same in both data set")
  }
  if(!(year %in% colnames(complete) & year %in% colnames(incomp)))
  {
    stop("Please make sure Year column name is same in both data set")
  }
  if(occ_code %in% key)
  {
    stop("Please remove occupation code from the key. It will be taken care wherever needed.")
  }
  
  
  if(!(occ_code %in% colnames(complete) & occ_code %in% colnames(incomp)))
  {
    stop("Please make sure the Occupation Code column name is same in both the dataset")
  }else
  {
    complete=complete %>%
      mutate_(job_code=occ_code,
              supply=compl_supply) %>%
      mutate(len_job_code=nchar(job_code))
    if(length(unique(complete$len_job_code))!=1)
    {
      stop("The length of job codes in Complete dataset varies.")
    } else
    {
      len_comp=unique(complete$len_job_code)
    }
    
    incomp=incomp %>%
      mutate_(job_code=occ_code,
              supply=incomp_supply) %>%
      mutate(len_job_code=nchar(job_code))
    if(length(unique(incomp$len_job_code))!=1)
    {
      stop("The length of job codes in Incomplete dataset varies.")
    }else
    {
      len_incomp=unique(incomp$len_job_code)
    }
    
    if(len_comp!=len_incomp)
    {
      stop("The length in both the dataset varies. Please make it uniform and try again.")
    }
    else if(len_comp==1)
    {
      stop("The Occupation code is at 1 Digit level. Make sure to impute we require job code at level greater than 1")
    }else
    {
      
      if(is.na(digit_coding))
      {
        digit_coding=data.frame()
        digit_coding=rbind(data.frame(complete %>% group_by(job_code) %>% summarise(cnt=1)),
                           data.frame(incomp %>% group_by(job_code) %>% summarise(cnt=1))) %>%
          select(-cnt) %>% unique()
        for(i in 1:(len_incomp))
        {
          var_digit=paste0("D",i,"_code")
          digit_coding=digit_coding %>%
            mutate(!!var_digit := substr(job_code,1,i))
        }
      }else if(is.na(digit_colnames))
      {
        stop("If the digit coding file is given then please give the colnames too.
             Please make sure first column is the name of 1 level code and so on")
      }else
      {
        print("Note: assuming the first colname given in digit colname will be name for attribute with first digit and so on")
        for(i in 1:(len_incomp))
        {
          var_digit=paste0("D",i,"_code")
          colnames(digit_coding)[colnames(digit_coding)==digit_colnames[i]]=var_digit
        }
      }
      
      ######## Calculating growth rate for the complete dataset
      dc_1=digit_coding %>% select(1,(ncol(digit_coding)-1))
      colnames(dc_1)[2]="job_code_less"
      complete= left_join(complete, dc_1, by="job_code")
      
      complete_gr=complete %>%
        group_by_at(vars(one_of(c(key, "job_code_less", year, qtr)))) %>%
        summarise(sum_supply=sum(supply, na.rm=T)) %>%
        group_by_at(vars(one_of(c(key, "job_code_less")))) %>%
        mutate(lagged=lag(sum_supply,1)) %>%
        mutate(gr=sum_supply/lagged-1) %>%
        select_at(vars(one_of(c(key, "job_code_less", year, qtr, "gr"))))
      
      incomp=left_join(incomp, dc_1, by="job_code")
      incomp=incomp %>%
        arrange_at(vars(one_of(c(key, "job_code", year, qtr)))) %>%
        group_by_at(vars(one_of(key, "job_code"))) %>%
        mutate(rn=row_number())
      
      incomp_gr=left_join(incomp, complete_gr, by=c(key, "job_code_less", year, qtr))
      
      #####Imputing by 3 digit growth rate
      for(i in 1:max(incomp_gr$rn))
      {
        incomp_gr=incomp_gr %>%
          group_by_at(vars(one_of(c(key, occ_code)))) %>%
          mutate(supply=ifelse(is.na(supply),(1+gr)*lag(supply,1) ,supply)) %>%
          arrange_at(vars(one_of(c(key, "job_code", year, qtr))))
      }
      
      incomp_gr=incomp_gr %>%
        arrange_at(vars(one_of(c(key, "job_code", year, qtr))), desc)
      
      for(i in 1:max(incomp_gr$rn))
      {
        incomp_gr=incomp_gr %>%
          group_by_at(vars(one_of(c(key, occ_code)))) %>%
          mutate(supply=ifelse(is.na(supply), lag(supply,1)/(1+lag(gr,1)) ,supply))
      }
      
      incomp_gr=incomp_gr %>%
        arrange_at(vars(one_of(c(key, "job_code", year, qtr))))
      
      #####################################################################
      #####################################################################
      #############################Second set of imputation
      complete=data.frame(complete)
      incomp_gr=data.frame(incomp_gr)
      comp_data=rbind(complete %>%
                        select_at(vars(one_of(c(key, "job_code", year, qtr, "supply")))),
                      incomp_gr %>%
                        select_at(vars(one_of(c(key, "job_code", year, qtr, "supply"))))
      )  %>%
        arrange_at(vars(one_of(c(key, "job_code", year, qtr))))
      
      comp_yr= comp_data %>%
        group_by_at(vars(one_of(key, "job_code", year))) %>%
        summarise(supply=mean(supply, na.rm=T))
      
      ###Make first set of final file
      
      comp_yr=left_join(comp_yr, digit_coding, by="job_code")
      
      final=comp_yr %>%
        mutate(icode=D1_code) %>%
        group_by_at(vars(one_of(key, "icode", year))) %>%
        summarise(supply=mean(supply, na.rm=T))
      
      if(sum(is.na(final$supply))!=0)
      {
        warning("Some of the counts at 1 digit level is NA, for those subsequent code there will not be any imputation")
      }
      
      min_yr=min(as.numeric(unlist(final[,mget("year")[[1]]])))
      max_yr=max(as.numeric(unlist(final[,mget("year")[[1]]])))
      
      
      for(i in 1:(len_comp-1))
      {
        #####Derive the growth rates on mean value calculated in final file
        comp_yr_li=final %>%
          group_by_at(vars(one_of(key, "icode"))) %>%
          mutate(lagged=lag(supply,1)) %>%
          mutate(gr=supply/lagged-1) %>%
          select_at(vars(one_of(key, "icode", year, "gr")))
        
        #####Get one level granular data set and join to get the growth rates
        comp_yr_li1=comp_yr %>%
          mutate_(icode=paste0("D",i,"_code"),
                  i1code=paste0("D",(i+1),"_code")) %>%
          group_by_at(vars(one_of(key, "icode", "i1code", year))) %>%
          summarise(supply=sum(supply, na.rm=T))
        
        comp_yr_li1$supply[comp_yr_li1$supply==0]=NA
        comp_yr_li1=left_join(comp_yr_li1, comp_yr_li,
                              by=c(key, "icode", year))
        
        comp_yr_li1$supply[comp_yr_li1$supply==0]=NA
        #### Derive value using growth rate for all the years
        for(j in min_yr:max_yr)
        {
          comp_yr_li1=comp_yr_li1 %>%
            group_by_at(vars(one_of(c(key, "i1code")))) %>%
            mutate(supply=ifelse(is.na(supply),(1+gr)*lag(supply,1) ,supply)) %>%
            arrange_at(vars(one_of(c(key, "i1code", year))))
          
        }
        
        comp_yr_li1=comp_yr_li1 %>%
          arrange_at(vars(one_of(c(key, "i1code", year))), desc)
        
        for(i in min_yr:max_yr)
        {
          comp_yr_li1=comp_yr_li1 %>%
            group_by_at(vars(one_of(c(key, "i1code")))) %>%
            mutate(supply=ifelse(is.na(supply), lag(supply,1)/(1+lag(gr,1)) ,supply))
        }
        
        comp_yr_li1=comp_yr_li1 %>%
          arrange_at(vars(one_of(c(key, "i1code", year))))
        
        final=comp_yr_li1 %>%
          select(-gr)
        final$icode=NULL
        colnames(final)[colnames(final)=="i1code"]="icode"
      }
      
      colnames(final)[colnames(final)=="icode"]=occ_code
      final=final %>%
        rename(tot_emp=supply) %>%
        mutate(tot_emp=round(tot_emp))
      final
    }
  }
}
