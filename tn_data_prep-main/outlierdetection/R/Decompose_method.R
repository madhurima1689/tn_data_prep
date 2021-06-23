#########################################################################
#########################################################################
###################### DECOMPOSE METHOD #################################
#########################################################################


#' Decompose the time series
#' 
#' This function is called once made sure that all quarters have atleast one data point for a series. 
#' It will be called by another method decompose() once all the checks are done.
#' It will first convert the target column into time series and then split it into:
#' Trend, 
#' Seasonality and
#' Random Noise\cr
#' NOTE: The function is not exported, because it should not be called.
#' @param data: A data frame or data table with data of just one series
#' @param supply: Target attribute on which decomposition should happen
#' @param year: Year attribute of the data frame.

#######################Decompose the time series into its component
decompose_ts= function(data, supply, year)
{
  data=data.frame(data)
  if(nrow(data)>0)
  {
    data[,mget("supply")[[1]]]=ts(data[,mget("supply")[[1]]], frequency = 4, start = min(data[,mget("year")[[1]]]))
    data$seasonal=stlplus(data[,mget("supply")[[1]]], s.window = "periodic", na.action = na.approx)$data[,"seasonal"]
    data$trend=stlplus(data[,mget("supply")[[1]]], s.window = "periodic", na.action = na.approx)$data[,"trend"]
    data$remainder=stlplus(data[,mget("supply")[[1]]], s.window = "periodic", na.action = na.approx)$data[,"remainder"]
    data[,mget("supply")[[1]]]=as.numeric(data[,mget("supply")[[1]]])
    data
  }
}


#' A Decompose Time Series Method
#' 
#' User calls this method to decompose the time series into trend, seasonality and random noise.
#' The function will check if all the quarters in each key has atleast a value; if not it will throw an error.
#' The function will split the complete data according to its key and call decompose_ts() to decompose the time series.
#' @param data: Complete data in a data frame or a data table or a tibbe which needs to be decomposed
#' @param supply: Target attribute on which decomposition should happen
#' @param key: Key attributes in the table on which split should happen. For example: state_id, job_code_id
#' @param year: Year attribute of the data frame.
#' @param qtr_key: Attribute that contains the quarter value.
#' @return A data frame with attribute:\cr
#' trend: Trend component of the data\cr
#' seasonal: Seasonal component of the data\cr
#' remainder: Random Noise of the time series
#' @export
#' @example decompose(data=italy_data, "tot_emp", c("CODPRO", "PROFM"), year="year", qtr_key = "qtr")

####Call this function to decompose.
decompose=function(data, supply, key, year, qtr_key)
{
  columns <- c(key, qtr_key)
  
  ##It checks if there are data ppoints in each quarter
  data_comp_qtr_data_check=data %>%
    mutate_(sup=supply) %>%
    group_by_at(vars(one_of(columns))) %>%
    summarise(check_qtr=ifelse(is.na(mean(sup, na.rm=T)),0,1)) %>%
    group_by_at(vars(one_of(key))) %>%
    summarise(check=sum(check_qtr)) 
  if(min(data_comp_qtr_data_check$check)<4)
  {
    stop("Please make sure atleast one data point is there in each quarter")
  }
  else
  {
    data=data %>% arrange_at(vars(one_of(c(key, year, qtr_key))))
    ##Split according to key and decompose the data
    dec_data= lapply(split(data, data[,key]), decompose_ts, supply=supply, year)
    dec_data= bind_rows(dec_data) 
    dec_data
  }
}


#' Detect Outlier by decompose method
#' 
#' This funciton is called to get the outlier by decomposition method.
#' To run this function, the data should already be decomposed. Use decompose_ts() method to decompose the series.
#' The function will group the data based on the key and then try Mean Absolute Deviation method to detect outlier.
#' MAD method will be applied on the complete series of random noise. 
#' The MAD values will be checked against the cutoff and marked as outlier\cr
#' Please note: In this method, if there are less than two data points for a perticular quarter, 
#' then the seasonality component of that perticular quarter may not be correct. 
#' Precautions should be taken for such cases.\cr
#' If there are many such cases then this should be emsembled with other methods in some way.
#' @export
#' @param data: Th complete data frame or data table or tibble that contains the data
#' @param supply: Target attribute on which decomposition should happen
#' @param key: Key attributes in the table on which split should happen. For example: state_id, job_code_id
#' @param year: Year attribute of the data frame.
#' @param qtr_key: Attribute that contains the quarter value.
#' @param cutoff: The cutoff values against which the deviation will be checked. 
#' If the computed ratio is less than the cutoff, then the point if not an outlier. DEFAULT to 1.5
#' @param mad_const: Scale factor to calculate MAD values to maintain consistency. DEFAULT to 1.4826
#' @param replace: In the final column, the outliers will be replaced by this variable's value. DEFAULT to NA.
#' @return A data frame with two additional attributes:\cr
#' spl_without_otl_dec: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_dec: Outlier flag by decompose method. Outliers will be marked as 1.

###########################Function to detect outlier
decompose_outlier=function(data, supply, key, year, qtr_key, cutoff=1.5, mad_const=1.4826, replace=NA)
{
  data=data.frame(data)
  
  data=decompose(data, supply, key, year, qtr_key)
  clnm=colnames(data)
  #The function will run only when the time series is decomposed
  if("remainder" %in% clnm)
  {
    data=data %>% arrange_at(vars(one_of(c(key, year, qtr_key))))
    data=data %>%
      group_by_at(vars(one_of(key))) %>%
      mutate(MAD=abs((remainder-median(remainder,na.rm=TRUE))/mad(remainder,constant=mad_const,na.rm=TRUE)),
             otl_flag_dec=0) %>%
      mutate_(spl_without_otl_dec=supply)
    ##Flagging the identified indices as outliers
    data$spl_without_otl_dec[data$MAD>cutoff | is.na(data[,mget("supply")[[1]]])]=replace
    data$otl_flag_dec[data$MAD>cutoff | is.na(data[,mget("supply")[[1]]])]=1
    data$MAD=NULL
    data
  }
  else
  {
    stop("Please decompose before detecting outlier.")
  }
}
