 #' Running Median Outlier Detection Technique (On Time Series Data)
 #'
 #' Call this function to flag outliers.
 #' The function will first compute Median in running windows on the target variable data point.
 #' Computes the difference between the Supply (target variable in this case) and Median
 #' calculates quartile in order to provide a threshold for comparison.
 #' The function flags outliers if the diff > quartile \cr
 #'
 #' NOTE1: This technique works best with non-seasonal data (Trend as well)\cr
 #' NOTE2: Running Median Window is fixed as 3 to avoid more imputations\cr
 #' NOTE3: If window is even, first and last two data points would have to be imputed
 #' Hence, to minimize unnecessary imputations, it is advised not to change the window\cr
 #' NOTE4: Quantile may be changed based on the data requirements\cr
 #'
 #' @param data: The Data Frame or Data Table containing all the required variables and data points
 #' @param supply: target Variable of which the Median will be computed and other operations will be performed
 #' @param key: Attributes on which the split happens. For ex, Country/State/MSA ID, Occupation ID etc
 #' @param year: Year attribute of the data frame
 #' @param qtr_key: Attribute containing the quarter values. NOTE: For yearly data, the quarter column may contain the same value throug out
 #' @param quantile: Compute any of 25\%  50\%  75\%  100\% quartile depending on the data requirements
 #' @param replace: In the final column, the outliers will be replaced by this variable's value. DEFAULTS to NA.
 #'
 #' @return spl_without_otl_runmed: The target attribute in which all the outliers will be replaced by the replace variable \cr
 #' @return otl_flag_runmed: Outlier flag by running median method. Outliers will be marked as 1.\cr
 #' @export
 #'


running_median_outlier <- function(data, supply, key, year, qtr_key, quantile = 0.75, replace=NA){
  data=data %>% arrange_at(vars(one_of(c(key, year, qtr_key))))
  data=data %>%
    mutate_(sup=supply) %>%
    group_by_at(vars(one_of(key)))
  
  data_null=data %>%
    filter(is.na(sup)) %>%
    mutate(spl_without_otl_runmed=NA,
           otl_flag_runmed=1) %>%
    select(-sup)
  
  data=data %>%
    filter(!is.na(sup))
  
  data=data %>%
    mutate(median = runmed(sup, 3),
           diff = abs(sup-median),
           quartile = as.numeric(quantile(diff, quantile)),
           spl_without_otl_runmed = sup,
           otl_flag_runmed = 0) %>% as.data.frame()
  data$spl_without_otl_runmed[data$diff > data$quartile] = replace
  data$spl_without_otl_runmed[is.na(data[,mget("supply")[[1]]])] = replace
  data$otl_flag_runmed[data$diff>data$quartile | is.na(data[,mget("supply")[[1]]])] = 1
  data$median=NULL
  data$sup=NULL
  data$quartile=NULL
  data$diff=NULL
  
  data=bind_rows(data, data_null)
  data=data %>% arrange_at(vars(one_of(c(key, year, qtr_key))))
  
  return(data)
}



