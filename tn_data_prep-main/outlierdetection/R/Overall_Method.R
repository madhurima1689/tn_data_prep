#########################################################################
#########################################################################
############################### OVERALL MAD  ############################
#########################################################################

#' Overall MAD Outlier Detection Technique
#' 
#' This funciton is called to get the outlier by overall MAD method.
#' MAD method will be applied on the complete series of target variable. 
#' The MAD values will be checked against the cutoff and marked as outlier.\cr
#' Please note: In this method, the trend and seasonality component is not considered.\cr
#' This method should never be used alone. One should always ensemble it in some or the other way with other methods.\cr\cr
#' ADVISE: Do not keep the cutoff too low; lower the cutoff, greater will be the loss in trend.
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
#' spl_without_otl_overallMAD: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_overallMAD: Outlier flag by overall MAD calculation method. Outliers will be marked as 1.

##function to calculate outlier by overall MAD method
overall_outlier=function(data, supply, key, year, qtr_key, cutoff=1.5, replace=NA, mad_const=1.4826)
{
  data=data %>% arrange_at(vars(one_of(c(key, year, qtr_key))))
  data=data %>%
    mutate_(sup=supply) %>%
    group_by_at(vars(one_of(key))) %>%
    mutate(MAD=abs((sup-median(sup,na.rm=TRUE))/mad(sup,constant=1.4826,na.rm=TRUE)),
           spl_without_otl_overallMAD=sup,
           otl_flag_overallMAD=0)
  data$spl_without_otl_overallMAD[data$MAD>cutoff | is.na(data[,mget("supply")[[1]]])]=replace
  data$otl_flag_overallMAD[data$MAD>cutoff | is.na(data[,mget("supply")[[1]]])]=1
  data$MAD=NULL
  data$sup=NULL
  data
}