
#########################################################################
#########################################################################
############ ROLLING OUTLIER METHODS(with and without NAs) ##############
#########################################################################


#' MAD Outlier Function
#' 
#'  This function just calculates the mad ratios and return the replaced series.
#'  It is called by rollingout() to get the results.\cr
#' NOTE: The function is not exported, because it should not be called.
#' @param data: Target attribute on which MAD should be calculated
#' @param cutoff: The cutoff values against which the deviation will be checked. 
#' If the computed ratio is less than the cutoff, then the point if not an outlier. 
#' @param mad_const: Scale factor to calculate MAD values to maintain consistency. 
#' @param replace: In the final column, the outliers will be replaced by this variable's value.


#################Outlier detection using MAD
mad_outliers= function(data, cutoff, replace, mad_const){
  MADval=abs((data-median(data,na.rm=TRUE))/mad(data,constant=mad_const, na.rm=TRUE))
  data[MADval>cutoff]=replace
  return(round(data, digits=2))
}


#' Rollinf Outlier Detection technique
#' 
#' In this function, a time series for a perticular key is sent. 
#' This series is then grouped in 4 quarters and rolled by two quarters.
#' For each group MAD values are calculated.
#' Apart from the first 2 quarters and last 2 quarters, 
#' for all the other quarter a point will be qualified as outlier only when if is marked in both the groups.\cr
#' The same function can be used to find the outliers by rolling method without NAs by triggering the parameter nona.\cr
#' NOTE: This function is exported, but great care should be taken while using it. 
#' Only one time series should be sent to the function to get proper results.
#' @export
#' @param z: The data frame or data table or tibble that contains the data for one time series.
#' @param supply: Target attribute on which decomposition should happen.
#' @param cutoff: The cutoff values against which the deviation will be checked. 
#' If the computed ratio is less than the cutoff, then the point if not an outlier. DEFAULT to 1.5
#' @param mad_const: Scale factor to calculate MAD values to maintain consistency. DEFAULT to 1.4826
#' @param nona: The variable that can trigger whethere to include NAs in roling technique or not. DEFAULT to False.
#' @param replace: In the final column, the outliers will be replaced by this variable's value. DEFAULT to NA.
#' @return A data frame with two additional attributes:\cr
#' spl_without_otl_rMAD<_nona>: The target attribute in which all the outliers will be replaced by the replace variable
#' otl_flag_rMAD<_nona>: Outlier flag by rolling method. Outliers will be marked as 1.

###Rolling Outlier Detection Step
rollingout=function(z, supply, cutoff=1.5, replace=NA, mad_const=1.4826, nona=F){
  z=as.data.frame(z)
  ##The block is for rolling method without NAs
  if(nona & nrow(z)>0)
  {
    temp=z
    temp_colnames=colnames(temp)
    z=z %>%
      filter(!is.na(!!sym(supply)))
    z$spl_without_otl_rMAD_nona=NA
    z$otl_flag_rMAD_nona=NA
  }
  if (nrow(z)>0)
  {
    z1=z[,mget("supply")[[1]]]
    if(length(z1)%%2==1)
    {
      z1=c(z1,NA)
    }
    ##Applying rolling outlier detection in 1st set
    z2=rollapply(z1,width=4,by=4,FUN=mad_outliers, cutoff=cutoff, mad_const=mad_const, replace=replace)
    z2=as.vector(t(z2))
    index1=which(is.na(z2))
    
    ##Applying rolling outlier detection in the 2nd set
    z3=rollapply(z1[-c(1:2)],width=4,by=4,FUN=mad_outliers, cutoff=cutoff, mad_const=mad_const, replace=replace)
    z3=as.vector(t(z3))
    index2=which(is.na(z3))
    index2=index2+2
    ##Identifying outliers for both the sets and for the first 2 and last 2 observations
    finalindex=unique(c(index1[which(index1 %in% index2)],
                        index1[which(index1 %in% c(1,2,nrow(z)-1,nrow(z)))],
                        index2[which(index2 %in% c(1,2,nrow(z)-1,nrow(z)))]))
    ##Flagging the identified indices as outliers
    
    
    if(nona)
    {
      z=z %>%
        mutate_(spl_without_otl_rMAD_nona=supply) %>%
        mutate(otl_flag_rMAD_nona=0)
      z$spl_without_otl_rMAD_nona[finalindex]=replace
      z$otl_flag_rMAD_nona[finalindex]=1
      ##Left join to retain back the same table with NAs
      z=left_join(temp,z,by=temp_colnames)
      ##If the values were NA, then mark it as 1
      z$otl_flag_rMAD_nona[is.na(z$otl_flag_rMAD_nona)]=1
    }
    else
    {
      z=z %>%
        mutate_(spl_without_otl_rMAD=supply) %>%
        mutate(otl_flag_rMAD=0)
      z$spl_without_otl_rMAD[finalindex]=replace
      z$otl_flag_rMAD[finalindex]=1
    }
    z
  }
} 



#' Rooling Outlier Function
#' 
#' This function is called by the user to get the outlier by rolling methodology including or excluding NAs.
#' The data set is split on the key and sent to the rollingout() for further processing.
#' Please note: In this method, if there are less than two data points in a perticular set of 4 quarters, 
#' then even if the points are outliers they are not marked. 
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
#' @param nona: The variable that can trigger whethere to include NAs in roling technique or not. DEFAULT to False.
#' @return A data frame with two additional attributes:\cr
#' spl_without_otl_rMAD<_nona>: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_rMAD<_nona>: Outlier flag by rolling method. Outliers will be marked as 1.

##Call this function to perform rolling method. nona variable will decide with or without NAs
rolling_outlier=function(data, supply, key, year, qtr_key, cutoff=1.5, replace=NA, mad_const=1.4826, nona=F)
{
  data=data %>% arrange_at(vars(one_of(c(key, year, qtr_key))))
  roll_data= lapply(split(data, data[,key]), rollingout, 
                    supply=supply, cutoff=cutoff, replace=replace, mad_const=mad_const, nona=nona)
  roll_data= bind_rows(roll_data) 
  roll_data=roll_data %>% arrange_at(vars(one_of(c(key, year, qtr_key))))
  roll_data
}

