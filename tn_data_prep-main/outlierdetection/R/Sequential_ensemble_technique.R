#########################################################################
#########################################################################
######################## Sequential OTL Detection  ########################
#########################################################################

#' Sequential Ensemble Technique to detect Outlier
#'
#' In this perticular function, all the techniques will be applied in a sequential manner, that means the first method
#' will be applied on the initial set of target data, then the second method on the ouput of the first tecnique and so on.
#' The sequence can be given in any combination and permutation.\ The code for the methods are as follows:\cr
#' 1. Decomposiiton Method\cr
#' 2. Rolling Quarter Mehthod\cr
#' 3. Rolling Quarter Mehthod without NAs\cr
#' 4. Overall MAD method.\cr
#' 5. Running Median method \cr\cr
#' To call any one particular method we can call this
#' function with with only that methods number in sequence
#' @export
#' @param data: Th complete data frame or data table or tibble that contains the data
#' @param supply: Target attribute on which decomposition should happen
#' @param key: Key attributes in the table on which split should happen. For example: state_id, job_code_id
#' @param year: Year attribute of the data frame.
#' @param qtr_key: Attribute that contains the quarter value.
#' @param seq: This variable will get the sequence of the methods to be used. DEFAULT to (1,2,3,4,5)
#' @param dec_cutoff: The cutoff values against which the deviation will be checked for decomposition method.
#' If the computed ratio is less than the cutoff, then the point if not an outlier. DEFAULT to 1.5
#' @param dec_mad_const: Scale factor to calculate MAD values in decomposition method to maintain consistency.
#' DEFAULT to 1.4826
#' @param dec_replace: In the final column for decomposition method,
#' the outliers will be replaced by this variable's value. DEFAULT to NA.
#' @param rMAD_cutoff: The cutoff values against which the deviation will be checked for rolling MAD method.
#' If the computed ratio is less than the cutoff, then the point if not an outlier. DEFAULT to 1.5
#' @param rMAD_mad_const: Scale factor to calculate MAD values in rolling MAD method to maintain consistency.
#' DEFAULT to 1.4826
#' @param rMAD_replace: In the final column for rolling MAD method,
#' the outliers will be replaced by this variable's value. DEFAULT to NA.
#' @param rMAD_nona_cutoff: The cutoff values against which the deviation will be checked for rolling MAD method without NAs.
#' If the computed ratio is less than the cutoff, then the point if not an outlier. DEFAULT to 1.5
#' @param rMAD_nona_mad_const: Scale factor to calculate MAD values in rolling MAD method without NAs to maintain consistency.
#' DEFAULT to 1.4826
#' @param rMAD_nona_replace: In the final column for rolling MAD method without NAs,
#' the outliers will be replaced by this variable's value. DEFAULT to NA.
#' @param overallMAD_cutoff: The cutoff values against which the deviation will be checked for overall MAD method.
#' If the computed ratio is less than the cutoff, then the point if not an outlier. DEFAULT to 1.5
#' @param overallMAD_mad_const: Scale factor to calculate MAD values in overall MAD method to maintain consistency.
#' DEFAULT to 1.4826
#' @param overallMAD_replace: In the final column for overall MAD method,
#' the outliers will be replaced by this variable's value. DEFAULT to NA.
#' @param runmed_replace: In the final column for decomposition method,
#' the outliers will be replaced by this variable's value. DEFAULT to NA.
#' @param runmed_quantile: Compute any of 25\%  50\%  75\%  100\% quartile depending on the data requirements
#' @return The dataset with few extra attributes.
#' If the technique number is not present in the parameter seq, then attributes for that method will not be there.\cr
#' Technique related attributes will be:\cr
#' spl_without_otl_dec: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_dec: Outlier flag by decompose method. Outliers will be marked as 1.\cr
#' spl_without_otl_rMAD: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_rMAD: Outlier flag by rolling method. Outliers will be marked as 1.\cr
#' spl_without_otl_rMAD_nona: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_rMAD_nona: Outlier flag by rolling method without NAs. Outliers will be marked as 1.\cr
#' spl_without_otl_overallMAD: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_overallMAD: Outlier flag by overall MAD calculation method. Outliers will be marked as 1.\cr\cr
#' spl_without_otl_runmed: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_runmed: Outlier flag by running median method. Outliers will be marked as 1.\cr
#' seq_spl_without_otl: The final target variable without the outliers\cr
#' seq_flag: The final flag to the outliers.\cr
#' Final variable which will be present in all the files\cr
#' @example seq_test=seq_outlier_detection(data=data_comp2,key=c("CODPRO", "PROFM"), supply="tot_emp",
#' year="year", qtr_key = "qtr", seq=c(4,1))\cr
#' Here just the overall MAD and decomposition method will run in sequence


#1.decompose
#2.rolling qtr
#3.rolling qtr without NAs
#4.overall MAD
#5.running median
seq_outlier_detection=function(data, key, supply, year, qtr_key, seq=c(1,2,3,4,5),
                               dec_cutoff=1.5, dec_mad_const=1.4826, dec_replace=NA,
                               rMAD_cutoff=1.5, rMAD_mad_const=1.4826, rMAD_replace=NA,
                               rMAD_nona_cutoff=1.5, rMAD_nona_mad_const=1.4826, rMAD_nona_replace=NA,
                               overall_MAD_cutoff=1.5, overall_MAD_mad_const=1.4826, overall_MAD_replace=NA,
                               runmed_replace=NA, runmed_quantile = 0.75)
{

  if(sum(ifelse(grepl( '[0-5]', seq),0,1))>0)
  {
    stop("In sequence number 1 to 5 is acceptable.
         1. Decompose method,
         2. Rolling quarters using MAD,
         3. Rolling quarters without NAs using MAD,
         4. Overall MAD and
         5. Running Median")
  }
  else
  {
    data=data.frame(data)
    data$seq_supply_without_otl=data[,mget("supply")[[1]]]
    data$seq_flag=0
    data$seq_flag[is.na(data$seq_supply_without_otl)]=1
    for(s in seq)
    {
      if(s==1)
      {
        print("Decomposing method started")
        data=decompose_outlier(data, supply="seq_supply_without_otl", key=key,  year=year, qtr_key=qtr_key,
                               cutoff=dec_cutoff, mad_const = dec_mad_const, replace=dec_replace)
        data$seq_supply_without_otl=data$spl_without_otl_dec
        data$seq_flag=data$otl_flag_dec
        print("Outlier detected by decomposing method")

        gc()
      }
      else if(s==2)
      {
        print("Rolling Method started")
        data=rolling_outlier(data, supply="seq_supply_without_otl", key=key, year=year, qtr_key=qtr_key,
                             cutoff=rMAD_cutoff, mad_const = rMAD_mad_const, replace=rMAD_replace, nona=F)
        data$seq_supply_without_otl=data$spl_without_otl_rMAD
        data$seq_flag=data$otl_flag_rMAD
        print("Outlier detected by rolling method")

        gc()
      }
      else if(s==3)
      {
        print("Rolling Method without NAs started")
        data=rolling_outlier(data, supply="seq_supply_without_otl", key=key, year=year, qtr_key=qtr_key,
                             cutoff=rMAD_nona_cutoff, mad_const = rMAD_nona_mad_const,
                             replace=rMAD_nona_replace, nona=T)
        data$seq_supply_without_otl=data$spl_without_otl_rMAD_nona
        data$seq_flag=data$otl_flag_rMAD_nona
        print("Outlier detected by rolling method without NAs")

        gc()
      }
      else if(s==4)
      {
        print("Overall MAD method started")
        data=overall_outlier(data, supply="seq_supply_without_otl", key=key,  year=year, qtr_key=qtr_key,
                             cutoff=overall_MAD_cutoff, mad_const = overall_MAD_mad_const, replace=overall_MAD_replace)
        data$seq_supply_without_otl=data$spl_without_otl_overallMAD
        data$seq_flag=data$otl_flag_overallMAD
        print("Outlier detected by Overall MAD method")

        gc()
      }
      else if(s==5)
      {
        print("Running Median method started")
        data=running_median_outlier(data, supply="seq_supply_without_otl", key=key,  year=year, qtr_key=qtr_key,
                                    quantile = runmed_quantile, replace=runmed_replace)
        data$seq_supply_without_otl=data$spl_without_otl_runmed
        data$seq_flag=data$otl_flag_runmed
        print("Outlier detected by Running Median method")

        gc()
      }
    }
    data
  }
  }
