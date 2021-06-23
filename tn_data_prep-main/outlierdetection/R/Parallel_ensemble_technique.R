#########################################################################
#########################################################################
######################## Parallel OTL Detection  ########################
#########################################################################


#' Parallel Ensemble Technique to detect Outliers
#'
#' In this perticular function, all the techniques will be applied on the initial set of target column,
#' then according to the rule specified the final flag will be marked.\cr
#' The weightage system in parallel method will enable us to set different kind of rules. For example:\cr
#' 1.	Mark a point as outlier if it is present in either decompose method or Rolling MAD with NAs or,
#' in both the methods of rolling outlier without NA and overall MAD; then the weights can be dec_wt=0.4,
#'  rMAD_wt=0.4, rMAD_nona_wt=0.2 and overallMAD=0.2 with threshold as 0.3\cr
#' 2.	Mark as point as outlier if it is in decosition method or rolling method without NAs,
#' then the weights can be as dec_wt=0.5, rMAD_wt=0.0, rMAD_nona_wt=0.5 and overallMAD=0.0 with
#' threshold as 0.4\cr
#' 3.	Mark the point as outlier if it is present in any method, ,
#' then the weights can be as dec_wt=0.25, rMAD_wt=0.25, rMAD_nona_wt=0.25 and overallMAD=0.25 with
#' threshold as 0.2.\cr\cr
#' To call any one particular method we can call this
#' function with that method's weight as 1 and all others method's weight as 0
#' @export
#' @param data: Th complete data frame or data table or tibble that contains the data
#' @param supply: Target attribute on which decomposition should happen
#' @param key: Key attributes in the table on which split should happen. For example: state_id, job_code_id
#' @param year: Year attribute of the data frame.
#' @param qtr_key: Attribute that contains the quarter value.
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
#' @param dec_wt: Weight that should be applied on otl_flag_dec to get the final flag weight.
#' @param rMAD_wt: Weight that should be applied on otl_flag_rMAD to get the final flag weight.
#' @param rMAD_nona_wt: Weight that should be applied on otl_flag_rMAD_nona to get the final flag weight.
#' @param overallMAD_wt: Weight that should be applied on otl_flag_overallMAD to get the final flag weight.
#' @param runmed_wt: Weight that should be applied on otl_flag_runmed to get the final flag weight.
#' @param runmed_replace: In the final column for decomposition method,
#' the outliers will be replaced by this variable's value. DEFAULT to NA.
#' @param runmed_quantile: Compute any of 25\% 50\% 75\% 100\% quartile depending on the data requirements
#' @param threshold: the threshold value greater than which the final weight will be marked as an outlier.
#' @return The dataset with few extra attributes.
#' If the weigth of a perticular method is 0, then attributes for that method will not be there.\cr
#' Technique related attributes will be:\cr
#' spl_without_otl_dec: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_dec: Outlier flag by decompose method. Outliers will be marked as 1.\cr
#' spl_without_otl_rMAD: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_rMAD: Outlier flag by rolling method. Outliers will be marked as 1.\cr
#' spl_without_otl_rMAD_nona: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_rMAD_nona: Outlier flag by rolling method without NAs. Outliers will be marked as 1.\cr
#' spl_without_otl_overallMAD: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_overallMAD: Outlier flag by overall MAD calculation method. Outliers will be marked as 1.\cr
#' spl_without_otl_runmed: The target attribute in which all the outliers will be replaced by the replace variable\cr
#' otl_flag_runmed: Outlier flag by running median method. Outliers will be marked as 1.\cr
#' Final variable which will be present in all the files\cr
#' pll_spl_without_otl: The final target variable without the outliers\cr
#' pll_wt: The calculated parallel weight\cr
#' pll_flag: The final flag to the outliers.
#' @example parallel_outlier_detection(data=italy_data_decomposed,key=c("CODPRO", "PROFM"), supply="tot_emp",
#' year="year", qtr_key = "qtr")
#' @example parallel_outlier_detection(data=italy data,key=c("CODPRO", "PROFM"), supply="tot_emp",
#' year="year", qtr_key = "qtr", dec_wt = 0, rMAD_wt = 0.5, rMAD_nona_wt = 0.35, overallMAD_wt = 0.15, threshold = 0.5)\cr
#' In this example the data may not be decomposed since the decomposition method is not desired hence dec_wt is 0.

parallel_outlier_detection=function(data, key, supply, year, qtr_key,
                                    dec_cutoff=1.5, dec_mad_const=1.4826, dec_replace=NA,
                                    rMAD_cutoff=1.5, rMAD_mad_const=1.4826, rMAD_replace=NA,
                                    rMAD_nona_cutoff=1.5, rMAD_nona_mad_const=1.4826, rMAD_nona_replace=NA,
                                    overall_MAD_cutoff=1.5, overall_MAD_mad_const=1.4826, overall_MAD_replace=NA,
                                    dec_wt=0.4, rMAD_wt=0.3, rMAD_nona_wt=0.2, overallMAD_wt=0.1,
                                    runmed_wt = 0.3, runmed_quantile = 0.75, runmed_replace = NA, threshold=0.3)
{
  txt=""
  if(dec_wt!=0)
  {
    print("Decomposing method started")
    data=decompose_outlier(data, supply=supply, key=key,  year=year, qtr_key=qtr_key,
                           cutoff=dec_cutoff, mad_const = dec_mad_const, replace=dec_replace)
    print("Outlier detected by decomposing method")
    if(nchar(txt)>0)
    {
      txt=paste0(txt,"+")
    }
    txt=paste0(txt,"(dec_wt*coalesce(otl_flag_dec,0))")
  }
  gc()

  if(rMAD_wt!=0)
  {
    print("Rolling Method started")
    data=rolling_outlier(data, supply=supply, key=key, year=year, qtr_key=qtr_key,
                         cutoff=rMAD_cutoff, mad_const = rMAD_mad_const, replace=rMAD_replace, nona=F)
    print("Outlier detected by rolling method")
    if(nchar(txt)>0)
    {
      txt=paste0(txt,"+")
    }
    txt=paste0(txt,"(rMAD_wt*coalesce(otl_flag_rMAD,0))")
  }

  gc()

  if(rMAD_nona_wt!=0)
  {
    print("Rolling Method without NAs started")
    data=rolling_outlier(data, supply=supply, key=key, year=year, qtr_key=qtr_key,
                         cutoff=rMAD_nona_cutoff, mad_const = rMAD_nona_mad_const, replace=rMAD_nona_replace, nona=T)
    print("Outlier detected by rolling method without NAs")
    if(nchar(txt)>0)
    {
      txt=paste0(txt,"+")
    }
    txt=paste0(txt,"(rMAD_nona_wt*coalesce(otl_flag_rMAD_nona,0))")
  }

  gc()

  if(overallMAD_wt!=0)
  {
    print("Overall MAD method started")
    data=overall_outlier(data, supply=supply, key=key,  year=year, qtr_key=qtr_key,
                         cutoff=overall_MAD_cutoff, mad_const = overall_MAD_mad_const, replace=overall_MAD_replace)
    print("Outlier detected by Overall MAD method")
    if(nchar(txt)>0)
    {
      txt=paste0(txt,"+")
    }
    txt=paste0(txt,"(overallMAD_wt*coalesce(otl_flag_overallMAD,0))")
  }

  gc()

  if(runmed_wt!=0)
  {
    print("Running Median method started")
    data=running_median_outlier(data, supply=supply, key=key,  year=year, qtr_key=qtr_key,
                                quantile = runmed_quantile, replace=runmed_replace)
    print("Outlier detected by Running Median method")
    if(nchar(txt)>0)
    {
      txt=paste0(txt,"+")
    }
    txt=paste0(txt,"(runmed_wt*coalesce(otl_flag_runmed,0))")
  }

  gc()

  data=data %>%
    mutate(pll_wt=eval(parse(text=txt))) %>%
    mutate(pll_flag=ifelse(pll_wt>threshold,1,0)) %>%
    mutate_(sup=supply)%>%
    mutate(pll_spl_without_otl=ifelse(pll_flag==1,NA,sup))%>%
    select(-sup)
  data
}
