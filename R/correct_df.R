#' Hardcode changes to web scraped PT cases and deaths
#'
#'This function allows one to make corrections to webscraped case and death data with a more intuitive syntax.
#'
#' @param data              dataset you wish to correct. df_corrected data set as default
#' @param metric            choice between updating "cases" or "deaths" = other text inputs will be ignored
#' @param Jurisdiction      region that is being corrected. Only takes one input
#' @param correction_date   date that you want to make correction for
#' @param corrected_value   new input value
#'
#' @return
#' @export
#'
#' @examples
#'
correct_df<-function(data=df_corrected,metric="",Jurisdiction="",correction_date="",corrected_value=""){
  correction_date=as.Date(correction_date)
  if (metric=="cases"){
    data[data$prname==Jurisdiction & data$date==correction_date, "numtoday"]<-corrected_value
  }else if (metric=="deaths"){
    data[data$prname==Jurisdiction & data$date==correction_date, "numdeathstoday"]<-corrected_value
  }
  return(data)
}
