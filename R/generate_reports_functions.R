# Generate reports functions

#' Generating the daily trend report
#'
#' @param report_date
#'
#' @return
#' @export
#'
#' @examples
generate_trend_report<-function(report_date=""){

  if (report_date==""){
    report_date<-format(Sys.Date(), "%d%b%Y")
  }
  library("rmarkdown")
  setwd("C:/rmd/")

  rmarkdown::render('Trend_Report.rmd',
                    output_file = paste0('DailyTrendReport_', report_date,'.pptx'),
                    envir = parent.frame())
}

############################################################################################################################################ #
############################################################################################################################################ #

#' Generating the Nemer report
#'
#' @param report_date
#'
#' @return
#' @export
#'
#' @examples
generate_Nemer_report<-function(report_date=""){

  if (report_date==""){
    report_date<-format(Sys.Date(), "%d%b%Y")
  }
  library("rmarkdown")
  setwd("C:/rmd/")
  #eventually, can write to: Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Nemer Report

  input_params<-list(report="Nemer")
  #eventually, can submit report_date as a param function

  rmarkdown::render('Trend_Report.rmd',
                    output_file = paste0('TrendReport_', report_date,'.pptx'),
                    params = input_params,
                    envir = parent.frame())

}
