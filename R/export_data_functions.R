#export_data_functions
# should add validation steps so that if a wrong "input_data" value is given, previously correct export is not overwritten


export_qry_lab_onset<-function(input_data){
  write.csv(qry_lab_onset, 'Y:\\PHAC\\IDPCB\\CIRID\\VIPS-SAR\\EMERGENCY PREPAREDNESS AND RESPONSE HC4\\EMERGENCY EVENT\\WUHAN UNKNOWN PNEU - 2020\\EPI SUMMARY\\Trend analysis\\_Current\\Trend Report\\rmd\\onset.csv')
}


