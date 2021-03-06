#' Format the Cases and Deaths table for presentation in the Daily Trend Report!
#'
#' @param input_table
#'
#' @return
#' @export
#'
#' @examples
format_casedeath_table<-function(input_table){
  input_table<-input_table %>%
    mutate(National_Case_Proportion=percent(National_Case_Proportion,accuracy=0.1),
           National_Death_Proportion=percent(National_Death_Proportion,accuracy=0.1),
           Deaths_Daily_7MA=round(Deaths_Daily_7MA,1),
           Cases_Daily_7MA=ifelse(Cases_Daily_7MA<1, number(Cases_Daily_7MA, accuracy = 0.1), number(Cases_Daily_7MA, big.mark = ",")),
           Cases_7MA_per100k=round(Cases_7MA_per100k,digits = 1),
           Deaths_7MA_per100k=round(Deaths_7MA_per100k, digits = 2)) %>%
    PHACTrendR::turn_num_to_percent_change(numeric_variable="Weekly_Change_Cases") %>%
    PHACTrendR::turn_num_to_percent_change(numeric_variable="Weekly_Change_Deaths")

  ft <- flextable(input_table)
  ft <- color(ft, j = "Weekly_Change_Cases", i = ~ str_detect(Weekly_Change_Cases, "\\+"), color="red")
  ft <- color(ft, j = "Weekly_Change_Cases", i = ~ str_detect(Weekly_Change_Cases, "\\-"), color="green4")
  ft <- color(ft, j = "Weekly_Change_Deaths", i = ~ str_detect(Weekly_Change_Deaths, "\\+"), color="red")
  ft <- color(ft, j = "Weekly_Change_Deaths", i = ~ str_detect(Weekly_Change_Deaths, "\\-"), color="green4")
  ft <- set_header_labels(ft, Cases_Daily="Daily Cases", Cases_Daily_7MA="7 Day MA, Cases",Cases_7MA_per100k="7 Day cases MA per 100,000",Weekly_Change_Cases="Weekly Change in Cases",National_Case_Proportion="National Proportion of Cases",Deaths_Daily="Daily Deaths",Deaths_Daily_7MA="7 Day MA, Deaths",Deaths_7MA_per100k="7 Day deaths MA per 100,000",Weekly_Change_Deaths="Weekly Change in Deaths",National_Death_Proportion="National Proportion of Deaths")
  ft<-width(ft, j=1, width = 1.5)
  ft<-width(ft, j=2:ncol(input_table), width = 1)
  ft <- height(ft, height=.52, part = "header")
  ft <- height(ft, height=.26, part = "body")
  ft <- fontsize(ft, size = 12, part="all")
  ft <- fontsize(ft, j = 3:ncol(input_table),size = 14, part="body")
  ft <- align(ft, align = "center", part="header")
  ft <- align(ft, j = 1:2,align = "left", part="body")
  ft <- align(ft, j=3:ncol(input_table), align = "right", part="body")
  #Fill colours - first two columns in green
  ft<-bg(ft,j=1:2, bg="#9bbb59", part="header")
  ft<-bg(ft,i=seq(1,nrow(ft$body$dataset),2),j=1:2, bg="#d7e4bd")
  ft<-bg(ft,i=seq(2,nrow(ft$body$dataset),2),j=1:2, bg="#ebf1de")
  #columns 3-7 in blue
  ft<-bg(ft,j=3:7, bg="#17375e", part="header")
  ft<-bg(ft,i=seq(1,nrow(ft$body$dataset),2),j=3:7, bg="#b9cde5")
  ft<-bg(ft,i=seq(2,nrow(ft$body$dataset),2),j=3:7, bg="#dce6f2")
  #columns 8-12 in grey
  ft<-bg(ft,j=8:12, bg="#7f7f7f", part="header")
  ft<-bg(ft,i=seq(1,nrow(ft$body$dataset),2),j=8:12, bg="#d9d9d9")
  ft<-bg(ft,i=seq(2,nrow(ft$body$dataset),2),j=8:12, bg="#f2f2f2")
  #Header colour to white
  ft<-color(ft,color="white",part="header")
  big_border = fp_border(color="white", width=2)
  border_v = fp_border(color="white")
  border_h = fp_border(color="white")
  ft <- border_outer(ft, part = "all", border = big_border)
  ft <- border_inner_v(ft, part = "all", border = border_v)
  ft <- border_inner_h(ft, part = "all", border = border_h)

  ft_1 <- ft %>% footnote(., i=1, j=6, value = as_paragraph(
    c("Weekly change is how the current week (days 1 to 7 days ago) compares against the previous week (8 to 14 days ago).  e.g. previous week = 75 total cases; current week = 50 total cases; weekly percent change from previous week to current week = 33.3% decrease (-33.3%). 'NA' is used when there were no cases or deaths during either week, 'Inf' is used when there were is at least one case or death during the current week, but none the prior week"#,
      #"Source: Provincial and territorial website data")
    )),
    ref_symbols = c("*"),
    part = "header") %>%
    footnote(., i=1, j=11, value = as_paragraph(
      c("Weekly change is how the current week (days 1 to 7 days ago) compares against the previous week (8 to 14 days ago).  e.g. previous week = 75 total cases; current week = 50 total cases; weekly percent change from previous week to current week = 33.3% decrease (-33.3%). 'NA' is used when there were no cases or deaths during either week, 'Inf' is used when there were is at least one case or death during the current week, but none the prior week"#,
        #"Source: Provincial and territorial website data")
      )),
      ref_symbols = c("*"),
      part = "header") %>% merge_v(part="footer") %>%
    footnote(., value = as_paragraph(
      if(any_non_report_flag==FALSE){
        "All PTs provided daily updates today"
      } else {
        paste0("Note that daily updates were not provided by: ",turn_char_vec_to_comma_list(key_PTs_nonreport),". 7MA of cases and deaths for these PTs are based on the date of last report. For calculation of the national 7MA of cases and deaths, days after the date of last report for these PTs were omitted.")
      }),
      ref_symbols = c(""),
      part = "header") %>%

    footnote(., value = as_paragraph(
      paste0("Source: Provincial and territorial website data. ","Updated daily (Sun-Thurs). Data as of: ",max(Case_Death_Stats$Date))),
    ref_symbols = c(""),
    part = "header")

  return(ft_1)
}

#' Format the hospitalizations table for presentation in the Daily Trend Report!
#'
#' @param input_table
#'
#' @return
#' @export
#'
#' @examples
format_hospicu_table<-function(input_table){
  input_table<-input_table %>%
    mutate(Date=format(Date, "%B %d"))%>%
    PHACTrendR::turn_num_to_percent_change(numeric_variable = "delta7h") %>%
    PHACTrendR::turn_num_to_percent_change(numeric_variable = "delta7i")


  ft <- flextable(input_table)
  ft <- color(ft, j = "delta7h", i = ~ str_detect(delta7h, "\\+"), color="red")
  ft <- color(ft, j = "delta7h", i = ~ str_detect(delta7h, "\\-"), color="green4")
  ft <- color(ft, j = "delta7i", i = ~ str_detect(delta7i, "\\+"), color="red")
  ft <- color(ft, j = "delta7i", i = ~ str_detect(delta7i, "\\-"), color="green4")
  ft <- set_header_labels(ft, hosp7ma="Hospitalizations, 7 Day MA", delta7h="Weekly Change in Hospitalizations",icu7ma="ICU, 7 Day MA",delta7i="Weekly Change in ICU")
  ft<-width(ft, j=1, width = 2.5)
  ft<-width(ft, j=2, width = 1.25)
  ft<-width(ft, j=3:ncol(input_table), width = 1.5)
  ft <- height(ft, height=.39, part="header")
  ft <- height(ft, height=.26, part="body")
  ft <- fontsize(ft, size = 12, part="all")
  ft <- align(ft, align = "center", part="header")
  ft <- align(ft, j = 1:2,align = "left", part="body")
  ft <- align(ft, j=3:ncol(input_table), align = "right", part="body")

  #shading headers, and alternating rows.
  ft<-bg(ft, bg="#4f81bd", part="header")
  ft<-bg(ft,i=seq(1,nrow(ft$body$dataset),2), bg="#d0d8e8")
  ft<-bg(ft,i=seq(2,nrow(ft$body$dataset),2), bg="#e8edf4")
  ft<-color(ft,color="white",part="header")

  big_border = fp_border(color="black", width=2)
  border_v = fp_border(color="black")
  border_h = fp_border(color="black")

  ft <- border_outer(ft, part = "all", border = big_border)
  ft <- border_inner_v(ft, part = "all", border = border_v)
  ft <- border_inner_h(ft, part = "all", border = border_h)

  ft_1 <- footnote( ft, value = as_paragraph(
    c("Source: Provincial and territorial website data. When a PT does not report updated hospitalization/ICU numbers, the previous day's values are carried over.")),
    ref_symbols = c("")) %>%
    footnote(., value = as_paragraph(
      paste0("Updated Daily (Sun-Thurs). Data as of: ",format(max(Hosp_Metrics_Table$Date),"%B %d"))),
      ref_symbols = c(""),
      part = "header")



  return(ft_1)
}


#' Format the lab testing table for presentation in the Daily Trend Report!
#'
#' @param input_table
#'
#' @return
#' @export
#'
#' @examples
format_labtesting_table<-function(input_table){

  input_table <- input_table %>%
    mutate(across(contains("Average"),number, big.mark=","),
           across(contains("Percent"),label_percent(accuracy = 0.1)))%>%
    PHACTrendR::turn_num_to_percent_change(numeric_variable = "change_in_tests")%>%
    PHACTrendR::turn_num_to_percent_change(numeric_variable = "change_in_positivity")%>%
    rename(`Weekly Change in Tests` = change_in_tests,
           `Weekly Change in Percent Positivity` = change_in_positivity)


  ft <- flextable(input_table)
  ft<-width(ft, j=1, width = 2.75)
  ft<-width(ft, j=2:ncol(input_table), width = 1.5)
  ft <- height_all(ft, height=.26)
  ft <- fontsize(ft, size = 14, part="all")
  ft <- align(ft, align = "center", part="header")
  ft <- align(ft, j = 1,align = "left", part="body")
  ft <- align(ft, j=2:ncol(input_table), align = "right", part="body")

  big_border = fp_border(color="black", width=2)
  border_v = fp_border(color="black")
  border_h = fp_border(color="black")

  ft <- border_outer(ft, part = "all", border = big_border)
  ft <- border_inner_v(ft, part = "all", border = border_v)
  ft <- border_inner_h(ft, part = "all", border = border_h)

  #shading headers, and alternating rows.
  ft<-bg(ft, bg="#4f81bd", part="header")
  ft<-bg(ft,i=seq(1,nrow(ft$body$dataset),2), bg="#d0d8e8")
  ft<-bg(ft,i=seq(2,nrow(ft$body$dataset),2), bg="#e8edf4")
  ft<-color(ft,color="white",part="header")

  #commented out the below for this week as causing program to crash due to non-existent columns
  #add conditional colour to change in testing variable
  ft <- color(ft, j = "Weekly Change in Tests", i = ~ str_detect(`Weekly Change in Tests`, "\\-"), color="red")
  ft <- color(ft, j = "Weekly Change in Tests", i = ~ str_detect(`Weekly Change in Tests`, "\\+"), color="green4")
  ft <- color(ft, j = "Weekly Change in Percent Positivity", i = ~ str_detect(`Weekly Change in Percent Positivity`, "\\+"), color="red")
  ft <- color(ft, j = "Weekly Change in Percent Positivity", i = ~ str_detect(`Weekly Change in Percent Positivity`, "\\-"), color="green4")

  ft_1 <- footnote(ft, value = as_paragraph(
    paste0("Note: Lab testing numbers may vary slightly day to day as PTs continually update lab testing data. Updated Daily (Sun-Thurs). Data as of: ", format(max(SALT$update_date), "%B %d"))),
    ref_symbols = c(""),
    part = "header")

  return(ft_1)
}
