#import data functions


#' Import raw web-scraped case and death numbers, and apply hardcode corrections
#'
#'
#'
#'
#' @return
#' @export
#'
#' @examples
import_raw_case_death_data<-function(){
  df_raw <- readr::read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv") %>%
    dplyr::mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
    rename(Jurisdiction=prname,
           Jurisdiction_FR=prnameFR)
  return(df_raw)
}


#' Import corrected web-scraped case and death numbers, and apply hardcode corrections
#'
#' Corrections for data dumps, etc. Note - don't use cumulative totals from this, as they are missing some cases that could not be assigned specific dates.
#'
#'
#'
#' @return
#' @export
#'
#' @examples
import_adjusted_case_death_data<-function(){
df_raw <- readr::read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv") %>%
  dplyr::mutate(date = as.Date(date, format = "%d-%m-%Y")) %>%
  rename(Jurisdiction=prname,
         Jurisdiction_FR=prnameFR)


#Removing trailing unreported days from PTs, in the past 7 days.
df<-df_raw %>%
  dplyr::group_by(Jurisdiction) %>%
  dplyr::filter(!(!Jurisdiction %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  dplyr::filter(!(!Jurisdiction %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  dplyr::filter(!(!Jurisdiction %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  dplyr::filter(!(!Jurisdiction %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  dplyr::filter(!(!Jurisdiction %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  dplyr::filter(!(!Jurisdiction %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE)) %>%
  dplyr::filter(!(!Jurisdiction %in% c("Canada", "Repatriated travellers")&date==max(date)&update==FALSE))

df_corrected<-df %>%
  select(-numtotal,-numdeaths)

########### Hard-coded manual corrections
# May 3rd, extra 1317 cases from April were reported in QC, real value should be 892 (https://www.cbc.ca/news/canada/montreal/covid-19-quebec-may-3-1.5553881)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Quebec",correction_date = "2020-05-03",corrected_value = 892)
# May 31st, extra 165 cases were reported in QC, real value should be 37 (https://montreal.ctvnews.ca/quebec-records-37-new-covid-19-deaths-but-adds-165-that-weren-t-recorded-1.4962370)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Quebec",correction_date = "2020-05-31",corrected_value = 37)

# #data dump of deaths in Ontario on October 2-4
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Ontario",correction_date = "2020-10-02",corrected_value = 2)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Ontario",correction_date = "2020-10-03",corrected_value = 4)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Ontario",correction_date = "2020-10-04",corrected_value = 4)
#Oct10-12 weekend
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "British Columbia",correction_date = "2020-10-10",corrected_value = 170)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "British Columbia",correction_date = "2020-10-11",corrected_value = 159)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "British Columbia",correction_date = "2020-10-12",corrected_value = 119)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "British Columbia",correction_date = "2020-10-13",corrected_value = 101)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Alberta",correction_date = "2020-10-10",corrected_value = 236)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Alberta",correction_date = "2020-10-11",corrected_value = 260)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Alberta",correction_date = "2020-10-12",corrected_value = 246)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Alberta",correction_date = "2020-10-13",corrected_value = 220)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Ontario",correction_date = "2020-10-12",corrected_value = 807)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Ontario",correction_date = "2020-10-13",corrected_value = 736)
# cases over Xmas and NY 2020
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Quebec",correction_date = "2020-12-25",corrected_value = 2246)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Quebec",correction_date = "2020-12-26",corrected_value = 2246)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Manitoba",correction_date = "2020-12-25",corrected_value = 173.66)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Manitoba",correction_date = "2020-12-26",corrected_value = 173.67)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Manitoba",correction_date = "2020-12-27",corrected_value = 173.67)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Manitoba",correction_date = "2021-01-01",corrected_value = 163)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Manitoba",correction_date = "2021-01-02",corrected_value = 163)
#deaths over Xmas and NY 2020
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-25",corrected_value = 3.25)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-26",corrected_value = 3.25)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-27",corrected_value = 3.25)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-28",corrected_value = 3.25)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Ontario",         correction_date = "2020-12-25",corrected_value = 40.5)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Ontario",         correction_date = "2020-12-26",corrected_value = 40.5)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Manitoba",        correction_date = "2020-12-25",corrected_value = 9.33)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Manitoba",        correction_date = "2020-12-26",corrected_value = 9.33)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Manitoba",        correction_date = "2020-12-27",corrected_value = 9.34)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-01",corrected_value = 11.25)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-02",corrected_value = 11.25)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-03",corrected_value = 11.25)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-04",corrected_value = 11.25)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Alberta",         correction_date = "2020-12-31",corrected_value = 19.2)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Alberta",         correction_date = "2021-01-01",corrected_value = 19.2)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Alberta",         correction_date = "2021-01-02",corrected_value = 19.2)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Alberta",         correction_date = "2021-01-03",corrected_value = 19.2)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Alberta",         correction_date = "2021-01-04",corrected_value = 19.2)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Manitoba",        correction_date = "2021-01-01",corrected_value = 5.5)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Manitoba",        correction_date = "2021-01-02",corrected_value = 5.5)
#Weekend Jan23-24 stuff
#NOTE: We are excluding 380 cases AB reported on Jan25 as they were from "previous weeks". Not sure where to reassign them at the moment but should figure out a better solution.
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-23",corrected_value = 8.66)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British columbia",correction_date = "2021-01-24",corrected_value = 8.67)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-25",corrected_value = 8.67)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Alberta",correction_date = "2021-01-25",corrected_value = 360)
#Weekend Jan30-31stuff (21 deaths reported on Feb.1 after not reporting over the weekend)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-30",corrected_value = 7)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-31",corrected_value = 7)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-01",corrected_value = 7)
#Weekend Feb6-7stuff (13 deaths reported on Feb.8 after not reporting over the weekend)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-06",corrected_value = 4.33)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-07",corrected_value = 4.33)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-08",corrected_value = 4.34)
#Feb15+16 death corrections
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Manitoba",correction_date = "2021-02-15",corrected_value = 2)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Manitoba",correction_date = "2021-02-16",corrected_value = 2)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Saskatchewan",correction_date = "2021-02-15",corrected_value = 1.5)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Saskatchewan",correction_date = "2021-02-16",corrected_value = 1.5)

#getting corrected values for the national number now
can_corrected_case_death<-df_corrected %>%
  filter(!Jurisdiction=="Canada") %>%
  select(Jurisdiction, date, numtoday, numdeathstoday) %>%
  group_by(date) %>%
  summarise(can_numtoday=sum(numtoday, na.rm = TRUE),
            can_deathstoday=sum(numdeathstoday, na.rm = TRUE)) %>%
  ungroup()

df_corrected2<-df_corrected %>%
  dplyr::left_join(can_corrected_case_death, by = "date") %>%
  dplyr::mutate(numtoday=ifelse(Jurisdiction=="Canada", can_numtoday, numtoday),
         numdeathstoday=ifelse(Jurisdiction=="Canada",can_deathstoday,numdeathstoday),
         numtoday=as.numeric(numtoday),
         numdeathstoday=as.numeric(numdeathstoday)) %>%
  dplyr::select(-can_numtoday, -can_deathstoday)%>%
  dplyr::group_by(Jurisdiction)%>%
  dplyr::mutate(numtotal=cumsum(numtoday),
         numdeaths=cumsum(numdeathstoday))

return(df_corrected2)
}


#' Import international data
#'
#' @return
#' @export
#'
#' @examples
import_international_data<-function(){
df_int <- readr::read_csv('https://covid.ourworldindata.org/data/owid-covid-data.csv') %>%
  dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d"))
return(df_int)
}



#' get demographic data
#'
#' @return
#'
#' list with 2 values. First value: latest_can_pop. Second value: pt_pop_20
#'
#' @export
#'
#' @examples
import_demographic_data<-function(){
pt_pop_raw <- cansim::get_cansim("17-10-0005-01")

#Filter to latest year of data
latest_can_pop <- pt_pop_raw %>%
  dplyr::mutate(REF_DATE=as.numeric(REF_DATE)) %>%
  dplyr::filter(`Age group`=="All ages",REF_DATE==max(REF_DATE),Sex=="Both sexes") %>%
  dplyr::select(GEO,VALUE) %>%
  dplyr::rename(Population=VALUE,
                Jurisdiction=GEO)


#Deriving population 20 year age groups
pt_pop20 <- pt_pop_raw %>%
  dplyr::mutate(REF_DATE=as.numeric(REF_DATE)) %>%
  dplyr::filter(str_detect(`Age group`, "to|90 years and over"),REF_DATE==max(REF_DATE),Sex=="Both sexes") %>%
  dplyr::mutate(age_group_20=case_when(`Age group`=="0 to 4 years" ~ "0 to 19",
                                `Age group`=="5 to 9 years" ~ "0 to 19",
                                `Age group`=="10 to 14 years" ~ "0 to 19",
                                `Age group`=="15 to 19 years" ~ "0 to 19",
                                `Age group`=="20 to 24 years" ~ "20 to 39",
                                `Age group`=="25 to 29 years" ~ "20 to 39",
                                `Age group`=="30 to 34 years" ~ "20 to 39",
                                `Age group`=="35 to 39 years" ~ "20 to 39",
                                `Age group`=="40 to 44 years" ~ "40 to 59",
                                `Age group`=="45 to 49 years" ~ "40 to 59",
                                `Age group`=="50 to 54 years" ~ "40 to 59",
                                `Age group`=="55 to 59 years" ~ "40 to 59",
                                `Age group`=="60 to 64 years" ~ "60 to 79",
                                `Age group`=="65 to 69 years" ~ "60 to 79",
                                `Age group`=="70 to 74 years" ~ "60 to 79",
                                `Age group`=="75 to 79 years" ~ "60 to 79",
                                `Age group`=="80 to 84 years" ~ "80 or plus",
                                `Age group`=="85 to 89 years" ~ "80 or plus",
                                `Age group`=="90 years and over" ~ "80 or plus",
                                TRUE ~ "remove")) %>%
  dplyr::filter(!age_group_20=="remove") %>%
  dplyr::select(GEO,VALUE, age_group_20) %>%
  dplyr::group_by(GEO, age_group_20) %>%
  dplyr::summarise(Population=sum(VALUE)) %>%
  dplyr::rename(Jurisdiction=GEO,
         AgeGroup20=age_group_20,
         Population20=Population)
return(list(latest_can_pop,pt_pop20))
}




#' Import web scraped hospital and ICU data
#'
#' This function imports web-scraped data on hospitalization of Covid-19 patients in Canada. There is a correction done for AB.
#'
#' @return
#' @export
#'
#' @examples
import_hosp_data<-function(){
googlesheets4::gs4_deauth()
hosp_data_raw<-googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/17KL40qJ8tpFalFeBv1XDopTXaFm7z3Q9J2dtqqsQaJg/edit?usp=sharing", sheet="hosp_and_icu") %>%
  dplyr::filter(!Jurisdiction=="AB") %>%
  dplyr::mutate(hosp=parse_number(as.character(hosp)),
         icu=parse_number(as.character(icu)))

# First scraped data for Alberta
AB_severity <- xml2::read_html("https://www.alberta.ca/stats/covid-19-alberta-statistics.htm") %>%
  html_nodes(xpath = "//*[@id='summary']/div/script/text()") %>%
  .[1] %>%
  html_text()

#extracts dates
AB_dates <- AB_severity %>%
  str_extract_all("\\d{4}-\\d{2}-\\d{2}") %>%
  unlist() %>%
  as.Date() %>%
  unique()

AB_counts <- AB_severity %>%
  str_extract_all("((?:\\d+,)+\\d+)") %>%
  unlist()

AB_non_icu <- AB_counts[15] %>%
  strsplit(split = ",") %>%
  unlist() %>%
  as.numeric()

AB_icu <- AB_counts[7] %>%
  strsplit(split = ",") %>%
  unlist() %>%
  as.numeric()

AB_all <- tibble(Date = AB_dates, hosp = AB_non_icu + AB_icu, icu=AB_icu) %>%
  mutate(Jurisdiction = "AB")

combined_hosp_data<-bind_rows(hosp_data_raw,AB_all) %>%
  arrange(Date) %>%
  filter(Date>"2020-03-31") %>%
  group_by(Date)

Canada_hosp_data<-combined_hosp_data %>%
  group_by(Date) %>%
  summarise(hosp=sum(hosp, na.rm = TRUE),
            icu=sum(icu, na.rm = TRUE)) %>%
  mutate(Jurisdiction="CAN")

all_hosp_data<-bind_rows(combined_hosp_data,Canada_hosp_data) %>%
  arrange(Date) %>%
  rename(hospitalized=hosp)%>%
  recode_PT_names_to_big() %>%
  factor_PT_west_to_east(size="big") %>%
  pivot_longer("hospitalized":"icu", names_to = "type", values_to = "cases") %>%
  mutate(Date=as.Date(Date))
#
# pt_hosp_icu<-all_hosp_data %>%
#   filter(!Jurisdiction=="Repatriated travellers")

return(all_hosp_data)
}



#' Import case report form data
#'
#' @param method A string with options: 'extract' or 'metabaser' depending on the way you want to access the data
#'
#' @return
#' @export
#'
#' @examples
import_case_report_form_data<-function(method="extract"){

if (method=="extract"){
qry_cases_raw <- readRDS("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/CaseReportForm/trend_extract.rds") %>%
  dplyr::mutate(onsetdate = as.Date(onsetdate),
         episodedate=as.Date(episodedate),
         earliestlabcollectiondate = as.Date(earliestlabcollectiondate)) %>%
  dplyr::rename(age=age_years)
} else if (method=="metabaser"){

#need to find solution for safely storing and accessing metabase credentials!
metabase_user='**********'
metabase_pass='**********'

handle<- metabaser::metabase_login(base_url = "https://discover-metabase.hres.ca/api",
                            database_id = 2, # phac database
                            username = metabase_user,
                            password = metabase_pass)

qry_cases_raw <- metabaser::metabase_query(handle, "select phacid, phacreporteddate, episodedate, pt, age_years, agegroup10, agegroup20, onsetdate, earliestlabcollectiondate, sex, gender, sexgender, coviddeath, hosp, icu, exposure_cat from all_cases;") %>%
rename(age=age_years)
}

  qry_canada <- qry_cases_raw %>%
    janitor::clean_names() %>%
    select(phacid, pt, episodedate, age, agegroup10, agegroup20) %>%
    filter(!is.na(age)) %>%
    group_by(episodedate, agegroup20) %>%
    tally() %>%
    mutate(Jurisdiction = "Canada") %>%
    filter(!is.na(episodedate))

  qry_cases <- qry_cases_raw %>%
    janitor::clean_names() %>%
    select(phacid, pt, episodedate, age, agegroup10, agegroup20) %>%
    mutate(Jurisdiction = toupper(pt)) %>%
    recode_PT_names_to_big() %>%
    group_by(episodedate, agegroup20, Jurisdiction) %>%
    dplyr::tally() %>%
    dplyr::filter(!is.na(episodedate)) %>%
    dplyr::bind_rows(qry_canada) %>%
    filter(Jurisdiction %in% c("Canada", recode_PT_names_to_big(PHACTrendR::PTs_big6))) %>%
    factor_PT_west_to_east(Canada_first=TRUE, size="big") %>%
    dplyr::rename(cases = n)

  qry_lab_onset <- qry_cases_raw %>%
    janitor::clean_names() %>%
    filter(pt != "Repatriate") %>%
    filter(onsetdate >= "2020-03-01") %>%
    filter(onsetdate <= (max(onsetdate - days(15)))) %>%
    select(onsetdate, earliestlabcollectiondate) %>%
    filter(!is.na(onsetdate)) %>%
    mutate(delay = earliestlabcollectiondate - onsetdate) %>%
    filter(between(delay, 0, 15)) %>% # filtering any outliers as identified in the SAS file
    group_by(onsetdate) %>%
    dplyr::summarise(mean_delay = mean(delay, na.rm = TRUE),
                     daily_case = n())
  return(list(qry_cases_raw,qry_cases,qry_lab_onset))
}


#' Import lab testing data from SALT
#'
#' @return
#' @export
#'
#' @examples
import_SALT_data<-function(){
salt_raw <- read.csv("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/SALT/Submitted+Reports.csv")
return(salt_raw)
}
