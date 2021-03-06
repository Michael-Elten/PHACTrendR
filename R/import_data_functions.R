#' Import raw web-scraped case and death numbers from Infobase
#'
#' This function downloads the covid19.csv dataset hosted at: \url{https://health-infobase.canada.ca/src/data/covidLive/covid19.csv}.
#' This dataset is maintained by the daily report epidemiologist from the Operations group at SED (former HPOC). Questions on contents of this dataset can be sent to hsfluepi generic account.
#'
#'
#'
#'
#' @return
#' @export
#'
#' @examples
#' df_raw<-import_raw_infobase_data()
#'
#' @family import functions
#'
#'
import_raw_infobase_data<-function(){
  df_raw <- readr::read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv",
                            col_types=cols(
                              pruid=col_double(),
                              prname=col_character(),
                              prnameFR=col_character(),
                              date=col_date(format="%d-%m-%Y"),
                              update=col_logical(),
                              numconf=col_double(),
                              numprob=col_double(),
                              numdeaths=col_double(),
                              numtotal=col_double(),
                              numtested=col_double(),
                              numtests=col_double(),
                              numrecover=col_double(),
                              percentrecover=col_double(),
                              ratetested=col_double(),
                              ratetests=col_double(),
                              numtoday=col_double(),
                              percentoday=col_double(),
                              ratetotal=col_double(),
                              ratedeaths=col_double(),
                              numdeathstoday=col_double(),
                              percentdeath=col_double(),
                              numtestedtoday=col_double(),
                              numteststoday=col_double(),
                              numrecoveredtoday=col_double(),
                              percentactive=col_double(),
                              numactive=col_double(),
                              rateactive=col_double(),
                              numtotal_last14=col_double(),
                              ratetotal_last14=col_double(),
                              numdeaths_last14=col_double(),
                              ratedeaths_last14=col_double(),
                              numtotal_last7=col_double(),
                              ratetotal_last7=col_double(),
                              numdeaths_last7=col_double(),
                              ratedeaths_last7=col_double(),
                              avgtotal_last7=col_double(),
                              avgincidence_last7=col_double(),
                              avgdeaths_last7=col_double(),
                              avgratedeaths_last7=col_double()
                            )) %>%
    rename(Jurisdiction=prname,
           Jurisdiction_FR=prnameFR)
  return(df_raw)
}


#' Import corrected web-scraped case and death numbers from Infobase
#'
#' This function downloads the covid19.csv dataset hosted at: \url{https://health-infobase.canada.ca/src/data/covidLive/covid19.csv}, but also makes additional corrections for data dumps, etc.
#' Important note - cumulative totals will not line up with other sources as there are instances where PTs reported
#' cases and deaths without specifying a date. These additional cases get removed in this function, but we are not able to
#' reassign them accurately.
#'
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#' df<-import_adjusted_infobase_data()
#'
#' @family import functions
#'
#'
import_adjusted_infobase_data_backup<-function(){
  df_raw <- readr::read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv",
                            col_types=cols(
                              pruid=col_double(),
                              prname=col_character(),
                              prnameFR=col_character(),
                              date=col_date(format="%d-%m-%Y"),
                              update=col_logical(),
                              numconf=col_double(),
                              numprob=col_double(),
                              numdeaths=col_double(),
                              numtotal=col_double(),
                              numtested=col_double(),
                              numtests=col_double(),
                              numrecover=col_double(),
                              percentrecover=col_double(),
                              ratetested=col_double(),
                              ratetests=col_double(),
                              numtoday=col_double(),
                              percentoday=col_double(),
                              ratetotal=col_double(),
                              ratedeaths=col_double(),
                              numdeathstoday=col_double(),
                              percentdeath=col_double(),
                              numtestedtoday=col_double(),
                              numteststoday=col_double(),
                              numrecoveredtoday=col_double(),
                              percentactive=col_double(),
                              numactive=col_double(),
                              rateactive=col_double(),
                              numtotal_last14=col_double(),
                              ratetotal_last14=col_double(),
                              numdeaths_last14=col_double(),
                              ratedeaths_last14=col_double(),
                              numtotal_last7=col_double(),
                              ratetotal_last7=col_double(),
                              numdeaths_last7=col_double(),
                              ratedeaths_last7=col_double(),
                              avgtotal_last7=col_double(),
                              avgincidence_last7=col_double(),
                              avgdeaths_last7=col_double(),
                              avgratedeaths_last7=col_double()
                            )) %>%
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


correct_df<-function(data,metric="",Jurisdiction="",correction_date="",corrected_value=""){
  if (metric=="cases"){
    data[data$Jurisdiction==Jurisdiction & data$date==correction_date, "numtoday"]<-corrected_value
  }else if (metric=="deaths"){
    data[data$Jurisdiction==Jurisdiction & data$date==correction_date, "numdeathstoday"]<-corrected_value
  }
  return(data)
}

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
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-25",corrected_value = 3.25)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-26",corrected_value = 3.25)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-27",corrected_value = 3.25)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Nova Scotia",     correction_date = "2020-12-28",corrected_value = 3.25)
#deaths over Xmas and NY 2020

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
#Weekend Jan23-24
#NOTE: We are excluding 380 cases AB reported on Jan25 as they were from "previous weeks". Not sure where to reassign them at the moment but should figure out a better solution.
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-23",corrected_value = 8.66)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British columbia",correction_date = "2021-01-24",corrected_value = 8.67)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-25",corrected_value = 8.67)
df_corrected<-correct_df(data=df_corrected, metric="cases",Jurisdiction = "Alberta",correction_date = "2021-01-25",corrected_value = 360)
#Weekend Jan30-31 (21 deaths reported on Feb.1 after not reporting over the weekend)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-30",corrected_value = 7)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-01-31",corrected_value = 7)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-01",corrected_value = 7)
#Weekend Feb6-7 (13 deaths reported on Feb.8 after not reporting over the weekend)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-06",corrected_value = 4.33)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-07",corrected_value = 4.33)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-08",corrected_value = 4.34)
#Feb15+16 death corrections
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Manitoba",correction_date = "2021-02-15",corrected_value = 2)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Manitoba",correction_date = "2021-02-16",corrected_value = 2)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Saskatchewan",correction_date = "2021-02-15",corrected_value = 1.5)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "Saskatchewan",correction_date = "2021-02-16",corrected_value = 1.5)
#Feb20-22 death corrections - BC: 0 deaths reported Feb 20, 21 and then 8 deaths reported Feb 22
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-20",corrected_value = 2.66)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-21",corrected_value = 2.66)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-22",corrected_value = 2.68)
#Feb27-Mar1 death corrections - BC: 0 deaths reported Feb 27, 28 and then 8 deaths reported Mar 1
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-27",corrected_value = 2.66)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-02-28",corrected_value = 2.66)
df_corrected<-correct_df(data=df_corrected, metric="deaths",Jurisdiction = "British Columbia",correction_date = "2021-03-01",corrected_value = 2.68)


#getting corrected values for the national number now
df_can_corrected<-df_corrected %>%
  filter(!Jurisdiction=="Canada") %>%
  select(Jurisdiction, date, numtoday, numdeathstoday) %>%
  group_by(date) %>%
  summarise(can_numtoday=sum(numtoday, na.rm = TRUE),
            can_deathstoday=sum(numdeathstoday, na.rm = TRUE)) %>%
  ungroup()

df_corrected2<-df_corrected %>%
  dplyr::left_join(df_can_corrected, by = "date") %>%
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

#' Experimental version! Import corrected web-scraped case and death numbers from Infobase
#'
#' @description
#'
#' This function downloads the covid19.csv dataset hosted at: \url{https://health-infobase.canada.ca/src/data/covidLive/covid19.csv}, but also makes additional corrections for data dumps, etc.
#'
#' To add new corrections to data, update the following google sheets file: \url{https://docs.google.com/spreadsheets/d/1lHTwMuZlGq8hXpiFMamy46jRkcBqetP16-1cYkfELJE/}. Currently you must be logged in under the trend epi google account to edit.
#'
#' Important note - cumulative totals will not line up with other sources as there are instances where PTs reported
#' cases and deaths without specifying a date. These additional cases get removed in this function, but we are not able to
#' reassign them accurately.
#'
#'
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#' df<-import_adjusted_infobase_data()
#'
#' @family import functions
#'
#'
import_adjusted_infobase_data<-function(){
  df_raw <- readr::read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19.csv",
                            col_types=cols(
                              pruid=col_double(),
                              prname=col_character(),
                              prnameFR=col_character(),
                              date=col_date(format="%d-%m-%Y"),
                              update=col_logical(),
                              numconf=col_double(),
                              numprob=col_double(),
                              numdeaths=col_double(),
                              numtotal=col_double(),
                              numtested=col_double(),
                              numtests=col_double(),
                              numrecover=col_double(),
                              percentrecover=col_double(),
                              ratetested=col_double(),
                              ratetests=col_double(),
                              numtoday=col_double(),
                              percentoday=col_double(),
                              ratetotal=col_double(),
                              ratedeaths=col_double(),
                              numdeathstoday=col_double(),
                              percentdeath=col_double(),
                              numtestedtoday=col_double(),
                              numteststoday=col_double(),
                              numrecoveredtoday=col_double(),
                              percentactive=col_double(),
                              numactive=col_double(),
                              rateactive=col_double(),
                              numtotal_last14=col_double(),
                              ratetotal_last14=col_double(),
                              numdeaths_last14=col_double(),
                              ratedeaths_last14=col_double(),
                              numtotal_last7=col_double(),
                              ratetotal_last7=col_double(),
                              numdeaths_last7=col_double(),
                              ratedeaths_last7=col_double(),
                              avgtotal_last7=col_double(),
                              avgincidence_last7=col_double(),
                              avgdeaths_last7=col_double(),
                              avgratedeaths_last7=col_double()
                            )) %>%
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

  infobase_correction_sheet_URL<-"https://docs.google.com/spreadsheets/d/1lHTwMuZlGq8hXpiFMamy46jRkcBqetP16-1cYkfELJE"

  googlesheets4::gs4_deauth()
  infobase_correction_info<-googlesheets4::read_sheet(infobase_correction_sheet_URL,
                                                      sheet="data_table",
                                                      col_types="ccDddcc")

  correct_df<-function(data,metric="",Jurisdiction="",correction_date="",corrected_value=""){
    if (metric=="cases"){
      data[data$Jurisdiction==Jurisdiction & data$date==correction_date, "numtoday"]<-corrected_value
    }else if (metric=="deaths"){
      data[data$Jurisdiction==Jurisdiction & data$date==correction_date, "numdeathstoday"]<-corrected_value
    }
    return(data)
  }

  for (i in 1:nrow(infobase_correction_info)){
    df_corrected<-correct_df(data=df_corrected,
                             metric=infobase_correction_info[[i,"metric"]],
                             Jurisdiction=infobase_correction_info[[i,"Jurisdiction"]],
                             correction_date=infobase_correction_info[[i,"correction_date"]],
                             corrected_value=infobase_correction_info[[i,"corrected_value"]])
  }

  #getting corrected values for the national number now
  df_can_corrected<-df_corrected %>%
    filter(!Jurisdiction=="Canada") %>%
    select(Jurisdiction, date, numtoday, numdeathstoday) %>%
    group_by(date) %>%
    summarise(can_numtoday=sum(numtoday, na.rm = TRUE),
              can_deathstoday=sum(numdeathstoday, na.rm = TRUE)) %>%
    ungroup()

  df_corrected2<-df_corrected %>%
    dplyr::left_join(df_can_corrected, by = "date") %>%
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
#'
#' df_int<-import_international_data()
#'
#' @family import functions
#'
import_international_data<-function(){
df_int <- readr::read_csv('https://covid.ourworldindata.org/data/owid-covid-data.csv',
                          col_types = cols(
                            iso_code=col_character(),
                            continent=col_character(),
                            location=col_character(),
                            date=col_date(format = "%Y-%m-%d")
                          )) %>%
  dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d"))
return(df_int)
}



#' Get demographic data
#'
#' @return
#'
#' list with 2 values. First value: latest_can_pop. Second value: pt_pop_20.
#'
#' Note - this function is optional, and is not currently recommended to use. These two datasets are also stored as R data in this package.
#' To access them, simply call: PHACTrendR::latest_can_pop, and PHACTrendR::pt_pop_20
#'
#' @export
#'
#' @examples
#'
#' demographic_data<-import_demographic_data()
#'
#' latest_can_pop<-demographic_data[[1]]
#' pt_pop20<-demographic_data[[2]]
#'
#' @family import functions
#'
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
#' This function imports human web-scraped data on hospitalization of Covid-19 patients in Canada. There is a retrospective correction done for AB by default.
#'
#' @param correct_AB  (optional) A boolean value (TRUE or FALSE) that indicates whether you would like to use current data from AB's website to correct AB values. TRUE by default.
#'
#' @return
#' @export
#'
#' @examples
#'
#' all_hosp_data<-import_hosp_data()
#'
#' @family import functions
#'
import_hosp_data<-function(correct_AB=TRUE){




spreadsheet_URL<-"https://docs.google.com/spreadsheets/d/17KL40qJ8tpFalFeBv1XDopTXaFm7z3Q9J2dtqqsQaJg"
googlesheets4::gs4_deauth()
hosp_data_raw<-googlesheets4::read_sheet(ss = spreadsheet_URL,sheet="hosp_and_icu") %>%
  dplyr::mutate(hosp=parse_number(as.character(hosp)),
         icu=parse_number(as.character(icu)))

if (correct_AB==TRUE){
  hosp_data_no_AB<-hosp_data_raw %>%
    filter(!Jurisdiction=="AB")

# First scraped data for Alberta
AB_severity <- xml2::read_html("https://www.alberta.ca/stats/covid-19-alberta-statistics.htm") %>%
  html_nodes(xpath = "//*[@id='summary']/div/script/text()") %>%
  .[1] %>%
  rvest::html_text()

#extracts dates
AB_dates <- AB_severity %>%
  stringr::str_extract_all("\\d{4}-\\d{2}-\\d{2}") %>%
  unlist() %>%
  as.Date() %>%
  unique()

AB_counts <- AB_severity %>%
  stringr::str_extract_all("((?:\\d+,)+\\d+)") %>%
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

final_hosp_data<-bind_rows(hosp_data_no_AB,AB_all) %>%
  arrange(Date) %>%
  filter(Date>"2020-03-31") %>%
  group_by(Date)
}
else if (correct_AB==FALSE){
  final_hosp_data<-hosp_data_raw %>%
    arrange(Date) %>%
    filter(Date>"2020-03-31") %>%
    group_by(Date)
} else{
  print("invalid input was received for `correct_AB`` term, please enter either TRUE or FALSE. Hint: Do not add quotes here")
  stop()
}

Canada_hosp_data<-final_hosp_data %>%
  group_by(Date) %>%
  summarise(hosp=sum(hosp, na.rm = TRUE),
            icu=sum(icu, na.rm = TRUE),
            .groups="drop_last") %>%
  mutate(Jurisdiction="CAN")

all_hosp_data<-bind_rows(final_hosp_data,Canada_hosp_data) %>%
  arrange(Date) %>%
  rename(hospitalized=hosp)%>%
  PHACTrendR::recode_PT_names_to_big() %>%
  PHACTrendR::factor_PT_west_to_east(size="big") %>%
  pivot_longer("hospitalized":"icu", names_to = "type", values_to = "cases") %>%
  mutate(Date=as.Date(Date))

return(all_hosp_data)
}




#' Import case report form data from DISCOVER
#'
#' @param method A string with options: 'extract' or 'metabaser' depending on the way you want to access the data
#'
#' @return
#' @export
#'
#' @examples
#'
#' qry_cases_raw<-import_DISCOVER_data()
#'
#' @family import functions
#'
import_DISCOVER_data<-function(method="extract", metabase_user="",metabase_pass=""){

  if (method=="extract"){
    qry_cases_raw <- readRDS("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/CaseReportForm/trend_extract.rds") %>%
      dplyr::mutate(onsetdate = as.Date(onsetdate),
                    episodedate=as.Date(episodedate),
                    earliestlabcollectiondate = as.Date(earliestlabcollectiondate),
                    earliestdate=as.Date(earliestdate)) %>%
      dplyr::rename(age=age_years)
  }else if (method=="metabaser"){

    handle<- metabaser::metabase_login(base_url = "https://discover-metabase.hres.ca/api",
                                database_id = 2, # phac database
                                username = metabase_user,
                                password = metabase_pass)

    qry_cases_raw <- metabaser::metabase_query(handle, "select phacid, phacreporteddate, episodedate, earliestdate, pt, age_years, agegroup10, agegroup20, onsetdate, earliestlabcollectiondate, sex, gender, sexgender, coviddeath, hosp, icu, exposure_cat from all_cases;") %>%
    rename(age=age_years)
  }
  return(qry_cases_raw)
}


#' Import lab testing data from SALT
#'
#' @return
#' @export
#'
#' @examples
#' salt_raw<-import_SALT_data()
#'
#' @family import functions
#'
import_SALT_data<-function(){
salt_raw <- read.csv("Y:/PHAC/IDPCB/CIRID/VIPS-SAR/EMERGENCY PREPAREDNESS AND RESPONSE HC4/EMERGENCY EVENT/WUHAN UNKNOWN PNEU - 2020/EPI SUMMARY/Trend analysis/_Current/_Source Data/SALT/Submitted+Reports.csv")
return(salt_raw)
}
