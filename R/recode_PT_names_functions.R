#Recode PT functions


#' Recode_pt_names_to_small
#'
#' @param dataset         (required) the dataset of interest. Can be a data.frame, tibble, or even just a character vector
#' @param geo_variable    (optional) if the dataset is not a character vector, designate the geographical variable of interest (defaults to "jurisdiction")
#'
#' @return
#' @export
#'
#' @examples
recode_PT_names_to_small <- function(dataset, geo_variable = "jurisdiction") {
  if (class(dataset)[1]=="character"){
    dataset<-dplyr::recode(dataset,
                    "British Columbia"="BC",
                    "Alberta" = "AB",
                    "Saskatchewan"="SK",
                    "Manitoba"="MB",
                    "Ontario"="ON",
                    "Quebec"="QC",
                    "Newfoundland and Labrador"="NL",
                    "New Brunswick"="NB",
                    "Nova Scotia"="NS",
                    "Prince Edward Island"="PE",
                    "Yukon"="YK",
                    "Northwest Territories"="NT",
                    "Nunavut"="NU",
                    "Canada"="CAN",
                    "Repatriated travellers"="Repatriated travellers")
  }
  else{
    dataset <- dataset %>%
      dplyr::mutate(
        !!geo_variable := dplyr::case_when(
          !!as.name(geo_variable) == "British Columbia" ~ "BC",
          !!as.name(geo_variable) == "Alberta" ~ "AB",
          !!as.name(geo_variable) == "Saskatchewan" ~ "SK",
          !!as.name(geo_variable) == "Manitoba" ~ "MB",
          !!as.name(geo_variable) == "Ontario" ~ "ON",
          !!as.name(geo_variable) == "Quebec" ~ "QC",
          !!as.name(geo_variable) == "Newfoundland and Labrador" ~ "NL",
          !!as.name(geo_variable) == "New Brunswick" ~ "NB",
          !!as.name(geo_variable) == "Nova Scotia" ~ "NS",
          !!as.name(geo_variable) == "Prince Edward Island" ~ "PE",
          !!as.name(geo_variable) == "Yukon" ~ "YK",
          !!as.name(geo_variable) == "Northwest Territories" ~ "NT",
          !!as.name(geo_variable) == "Nunavut" ~ "NU",
          !!as.name(geo_variable) == "Canada" ~ "CAN",
          !!as.name(geo_variable) == "Repatriated travellers" ~ "Repatriated travellers",
          TRUE~!!as.name(geo_variable)))
  }
  return(dataset)
}


#' recode_PT_names_to_big
#'
#' @param dataset
#' @param geo_variable
#'
#' @return
#' @export
#'
#' @examples
recode_PT_names_to_big <- function(dataset, geo_variable = "jurisdiction") {
  if (class(dataset)[1]=="character"){
    dataset<-dplyr::recode(dataset,
                    "BC" = "British Columbia",
                    "AB" = "Alberta",
                    "SK" = "Saskatchewan",
                    "MB" = "Manitoba",
                    "ON" = "Ontario",
                    "QC" = "Quebec",
                    "NL" = "Newfoundland and Labrador",
                    "NB" = "New Brunswick",
                    "NS" = "Nova Scotia",
                    "PE" = "Prince Edward Island",
                    "YK" = "Yukon",
                    "NT"= "Northwest Territories",
                    "NU" = "Nunavut",
                    "CAN" = "Canada",
                    "Repatriated travellers" = "Repatriated travellers")
  }
  else{
    dataset <- dataset %>%
      dplyr::mutate(
        !!geo_variable := dplyr::case_when(
          !!as.name(geo_variable) ==  "BC" ~ "British Columbia",
          !!as.name(geo_variable) ==  "AB" ~ "Alberta",
          !!as.name(geo_variable) ==  "SK" ~ "Saskatchewan",
          !!as.name(geo_variable) ==  "MB" ~ "Manitoba",
          !!as.name(geo_variable) ==  "ON" ~ "Ontario",
          !!as.name(geo_variable) ==  "QC" ~ "Quebec",
          !!as.name(geo_variable) ==  "NL" ~ "Newfoundland and Labrador",
          !!as.name(geo_variable) ==  "NB" ~ "New Brunswick",
          !!as.name(geo_variable) ==  "NS" ~"Nova Scotia",
          !!as.name(geo_variable) ==  "PE" ~ "Prince Edward Island",
          !!as.name(geo_variable) ==  "YK" ~ "Yukon",
          !!as.name(geo_variable) ==  "NT" ~ "Northwest Territories",
          !!as.name(geo_variable) ==  "NU" ~ "Nunavut",
          !!as.name(geo_variable) ==  "CAN" ~ "Canada",
          !!as.name(geo_variable) ==  "Repatriated travellers" ~ "Repatriated travellers",
          TRUE~ !!as.name(geo_variable)))

  }
  return(dataset)
}
