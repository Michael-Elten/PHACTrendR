
#' Factor PT names from West to East
#'
#'This function converts a character "jurisdiction" variable to an ordered factor to help arrange PTs in figures and tables from west to east!
#'
#' @param input_data      (required) an input data frame, tibble, etc.
#' @param geo_variable    (optional) the variable that contains the information on jurisdiction. Note: must be a character. Defaults to "Jurisdiction"
#' @param size            (optional) the format of the variable. "small" for abbreviations (ie. BC), "big" for complete words (ie. "British Columbia") defaults to "small".
#' @param Canada_first    (optional) Boolean that determines whether Canada values should be placed first or last in the order. Defaults to last.
#'
#' @return
#' @export
#'
#' @examples
factor_PT_west_to_east<-function(input_data,geo_variable="Jurisdiction", size="small",Canada_first=FALSE){
  if (size=="big"){
    if (Canada_first==TRUE){
      juriorder <- c("Canada","British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec","Newfoundland and Labrador","New Brunswick","Nova Scotia","Prince Edward Island","Yukon","Northwest Territories","Nunavut", "Repatriated travellers")
    } else if (Canada_first==FALSE){
      juriorder <- c("British Columbia","Alberta","Saskatchewan","Manitoba","Ontario","Quebec","Newfoundland and Labrador","New Brunswick","Nova Scotia","Prince Edward Island","Yukon","Northwest Territories","Nunavut", "Repatriated travellers","Canada")
    }
  }else if (size=="small"){
    if (Canada_first==TRUE){
      juriorder <- c("CAN","BC","AB","SK","MB","ON","QC","NL","NB","NS","PE","YK","NT","NU","Repatriated travellers")
    } else if (Canada_first==FALSE){
      juriorder <- c("BC","AB","SK","MB","ON","QC","NL","NB","NS","PE","YK","NT","NU","Repatriated travellers","CAN")
    }
  } else {
    print("Error in size argument of 'factor_PT_west_to_east()', use 'small' if PT names are abbreviated, 'big' if full names are used")
  }

  output_data<-input_data %>%
    mutate(!!geo_variable := factor(!!as.name(geo_variable), levels = juriorder))

  if (exists('output_data')==FALSE){
    output_data<-input_data
  }
  return(output_data)
}

#' Factor PT names alphabetically
#'
#'This function converts a character "jurisdiction" variable to an ordered factor to help arrange PTs in figures and tables from west to east!
#'
#' @param input_data      (required) an input data frame, tibble, etc.
#' @param geo_variable    (optional) the variable that contains the information on jurisdiction. Note: must be a character. Defaults to "Jurisdiction"
#' @param size            (optional) the format of the variable. "small" for abbreviations (ie. BC), "big" for complete words (ie. "British Columbia") defaults to "small".
#'
#' @return
#' @export
#'
#' @examples
factor_PT_alphabetical<-function(input_data,geo_variable="Jurisdiction",size="small"){
  if (size=="big"){
    juriorder <- c(sort(recode_PT_names_to_big(all_PTs)),"Repatriated travellers","Canada")
  } else if (size=="small"){
    juriorder <- c(sort(all_PTs),"Repatriated travellers","CAN")
  } else {
    print("Error in size argument of 'factor_PT_alphabetical()', use 'small' if PT names are abbreviated, 'big' if full names are used")
  }

  output_data<-input_data %>%
    mutate(!!geo_variable := factor(!!as.name(geo_variable), levels = juriorder))
  if (exists('output_data')==FALSE){
    output_data<-input_data
  }

  return(output_data)
}
