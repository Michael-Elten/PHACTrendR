# Misc functions

#' Convert vector to readable list
#'
#'This function takes as an input a character vector, and returns a human readable list, with commas and "ands". How nice!
#'
#' @param vector (Required). The character vector of interest. Must be a character vector or this just won't work.
#'
#' @return
#' @export
#'
#' @examples
turn_char_vec_to_comma_list<-function(vector){
  if(class(vector)!="character"){
    print("This is not a character vector")
    return(vector)
  }else if (length(vector)==1){
    final_string<-vector
  }else if (length(vector)==2){
    final_string<-str_c(vector, collapse = " and ")
  }else if (length(vector)>2){
    final_string<-paste0(str_c(vector[1:length(vector)-1], collapse = ", "), ", and ",vector[length(vector)])
  }
  return(final_string)
}



#' Title
#'
#' @param input_dataset
#' @param numeric_variable
#' @param accuracy
#'
#' @return
#' @export
#'
#' @examples
turn_num_to_percent_change<-function(input_dataset,numeric_variable, accuracy=0.1){
     if (class(input_dataset)[1]=="numeric"){
      output_dataset<-scales::percent(input_dataset, accuracy=accuracy, big.mark = "")
      output_dataset<-ifelse(stringr::str_detect(output_dataset,"-"), output_dataset, paste0("+",output_dataset))
      output_dataset<-ifelse(is.na(output_dataset), "NA",
                             ifelse(output_dataset=="+Inf" , "+Inf",
                                    ifelse(as.numeric(stringr::str_sub(output_dataset,2,-2))==0 ,stringr::str_sub(output_dataset,2), output_dataset)))
    }

  else{
      output_dataset <- input_dataset %>%
        dplyr::mutate(!!numeric_variable := scales::percent(!!as.name(numeric_variable),accuracy=accuracy,big.mark = "")) %>%
        dplyr::mutate(!!numeric_variable := ifelse(stringr::str_detect(!!as.name(numeric_variable),"-") , !!as.name(numeric_variable), paste0("+",!!as.name(numeric_variable)))) %>%
        dplyr::mutate(!!numeric_variable := ifelse(is.na(!!as.name(numeric_variable)), "NA",
                                                   ifelse(!!as.name(numeric_variable)=="+Inf" , "+Inf",
                                                           ifelse(as.numeric(stringr::str_sub(!!as.name(numeric_variable),2,-2))==0 , (stringr::str_sub(!!as.name(numeric_variable),2)),!!as.name(numeric_variable)))))


    }
    return(output_dataset)
}
