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
