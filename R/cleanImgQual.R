# Clean image quality tagging
#
# This function cleans the tagging of image quality that was done manually
#
#' @export

cleanImgQual <- function(x){
  if(grepl("ex", tolower(x)) | grepl("go", tolower(x))){
    return("good/excellent")
  } else if(grepl("ave", tolower(x))){
    return("average")
  } else if(grepl("ba", tolower(x))){
    return("bad")
  } else {
    return("idk")
  }
}
