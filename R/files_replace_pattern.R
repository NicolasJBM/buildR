#' Replace specific strings of characters in a set of documents.
#' @param path Character. Absolute path to the files.
#' @param pattern Character. Pattern to be replaced.
#' @param replacement Character. Pattern to be put instead.
#' @param type Character. Extension of the files to be processed
#' @param folders Logical. Whether subfolders should be included.
#' @return Replace in the same folder the documents where the string was found by the same documents where the string was replaced.
#' @importFrom readr read_lines
#' @importFrom readr write_lines
#' @importFrom stringr str_detect
#' @export


files_replace_pattern <- function(path,
                             pattern,
                             replacement,
                             type = ".Rmd",
                             folders = FALSE){
  
  stopifnot(
    is.character(path), is.character(pattern), is.character(replacement)
  )
  
  filenames <- list.files(path = path, pattern = type, recursive = folders)
  filenames <- paste0(path, filenames)
  
  for(f in filenames){
    
    doc <- readr::read_lines(f)
    
    if (sum(stringr::str_detect(doc, pattern)) > 0){
      newdoc <- gsub(pattern, replacement, doc)
      readr::write_lines(newdoc, file=f)
    }
    
  }
}



