#' @name replace_in_files
#' @title Replace a pattern in several documents
#' @author Nicolas Mangin
#' @description Replace the specified pattern of characters by another in all the documents within a path.
#' @param path        Character. Relative (from the working directory) or absolute path to the files.
#' @param pattern     Character. Pattern to find and replace in each document.
#' @param replacement Character. Pattern to put instead.
#' @param type        Character. Extension of the files to be processed
#' @param folders     Logical. Whether documents in sub-folders should be included.
#' @return Replace in the same folder the documents where the string was found by the same documents where the string was replaced. The change is destructive.
#' @importFrom readr read_lines
#' @importFrom readr write_lines
#' @importFrom stringr str_detect
#' @export


replace_in_files <- function(path,
                             pattern,
                             replacement,
                             type = ".Rmd",
                             folders = FALSE) {
  stopifnot(
    is.character(path), is.character(pattern), is.character(replacement)
  )

  # Identify the files in which changes should be made.
  filenames <- list.files(path = path, pattern = type, recursive = folders)
  filenames <- paste0(path, filenames)

  # Load each document, replace the pattern if found,
  # and save the new document if a replacement was made.
  for (f in filenames) {
    doc <- readr::read_lines(f)

    if (sum(stringr::str_detect(doc, pattern)) > 0) {
      newdoc <- gsub(pattern, replacement, doc)
      readr::write_lines(newdoc, file = f)
    }
  }
}
