#' @name boostr
#' @title Parallelize tasks to speed-up processes
#' @author Nicolas Mangin
#' @description Parallelize a function applied over the rows of a tibble.
#' @param x       Tibble. Data on which rows the function should be applied. Variables names should be the parameters of the function applied.
#' @param FUN     Function. Function to apply on the data.
#' @param intovar Character. Variable where the results should be stored.
#' @param threads Integer. Number of parallel threads.
#' @param ...     Additional arguments to be passed to the function.
#' @return The initial tibble with the results in the variable specified with intovar.
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterExport
#' @importFrom parallel clusterEvalQ
#' @importFrom parallel stopCluster
#' @importFrom pbapply pblapply
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom purrr pmap
#' @importFrom tidyr unnest
#' @export


boostr <- function(x, FUN, intovar, threads, ...) {
  id <- NULL
  result <- NULL

  t1 <- Sys.time()

  x <- tibble::as_tibble(x)

  y <- x %>%
    tibble::rownames_to_column("id") %>%
    dplyr::group_by(id) %>%
    tidyr::nest()
  
  cl <- parallel::makeCluster(threads)
  result <- pbapply::pblapply(
    cl = cl,
    X = y$data,
    FUN = FUN,
    ...
  )
  parallel::stopCluster(cl)

  x <- x %>%
    dplyr::mutate(result = result)
  x <- x[, names(x) != intovar]
  names(x) <- gsub("result", intovar, names(x))
  print(Sys.time() - t1)

  return(x)
}
