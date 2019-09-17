#' Parallelize a function applied over the rows of a tibble.
#' @param x       Tibble. Data on which rows the function should be applied.
#' @param FUN     Function. Function to apply on the data/
#' @param intovar Character. Variable where the results should be stored.
#' @param threads Integer. Number of parallel threads.
#' @param ...     Additional arguments to be passed.
#' @return The initial tibble with the results in the variable specified with intovar.
#' @import foreach
#' @importFrom doSNOW registerDoSNOW
#' @importFrom snow makeSOCKcluster
#' @importFrom snow stopCluster
#' @importFrom utils txtProgressBar
#' @importFrom utils setTxtProgressBar
#' @importFrom purrr pmap
#' @importFrom tidyr unnest
#' @export


boostr <- function(x, FUN, intovar, threads, ...){
  
  i <- NULL
  
  cl <-  snow::makeSOCKcluster(threads)
  doSNOW::registerDoSNOW(cl)
  pb <- utils::txtProgressBar(max=nrow(x), style=3)
  progress <- function(n) utils::setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  t1 <- Sys.time()
  
  x$intovar <- foreach(i = 1:nrow(x), .options.snow=opts) %dopar% {
    purrr::pmap(x[i,], FUN, ...)
  }
  
  close(pb)
  snow::stopCluster(cl = cl)
  
  x <- x[,setdiff(names(x), intovar)]
  x <- tidyr::unnest(x, cols = intovar)
  names(x) <- gsub("intovar", intovar, names(x))
  
  print(Sys.time() - t1)
  
  return(x)
}
