#' Make all the numeric variables of a dataframe discrete with the specified number of categories according to different algorithms (split based on quantiles or kmeans). The kmeans is chosen and if minimum differences are provided, differences inferior to the thresholds will be disregarded.
#' @param x dataframe. database in which all numeric values have to be discretized.
#' @param ncat integer. The number of discrete values desired for each variable (ncat = 2 makes binary variables).
#' @param method character. Specify whether the variables should be discretized using quantiles ("qt") or kmeans ("km")
#' @return A dataframe where all variables are discrete.
#' @importFrom dplyr mutate_if
#' @importFrom dplyr %>%
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr right_join
#' @importFrom dplyr ntile
#' @importFrom stats kmeans
#' @importFrom stats quantile
#' @export


stat_discretize <- function(x, ncat = 2, method = "km"){
  if (method == "km"){
    x <- mutate_if(x, is.numeric, kmeans_split, ncat = ncat)
  } else {
    x <- mutate_if(x, is.numeric, quantile_split, ncat = ncat)
  }
  return(x)
}


kmeans_split <- function(x, ncat) {
  
  ncat <- min((length(unique(x))-1), ncat)
  
  if (ncat > 0){
    
    tmp1 <- data.frame(x = x, newx = as.numeric(kmeans(x, centers = ncat)$cluster))
    
    tmp2 <- tmp1 %>%
      group_by(newx) %>%
      summarise(mean = mean(x)) %>%
      mutate(mean = ntile(mean, ncat)) %>%
      right_join(tmp1, by = "newx")
    
    newx <- tmp2$mean - 1
    
  } else newx <- rep(0, length(x))
  
  return(newx)
}



quantile_split <- function(x, ncat) {
  
  breaks <- c()
  for (i in 1:(ncat-1)) breaks[i] <- quantile(x, i/ncat)
  breaks <- c(min(x)-1, breaks, max(x)+1)
  x <- cut(x, breaks = breaks) %>%
    as.numeric()
  x - 1
  
}

