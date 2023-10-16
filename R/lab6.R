#' @import Rcpp
#' @useDynLib lab6package
#' @importFrom Rcpp sourceCpp

#' @title Solve the knapsack problem by Brute force search
#' 
#' @description
#' This function solves the knapsack problem by brute force search.
#' 
#' @param x A data frame with two variables (w, v) that contains the weight (w) and value (v) of each item.
#' @param W The knapsack capacity.
#' @param fast Logical, indicating whether to use the fast C++ implementation.
#' @returns A list contains the maximum knapsack value and which elements involved (rows in the data.frame x).
#' @export brute_force_knapsack

brute_force_knapsack <-  function(x, W, fast = FALSE){
  stopifnot(is.data.frame(x), is.numeric(W))
  stopifnot(names(x) == c('w','v'), all(x > 0) && W>0)
  if (fast) {
    knapsack_cpp(x, W)
  } else {
    finallist <- list(value=0, elements=NULL)
    for (i in 1:2^nrow(x)-1){
      cbtindex <- intToBits(i)
      cbtdf <- x[cbtindex==1,]
      totalvalue <- sum(cbtdf$v)
      totalweight <- sum(cbtdf$w)
      if (totalweight <= W && totalvalue > finallist$value){
        finallist$value <- round(totalvalue)
        finallist$elements <- as.numeric(rownames(cbtdf))
      }
    }
    return(finallist)
  }
}

#' @title Solve the knapsack problem by Dynamic programming
#' 
#' @description
#' This function solves the knapsack problem by Dynamic programming.
#' 
#' @param x A data frame with two variables (w, v) that contains the weight (w) and value (v) of each item.
#' @param W The knapsack capacity.
#' @returns A list contains the maximum knapsack value and which elements involved (rows in the data.frame x).
#' @examples
#' knapsack_dynamic(x = knapsack_objects[1:16,], W = 3500)
#' @export knapsack_dynamic

knapsack_dynamic <-  function(x, W){
  stopifnot(is.data.frame(x), is.numeric(W))
  stopifnot(names(x) == c('w','v'), all(x > 0) && W>0)
  finallist <- list(value=0, elements=NULL)
  # compute the maximum value, return a matrix
  value <- matrix(-1, nrow = nrow(x)+1, ncol = W+1)
  m <- function(i,j){
    if (i==0 || j<=0){
      value[i+1,j+1] <<- 0
      return()
    }
    if (value[i,j+1] == -1){
      m(i-1, j)
    }
    if (x$w[i] > j){
      value[i+1,j+1] <<- value[i,j+1]
    }else{
      if (value[i, j-x$w[i]+1] == -1){
        m(i-1,j-x$w[i])
      }
      value[i+1,j+1] <<- max(value[i,j+1], value[i,j-x$w[i]+1]+x$v[i])
    }
    return(value)
  }
  # pick up items of the optimal combinations, return a vector
  opt_vec <- c()
  pickitems <- function(i,j){
    if(i==0){
      return(opt_vec)
    }
    if(value[i+1,j+1] > value[i,j+1]){
      opt_vec <<- c(opt_vec,i)
      return(pickitems(i-1,j-x$w[i]))
    } else{
      return(pickitems(i-1,j))
    }
    return(opt_vec)
  }
  totalvalue<-m(nrow(x),W)[nrow(x)+1,W+1]
  finallist$value <- round(totalvalue)
  finallist$elements <- sort(pickitems(nrow(x),W))
  return(finallist)
}


#' @title Solve the knapsack problem by Greedy heuristic
#' 
#' @description
#' This function solves the knapsack problem by Greedy heuristic.
#' 
#' @param x A data frame with two variables (w, v) that contains the weight (w) and value (v) of each item.
#' @param W The knapsack capacity.
#' @returns A list contains the maximum knapsack value and which elements involved (rows in the data.frame x).
#' @examples
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#' @export greedy_knapsack

greedy_knapsack <-  function(x, W){
  stopifnot(is.data.frame(x), is.numeric(W))
  stopifnot(names(x) == c('w','v'), all(x > 0) && W>0)
  finallist <- list(value=0, elements=NULL)
  
  v_per_w <- x$v/x$w
  sort_items <- order(-v_per_w)
  totalvalue <- 0
  totalweight <- 0
  pick_item <- c()
  
  for (i in sort_items){
    if (totalweight+x$w[i] <= W){
      pick_item <- c(pick_item,i)
      totalvalue <- totalvalue+x$v[i]
      totalweight <- totalweight+x$w[i]
    }else{
    finallist$value <- round(totalvalue)
    finallist$elements <- pick_item
    return(finallist)
    }
  }
}



