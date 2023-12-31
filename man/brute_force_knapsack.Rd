% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lab6.R
\name{brute_force_knapsack}
\alias{brute_force_knapsack}
\title{Solve the knapsack problem by Brute force search}
\usage{
brute_force_knapsack(x, W, fast = FALSE)
}
\arguments{
\item{x}{A data frame with two variables (w, v) that contains the weight (w) and value (v) of each item.}

\item{W}{The knapsack capacity.}

\item{fast}{Logical, indicating whether to use the fast C++ implementation.}
}
\value{
A list contains the maximum knapsack value and which elements involved (rows in the data.frame x).
}
\description{
This function solves the knapsack problem by brute force search.
}
