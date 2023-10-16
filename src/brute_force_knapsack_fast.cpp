#include <Rcpp.h>
using namespace Rcpp;

//' C++ implementation of the knapsack problem
//' @param x A data frame with two variables (w, v) that contains the weight (w) and value (v) of each item.
//' @param W The knapsack capacity.
// [[Rcpp::export]]

List knapsack_cpp(DataFrame x, double W) {
  IntegerVector weights = x["w"];
  NumericVector values = x["v"];
  int n = weights.size();
  int maxSubset = pow(2, n);
  
  double maxValue = 0;
  IntegerVector selectedItems;
  
  for (int i = 1; i < maxSubset; ++i) {
    int totalWeight = 0;
    double totalValue = 0;
    for (int j = 0; j < n; ++j) {
      if (i & (1 << j)) {
        totalWeight += weights[j];
        totalValue += values[j];
      }
    }
    
    if (totalWeight <= W && totalValue > maxValue) {
      maxValue = totalValue;
      selectedItems = IntegerVector::create();
      for (int j = 0; j < n; ++j) {
        if (i & (1 << j)) {
          selectedItems.push_back(j + 1); 
        }
      }
    }
  }
  int finalvalue = round(maxValue);
  return List::create(_["value"] = finalvalue, _["elements"] = selectedItems);
}
