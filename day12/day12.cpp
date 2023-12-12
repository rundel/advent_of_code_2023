#include <Rcpp.h>
using namespace Rcpp;

typedef std::map<std::tuple<int,int,int>, double> mem_map;

double count_recr(CharacterVector const& seq, NumericVector const& code, int i, int j, int n, mem_map& memo) {
  std::tuple<int,int,int> key = std::make_tuple(i,j,n);
  
  if(memo.count(key) > 0)
    return memo[key];
  
  if (i == seq.length()) {
    if (j == code.length() & n == 0) {
      return(1);
    } else if (j == code.length()-1 & code[j] == n) {
      return(1);
    } else {
      return(0);
    }
  }
  
  double total = 0;
  CharacterVector const& chars = CharacterVector({".", "#"});
  
  for (auto chr : chars) {
    if (seq[i] == chr | seq[i] == "?") {
      if (chr == "." && n == 0) {
        total += count_recr(seq, code, i+1, j, 0, memo);
      } else if (chr == "." && n>0 && j < code.length() && code[j] == n) {
        total += count_recr(seq, code, i+1, j+1, 0, memo);
      } else if (chr == "#") {
        total += count_recr(seq, code, i+1, j, n+1, memo);
      }
    }
  }
  
  memo[key] = total;
  return(total);
}

// [[Rcpp::export]]
double count(CharacterVector const& seq, NumericVector const& code, int i, int j, int n) {
  mem_map memo;
  
  return count_recr(seq, code, i, j, n, memo);  
}



/*** R
s1 = c("?", "?", "?", "?", "?", "?", "#", "?", "#", "?", "#", "?", 
       "?", "?", "?", "?", "?", "?", "?", "?", "#", "?", "#", "?", "#", 
       "?", "?", "?", "?", "?", "?", "?", "?", "?", "#", "?", "#", "?", 
       "#", "?", "?", "?", "?", "?", "?", "?", "?", "?", "#", "?", "#", 
       "?", "#", "?", "?", "?", "?", "?", "?", "?", "?", "?", "#", "?", 
       "#", "?", "#", "?", "?")
c1 = c(2, 2, 6, 2, 2, 6, 2, 2, 6, 2, 2, 6, 2, 2, 6)

count(s1, c1, 0,0,0)
*/
