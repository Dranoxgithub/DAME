# all ordered such as no group like {2,1}
# s is a newly dropped set, delta is the set of previously processed sets
# s is a vector
GenerateNewActiveSets <- function(s, delta) {
  s <- sort(s)
  # get size k
  k <- length(s)
  
  # initialize the return set Z
  Z <-  list()
  
  # step 3 find all subsets of delta of size k and include s
  if (length(delta) == 0) return (Z)
  length_list <- lapply(delta, length)
  subset_index <- which(length_list == k)
  delta_k <- list()

  delta_k <- lapply(1:length(subset_index), function(i) {delta_k[[i]] <- delta[[subset_index[i]]]})
  delta_k[[length(delta_k) + 1]] <- s
  
  # step 4 get all covariates contained in stes in delta_k
  cov_delta_k <- delta_k[[1]]
  cov_delta_k <- lapply(1:length(delta_k), function(x) {union(cov_delta_k, delta_k[[x]])})[[length(delta_k)]]
  cov_delta_k <- sort(cov_delta_k)
  
  # step 5 find the support of covariate e in delta_k
  merge_delta_k <- unlist(delta_k)
  S_set <- plyr::count(merge_delta_k)
  
  # step 5 and 6: find the covariates in cov_ have enough support in delta_ks , including s first
  candidates <- S_set[which(S_set[2] >= k), 1]
  if (all(s %in% candidates)) {
    # exclude s
    new_cov_candidates <- setdiff(candidates, s)
    for (current_new_cov in new_cov_candidates) {
      current_set <- sort(c(s, current_new_cov))
      subset <- list()
      subset <- lapply(1:(k+1), function(x) current_set[-c(x)])
      check_subset <- lapply(1:(k+1), function(x) list(subset[[x]]) %in% delta_k) # set is in reversed order
      if (length(which(check_subset == TRUE)) == k + 1) {
        Z[[length(Z) + 1]] <- current_set
      }
    }
  }
  return (Z)
}

