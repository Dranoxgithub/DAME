# all ordered such as no group like {2,1}1
# s is a newly dropped set, delta is the set of previously processed sets 
# s is a vector, 
GenerateNewActiveSets <- function(s, delta) {
  # get size k 
  k <- length(s)
  
  # initialize the return set Z
  Z <-  list()
  
  # step 3 find all subsets of delta of size k and include s
  if (length(delta) == 0) return (Z)
  length_list <- lapply(delta, length)
  subset_index <- which(length_list == k)
  delta_k <- list()
  for (i in 1:length(subset_index)) {
    delta_k[[i]] <- delta[[subset_index[i]]]
  }
  delta_k[[length(delta_k) + 1]] <- s
  
  # step 4 get all covariates contained in stes in delta_k
  cov_delta_k <- delta_k[[1]]
  for (i in 1:length(delta_k)) {
    cov_delta_k <- union(cov_delta_k, delta_k[[i]])
  }
  cov_delta_k <- sort(cov_delta_k)
  
  # step 5 find the support of covariate e in delta_k
  S_set <- data.frame(cov_delta_k = cov_delta_k)
  S_set['support'] <- 0
  for (i in 1:length(delta_k)) {
    current_set <- delta_k[[i]]
    intersection <- intersect(current_set, cov_delta_k)
    if (length(intersection) == 0) {
      next
    }
    for (k in 1:length(intersection)) {
      intersect_number <- intersection[k]
      S_set[which(cov_delta_k == intersect_number), 2] <- S_set[which(S_set$cov_delta_k == intersect_number), 2] + 1
    }
  }
  
  # step 5 and 6: find the covariates in cov_ have enough support in delta_k, including s first
  candidates <- S_set[which(S_set$support >= k),1]
  check_s <- intersect(candidates, s)
  if (length(check_s) == length(s)) {
    # exclude s
    new_cov_candidates_true <- lapply(candidates, function(x) {x %in% s})
    new_cov_candidates <- which(new_cov_candidates_true == FALSE)
    for (i in 1:length(new_cov_candidates)) {
      current_set <- c(s, new_cov_candidates[i])
      current_set <- sort(current_set)
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
