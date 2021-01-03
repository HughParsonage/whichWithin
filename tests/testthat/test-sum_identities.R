test_that("sum_identities work", {
  ans <- test_sum_identities(k = 50, j = 2, N = 100)
  actual_sum_identities <- function(k = 50, j = 2, N = 100) {
    
    # Purpose for sum_n_le:
    CJ1 <- CJ(x = seq_len(N), 
              y = seq_len(N))
    sum_n_le <- nrow(CJ1[x < y])
    
    
    c(sum(seq_len(k)), 
      sum(N - (seq_len(j))), 
      sum_n_le,
      sum_n_le)
  }
  
  for (k in c(0L, 1L, 50L, 100L, 101L, 373L)) {
    for (j in c(0L, 1L, 2L, k - 1L, k)) {
      j <- pmax(j, 0L)
      for (N in c(0L, 1L, 2L, 100L, 150L, 1001L)) {
        ans <- test_sum_identities(k = k, j = j, N = N)
        
        expect_equal(ans, actual_sum_identities(k, j, N), info = toString(c(k, j, N)))
      }
    }
  }
})
