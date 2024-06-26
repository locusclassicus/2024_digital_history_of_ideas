nearest_neighbors <- function(df, token) {
  df |> 
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) / 
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ]^2)))
        
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(focal, dimension, value) |> 
    select(-item2)
}