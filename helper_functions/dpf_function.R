dpf <- function(word_matrix, alpha) {
  # Вычисление суммы всех элементов матрицы
  # т.е. общее количество встречаемостей слов
  total_count <- sum(word_matrix)
  
  # Вычисление DPF для каждой пары слов
  dpf_matrix <- matrix(0, nrow = nrow(word_matrix), ncol = ncol(word_matrix))
  rownames(dpf_matrix) <- rownames(word_matrix)
  colnames(dpf_matrix) <- colnames(word_matrix)
  
  for (i in 1:nrow(word_matrix)) {
    for (j in 1:ncol(word_matrix)) {
      p_xy <- word_matrix[i, j] / total_count
      p_x <- sum(word_matrix[i, ]) / total_count
      p_y <- sum(word_matrix[, j]) / total_count
      dpf_matrix[i, j] <- p_xy / (p_x * p_y)^alpha
    }
  }
  # Вывод результата
  return(dpf_matrix)
}



