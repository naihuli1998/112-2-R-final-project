library(tidyverse)
# 數據框描述 -----

library(dplyr)
library(tidyr)


# 整體描述
total_rows <- nrow(A17000000J_030243_zbf)

total_cols <- ncol(A17000000J_030243_zbf)

# 初始化結果列表
description <- list(
  total_rows = total_rows,
  total_cols = total_cols
)

# 逐欄描述函數
describe_column <- function(column) {
  column_data <- A17000000J_030243_zbf[[column]]
  na_count <- sum(is.na(column_data))
  na_percent <- (na_count / total_rows) * 100
  column_desc <- list(na_count = na_count, na_percent = na_percent)
  
  if (is.numeric(column_data)) {
    column_desc <- c(column_desc, 
                     range = range(column_data, na.rm = TRUE),
                     mean = mean(column_data, na.rm = TRUE),
                     max = max(column_data, na.rm = TRUE),
                     min = min(column_data, na.rm = TRUE),
                     quantiles = quantile(column_data, na.rm = TRUE),
                     median = median(column_data, na.rm = TRUE))
  } else {
    unique_vals <- unique(column_data)
    if (length(unique_vals) < 10) {
      val_counts <- table(column_data)
      val_percent <- prop.table(val_counts) * 100
      column_desc <- c(column_desc, 
                       value_counts = as.list(val_counts),
                       value_percent = as.list(val_percent))
    } else {
      column_desc <- c(column_desc, 
                       unique_count = length(unique_vals))
    }
  }
  return(column_desc)
}

# 對每個欄位進行描述
for (col in colnames(A17000000J_030243_zbf)) {
  description[[col]] <- describe_column(col)
}

# 顯示結果
description
