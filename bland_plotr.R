bland_plot <- function(datin,colA,colB) {
  df <- datin

  # convert to column headers to characters
  colnames(df) <- make.names(colnames(df))
  # convert column input to character
  A <- paste0("X",colA)
  B <- paste0("X",colB)
  # filter table 
  df2 <- df %>%
    dplyr::select(A,B) %>%
    as.data.frame() %>%
    mutate(avg = rowMeans(.),
           diff = .[[A]] - .[[B]])
  
  # find average difference
  mean_diff <- mean(df2$diff)
  
  # find lower 95% confidence interval limits
  lower <- mean_diff - 1.96*sd(df2$diff)
  
  # find upper 95% confidence interval limits
  upper <- mean_diff + 1.96*sd(df2$diff)
  
  # create Bland-Altman plot
  fig_b <- ggplot(df2, 
                  aes(x = avg, 
                      y = diff)) +
    geom_point(size = 2) +
    geom_hline(yintercept = mean_diff) +
    geom_hline(yintercept = lower, color = "red", linetype = "dashed") +
    geom_hline(yintercept = upper, color = "red", linetype = "dashed") +
    geom_text(aes(x = 11, y = upper + 0.02, label = paste0("+1.96 SD = ",round(upper,3))), col = "black", size = 2) +
    geom_text(aes(x = 11, y = mean_diff + 0.02, label = paste0("mean = ",round(mean_diff,3))), col = "black", size = 2) +
    geom_text(aes(x = 11, y = lower - 0.02, label = paste0("-1.96 SD = ",round(lower,3))), col = "black", size = 2) +
    theme_bw() +
    ylim(-0.3,0.3) +
    ggtitle(paste0("Bland-Altman Plot: ", gsub("X","",A)," vs. ", gsub("X","",B))) +
    ylab("Difference Between Measurements") +
    xlab("Average Measurement")
  
  # crate linear regression plot
  library(ggpubr)
  fig_r <- ggscatter(df2,
                     x = B,
                     y = A,
                     add = "reg.line") +
    stat_cor(label.x = 6, label.y = 10) +
    stat_regline_equation(label.x = 6, label.y = 11) +
    labs(x = paste0(gsub("X","",B)," (min)"),
         y =  paste0(gsub("X","",A)," (min)"))
  
  outputs <- list("Bland_Altman" = fig_b,
                  "Linear_Regression" = fig_r)
  
  return(outputs)
}