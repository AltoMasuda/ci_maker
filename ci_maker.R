ci_maker <- function(data, cflevel = 0.95){
  n <- length(data) - sum(is.na(HR$age))
  smean <- mean(data, na.rm = TRUE)
  svar <- mean((data)^2, na.rm = TRUE) - (smean)^2
  uvar <- svar*(n/(n-1))
  tstat <- qt((1 - cflevel)/2, n-1, lower.tail = FALSE)
  ci_upper <- smean + tstat*(sqrt(uvar)/sqrt(n))
  ci_lower <- smean - tstat*(sqrt(uvar)/sqrt(n))
  return(c(ci_lower, ci_upper))
}
ci <- ci_maker(HR$age)
ci[1]