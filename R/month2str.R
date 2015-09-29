
month2str <- function(month) {
    if (is.data.frame(month)) {
        month <- month[, 1]
    }
    if (all(month %in% 1 : 12)) {
        monthStr <- month.abb[month]
    } else {
        nega <- (month < 0)
        month1_12 <- month %% 12
        month1_12[month1_12 == 0] <- 12
        monthAbb <- month.abb[month1_12]
        monthStr <- paste(monthAbb, '.', month, sep = '')
    }
    return(monthStr)
}
