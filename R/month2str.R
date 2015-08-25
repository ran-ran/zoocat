
month2Str <- function(month) {
    if (all(month %in% 1 : 12)) {
        monthStr <- month.abb[month]
    } else {
        nega <- (month < 0)
        month1_12 <- month %% 12
        month1_12[month1_12 == 0] <- 12
        monthAbb <- month.abb[month1_12]
        monthStr <- rep('', length(month))
        monthStr[!nega] <- paste(monthAbb[!nega], '.', month[!nega], sep = '')
        monthStr[nega] <- paste(monthAbb[nega], '.neg', abs(month[nega]), sep = '')
    }
    return(monthStr)
}
