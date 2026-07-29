
# test_times <- c("2026-07-22 09:09:51", "2026-07-22 08:07:02", "2026-07-22 08:08:13")
# test_times2 <- c("2026-07-22 09:15:20", "2026-07-22 09:16:35", "2026-07-22 09:17:45", "2026-07-22 09:18:10")


sort_n_format <- function(x) {
        x <- x[!(is.na(x) | x=="")]
        if(!length(x)) return(NA)
  
        posix <- as.POSIXct(x) |> sort()
        diffs <-difftime(posix, data.table:::shift(posix),units="secs" )[-1] |> 
          as.integer()
        diffs <- sprintf("%02d:%02d:%02d", 
                          diffs %/% 3600, 
                          (diffs %% 3600) %/% 60, 
                          diffs %% 60)
        return(c(as.character(posix[1]),diffs))
}


