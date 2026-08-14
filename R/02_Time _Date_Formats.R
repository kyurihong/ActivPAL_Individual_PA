#### -------------------
#### Time & Date format
#### -------------------

fix_hm <- function(x) {
  x <- as.character(x)
  x <- ifelse(is.na(x) | x == "", NA_character_, x)
  x <- gsub(";", ":", x)
  x <- ifelse(!is.na(x) & nchar(x) == 4 & substr(x, 2, 2) == ":", paste0("0", x), x)
  x <- ifelse(!is.na(x) & nchar(x) == 5, paste0(x, ":00"), x)
  x
}

mk_posix <- function(d, t) {
  parse_date_time(paste(d, t), orders = "Y-m-d H:M:S", tz = "UTC", exact = FALSE)
}

