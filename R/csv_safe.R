################
#CSV downloads
################

# Spreadsheet programs read a text cell that starts with =, +, -, @, a tab or a carriage
# return as a formula, so a crafted value in uploaded data (e.g. =HYPERLINK(...)) would run
# when a colleague opens a downloaded CSV. A leading ' keeps such cells, and such column
# names, as text. Only text columns are changed: numbers are written as numbers (a negative
# number is not a formula).
csv_safe <- function(df) {
  risky <- function(x) !is.na(x) & grepl("^[-=+@\t\r]", x)
  names(df) <- ifelse(risky(names(df)), paste0("'", names(df)), names(df))
  for (i in seq_along(df)) {
    if (is.character(df[[i]]) || is.factor(df[[i]])) {
      x <- as.character(df[[i]])
      hit <- risky(x)
      x[hit] <- paste0("'", x[hit])
      df[[i]] <- x
    }
  }
  df
}
