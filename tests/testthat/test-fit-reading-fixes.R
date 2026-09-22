#reading uploaded files in the distribution fitting tool: encodings, ragged rows, line endings and
#blank lines before the header

#writes the bytes of a file as they are (no re-encoding, no line ending conversion)
read_fix_bytes <- function(bytes) {
  path <- tempfile(fileext = ".csv")
  con <- file(path, "wb")
  writeBin(bytes, con)
  close(con)
  path
}

read_fix_lines <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  path
}

read_fix_upload <- function(session, path, sep = ",", dec = ".") {
  session$setInputs(file1 = list(datapath = path, name = basename(path)), data_includes_header = TRUE, sep = sep,
                    quote = intToUtf8(34), dec = dec)
}

test_that("a Windows-1252 file with accented characters is read, and the session survives it", {
  #the CSV that Excel writes on a German or French Windows: read as UTF-8, the byte of an o-umlaut
  #made a string that is not valid text, and trimws() in the numeric detection stopped with "input
  #string 1 is invalid UTF-8" inside an observer, which ended the session
  o_umlaut <- as.raw(0xf6)
  path <- read_fix_bytes(c(charToRaw("Schadenh"), o_umlaut, charToRaw("he;Anzahl;Region\n1.234,5;2;K"), o_umlaut,
                           charToRaw("ln\n2.500;3;Bonn\n800;4;K"), o_umlaut, charToRaw("ln\n")))
  expect_false(all(validUTF8(readLines(path, warn = FALSE))))
  amount <- paste0("Schadenh", intToUtf8(246), "he")
  df <- dft_read_data(path, sep = ";", dec = ",")
  expect_true(all(validUTF8(names(df))))
  expect_equal(names(df), c(amount, "Anzahl", "Region"))
  expect_equal(df$Region, c(paste0("K", intToUtf8(246), "ln"), "Bonn", paste0("K", intToUtf8(246), "ln")))
  shiny::testServer(distribution_fitting_tool_Server, {
    read_fix_upload(session, path, sep = ";", dec = ",")
    expect_equal(numeric_columns(), c(amount, "Anzahl"))
    expect_equal(column_choices()$selected$severity_var, amount)
    expect_match(output$data_table$html, paste0("K", intToUtf8(246), "ln"), fixed = TRUE)
    session$setInputs(severity_var = amount, execute_sev_analysis = 1)
    expect_equal(severity_data(), c(1234.5, 2500, 800), tolerance = 1e-10)
  })
  #a file that is valid UTF-8 is still read as UTF-8
  path <- read_fix_bytes(charToRaw(paste0("sev,ort\n100,K", intToUtf8(246), "ln\n")))
  expect_equal(dft_read_data(path)$ort, paste0("K", intToUtf8(246), "ln"))
  #the numeric detection does not depend on the text being valid: whatever the encoding, such a value
  #is not a number
  invalid <- rawToChar(c(charToRaw("1"), o_umlaut))
  expect_equal(dft_as_numeric(c(" 12 ", invalid, "1,5"), dec = ","), c(12, NA, 1.5))
  expect_equal(dft_numeric_columns(data.frame(a = c("1", "2", "3"), b = c("1", invalid, "3"), c = c(invalid, "", NA))), "a")
})

test_that("a row with more fields than the first rows keeps them in its own columns", {
  #read without a header (the header is read on its own), read.table() sized the columns from the
  #first five rows; a later row with an extra field was split, and the surplus became a new row in
  #the first column: 11 rows instead of 8, and three claims (7, 12 and 900) that were never in the file
  path <- read_fix_lines(c("sev,n,recovery", "100,1", "250,2", "300,3", "80,4", "90,5", "500,6,7", "700,8,12", "1200,9,900"))
  df <- dft_read_data(path)
  expect_equal(names(df), c("sev", "n", "recovery"))
  expect_equal(nrow(df), 8)
  expect_equal(df$sev, c(100, 250, 300, 80, 90, 500, 700, 1200))
  expect_equal(df$n, c(1, 2, 3, 4, 5, 6, 8, 9))
  expect_equal(df$recovery, c(NA, NA, NA, NA, NA, 7, 12, 900))
  expect_equal(df, utils::read.csv(path), ignore_attr = TRUE)
  shiny::testServer(distribution_fitting_tool_Server, {
    read_fix_upload(session, path)
    expect_equal(nrow(data()), 8)
    session$setInputs(severity_var = "sev", execute_sev_analysis = 1)
    expect_equal(severity_data(), c(100, 250, 300, 80, 90, 500, 700, 1200))
  })
  #more fields than the header names: the extra column gets a V name
  path <- read_fix_lines(c("sev,n", "100,1", "250,2", "300,3", "80,4", "90,5", "500,6,7,8"))
  df <- dft_read_data(path)
  expect_equal(names(df), c("sev", "n", "V3", "V4"))
  expect_equal(dim(df), c(6, 4))
  expect_equal(df$V4, c(rep(NA, 5), 8))
  #and without a header
  path <- read_fix_lines(c("100,1", "250,2", "300,3", "80,4", "90,5", "500,6,7"))
  df <- dft_read_data(path, header = FALSE)
  expect_equal(names(df), c("V1", "V2", "V3"))
  expect_equal(df$V1, c(100, 250, 300, 80, 90, 500))
  #a separator at the end of each row still gives an empty last column, not row names
  path <- read_fix_lines(c("sev,n", "100,1,", "100,2,", "250,3,"))
  df <- dft_read_data(path)
  expect_equal(names(df), c("sev", "n", "V3"))
  expect_equal(df$sev, c(100, 100, 250))
  #quoted separators do not count as fields
  path <- read_fix_lines(c("name,sev", "\"Smith, J\",100", "\"Jones, A\",250"))
  df <- dft_read_data(path)
  expect_equal(names(df), c("name", "sev"))
  expect_equal(df$name, c("Smith, J", "Jones, A"))
  expect_equal(df$sev, c(100, 250))
})

test_that("the header is found after more than 100 blank lines", {
  #only the first 100 lines were searched: with 120 blank lines the header was read as a data row
  #("sev" and "n" the first row, and three rows instead of two)
  path <- read_fix_lines(c(rep("", 120), "sev,n", "1,2", "3,4"))
  df <- dft_read_data(path)
  expect_equal(names(df), c("sev", "n"))
  expect_equal(df$sev, c(1, 3))
  expect_equal(df$n, c(2, 4))
  #blank lines, then the header only
  expect_equal(dim(dft_read_data(read_fix_lines(c(rep("", 120), "sev,n")))), c(0, 2))
})

test_that("files with Windows, old Mac and Unix line endings, a byte order mark, or nothing in them read alike", {
  expected <- data.frame(sev = c(100, 250, 300), n = 1:3)
  text <- "sev,n\n100,1\n250,2\n300,3\n"
  for (eol in c("\n", "\r\n", "\r")) {
    df <- dft_read_data(read_fix_bytes(charToRaw(gsub("\n", eol, text, fixed = TRUE))))
    expect_equal(df, expected, ignore_attr = TRUE, info = deparse(eol))
    #with a byte order mark
    df <- dft_read_data(read_fix_bytes(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(gsub("\n", eol, text, fixed = TRUE)))))
    expect_equal(df, expected, ignore_attr = TRUE, info = deparse(eol))
  }
  #no final line ending
  expect_equal(dft_read_data(read_fix_bytes(charToRaw("sev,n\n100,1\n250,2\n300,3"))), expected, ignore_attr = TRUE)
  #a file with nothing in it has no rows, which the tool reports
  path <- read_fix_bytes(raw(0))
  expect_equal(nrow(dft_read_data(path)), 0)
  shiny::testServer(distribution_fitting_tool_Server, {
    read_fix_upload(session, path)
    expect_error(data(), "no data rows")
  })
})
