file_response <- function(body, filename, content_type) {
  list(body = body, filename = filename, content_type = content_type,
       status = 200L)
}

dta_bytes <- function() {
  tmp <- withr::local_tempfile(fileext = ".dta")
  haven::write_dta(data.frame(x = 1:3), tmp)
  file_response(readBin(tmp, "raw", file.size(tmp)), "a.dta", "dta")
}
