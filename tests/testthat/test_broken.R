test_that("README contains no broken links", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("httr2")
  skip_if_not_installed("stringr")

  # Locate the rendered README, wherever the test is run from.
  readme <- Filter(file.exists, c(
    "README.md",
    file.path("..", "..", "README.md")
  ))
  skip_if(length(readme) == 0L, "README.md not found")

  text <- readme[[1]] |>
    readLines(warn = FALSE, encoding = "UTF-8") |>
    paste(collapse = "\n")

  # Extract every http(s) target inside markdown ()-style links.
  links <- text |>
    stringr::str_extract_all("\\((https?://[^)\\s]+)\\)") |>
    unlist() |>
    stringr::str_remove_all("^\\(|\\)$") |>
    unique()

  # Status for a single URL: follow redirects with a browser-like
  # user-agent and retry transient failures. Anti-bot codes
  # (401/403/429) still mean the page exists; NA means unreachable.
  status_of <- function(url) {
    resp <- tryCatch(
      url |>
        httr2::request() |>
        httr2::req_user_agent("Mozilla/5.0 (compatible; free-data-science link checker)") |>
        httr2::req_timeout(30) |>
        httr2::req_retry(max_tries = 3) |>
        httr2::req_error(is_error = function(resp) FALSE) |>
        httr2::req_perform(),
      error = function(e) NULL
    )
    if (is.null(resp)) NA_integer_ else httr2::resp_status(resp)
  }

  status <- vapply(links, status_of, integer(1L))
  reachable <- !is.na(status) & (status < 400L | status %in% c(401L, 403L, 429L))
  broken <- links[!reachable]

  if (length(broken) > 0L) {
    codes <- ifelse(is.na(status[!reachable]), "ERR", status[!reachable])
    cat("\nBroken or unreachable links:\n", sep = "")
    cat(sprintf("  [%s] %s", codes, broken), sep = "\n")
  }

  expect_equal(length(broken), 0L)
})
