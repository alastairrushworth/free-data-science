library(htmldf)
library(rvest)
context("Check broken links")


test_that("Check broken", {
  # gather links from page
  pp <- read_html('https://github.com/alastairrushworth/free-data-science') 
  links <- 
    pp %>% 
    html_nodes("article[class='markdown-body entry-content container-lg']") %>%
    html_nodes('a') %>%
    html_attr('href') %>%
    grep('^http', ., value = TRUE) %>%
    unique(.)
  
  # check links for problems
  vv <- html_df(links)
  
  # check for any broken links
  broken_links <- which(sapply(vv$source, function(v) class(v)[1]) == 'try-error')
  
  # 
  if(length(broken_links) > 0) print(vv$url[broken_links])
  
  # expect
  expect_equal(length(broken_links), 0)
  
})