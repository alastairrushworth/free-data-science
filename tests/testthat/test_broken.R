library(htmldf)
library(dplyr)
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
  link_visit <- html_df(links)
  
  # check for any broken links
  link_broken <- link_visit %>% filter(status != 200)
  
  # print any broken links
  if(nrow(link_broken) > 0) print(link_broken$url)
  
  # expect
  expect_equal(nrow(link_broken), 0)
  
})