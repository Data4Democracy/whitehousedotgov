library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)

urlBase <- 'https://www.whitehouse.gov/news/'

html <- read_html(urlBase)

pageCount <- html %>% html_nodes('div.pagination a.page-numbers') %>% tail(1) %>% html_text() %>% as.integer()

if (is.na(pageCount)) {
  stop('Page count not obtained...parse error')
}

parsePage <- function(url) {
  writeLines(paste0('Index page: ', url))
  parseArticles <- function(articleList) {
    articleList %>% map_dfr(function(article) {
      titleParentDiv <- article %>% html_node('h2') %>% html_node(xpath='..')
      link <- article %>% html_node('h2 a') %>% html_attr('href')
      title <- article %>% html_node('h2 a') %>% html_text()
      writeLines(paste0('Downloading page with title ', title))
      slp <- rchisq(1, 0.5)
      writeLines(paste0('Sleeping for ', slp, ' seconds...'))
      Sys.sleep(slp)
      page <- read_html(link) %>% html_node('div.page-content')
      pageText <- page %>% html_nodes('p,li:not(.share__item)') %>% map_chr(html_text) %>% paste0(collapse='\n')
      tibble(
        type=article %>% html_attr('class') %>% str_split(' ') %>% unlist() %>% .[1],
        subType=titleParentDiv %>% html_node('p') %>% html_text(),
        link=link,
        title=title,
        date=titleParentDiv %>% html_node('h2') %>% html_node(xpath='following-sibling::*[1]') %>% html_node('time') %>% html_text() %>% mdy(),
        text=pageText
      )
    })
  }
  ret <- parseArticles(read_html(url) %>% html_nodes('article') %>% rev())
  diff <- base::setdiff(read_html(url) %>% html_nodes('article h2 a') %>% html_attr('href'), ret$link)
  while(length(diff)) {
    extras <- parseArticles(read_html(url) %>% rev(html_nodes('article') %>% keep(function(node) {node %>% html_node('h2 a') %>% html_attr('href') %in% diff})))
    writeLines(paste0('Picked up ', nrow(extras), ' articles added to page ', url, ' since previous parse'))
    ret <- bind_rows(ret, extras)
    diff <- base::setdiff(read_html(url) %>% html_nodes('article h2 a') %>% html_attr('href'), ret$link)
  }
  ret
}

articles <- map_df(rev(tail(seq(pageCount), -1)), function(p) {
      slp <- rchisq(1, 0.9)
      writeLines(paste0('Sleeping for ', slp, ' seconds...'))
      Sys.sleep(slp)
      writeLines(paste0('Parsing page ', p, ' of ', pageCount))
      parsePage(paste0(urlBase, 'page/', p, '/'))
    }) %>% bind_rows(parsePage(urlBase))

articles <- articles %>%
  mutate_if(is.character, function(s) {
    gsub(x=s, pattern='(.+)\\s$', replacement='\\1') %>% trimws()
  }) %>% as_tibble() %>%
  mutate(text=gsub(x=text, pattern='\n', replacement='<br>'),
         text=gsub(x=text, pattern='“|”', replacement='"'),
         text=gsub(x=text, pattern='^"', replacement=''),
         text=gsub(x=text, pattern='"$', replacement=''))

dwapi::configure(Sys.getenv("DATA_WORLD_RW_API_KEY"))
dwapi::upload_data_frame("data4democracy/trump-white-house-news-posts", articles2, 'TrumpWhiteHouseNewsPosts.csv')

