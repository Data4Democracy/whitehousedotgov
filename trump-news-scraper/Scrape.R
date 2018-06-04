library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)

DW_DATASET_NAME = "data4democracy/trump-white-house-news-posts"
DW_FILE_NAME = 'TrumpWhiteHouseNewsPosts.csv'
URL_BASE = 'https://www.whitehouse.gov/news/'

dwapi::configure(Sys.getenv("DATA_WORLD_RW_API_KEY"))

existingArticlesDf <- NULL
resp <- NULL

tryCatch({
  tf <- tempfile(fileext='csv')
  resp <- dwapi::download_file(DW_DATASET_NAME, DW_FILE_NAME, tf)
  existingArticlesDf <- read_csv(tf)
}, error=function(e) {
  writeLines(paste0('No existing file on data.world', resp$message))
})

html <- read_html(URL_BASE)

pageCount <- html %>% html_nodes('div.pagination a.page-numbers') %>% tail(1) %>% html_text() %>% as.integer()

if (is.na(pageCount)) {
  stop('Page count not obtained...parse error')
}

existingArticleLinks <- existingArticlesDf$link

parsePage <- function(url) {
  writeLines(paste0('Index page: ', url))
  parseArticles <- function(articleList) {
    articleList %>% map_dfr(function(article) {
      link <- article %>% html_node('h2 a') %>% html_attr('href')
      link <- gsub(x=link, pattern='(.+)\\s$', replacement='\\1') %>% trimws()
      title <- article %>% html_node('h2 a') %>% html_text()
      if (!(link %in% existingArticleLinks)) {
        titleParentDiv <- article %>% html_node('h2') %>% html_node(xpath='..')
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
      } else {
        writeLines(paste0("skipping article with title ", title, " because it's in existing dataset"))
      }
    })
  }
  ret <- parseArticles(read_html(url) %>% html_nodes('article') %>% rev())
  if (nrow(ret)) {
    diffnc <- base::setdiff(read_html(url) %>% html_nodes('article h2 a') %>% html_attr('href'), c(ret$link, existingArticleLinks))
    while(length(diffnc)) {
      extras <- parseArticles(rev(read_html(url) %>% html_nodes('article') %>% keep(function(node) {(node %>% html_node('h2 a') %>% html_attr('href')) %in% diffnc})))
      writeLines(paste0('Picked up ', nrow(extras), ' articles added to page ', url, ' since previous parse'))
      ret <- bind_rows(ret, extras)
      diffnc <- base::setdiff(read_html(url) %>% html_nodes('article h2 a') %>% html_attr('href'), c(ret$link, existingArticleLinks))
    }
  }
  ret
}

articles <- map_df(rev(tail(seq(pageCount), -1)), function(p) {
      slp <- rchisq(1, 0.9)
      writeLines(paste0('Sleeping for ', slp, ' seconds...'))
      Sys.sleep(slp)
      writeLines(paste0('Parsing page ', p, ' of ', pageCount))
      parsePage(paste0(URL_BASE, 'page/', p, '/'))
    }) %>% bind_rows(parsePage(URL_BASE))

articles <- articles %>%
  mutate_if(is.character, function(s) {
    gsub(x=s, pattern='(.+)\\s$', replacement='\\1') %>% trimws()
  }) %>% as_tibble() %>%
  mutate(text=gsub(x=text, pattern='\n', replacement='<br>'),
         text=gsub(x=text, pattern='“|”', replacement='"'),
         text=gsub(x=text, pattern='^"', replacement=''),
         text=gsub(x=text, pattern='"$', replacement='')) %>%
  bind_rows(existingArticlesDf) %>%
  arrange(desc(date))

#dwapi::upload_data_frame(DW_DATASET_NAME, articles, DW_FILE_NAME)
#saveRDS(articles, 'articles.rds')

