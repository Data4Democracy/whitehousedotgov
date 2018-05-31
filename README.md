### Scraping and analyzing data from White House news feeds

This repo contains code and other artifacts that support scraping and analyzing data from the news feeds from
the White House (US Presidential administrations).  Initially the focus will be on curating data from the Trump
Administration, but we'll collect data from past administrations as well.

#### Accessing the data

We are maintaining the scraped data on [datadotworld](data.world) at https://data.world/data4democracy/trump-white-house-news-posts.

#### Running the Trump Admin news scraper

The current scraper processes the news articles posted on the [White House website](https://www.whitehouse.gov/news/).  It is
an R script, and the repo includes an RStudio project.  Be sure to check the list of `library` statements at the top to make
sure that those packages are installed in your R environment.  Then just run the script.  As of May 2018, it takes a couple hours
to run (it pauses a bit between each http request, to be nice).
