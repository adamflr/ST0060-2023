library(rvest)

url <- "https://www.imdb.com/name/nm3154303/"
html <- read_html(url)

html %>% 
  html_elements((".filmo-row > b > a")) %>% 
  html_text()

hrefs <- html %>% 
  html_elements(".filmo-row > b > a") %>% 
  html_attr("href")
hrefs <- paste0("https://www.imdb.com", hrefs)

html %>% html_elements("#filmography")

html %>% html_children() %>% html_element("p")

url <- "https://www.imdb.com/title/tt8315792/reviews?ref_=tt_urv"
html <- url %>% read_html()
html %>% 
  html_elements(".ipl-ratings-bar") %>% 
  html_element(".title")
  
html %>% html_elements(".title") %>% html_text()

# url <- "https://www.imdb.com/title/tt12593682/"
url <- "https://www.imdb.com/title/tt6723592/"
foo <- function(url) {
  html <- url %>% 
    read_html()
  html %>% 
    html_elements("#__next > main > div > section.ipc-page-background.ipc-page-background--base.sc-c7f03a63-0.kUbSjY > section > div:nth-child(4) > section > section > div.sc-80d4314-0.fjPRnj > div.sc-db8c1937-0.eGmDjE.sc-80d4314-3.iBtAhY > div > div:nth-child(1) > a > div > div > div.sc-7ab21ed2-0.fAePGh > div.sc-7ab21ed2-2.kYEdvH") %>% 
    html_text()
}

url <- "https://www.falsterbofagelstation.se/"
html <- read_html(url)

html %>% html_elements("#visibleMigrationTable")
html %>% html_table()
html %>% html_elements("table") %>% html_table()

url <- "https://www.falsterbofagelstation.se/strack/dagssummor/?date=2022-08-05"
View(html)
html %>% html_text()

url <- "https://www.slu.se/utbildning/program-kurser/program-pa-grundniva/tradgardsingenjor-odling/"
html <- url %>% read_html()

html %>% html_elements("accordion_ramschema")

url <- "https://en.wikipedia.org/wiki/List_of_Nobel_laureates_in_Literature"
tabs <- url %>% 
  read_html() %>% 
  html_table()
tab <- tabs[[1]]

ggplot(tab, aes(Country)) + geom_bar() + coord_flip()

tab %>% 
  separate(Country, "Country", sep = "\\(", extra = "drop") %>% 
  ggplot(aes(y = Country)) + geom_bar()

# Top 250
url <- "https://www.imdb.com/chart/top/?ref_=nv_mv_250"
html <- read_html(url)

## Title
titles <- html %>% 
  html_elements(".titleColumn > a") %>% 
  html_text()

## Rating
html %>% 
  html_elements(".ratingColumn > strong") %>% 
  html_attr("title") %>% 
  tibble() %>% 
  rename("text" = ".") %>% 
  separate(text, c("Rating", "based", "on", "Votes", "user", "ratings"), sep = " ") %>% 
  select(Rating, Votes) %>% 
  mutate(Rating = as.numeric(Rating),
         Votes = gsub(",", "", Votes),
         Votes = as.numeric(Votes),
         Title = titles) %>% 
  ggplot(aes(Votes, Rating)) + geom_point() + geom_text(aes(label = Title))

# Criterion collection
url <- "https://www.criterion.com/shop/browse/list"
html <- read_html(url)

tabs <- html %>% html_table()
tab <- tabs[[1]] %>% select(-2)

tab %>% count(Director, sort = T)

tab %>% filter(grepl("ergman", Director)) %>% pull(Title)
