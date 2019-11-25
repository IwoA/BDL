library(xml2)
library(lubridate)
library(dplyr)
galewice <- "051011818032"

a <- read_xml("https://bdl.stat.gov.pl/api/v1/data/by-unit/051011818032?var-id=450540&format=xml")

unit <-  a %>% xml_find_all("//unitName") %>% xml_text()
year <- a %>% xml_find_all("//year") %>% xml_text() %>% parse_date_time(., orders = "y")
value <- a %>% xml_find_all("//val") %>% xml_text() %>% as.numeric(.)
result <- data.frame(unit, year, value)

### Lista wszystkich miejscowości z kodami

u <- read_xml("https://bdl.stat.gov.pl/api/v1/units?format=xml&page=0&page-size=100")

last <- u %>% xml_find_all("//last") %>% xml_text()
pages <- stringr::str_extract(last, "\\d{2}")

id <- u %>% xml_find_all("//id") %>% xml_text()
name <- u %>% xml_find_all("//name") %>% xml_text()
units <- data.frame(id,name)



for (i in 1:as.numeric(pages)) {
     url <- paste0("https://bdl.stat.gov.pl/api/v1/units?format=xml&page=", i, "&page-size=100")
     u <- read_xml(url)
     id <- u %>% xml_find_all("//id") %>% xml_text()
     name <- u %>% xml_find_all("//name") %>% xml_text()
     tmp <- data.frame(id,name)
     units <- bind_rows(units, tmp)
}


write.csv(units, "dbl.csv", fileEncoding = "UTF8")
