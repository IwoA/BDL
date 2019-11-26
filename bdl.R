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

#### Struktura danych BDL
# K - kategoria
# G - grupa
# P - podgrupa

g <- read_xml("https://bdl.stat.gov.pl/api/v1/subjects?format=xml&page-size=35")
id <- g %>% xml_find_all("//id") %>% xml_text()
name <- g %>% xml_find_all("//name") %>% xml_text()
id_k <- id[grep("K",id)]
id_g  <- id[grep("G",id)]
kategorie <- data.frame(id_k,name)

## Wyszukiwanie
# https://bdl.stat.gov.pl/api/v1/subjects/search?name=p%C5%82ci&format=xml

s <- read_xml("https://bdl.stat.gov.pl/api/v1/subjects/search?name=14&format=xml")

id <- s %>% xml_find_all("//id") %>% xml_text()
id <- id[grep("P",id)]
parentId <- s %>% xml_find_all("//parentId") %>% xml_text()
children <- s %>% xml_find_all("//children") %>% xml_children()
name <- s %>% xml_find_all("//name") %>% xml_text()
search <- data.frame(name, id, parentId)

bdl_search <- function(text){
        text <- as.character(text) %>% iconv(., from = "windows-1250", to = "UTF-8") %>% URLencode(.) #zmienia kodowanie na UTF8 co jest konieczne do prawidłowego zakodowania URL
        url <- paste0("https://bdl.stat.gov.pl/api/v1/subjects/search?name=", text, "&format=xml")
        s <- read_xml(url)
        id <- s %>% xml_find_all("//id") %>% xml_text()
        id_g <- id[grep("G",id)]
        id_p <- id[grep("P",id)]
        if (length(id_g)==0) {
                id <- id_p
        } else {
                id <- id_g
                children <- id_p
        }
        parentId <- s %>% xml_find_all("//parentId") %>% xml_text()
        name <- s %>% xml_find_all("//name") %>% xml_text()
        search <- data.frame(name, id, parentId)
        return (search)
}

tree <- XML::xmlTreeParse(s, asTree = TRUE)
db <- plyr::ldply(XML::xmlToList(tree), data.frame) %>% select(starts_with("subject"))
db1 <- unlist(db) %>% as.data.frame(.)
db2 <- db1 %>% filter(. != "false")
db2 <- db2[!grepl("^\\d", as.character(db2[,1])),] %>% as.data.frame(.)
# db[dhttp://www.informit.com/articles/article.aspx?p=2215520
# https://cran.r-project.org/web/packages/XML/XML.pdf
