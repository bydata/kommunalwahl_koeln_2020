library(tidyverse)
library(rvest)


# parses a character vector formatted like "00.0 %" into double
parse_percentages <- function(x) {
  formatted <- str_replace(x, ",", ".") %>%
    str_replace(" %", "") %>%
    as.numeric()
  formatted
}

# # scrape results page Wahlbezirke
# result_url <- "https://wahlen.stadt-koeln.de/prod/KW2020/05315000/html5/Ratswahl_NRW_42_Uebersicht_wahlb.html"
# 
# page <- read_html(result_url)
# table <- html_node(page, css = "table") 
# df <- html_table(table)
# 
# str(df)
# 
# 
# df <- 
#   df %>%
#   filter(Wahlbezirk != "Stadt Köln") %>% 
#   rename(eligible = `Wahlberechtigte`,
#          turnout = `Wähler/innen`,
#          valid = `gültig`,
#          LINKE = `DIE LINKE`) %>%
#   # transform number format
#   mutate_at(vars(turnout:`Sonstige`), parse_percentages) %>%
#   # get Wahlbezirk ID
#   separate(Wahlbezirk, into = c("NUMMER", "NAME"), sep = " ") %>% 
#   # get count 
#   separate(Stand, into = c("counted", "stimmbezirke"), sep = " von ") %>% 
#   mutate_at(vars(counted, stimmbezirke), as.numeric)



# scrape results page Wahlbezirke
result_urls <- c(
  "ratswahl" = "https://wahlen.stadt-koeln.de/prod/KW2020/05315000/html5/Ratswahl_NRW_42_Uebersicht_wahlb.html",
  "bezirkswahl" = "https://wahlen.stadt-koeln.de/prod/KW2020/05315000/html5/Bezirksvertretungswahl_NRW_43_Uebersicht_wahlb.html",
  "obwahl" = "https://wahlen.stadt-koeln.de/prod/KW2020/05315000/html5/Buergermeisterwahl_NRW_44_Uebersicht_wahlb.html"
  )

scrape_results <- function(result_url) {
  page <- read_html(result_url)
  table <- html_node(page, css = "table") 
  df <- html_table(table)
  
  df <- 
    df %>%
    filter(Wahlbezirk != "Stadt Köln") %>% 
    rename(eligible = `Wahlberechtigte`,
           turnout = 4,
           valid = 5) %>%
    # transform number format
    # mutate_at(vars(turnout:`Sonstige`), parse_percentages) %>%
    mutate_at(vars(-Wahlbezirk, -Stand), parse_percentages) %>%
    # get Wahlbezirk ID
    separate(Wahlbezirk, into = c("NUMMER", "NAME"), sep = " ") %>% 
    # get count 
    separate(Stand, into = c("counted", "stimmbezirke"), sep = " von ") %>% 
    mutate_at(vars(counted, stimmbezirke), as.numeric) %>% 
    mutate(NUMMER = str_pad(NUMMER, 2, "left", "0"))
}


results <- map(result_urls, scrape_results)
  
# store results
write_rds(df, "output/results_koeln_2020.RData")

