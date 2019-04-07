# this script takes the official election results,
# turns them into a form compatible with the rcv package,
# and plots with our Sankey method

# it then saves the plot to a file
# this process was moved to a script to save knit time, because this data isn't changing

library(dplyr)
library(rvest)
library(here)
# data from the html table given by the state
link <- "https://sfelections.org/results/20180605/data/20180627/mayor/20180627_mayor.html"
path <- '//*[@id="ResultsContainer"]/table'
official <- read_html(link) %>%
  html_nodes(xpath = path) %>%
  html_table() %>%
  as.data.frame()
official2 <- select(official, matches("^[A-z]+\\.\\d$")) %>%
  rbind(c("NA", as.numeric(unlist(.[16, -1])) - as.numeric(unlist(.[15, -1])))) %>%
  slice(c(-1,-12:-17))
official2[official2 == 0] <- NA
official2 <- official2[c(1:8,10,9,11), ]
rownames(official2) <- NULL
official2$Round.1[9:10] <- official2$Round.2[9] <- 0
colnames(official2) <- c('candidate', paste0("round", 1:9))
official2 <- official2 %>%
  arrange(candidate == "NA",
          rowSums(is.na(.)),
          dplyr::desc(.[ ,ncol(.)])) %>%
  mutate_at(vars(starts_with('round')), as.numeric)
renamed <- mutate(official2,
                  first = stringr::str_sub(candidate, 1, 1),
                  last = stringr::str_extract(candidate, "[A-z]+$"),
                  candidate = paste(first, last, sep = ". ")) %>%
  select(-first, -last, -round1, -round2) %>%
  slice(-c(9,10))
renamed[9,1] <- "NA"

for_plotting <- rcv::make_d3list(results = renamed)

plot <- networkD3::sankeyNetwork(Links = for_plotting$values, Nodes = for_plotting$names,
                         Source = "source", Target = "target",
                         Value = "value", NodeID = "candidate", units = "voters",
                         fontSize = 10, nodeWidth = 20, width = 800)

htmlwidgets::saveWidget(plot, here('misc', 'sankey_official_sfmayor18.html'))
webshot::webshot(here('misc', 'sankey_official_sfmayor18.html'),
                 here('img', 'sankey_official_sfmayor18.png'))
