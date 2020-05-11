library(tidyverse)
library(pdftools)
options(digits = 3)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

system2("open", args = fn)

txt <- pdf_text(fn)

x <- str_split(txt[9], "\n")

s <- x[[1]]
s <- str_trim(s)

header_index <- str_which(s, "2015")[1]
tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
month <- tmp[1]
header <- tmp[-1]

tail_index  <- str_which(s, "Total")
n <- str_count(s,"\\d+")

out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
s <- str_remove_all(s, "[^\\d\\s]")

tab <- s %>% 
  as_data_frame() %>% 
  setNames(c("day", header)) %>%
  mutate_all(as.numeric)

tab <- tab %>% gather(year, deaths, -day) %>%
 mutate(deaths = as.numeric(deaths))

tab %>% filter(year < 2018) %>% 
  ggplot(aes(day, deaths, color = year)) +
  geom_smooth() +
  geom_vline(xintercept = 20, alpha=0.4) +
  geom_point()