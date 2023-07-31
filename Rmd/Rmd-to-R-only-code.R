# Rmd to R. Code only
library(tidyverse)
for(i in 1:8){
  id_no <- i
  file <- paste0("Rmd/Datorövning-", id_no, ".Rmd")
  text <- read_lines(file)
  
  # Fix exercise lines
  ex_lines <- which(grepl("::: {.exercise", text, fixed = T))
  point_lines <- which(grepl(":::", text, fixed = T))
  
  dat_temp <- tibble(ex_lines) %>% 
    mutate(end_section = map_dbl(ex_lines, ~ min(point_lines[point_lines > .x]))) %>% 
    mutate(lines = map2(ex_lines, end_section, ~ .x:.y)) %>% 
    unnest(lines)
  
  text <- text[-dat_temp$lines]
  
  # Fix chunk lines
  ex_lines <- which(grepl("```{r", text, fixed = T))
  point_lines <- which(grepl("```", text, fixed = T))
  
  dat_temp <- tibble(ex_lines) %>% 
    mutate(end_section = map_dbl(ex_lines, ~ min(point_lines[point_lines > .x]))) %>% 
    mutate(lines = map2(ex_lines, end_section, ~ (.x):(.y))) %>% 
    unnest(lines)
  
  text <- text[dat_temp$lines]
  
  # Add empty lines at chunk end and start
  text <- text[!grepl("```{r", text, fixed = T)]
  
  point_lines <- which(grepl("```", text, fixed = T))
  text[point_lines] <- ""
  
  # Clear comments
  text <- map_chr(text, \(x) {
    l <- length(strsplit(x, "#")[[1]])
    ifelse(x == "", "", paste0(strsplit(x, "#")[[1]][1:(l - 1)], collapse = "#") %>% trimws("right"))
    })
  
  # Write files
  write_lines(text, paste0("Skriptversioner/Datorövning-", id_no, "-enbart-kod.R"))
}
