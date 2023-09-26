# Rmd to R. Code only
library(tidyverse)
for(i in 1:8){
  id_no <- i
  file <- paste0("Rmd/Datorövning-", id_no, ".Rmd")
  text <- read_lines(file)
  
  # Fix exercise lines
  ex_lines <- which(grepl("::: {.exercise", text, fixed = T))
  point_lines <- which(grepl(":::", text, fixed = T))
  end_lines <- point_lines[!(point_lines %in% ex_lines)]
  
  # Set headers for exercises
  text[ex_lines] <- paste0("##### Övningsuppgift ", i, ".", 1:length(ex_lines), " #####")
  text[end_lines] <- "##########"
  
  # Identify which rows are exercises
  dat_temp_ex <- tibble(ex_lines) %>% 
    mutate(end_section = map_dbl(ex_lines, ~ min(point_lines[point_lines > .x]))) %>% 
    mutate(lines = map2(ex_lines, end_section, ~ .x:.y)) %>% 
    unnest(lines)
  
  # Fix chunk lines
  ex_lines <- which(grepl("```{r", text, fixed = T))
  point_lines <- which(grepl("```", text, fixed = T))
  
  # Identify which rows are in chunks
  dat_temp_chunks <- tibble(ex_lines) %>% 
    mutate(end_section = map_dbl(ex_lines, ~ min(point_lines[point_lines > .x]))) %>% 
    mutate(lines = map2(ex_lines, end_section, ~ (.x):(.y))) %>% 
    unnest(lines)
  
  # Add empty lines at chunk end and start
  # text <- text[!grepl("```{r", text, fixed = T)]
  # 
  # point_lines <- which(grepl("```", text, fixed = T))
  # text[point_lines] <- ""
  
  # Clear comments
  # text <- map_chr(text, \(x) {
  #   l <- length(strsplit(x, "#")[[1]])
  #   ifelse(x == "", "", paste0(strsplit(x, "#")[[1]][1:(l - 1)], collapse = "#") %>% trimws("right"))
  #   })
  
  # Identify headers
  dat_temp_head <- which(substring(text, 1, 1) == "#")
  
  # Add # to everything not a chunk or a header
  selection <- unique(sort(c(dat_temp_chunks$lines, dat_temp_head)))
  text[-selection] <- paste("#", text[-selection])
  
  # Remove everything outside of exercises, chunks, and headers
  new_text <- text[unique(sort(c(dat_temp_head, dat_temp_ex$lines, dat_temp_ex$lines - 1, dat_temp_ex$lines + 1, dat_temp_chunks$lines)))]
  
  ex_lines <- which(grepl("```{r", new_text, fixed = T))
  point_lines <- which(grepl("```", new_text, fixed = T))
  
  new_text[c(ex_lines, point_lines)] <- ""
  new_text[new_text == "# "] <- ""
  
  # Remove double empty lines
  new_text <- new_text[-which(new_text == "")[diff(which(new_text == "")) == 1]]
  
  # cat(paste(new_text, collapse = "\n"))
  
  # Write files
  write_lines(new_text, paste0("Skriptversioner/Datorövning-", id_no, ".R"))
}
