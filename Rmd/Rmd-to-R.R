# Rmd to script ----
library(tidyverse)
for(i in 1:8){
  id_no <- i
  file <- paste0("Rmd/Datorövning-", id_no, ".Rmd")
  text <- read_lines(file)
  
  # Fix exercise lines
  ex_lines <- which(grepl("::: {.exercise", text, fixed = T))
  
  tab_ex <- tibble(text = text[ex_lines]) %>% 
    mutate(Inner = substring(text, 22, nchar(text) - 2),
           id = 1:n(),
           New = paste0("Uppgift ", id_no, ".", id, ". (", Inner, ")"))
  
  text[ex_lines] <- tab_ex$New
  
  # Comment any non-code
  start <- which(grepl("```{", text, fixed = T))
  end <- which(text == "```")
  
  tab_com <- tibble(start = start, end = end) %>% 
    mutate(lines = map2(start, end, ~ .x:.y)) %>% 
    unnest(cols = lines)
  
  text[-tab_com$lines] <- paste0("# ", text[-tab_com$lines])
  
  text[c(start, end)] <- ""
  
  # Remove possible top part ----
  dashes <- which(grepl("---", text))
  
  if (any(dashes == 1)) text <- text[(dashes[2] + 1):length(text)]
  
  # cat(paste(text, collapse = "\n"))
  
  # Wrap comments ----
  tab_text <- tibble(text = text) %>% 
    mutate(Comment = substring(text, 1, 1) == "#") %>% 
    mutate(Wrapped = map(text, ~ strwrap(.x, width = 100))) %>% 
    mutate(Wrapped = ifelse(Comment, Wrapped, text)) %>% 
    unnest(cols = Wrapped) %>% 
    mutate(Starts_w_hastag = substring(Wrapped, 1, 1) == "#") %>% 
    mutate(New = ifelse(Comment & !Starts_w_hastag, paste("#", Wrapped), Wrapped))
  # tab_text %>% print(n = 45)
  
  # cat(paste(tab_text$New, collapse = "\n"))
  
  text <- tab_text$New
  
  # Write files
  write_lines(text, paste0("Skriptversioner/Datorövning-", id_no, "-fulltext.R"))
}
