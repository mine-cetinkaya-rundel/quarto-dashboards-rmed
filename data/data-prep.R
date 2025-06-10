# load packages ---------------------------------------------------------------

library(tidyverse) 
library(glue)

# load data -------------------------------------------------------------------

program_raw <- read_csv("data/program-05-25.csv")

# remove emails ---------------------------------------------------------------

program_interim <- program_raw |>
  select(!submitter_email)

write_csv(program_interim, "data/program-interim.csv")

# format data for pretty tables -----------------------------------------------

program <- program_interim |>
  arrange(session, title) |>
  mutate(
    content = str_replace_all(content, "\\*", "<br><br>*"),
    content = str_replace_all(content, ": NA", ": None"),
    content = str_replace(content, "Learning goals:", "<br>**Learning goals:**"),
    content = str_replace(content, "Target audience:", "<br>**Target audience:**"),
    content = str_replace(content, "Prerequisites:", "<br>**Prerequisites:**"),
    speakers = str_replace_all(speakers, "\\;", ","),
    co_authors = str_replace_all(co_authors, "\\;", ","),
    co_authors = if_else(is.na(co_authors), "", co_authors),
    formatted_date = format(date, format = "%a, %b %e, %Y"),
    info = if_else(
      session == "Poster",
      paste0(
      "<span style='color:#2165b6;'>**",title,"**</span>", 
      "<br><br><details><summary>More info</summary>", 
      "<p style='font-size:90%;'>", 
      content, 
      "<br><br>**Date and time:** ", formatted_date, " - ", time,
      "<br><br>**Author(s):** ", speakers, 
      if_else(co_authors == "", "", paste(";", co_authors)), 
      "<br><br>**Keyword(s):** ", keywords, 
      "</p>",
      "</details>"
      ),
      paste0(
      "<span style='color:#2165b6;'>**",title,"**</span>", 
      "<br><br><details><summary>More info</summary>", 
      "<p style='font-size:90%;'>", 
      content, 
      "<br><br>**Date and time:** ", formatted_date, " - ", time,
      "<br><br>**Author(s):** ", speakers, 
      if_else(co_authors == "", "", paste(";", co_authors)), 
      "<br><br>**Keyword(s):** ", keywords, 
      "<br><br>**Video recording available after conference:** ", video_recording, 
      "</p>",
      "</details>"
      )
    )
    ,
    speakers = str_replace_all(speakers, "\\, ", "<br>")
  ) |>
  relocate(info, .after = room) |>
  select(!c(id, title, content, video_recording, co_authors, formatted_date))

# write data without emails ---------------------------------------------------

write_csv(program, "data/program.csv")
