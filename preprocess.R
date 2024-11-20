library(tidyverse)

data <- read.csv("artvis_dump.csv")
data_names <- data %>% colnames()
# e.title is inconsistent, it sometimes is enclosed by quotes but not always
# it also contains commas which makes the CSV worthless
# what we do is split by commas for every column until e.title
# then we do the same thing for evry column after e.title
# then whats left is e.title
data_split <- data.frame()
for (i in 1:length(data$a.id)){
  row <- data$a.id[i]
  # Den Haag is written with a comma for some reason, were changing that right here
  if (grepl("Hague, The", row)){
    # stop(row)
    row <- row %>% str_replace("Hague, The", "Den Haag")
  }
  
  row <- row %>% str_split(",") %>% .[[1]]
  if (i %% 100 == 0) print(i)
  nsplits <- length(row)
  newrow <- data.frame(
    a.id=row[1],
    a.firstname=row[2],
    a.lastname=row[3],
    a.gender=row[4],
    a.birthdate=row[5],
    a.deathdate=row[6],
    a.birthplace=row[7],
    a.deathplace=row[8],
    a.nationality=row[9],
    e.id=row[10]
  )
  # iterate through split text backwards
  for (j in 0:7){
    colname_from_back <- data_names[length(data_names) - j] 
    newrow[[colname_from_back]] <- row[nsplits - j]
  }
  # now also add the middle which is the e.title
  newrow$e.title <- paste(row[11:(nsplits-8)], collapse = ",")
  
  data_split <- data_split %>% rbind(newrow)
}
# now replace the original data variable
data <- data_split %>% 
  select(data_names)

data
data$e.startdate %>% unique()

data <- data %>% 
  mutate(
    a.birthdate = a.birthdate %>% str_remove_all("\"") %>% as.Date(),
    a.deathdate = a.deathdate %>% str_remove_all("\"") %>% as.Date(),
    a.birthyear = format(as.Date(a.birthdate), "%Y"),
    a.deathyear = format(as.Date(a.deathdate), "%Y")
  ) %>% 
  select(starts_with("a"), starts_with("e"), everything())
  


write_rds(data, "artvis.rds")




# there is 1 more row thatmakes a problem, where there was a comma
# in the venue name in row artvis[2633,]
# artvis[2633,]$e.title
# artvis[2633,]$e.title %>% str_split(",") %>% .[[1]]
 