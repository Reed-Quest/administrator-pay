library(tidyverse)
library(rvest)

split_text_to_columns <- function(df,column,character) {
  temp_table <- as.data.frame(matrix(ncol=3,nrow=length(df[[{{column}}]])))
  colnames(temp_table) <- c("Original","Left_Split","Right_Split")
  temp_table$Original <- df[[{{column}}]]
  for (i in 1:length(temp_table$Original)) {
    curr <- temp_table[i,]$Original
    split <- gregexpr(character,curr,fixed=TRUE)[[1]][1]
    temp_table[i,]$Left_Split <- substr(curr,1,split)
    temp_table[i,]$Right_Split <- substr(curr,split,str_length(curr))
  }

  temp_table$Left_Split <- gsub(character,"",temp_table$Left_Split,fixed=TRUE)
  temp_table$Right_Split <- gsub(character,"",temp_table$Right_Split,fixed=TRUE)

  colnames(temp_table) <- c(column,"Left_Split","Right_Split")

  df <- left_join(df,temp_table,by={{column}})

  return(df)
}

get_table_from_filing <- function(filing) {
  test_filing <- filing
  year <- test_filing %>% html_nodes(".year-label") %>% html_text()
  year <- year[1]

  curr_table <- test_filing %>% html_nodes(".employee")

  if (length(curr_table) > 0) {
    temp_df <- curr_table[1] %>% html_table()
    temp_df <- as.data.frame(temp_df)
    colnames(temp_df) <- c("Name","Compensation")

    temp_df$Name <- str_squish(temp_df$Name)
    temp_df$Compensation <- parse_number(temp_df$Compensation)
    temp_df <- temp_df %>% filter(!is.na(Compensation))

    temp_df <- split_text_to_columns(temp_df,'Name',"(")
    temp_df <- temp_df %>% select(!Name)
    colnames(temp_df) <- c("Compensation","Name","Position")

    temp_df <- cbind(temp_df,Year=year)
  } else {
    temp_df <- as.data.frame(matrix(ncol=4,nrow=0))
    colnames(temp_df) <- c("Compensation","Name","Position","Year")
  }

  return(temp_df)
}

get_college_filings <- function(url,college_name) {
  page <- read_html(url)

  all_filings <- page %>% html_nodes(".single-filing")

  df <- as.data.frame(matrix(ncol=4,nrow=0))
  colnames(df) <- c("Compensation","Name","Position","Year")

  for (i in 1:length(all_filings)) {
    message(i)
    x <- all_filings[i]
    temp_df <- get_table_from_filing(x)
    df <- rbind(df,temp_df)
  }

  colnames(df) <- c("Compensation","Name","Position","Date")
  df$Date <- my(df$Date)

  df <- cbind(df,Year=NA)
  df$Year <- year(df$Date)

  df <- cbind(df,College=college_name)

  return(df)
}

df <- as.data.frame(matrix(ncol=6,nrow=0))
colnames(df) <- c("Compensation","Name","Position","Date","Year","College")

dict <- read_csv("peers_propublica.csv")

for (i in 1:length(dict$College)) {
  college_name <- dict[i,]$College
  url <- dict[i,]$Url
  temp_df <- get_college_filings(url,college_name)
  df <- rbind(df,temp_df)
}

df$Position <- gsub(")","",df$Position,fixed=TRUE)
df$Name <- str_to_title(df$Name)
df$Position <- str_to_title(df$Position)

beeswarm <- df %>% filter(Position != "Trustee") %>% filter(Compensation > 0)

beeswarm <- cbind(beeswarm,Color=NA)
for (i in 1:length(beeswarm$College)) {
  curr <- beeswarm[i,]$College
  if (curr == "Reed College") {
    beeswarm[i,]$Color <- "Reed"
  } else {
    beeswarm[i,]$Color <- "Not Reed"
  }
}

reed_presidents <- df %>% filter(College == "Reed College") %>% filter(Position == "President")

all_presidents <- df %>% filter(Position == "President")









