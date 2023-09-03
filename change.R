
calculate_change <- function(df,id_column,id,curr_year,past_year,college) {
  df <- df %>% filter(College == college)
  indices <- which(df[[{{id_column}}]] == id)
  curr_value <- NA
  past_value <- NA
  for (x in indices) {
    if (df[x,]$Year == curr_year) {
      curr_value <- df[x,]$Compensation
    } else if (df[x,]$Year == past_year) {
      past_value <- df[x,]$Compensation
    }
  }
  if (!is.na(curr_value) && !is.na(past_value)) {
    change <- (curr_value/past_value) - 1
    return(change)
  } else {
    return(NA)
  }
}

df <- read_csv("df.csv")

change_df <- df %>% filter(Year %in% c("2019","2022"))
change_df <- change_df %>% filter(Compensation > 0)
change_df$Name <- str_squish(change_df$Name)

change_df <- cbind(change_df,Raise=NA)
for (i in 1:length(change_df$Raise)) {
  if (change_df[i,]$Year == "2022") {
    curr_name <- change_df[i,]$Position
    curr_college <- change_df[i,]$College
    curr_change <- calculate_change(change_df,'Position',curr_name,"2022","2019",curr_college)
    change_df[i,]$Raise <- curr_change
  }
}
