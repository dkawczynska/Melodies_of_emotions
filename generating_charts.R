library(dplyr)
library(ggplot2)
library(fmsb)
library(lattice)
library(data.table)
library(tidyr)
library(reshape2)


# Loading the data frame
mxmh <- read.csv("mxmh_survey_results.csv")

# Replacing spaces in column names with '_'
colnames(mxmh) <- gsub("\\.", "_", colnames(mxmh))

# Violin plot depicting the level of insomnia based on the daily
# hours spent listening to music
violin_insomnia <- {
  mxmh %>% 
  select(Insomnia, Hours_per_day) %>% 
  filter(!is.na(Hours_per_day) & Hours_per_day <= 24) %>% 
  mutate(insomnia_level = case_when(Insomnia <= 4 ~ "low level (lower than 4)",
                                    Insomnia > 4 & Insomnia < 7 ~ "medium level (between 4 and 7)",
                                    Insomnia >= 7 ~ "high level (higher than 7)"),
         insomnia_level = factor(insomnia_level, levels = c("low level (lower than 4)",
                                           "medium level (between 4 and 7)",
                                           "high level (higher than 7)"))) %>%
  ggplot(aes(x = insomnia_level, y = Hours_per_day)) +
  geom_violin()+
  labs(title = "Violin plot depicting the level of insomnia",
        x = "Insomnia level",
        y = "Daily hours spent listening to music") +
  theme_light()
}

# Violin plot depicting the level of anxiety based on the daily
# hours spent listening to music
violin_anxiety <- {
  mxmh %>% 
    select(Anxiety, Hours_per_day) %>% 
    filter(!is.na(Hours_per_day) & Hours_per_day <= 24) %>% 
    mutate(anxiety_level = case_when(Anxiety <= 4 ~ "low level (lower than 4)",
                                      Anxiety > 4 & Anxiety < 7 ~ "medium level (between 4 and 7)",
                                      Anxiety >= 7 ~ "high level (higher than 7)"),
           anxiety_level = factor(anxiety_level, levels = c("low level (lower than 4)",
                                                              "medium level (between 4 and 7)",
                                                              "high level (higher than 7)"))) %>%
    ggplot(aes(x = anxiety_level, y = Hours_per_day)) +
    geom_violin()+
    labs(title = "Violin plot depicting the level of anxiety",
         x = "Anxiety level",
         y = "Daily hours spent listening to music") +
    theme_light()
}

# Violin plot depicting the level of depression based on the daily
# hours spent listening to music
violin_depression <- {
  mxmh %>% 
    select(Depression, Hours_per_day) %>% 
    filter(!is.na(Hours_per_day) & Hours_per_day <= 24) %>% 
    mutate(depression_level = case_when(Depression <= 4 ~ "low level (lower than 4)",
                                        Depression > 4 & Depression < 7 ~ "medium level (between 4 and 7)",
                                        Depression >= 7 ~ "high level (higher than 7)"),
           depression_level = factor(depression_level, levels = c("low level (lower than 4)",
                                                            "medium level (between 4 and 7)",
                                                            "high level (higher than 7)"))) %>%
    ggplot(aes(x = depression_level, y = Hours_per_day)) +
    geom_violin() +
    labs(title = "Violin plot depicting the level of depression",
         x = "Depression level",
         y = "Daily hours spent listening to music") +
    theme_light()
}

# A box plot describing the relationship between the tempo of the listened music
# and the rating of anxiety, depression, insomnia, and OCD levels by the respondents
boxplot_tempo <- {
  mxmh %>% 
  filter(!is.na(BPM)) %>% 
  mutate(tempo = case_when( BPM <= 70 ~ "slow \n(BPM lower than 70)",
                            BPM > 70 & BPM <= 120 ~ "moderato \n(BPM between 70 and 120)",
                            BPM > 120 ~ "quick \n(BPM higher than 120)"),
          tempo = factor(tempo, levels = c("slow \n(BPM lower than 70)",
                                          "moderato \n(BPM between 70 and 120)",
                                          "quick \n(BPM higher than 120)"))) %>%
  select(tempo, Anxiety, Depression, Insomnia, OCD) %>%
  melt(id.vars = "tempo", 
       measure.vars = c("Anxiety", "Depression", "Insomnia", "OCD")) %>%
  ggplot(aes(x = tempo, y = value, color = variable)) + 
  geom_boxplot() +
  labs(x = "Tempo level",
       y = "Rating (on a scale of 0-10) of the condition",
       color = "Type of illness") +
  theme_light()
}

# An auxiliary data frame where the number of hours spent listening to music
# during the day is divided into intervals
hours_table <- {
  mxmh %>% 
  filter(!is.na(While_working) & While_working != "" &
           !is.na(Music_effects) & Music_effects != "") %>% 
  mutate(hours = case_when( Hours_per_day < 1 ~ "[0, 1)",
                            Hours_per_day >= 1 & Hours_per_day < 4 ~ "[1,4)",
                            Hours_per_day >= 4 & Hours_per_day < 8 ~ "[4,8)",
                            Hours_per_day >= 8 & Hours_per_day < 12 ~ "[8,12)",
                            Hours_per_day >= 12 ~ "12+"),
         hours = factor(hours, levels = c("[0, 1)", "[1,4)", "[4,8)", 
                                          "[8,12)", "12+"))) %>% 
  group_by(hours, While_working) %>%
  select(hours, While_working,
         Anxiety, Depression, Insomnia, OCD)
}

# The data frame hours_table for the option of listening to music during work
hours_table_ww <- {
  hours_table %>%
  filter(While_working == "Yes")
}

# The data frame hours_table for the option of not listening to music during work
hours_table_nww <- {
  hours_table %>%
    filter(While_working == "No")
}

# A box plot describing the relationship between listening to music during work,
# the number of hours spent listening to music during the day, and the rating of
# anxiety, depression, insomnia, and obsessive-compulsive disorder levels by the respondents
boxplot_ww <- {
  bwplot(Anxiety +  Depression + Insomnia + OCD ~ hours,
         data = hours_table_ww,
         groups = hours,   
         coef = 2, 
         do.out = TRUE,
         outer = TRUE,
         xlab = "The number of hours spent listening to music per day",
         ylab = "Rating (on a scale of 0-10) of the condition",
         main = "Listening to music during work")
}

boxplot_nww <- {
  bwplot(Anxiety +  Depression + Insomnia + OCD ~ hours,
         data = hours_table_nww,
         groups = hours,   
         coef = 2, 
         do.out = TRUE,
         outer = TRUE,
         xlab = "The number of hours spent listening to music per day",
         ylab = "Rating (on a scale of 0-10) of the condition",
         main = "Not listening to music during work")
}

# Radar charts for median perception levels of: Anxiety, Depression, Insomnia,
# OCD based on favorite music genre.
fav_genre_table <- {
  mxmh %>% 
  group_by(Fav_genre) %>% 
  filter(!is.na(Fav_genre) & Fav_genre != "") %>%
  select(Fav_genre, Anxiety, Depression, Insomnia, OCD) %>% 
  summarise_at(vars("Anxiety","Depression","Insomnia", "OCD"),median)
}

# Anxiety
{
  fav_genre_table_a <- fav_genre_table %>%
    select(Fav_genre, Anxiety) %>%
    pivot_wider(names_from = Fav_genre, values_from = Anxiety) %>%
    as.data.frame()
  
  rownames(fav_genre_table_a) <- fav_genre_table_a$Fav_genre
  # Removing the Fav_genre column
  fav_genre_table_awf <-fav_genre_table_a[, -1]
  
  # Adding new columns specifying the range of chart
  fav_genre_table_ac <- rbind(rep(10, ncol(fav_genre_table_awf)), 
                              rep(0, ncol(fav_genre_table_awf)), 
                              fav_genre_table_awf)
  
  radar_chart_a <- radarchart(fav_genre_table_ac, axistype = 0, seg = 10, 
                              cglwd = 1,cglty = 1, plwd = 2, vlcex = 0.8, 
                              title = "Anxiety",axislabcol = "grey40", 
                              cglcol = "grey40", pcol = "#3196CC")
}

# Depression
{
fav_genre_table_d <- fav_genre_table %>%
  select(Fav_genre, Depression) %>%
  pivot_wider(names_from = Fav_genre, values_from = Depression) %>%
  as.data.frame()

rownames(fav_genre_table_d) <- fav_genre_table_d$Fav_genre
# Removing the Fav_genre column
fav_genre_table_dwf <-fav_genre_table_d[, -1]

# Adding new columns specifying the range of chart
fav_genre_table_dc <- rbind(rep(10, ncol(fav_genre_table_dwf)), 
      rep(0, ncol(fav_genre_table_dwf)), 
      fav_genre_table_dwf)

radar_chart_d <- radarchart(fav_genre_table_dc, axistype = 0, seg = 10, 
                            cglwd = 1,cglty = 1, plwd = 2, vlcex = 0.8, 
                            title = "Depression",axislabcol = "grey40", 
                            cglcol = "grey40", pcol = "#3196CC")
}

# Insomnia
{
  fav_genre_table_i <- fav_genre_table %>%
    select(Fav_genre, Insomnia) %>%
    pivot_wider(names_from = Fav_genre, values_from = Insomnia) %>%
    as.data.frame()
  
  rownames(fav_genre_table_i) <- fav_genre_table_i$Fav_genre
  # Removing the Fav_genre column
  fav_genre_table_iwf <-fav_genre_table_i[, -1]
  
  # Adding new columns specifying the range of chart
  fav_genre_table_ic <- rbind(rep(10, ncol(fav_genre_table_iwf)), 
                              rep(0, ncol(fav_genre_table_iwf)), 
                              fav_genre_table_iwf)
  
  radar_chart_i <- radarchart(fav_genre_table_ic, axistype = 0, seg = 10, 
                              cglwd = 1,cglty = 1, plwd = 2, vlcex = 0.8, 
                              title = "Insomnia",axislabcol = "grey40", 
                              cglcol = "grey40", pcol = "#3196CC")
}

# OCD
{
  fav_genre_table_o <- fav_genre_table %>%
    select(Fav_genre, OCD) %>%
    pivot_wider(names_from = Fav_genre, values_from = OCD) %>%
    as.data.frame()
  
  rownames(fav_genre_table_o) <- fav_genre_table_o$Fav_genre
  # Removing the Fav_genre column
  fav_genre_table_owf <-fav_genre_table_o[, -1]
  
  # Adding new columns specifying the range of chart
  fav_genre_table_oc <- rbind(rep(10, ncol(fav_genre_table_owf)), 
                              rep(0, ncol(fav_genre_table_owf)), 
                              fav_genre_table_owf)
  
  radar_chart_o <- radarchart(fav_genre_table_oc, axistype = 0, seg = 10, 
                              cglwd = 1,cglty = 1, plwd = 2, vlcex = 0.8, 
                              title = "OCD",axislabcol = "grey40", 
                              cglcol = "grey40", pcol = "#3196CC")
}

# Bar charts comparing the median ratings of anxiety, depression, insomnia, and
# OCD based on favorite music genre

# Anxiety 
{
  # The overall median of anxiety
  median_a = mxmh %>%
    summarise(median = median(Anxiety)) %>%
    as.numeric()
  
  bar_chart_a <- fav_genre_table %>% 
    ggplot(aes(x = reorder(Fav_genre, Anxiety), y = Anxiety , 
               fill = factor(ifelse(Anxiety > median_a,
                                    "Above the overall median",
                                    "Below the overall median")))) +
    scale_fill_manual(name = "", values = c("#3196CC", "grey40")) +
    scale_x_discrete("Music genre") +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    geom_col() +
    theme_light()
}

# Depression
{
  # The overall median of anxiety
  median_d = mxmh %>%
    summarise(median = median(Depression)) %>%
    as.numeric()
  
  bar_chart_d <- fav_genre_table %>% 
    ggplot(aes(x = reorder(Fav_genre, Depression), y = Depression , 
               fill = factor(ifelse(Depression > median_d,
                                    "Above the overall median",
                                    "Below the overall median")))) +
    scale_fill_manual(name = "", values = c("#3196CC", "grey40")) +
    scale_x_discrete("Music genre") +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    geom_col() +
    theme_light()
}

# Insomnia
{
  # The overall median of anxiety
  median_i = mxmh %>%
    summarise(median = median(Insomnia)) %>%
    as.numeric()
  
  bar_chart_i <- fav_genre_table %>% 
    ggplot(aes(x = reorder(Fav_genre, Insomnia), y = Insomnia , 
               fill = factor(ifelse(Insomnia > median_i,
                                    "Above the overall median",
                                    "Below the overall median")))) +
    scale_fill_manual(name = "", values = c("#3196CC", "grey40")) +
    scale_x_discrete("Music genre") +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    geom_col() +
    theme_light()
}

# OCD
{
  # The overall median of anxiety
  median_o = mxmh %>%
    summarise(median = median(OCD)) %>%
    as.numeric()
  
  bar_chart_o <- fav_genre_table %>% 
    ggplot(aes(x = reorder(Fav_genre, OCD), y = OCD , 
               fill = factor(ifelse(OCD > median_o,
                                    "Above the overall median",
                                    "Below the overall median")))) +
    scale_fill_manual(name = "", values = c("#3196CC", "grey40")) +
    scale_x_discrete("Music genre") +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    geom_col() +
    theme_light()
}
