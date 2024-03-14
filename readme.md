---
title: "README"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Melodies of emotions
<div style="text-align: justify">
Visualization of the interaction between music and mental health. The poster (in the Polish language version below) illustrates the relationship between the average level of positivity of songs listened to by respondents and the season, emphasizing the importance of the influence of surrounding sounds on well-being and ailments such as insomnia. Additionally, it includes brief conclusions from the obtained visualizations.
</div>

![](poster.png)

## Inspiration

<div style="text-align: justify">
The poster was a student project that we worked on in small groups as part of a data visualization course. The main theme was music. My group created several charts (the ones I personally contributed to are in the file: generating_charts.R) to select the most interesting ones for the poster.
</div>

## Data
The data used in the project: <br>
- [Survey results on music taste and self-reported mental health](https://www.kaggle.com/datasets/catherinerasgaitis/mxmh-survey-results) <br>
- [https://charts.spotify.com/charts/overview/global](https://charts.spotify.com/charts/overview/global)

## Technologies
<div style="text-align: justify">
All charts were created in RStudio using the R language version 4.2.2. Libraries such as ggplot2, dplyr, fmsb, lattice etc,  were used. Additionally, the Canva program was used to create the poster itself.
</div>

## Sample charts that were not used
```{r, include=FALSE}
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
```
```{r, echo=FALSE, out.width="100%"}
boxplot_ww

```
<div style="text-align: justify">
A box plot was created to describe the relationship between listening to music during work, the number of hours spent listening to music during the day, and the rating of anxiety, depression, insomnia, and obsessive-compulsive disorder levels by the respondents. 
</div>

```{r, include=FALSE}
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
```

```{r, echo=FALSE, out.width="100%"}
violin_insomnia

```

A violin plot was created to depict the level of insomnia based on the daily hours spent listening to music.

## How to run the script?
The generating_charts.R script should be executed in RStudio. An example of running the code to generate a plot and display it:

```{r}
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
```
Displaying the plot

```{r, out.width="100%"}
boxplot_tempo 

```

