library(pacman)

pacman::p_load(
  rio, here, tidyverse, knitr, kableExtra, skimr, 
  formatR, gridExtra, janitor
)

# import Alzheimer's data
alzheimer_data <- import(here("DATA", "alzheimers_data_clean.csv"))

library(ggplot2)

ggplot(data = alzheimer_data, aes(x = bmi)) +
  geom_histogram(color = "pink") +
  labs(x = "BMI") +
  theme(text = element_text(size = 12))

names(alzheimer_data)

summary(alzheimer_data$sleep_quality)
unique(alzheimer_data$sleep_quality)

summary(alzheimer_data$memory)
unique(alzheimer_data$memory)

library(ggplot2)

ggplot(alzheimer_data, aes(x = memory, y = sleep_quality)) +
  geom_boxplot() +
  labs(
    title = "Sleep quality differs by memory complaint status",
    x = "Memory Complaint Status",
    y = "Sleep Quality"
  ) +
  theme_minimal()

alzheimer_data |>
  group_by(memory) |>
  summarise(
    n = n(),
    mean_sleep = mean(sleep_quality),
    median_sleep = median(sleep_quality),
    sd_sleep = sd(sleep_quality)
  )

summary(alzheimer_data$family_history_ad)
unique(alzheimer_data$family_history_ad)

summary(alzheimer_data$hypertension)
unique(alzheimer_data$hypertension)

table(
  alzheimer_data$family_history_ad,
  alzheimer_data$hypertension
)

library(dplyr)

family_hypertension <- alzheimer_data %>%
  count(family_history_ad, hypertension) %>%
  group_by(family_history_ad) %>%
  mutate(percent = n / sum(n) * 100)

family_hypertension

ggplot(family_hypertension,
       aes(x = family_history_ad,
           y = percent,
           fill = hypertension)) +
  
  geom_col() +
  
  scale_fill_manual(
    values = c(
      "No Hypertension" = "#BFD7EA",
      "With Hypertension" = "red"
    )
    
  ) 
  labs(
    title = "Hypertension Status by Alzheimer's Family History",
    subtitle = "Percentage of participants with hypertension within each family-history group",
    x = "Family History of Alzheimer's Disease",
    y = "Participants (%)",
    fill = "Hypertension Status"
  )
   +
  
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20)
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(
      size = 11,
      color = "gray40"
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

  ggplot(family_hypertension,
         aes(x = family_history_ad,
             y = percent,
             fill = hypertension)) +
    
    geom_col() +
    
    scale_fill_manual(
      values = c(
        "No Hypertension" = "#BFD7EA",
        "With Hypertension" = "red"
      )
    ) +
    
    labs(
      title = "Hypertension Status by Family History of Alzheimer's Disease",
      subtitle = "Percentage of participants with hypertension within each family-history group",
      x = "Family History of Alzheimer's Disease",
      y = "Participants (%)",
      fill = "Hypertension Status"
    ) +
    
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20)
    ) +
    
    theme_minimal(base_size = 13) +
    
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(
        size = 11,
        color = "gray40"
      ),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  