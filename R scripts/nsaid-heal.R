#NSAID-HEAL Weekly Metrics
#Written by Sarah Darnell, last edited 11.4.25

library(jsonlite)
library(dplyr)
library(lubridate)
library(ggplot2)
library(htmltools)

setwd("~/Sarah work stuff/2025 Data Projects/Weekly Metrics")

token <- Sys.getenv("NSAIDHEAL_REDCAP_TOKEN")

#!/usr/bin/env Rscript (generated from API playground)
token <- token
url <- "https://survey.northshore.org/api/"
formData <- list("token"=token,
                 content='report',
                 format='json',
                 report_id='4336',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)
print(result)

#convert to dateframe
response_text <- httr::content(response, as = "text")
result_df <- fromJSON(response_text, flatten = TRUE)

#convert date/time fields from character to POSIXct
result_df <- result_df %>%
  mutate(eligibility_screen_timestamp = ymd_hms(eligibility_screen_timestamp)) %>%
  mutate(consent_form_timestamp = ymd_hms(consent_form_timestamp)) %>%
  mutate(cf_date = ymd(cf_date)) %>%
  mutate(vbt_question_launch_date = ymd(vbt_question_launch_date)) %>%
  mutate(vbt_done_date = ymd(vbt_done_date)) %>%
  mutate(bl_visit_schedule_date = ymd(bl_visit_schedule_date)) %>%
  mutate(vitals_date_today = ymd(vitals_date_today))

#pivot from long to wide format
result_df_bl <- result_df %>%
  filter(redcap_event_name == "baseline_visit_arm_1") %>%
  select(1, 15) %>%
  rename(vitals_date_bl = vitals_date_today)

result_df_bs3 <- result_df %>%
  filter(redcap_event_name == "biospecimen_visit_arm_1") %>%
  select(1, 15) %>%
  rename(vitals_date_bs3 = vitals_date_today)

result_df_comb <- left_join(result_df_bl, result_df_bs3, by = "record_id")

result_df <- result_df %>%
  filter(redcap_event_name == "consent_ids_arm_1") %>%
  select(-15)

result_df <- left_join(result_df, result_df_comb, by = "record_id")

#add columns that flag for various completions in last 7 days
result_df <- result_df %>%
  mutate(`Prescreens this week` = eligibility_screen_timestamp >= now() - days(8)) %>%
  mutate(`Consents this week` = consent_form_timestamp >= now() - days(8)) %>%
  mutate(`VBTs this week` = vbt_done_date >=now() - days(8)) %>%
  mutate(`Baseline visits this week` = vitals_date_bl >=now() - days(8)) %>%
  mutate(`Biospecimen cycle 3 this week` = vitals_date_bs3 >=now() - days(8))

#add columns that flag for various completions in last 14 days, not counting last 7
result_df <- result_df %>%
  mutate(`Prescreens last week` = eligibility_screen_timestamp >= now() - days(15) & 
           `Prescreens this week` != TRUE) %>%
  mutate(`Consents last week` = consent_form_timestamp >= now() - days(15) & 
  `Consents this week` != TRUE) %>%
  mutate(`VBTs last week` = vbt_done_date >= now() - days(15) & 
           `VBTs this week` != TRUE) %>%
  mutate(`Baseline visits last week` = vitals_date_bl >= now() - days(15) & 
           `Baseline visits this week` != TRUE) %>%
  mutate(`Biospecimen cycle 3 last week` = vitals_date_bs3 >= now() - days(15) & 
           `Biospecimen cycle 3 this week` != TRUE)

#save metrics
metrics <- result_df %>%
  summarise(across(c(`Prescreens this week`, `Prescreens last week`, 
                     `Consents this week`, `Consents last week`,
                     `VBTs this week`, `VBTs last week`, 
                     `Baseline visits this week`, `Baseline visits last week`, 
                     `Biospecimen cycle 3 this week`, `Biospecimen cycle 3 last week`
                     )
                   , ~ sum(.x, na.rm = TRUE)))

write.csv(metrics, "Output/nsaid-heal-metrics.csv")


#Plot of daily prescreens hits plus trend line
result_df_plot <- result_df %>%
  mutate(screen_date = as.Date(eligibility_screen_timestamp)) %>%
  filter(!is.na(screen_date)) %>%
  count(screen_date)

p1 <- ggplot(result_df_plot, aes(x = screen_date, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", linetype = "dashed") +
  labs(title = "Daily Eligibility Screens Over Time",
       x = "Date",
       y = "Number of Screens",
       caption = "Blue line = actual daily counts | Red dashed = trend") +
  theme_minimal()


#Plot of weekly prescreens hits plus trend line
result_df_weekly <- result_df %>%
  mutate(screen_date = as.Date(eligibility_screen_timestamp),
         week_start = floor_date(screen_date, unit = "week", week_start = 4)) %>%
  filter(!is.na(week_start)) %>%
  count(week_start)

p2 <- ggplot(result_df_weekly, aes(x = week_start, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", linetype = "dashed") +
  labs(title = "Weekly Eligibility Screens Over Time",
       x = "Week Starting",
       y = "Number of Screens",
       caption = "Blue line = actual weekly counts | Red dashed = trend") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks")


#Plot of daily consents hits plus trend line
result_df_plot <- result_df %>%
  mutate(consent_date = as.Date(consent_form_timestamp)) %>%
  filter(!is.na(consent_date)) %>%
  count(consent_date)

p3 <- ggplot(result_df_plot, aes(x = consent_date, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", linetype = "dashed") +
  labs(title = "Daily Consents Over Time",
       x = "Date",
       y = "Number of Consents",
       caption = "Blue line = actual daily counts | Red dashed = trend") +
  theme_minimal()

#Plot of weekly consents plus trend line
result_df_weekly <- result_df %>%
  mutate(consent_date = as.Date(consent_form_timestamp),
         week_start = floor_date(consent_date, unit = "week", week_start = 4)) %>%
  filter(!is.na(week_start)) %>%
  count(week_start)

p4 <- ggplot(result_df_weekly, aes(x = week_start, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", linetype = "dashed") +
  labs(title = "Weekly Consents Over Time",
       x = "Week Starting",
       y = "Number of Consents",
       caption = "Blue line = actual weekly counts | Red dashed = trend") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %d", date_breaks = "2 weeks")

#monthly prescreens and consents overlay plot
prescreen_counts <- result_df %>%
  mutate(prescreen_date = as.Date(eligibility_screen_timestamp)) %>%
  filter(!is.na(prescreen_date)) %>%
  mutate(month = factor(format(prescreen_date, "%b"), levels = month.abb)) %>%
  group_by(month) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(type = "All prescreens")


consent_counts <- result_df %>%
  mutate(consent_date = as.Date(consent_form_timestamp)) %>%
  filter(!is.na(consent_date)) %>%
  mutate(month = factor(format(consent_date, "%b"), levels = month.abb)) %>%
  group_by(month) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(type = "Signed consents")

eligible_counts <- result_df %>%
  mutate(ps_eligible_date = case_when(
    ps_eligible == "1" ~ as.Date(eligibility_screen_timestamp))) %>%
  filter(!is.na(ps_eligible_date)) %>%
  mutate(month = factor(format(ps_eligible_date, "%b"), levels = month.abb)) %>%
  group_by(month) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(type = "Eligible Prescreens")

monthly_counts <- bind_rows(prescreen_counts, consent_counts, eligible_counts)


p5 <- ggplot(monthly_counts, aes(x = month, y = count, fill = type)) +
  geom_col(position = "dodge") +   
  labs(x = "Month", y = "Count",
       title = "Prescreens vs Consents per Month") +
  scale_fill_manual(values = c("All prescreens" = "skyblue", 
                               "Eligible Prescreens" = "tomato2",
                               "Signed consents" = "mediumseagreen")) +
  theme_minimal()

#saving plots
plots <- c("p1", "p2", "p3", "p4", "p5")

for (i in seq_along(plots)) {
  ggsave(
    filename = sprintf("Output/Plots/%s.png", plots[i]),
    plot = get(plots[i]),
    width = 7, height = 5, dpi = 300
  )
}

#setting plots to display on single html page via github
image_files <- list.files("Output/Plots", pattern = "*.png", full.names = FALSE)

html_images <- lapply(image_files, function(img) {
  tags$img(src = paste0("Output/Plots/", img), style = "width: 80%; margin-bottom: 30px;")
})

html_page <- tags$html(
  tags$head(tags$title("Weekly Plots")),
  tags$body(
    tags$h1("Weekly Plot Gallery"),
    tags$p("Auto-updated from R"),
    html_images
  )
)

save_html(html_page, file = "Output/Plots/index.html")











