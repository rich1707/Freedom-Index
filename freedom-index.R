# load libraries ----

library(tidyverse)
library(tidymodels)
library(janitor)
library(scales)
library(WDI)
library(countrycode)
library(vip)

# read in and clean data ----

freedom_data <- read_csv("freedom_data.csv")

freedom_data |> 
   summarise(across(everything(), \(x) sum(is.na(x)))) |> 
   pivot_longer(cols = everything(), names_to = "vars", values_to = "totals") |> 
   arrange(vars)

freedom_data <- freedom_data |> 
   clean_names() |> 
   rename(civil_liberties = cl, political_rights = pr) |> 
   mutate(country = if_else(country == "CÃƒÂ´te dÃ¢â‚¬â„¢Ivoire", "Ivory Coast", country))

# exploratory analysis ----

freedom_data |> 
   filter(year == 2020) |> 
   group_by(region_name) |> 
   summarise(
      total_countries = n(),
      total_free = sum(status == "F"),
      total_oppressed = total_countries-total_free,
      percentages = total_free/total_countries,
      .groups = "drop"
   ) |> 
   mutate(
      region_name = factor(
         region_name, 
         levels = c("Asia", "Africa", "Americas", "Europe", "Oceania")
      )
   ) |> 
   arrange(desc(percentages)) |> 
   ggplot(aes(x = region_name, y = percentages)) +
   geom_point(colour = "#34495E", size = 3.5) +
   geom_segment(
      aes(x = region_name, xend = region_name, y = 0, yend = percentages),
      colour = "#34495E",
      linewidth = 1.5
   ) +
   coord_flip() +
   scale_y_continuous(
      breaks = c(0, 0.25, 0.5, 0.75, 1), 
      limits = c(0, 1), 
      labels = percent
   ) +
   labs(
      x = NULL,
      y = NULL,
      title = "Percentage of free countries by region",
      subtitle = 
         "Showing the different levels of freedom across continents for the year 2020"
   ) +
   theme_light() +
   theme(
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 10, face = "italic")
   )

freedom_data |> 
   group_by(year, is_ldc) |> 
   mutate(
      free = sum(status == "F"),
      part_free = sum(status == "PF"),
      not_free = sum(status == "NF")
   ) |> 
   ungroup() |> 
   select(year, is_ldc, contains("free")) |> 
   distinct() |>
   group_by(year, is_ldc) |> 
   mutate(totals = free + part_free + not_free) |> 
   ungroup() |>
   mutate(
      free_percent = free / totals,
      part_free_percent = part_free / totals,
      not_free_percent = not_free / totals
   ) |>
   select(year, is_ldc, contains("percent")) |>
   pivot_longer(
      cols = contains("percent"),
      names_to = "names",
      values_to = "values"
   ) |>
   mutate(is_ldc = if_else(is_ldc == 1, "Least Developed", "Developed")) |> 
   ggplot(aes(x = year, y = values, colour = names)) +
   geom_line(size = 2, alpha = 0.9) +
   scale_y_continuous(labels = label_percent(accuracy = 1)) +
   scale_color_manual(
      values = c("#F39C12", "#bab86c", "#179FA3"),
      labels = c("Free", "Partly Free", "Not Free")
   ) +
   labs(
      x = NULL, 
      y = NULL,
      title = "Percentage of free countries by developed status",
      subtitle = "Showing that economic development is highly correlated with freedom"
   ) +
   theme_light() +
   facet_wrap(~is_ldc) +
   theme(
      axis.text.x = element_text(size = 10, face = "bold"),
      axis.text.y = element_text(size = 10, face = "bold"),
      strip.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12, face = "italic"),
      legend.text = element_text(size = 11, face = "bold"),
      strip.background = element_rect(fill = "#3b4d5b"),
      strip.text = element_text(colour= "#f8f8f2"),
      legend.title = element_blank(),
      legend.key.width = unit(1.5, "cm"),
      legend.position = "bottom"
   ) 

diff_hat <- freedom_data |> 
   mutate(is_ldc = if_else(is_ldc == 1, "Least Developed", "Developed")) |> 
   specify(status ~ is_ldc) |> 
   calculate(stat = "Chisq")

freedom_data |> 
   mutate(is_ldc = if_else(is_ldc == 1, "Least Developed", "Developed")) |> 
   specify(status ~ is_ldc) |> 
   assume(distribution = "Chisq") |> 
   get_p_value(obs_stat = diff_hat, direction = "two-sided")

freedom_data |> 
   filter(region_name == "Europe") |> 
   mutate(
      percent_pr = political_rights / 7, # here the ratings are from 1 to 7
      percent_cl = civil_liberties / 7
   ) |> 
   group_by(year, region_name) |> 
   summarise(
      pr_x_year = 1 - mean(percent_pr),
      cl_x_year = 1 - mean(percent_cl),
      .groups = "drop"
   ) |> 
   pivot_longer(
      cols = pr_x_year:cl_x_year,
      names_to = "names",
      values_to = "values"
   ) |> 
   ggplot(aes(x = year, y = values, colour = names)) + 
   geom_line(linewidth = 2, alpha = 0.85) + 
   theme_light() + 
   scale_y_continuous(labels = label_percent(accuracy = 1)) + 
   scale_color_manual(
      values = c("#179FA3", "#F39C12"),
      labels = c("Civil Liberties", "Political Rights"),
      guide = guide_legend(reverse = TRUE)
   ) + 
   labs(
      x = NULL, 
      y = NULL,
      title = "The evolution of Civil Liberties and Political Rights of countries in Europe",
      subtitle = "Showing the average points awarded as a percentage of the maximum possible"
   ) + 
   theme(
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      plot.title = element_text(size = 13, face = "bold"),
      plot.subtitle = element_text(face = "italic"),
      legend.text = element_text(size = 11, face = "bold"),
      legend.title = element_blank(),
      legend.position = "bottom"
   )

freedom_data |> 
   filter(region_name != "Europe") |> 
   mutate(region_name = factor(
      region_name, 
      levels = c("Asia", "Africa", "Oceania", "Americas")
   )) |> 
   mutate(
      percent_pr = political_rights / 7,
      percent_cl = civil_liberties / 7
   ) |> 
   group_by(year, region_name) |> 
   summarise(
      pr_x_year = 1 - mean(percent_pr),
      cl_x_year = 1 - mean(percent_cl),
      .groups = "drop"
   ) |> 
   pivot_longer(
      cols = pr_x_year:cl_x_year,
      names_to = "names",
      values_to = "values"
   ) |>
   ggplot(aes(x = year, y = values, colour = names)) + 
   geom_line(linewidth = 1.75, alpha = 0.85) + 
   theme_light() + 
   scale_y_continuous(labels = label_percent(accuracy = 1)) + 
   scale_color_manual(
      values = c("#179FA3", "#F39C12"),
      labels = c("Civil Liberties", "Political Rights"),
      guide = guide_legend(reverse = TRUE)
   ) + 
   labs(
      x = NULL, 
      y = NULL,
      title = 
         "The evolution of Civil Liberties and Political Rights of countries outside Europe",
      subtitle = 
         "Showing the average points awarded as a percentage of the maximum possible"
   ) + 
   facet_wrap(~region_name, scales = "free") + 
   theme(
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 11, face = "bold"),
      strip.text.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(face = "italic"),
      legend.text = element_text(size = 11, face = "bold"),
      strip.background = element_rect(fill = "#3b4d5b"),
      strip.text = element_text(colour= "#f8f8f2"),
      legend.title = element_blank(),
      legend.position = "bottom"
   )

freedom_data |> 
   specify(political_rights ~ civil_liberties) |> 
   calculate(stat = "correlation") |> 
   pull(stat)

# reading in extra data ----

world_bank_data <- WDI(indicator = "NY.GDP.PCAP.CD", extra = TRUE)

freedom_data |> 
   anti_join(world_bank_data, by = "country") |> 
   distinct(country)

freedom_data <- freedom_data |> 
   mutate(iso2c = countrycode(country, "country.name", "iso2c"))

world_bank_data <- world_bank_data |> 
   rename(gdp = NY.GDP.PCAP.CD) |> 
   filter(year >= 1995, year <= 2020) |> 
   select(iso2c, year, gdp, income)

freedom_data <- freedom_data |> 
   left_join(world_bank_data, by = c("iso2c", "year")) |> 
   drop_na()

# exploring the new dataset ----

glimpse(freedom_data)

freedom_data |> 
   filter(year == 2020) |> 
   mutate(income = case_when(
      income == "Lower middle income" ~ "Lower middle",
      income == "Upper middle income" ~ "Upper middle",
      TRUE ~ as.character(income)
   )) |> 
   mutate(income = fct_reorder(income, gdp, .desc = FALSE)) |> 
   group_by(income) |> 
   summarise(
      min_gdp = min(gdp),
      mean_gdp = mean(gdp),
      max_gdp = max(gdp),
      .groups = "drop"
   ) |> 
   pivot_longer(
      cols = ends_with("_gdp"),
      names_to = "names",
      values_to = "values"
   ) |> 
   mutate(names = factor(
      names, 
      levels = c("min_gdp", "mean_gdp", "max_gdp")
   )) |> 
   ggplot(aes(x = income, y = values, fill = names)) + 
   geom_col(position = "dodge2", alpha = 0.85, width = .45) +
   theme_light() + 
   scale_fill_manual(
      values = c("#F39C12", "#bab86c", "#179FA3"),
      labels = c("Minimum GDP of group", "Average GDP of group", "Maximum GDP of group")
   ) +
   labs(
      x = NULL, 
      y = NULL,
      title = "The differences in income (largely regional)",
      subtitle = 
         "Showing the minimum, the mean, and the maximum for countries in each income bracket"
   ) +
   scale_y_continuous(labels = label_dollar()) +
   theme(
      axis.text.x = element_blank(),
      axis.text.y = element_text(face = "bold"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11, face = "italic"),
      strip.background = element_rect(fill = "#3b4d5b"),
      strip.text = element_text(size = 11, face = "bold", colour= "#f8f8f2"), #005300
      legend.title = element_blank(),
      legend.text = element_text(size = 11, face = "bold"),
      legend.position = "bottom"
   ) +
   facet_wrap(~income, scales = "free")

# a simple model ----

model_data <- freedom_data |> 
   filter(year == 2020) |> 
   mutate(is_free = if_else(status == "F", "free", "not_free")) |>
   select(is_free, region_name, gdp, is_ldc)

set.seed(2022)

freedom_split <- initial_split(model_data, prop = 0.75, strata = region_name)

freedom_training <- training(freedom_split)

freedom_testing <- testing(freedom_split)

freedom_recipe <- model_data |> 
   recipe(is_free ~ gdp + region_name + is_ldc) |> 
   step_string2factor(is_free, skip = TRUE) |> 
   step_string2factor(all_nominal_predictors()) |> 
   step_normalize(gdp)

freedom_spec <- logistic_reg() |> 
   set_engine("glm") |> 
   set_mode("classification")

freedom_workflow <- workflow() |> 
   add_model(freedom_spec) |> 
   add_recipe(freedom_recipe)

freedom_model <- fit(freedom_workflow, data = freedom_training)

# evaluation ----

freedom_model |> 
   extract_fit_engine() |> 
   vi() |> 
   mutate(Variable = fct_reorder(Variable, Importance)) |> 
   ggplot(aes(x = Variable, y = Importance)) + 
   geom_point(size = 3, colour = "#34495E", alpha = 0.85) + 
   geom_segment(
      aes(x = Variable, xend = Variable, y = 0, yend = Importance), 
      colour = "#34495E",
      alpha = 0.65,
      linewidth = 1.5
   ) +
   theme_light() + 
   labs(x = NULL, y = NULL, 
        title = "Showing variable importance for each feature") + 
   theme(
      axis.text.x = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold"),
      plot.title = element_text(face = "bold", size = 14),
   ) + 
   coord_flip()

freedom_preds <- predict(freedom_model, new_data = freedom_testing)

freedom_preds <- freedom_testing |> 
   select(is_free) |> 
   mutate(is_free = as.factor(is_free)) |> 
   bind_cols(freedom_preds)

freedom_preds |> 
   accuracy(is_free, .pred_class)










































