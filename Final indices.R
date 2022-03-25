options(scipen = 999)
library(tidyverse)
library(lavaan)
library(countrycode)
library(ggthemes)

#### Load in all required data
covid_deaths <- read.csv("r_covid_deaths.csv", stringsAsFactors = FALSE)
excess_deaths <- read.csv("r_excess_impact.csv", stringsAsFactors = FALSE)
excess_incl_fit <- read.csv("r_excess_incl_fit.csv", stringsAsFactors = FALSE)
gdp_ppp <- read.csv("r_gdp_ppp_impact.csv", stringsAsFactors = FALSE)
lockdown <- read.csv("r_lockdown_rankings.csv", stringsAsFactors = FALSE)

#### Index 1: Covid deaths + GDP PPP impact + lockdown stringency
index_1 <- inner_join(covid_deaths, gdp_ppp, by = "iso") %>%
  inner_join(lockdown, by = "iso") %>%
  mutate(index_1 = (covid_deaths_index + gdp_ppp_index + lockdown_index) / 3) %>%
  select(country = country.x, iso, index_1,
         covid_deaths_index, covid_deaths_per_100k,
         gdp_ppp_index, ppp_percent_change,
         lockdown_index, lockdown_score) %>%
  arrange(-index_1) %>%
  mutate(across(3:9, round_half_up, digits = 2))

# Correlation between indicators
lavCor(index_1[c(4, 6, 8)])

# Checking to see whether any countries didn't join properly
no_join_1 <- full_join(covid_deaths, gdp_ppp, by = "iso") %>%
  full_join(lockdown, by = "iso") %>%
  anti_join(index_1, by = "iso") %>%
  select(country.x, country.y, country, everything())

# Map visualisation
world_map <- map_data("world") %>%
  mutate(iso = countrycode(region, "country.name", "iso3c"))

world_map$iso[world_map$region == "Kosovo"] <- "UVK"

world_map_1 <- world_map %>%
  left_join(index_1, by = "iso")

ggplot(world_map_1) +
  geom_map(aes(map_id = region, fill = index_1), map = world_map_1, color = "black", size = 0.1) +
  expand_limits(x = c(-155, 175), y = c(-50, 80)) +
  scale_fill_gradient2(low = "seagreen3",
                       mid = "khaki1",
                       high = "red",
                       name = "   Impact Index\n",
                       midpoint = median(world_map_1$index_1, na.rm = TRUE),
                       breaks = c(min(world_map_1$index_1, na.rm = TRUE),
                                  median(world_map_1$index_1, na.rm = TRUE),
                                  max(world_map_1$index_1, na.rm = TRUE)),
                       labels = c("-16 (best)", "31 (median)", "74 (worst)")) +
  theme_map() +
  ggtitle("COVID-19 Country Impact Index for 2020, version one",
          "Indicators are registered COVID-19 deaths, PPP GDP change from prediction, and restriction stringency index") +
  theme(panel.background = element_rect(fill = "lightblue1"),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12),
        legend.justification = c(-0.01, 0)) +
  guides(fill = guide_colourbar(ticks = FALSE))


#### Index 2: Excess official deaths + GDP PPP impact + lockdown stringency
index_2 <- inner_join(excess_deaths, gdp_ppp, by = "iso") %>%
  inner_join(lockdown, by = "iso") %>%
  mutate(index_2 = (excess_index + gdp_ppp_index + lockdown_index) / 3) %>%
  select(country = country.x, iso, index_2,
         excess_index, model_excess_per_100k,
         gdp_ppp_index, ppp_percent_change,
         lockdown_index, lockdown_score) %>%
  arrange(-index_2) %>%
  mutate(across(3:9, round_half_up, digits = 2))

# Correlation between indicators
lavCor(index_2[c(4, 6, 8)])

# Checking to see whether any countries didn't join properly
no_join_2 <- full_join(excess_deaths, gdp_ppp, by = "iso") %>%
  full_join(lockdown, by = "iso") %>%
  anti_join(index_2, by = "iso") %>%
  select(country.x, country.y, country, everything())

# Map visualisation
world_map_2 <- world_map %>%
  left_join(index_2, by = "iso")

ggplot(world_map_2) +
  geom_map(aes(map_id = region, fill = index_2), map = world_map_2, color = "black", size = 0.1) +
  expand_limits(x = c(-155, 175), y = c(-50, 80)) +
  scale_fill_gradient2(low = "seagreen3",
                       mid = "khaki1",
                       high = "red",
                       name = "   Impact Index\n",
                       midpoint = median(world_map_2$index_2, na.rm = TRUE),
                       breaks = c(min(world_map_2$index_2, na.rm = TRUE),
                                  median(world_map_2$index_2, na.rm = TRUE),
                                  max(world_map_2$index_2, na.rm = TRUE)),
                       labels = c("-3 (best)", "35 (median)", "66 (worst)")) +
  theme_map() +
  ggtitle("COVID-19 Country Impact Index for 2020, version two",
          "Indicators are excess all-cause deaths, PPP GDP change from prediction, and restriction stringency index") +
  theme(panel.background = element_rect(fill = "lightblue1"),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12),
        legend.justification = c(-0.01, 0)) +
  guides(fill = guide_colourbar(ticks = FALSE))

# Correlation between Index 1 and Index 2
combined_1_2 <- inner_join(index_1, index_2, by = "iso") %>%
  select(country = country.x, 2:3, index_2, covid_deaths_per_100k, model_excess_per_100k)

lavCor(combined_1_2[c(3:4)])


#### Index 3: Excess deaths incl. fitted + GDP PPP impact + lockdown stringency
index_3 <- inner_join(excess_incl_fit, gdp_ppp, by = "iso") %>%
  inner_join(lockdown, by = "iso") %>%
  mutate(index_3 = (excess_incl_fit_index + gdp_ppp_index + lockdown_index) / 3) %>%
  select(country = country.x, iso, index_3,
         excess_incl_fit_index, excess_incl_fit_per_100k,
         gdp_ppp_index, ppp_percent_change,
         lockdown_index, lockdown_score) %>%
  arrange(-index_3) %>%
  mutate(across(3:9, round_half_up, digits = 2))

# Correlation between indicators
lavCor(index_3[c(4, 6, 8)])

# Checking to see whether any countries didn't join properly
no_join_3 <- full_join(excess_incl_fit, gdp_ppp, by = "iso") %>%
  full_join(lockdown, by = "iso") %>%
  anti_join(index_3, by = "iso") %>%
  select(country.x, country.y, country, everything())

# Countries imputed
imputed <- anti_join(index_3, index_2, by = "iso")

# Map visualisation
world_map_3 <- world_map %>%
  left_join(index_3, by = "iso")

ggplot(world_map_3) +
  geom_map(aes(map_id = region, fill = index_3), map = world_map_3, color = "black", size = 0.1) +
  expand_limits(x = c(-155, 175), y = c(-50, 80)) +
  scale_fill_gradient2(low = "seagreen3",
                       mid = "khaki1",
                       high = "red",
                       name = "   Impact Index\n",
                       midpoint = median(world_map_3$index_3, na.rm = TRUE),
                       breaks = c(min(world_map_3$index_3, na.rm = TRUE),
                                  median(world_map_3$index_3, na.rm = TRUE),
                                  max(world_map_3$index_3, na.rm = TRUE)),
                       labels = c("-12 (best)", "32 (median)", "66 (worst)")) +
  theme_map() +
  ggtitle("COVID-19 Country Impact Index for 2020, version three",
          "Indicators are fitted excess deaths, PPP GDP change from prediction, and restriction stringency index") +
  theme(panel.background = element_rect(fill = "lightblue1"),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12),
        legend.justification = c(-0.01, 0)) +
  guides(fill = guide_colourbar(ticks = FALSE))
