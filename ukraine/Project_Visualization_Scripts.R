# Load required packages
library(ggplot2)
library(rworldmap)
library(dplyr)
library(tidyr)
library(sf)
library(scales)
library(extrafont)
library(stringr)
library(av)
library(gganimate)
library(transformr)
library(ggrepel)
library(plotly)
library(htmlwidgets)
library(rnaturalearth)
library(rnaturalearthdata)


map.theme <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                   axis.title.x=element_blank(), axis.title.y=element_blank(),
                   panel.background=element_blank(), panel.border=element_blank(),
                   panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                   plot.background=element_blank())

loadfonts(device = "win")
windowsFonts()

# Load world map data from the sf package
world_coordinates <- map_data("world")
world_coordinates <- subset(world_coordinates, region != "Antarctica")
world_coordinates$region <- ifelse(world_coordinates$region == "USA",
                                   "United States", world_coordinates$region)

# Load world shapefile
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load CountryToContinents
country_to_continent <- read.csv("countryContinent.csv")

# Ukrainian Exports (2021 & 2022)
# Load Data
ukraine_exports <- read.csv("UkraineAnnualExports.csv")

# Change this line to get diff years
ukraine_exports <- subset(ukraine_exports, RefYear == "2021") 

ukraine_exports$PartnerDesc <- ifelse(ukraine_exports$PartnerDesc == "USA", "United States", ukraine_exports$PartnerDesc)
ukraine_exports$CmdDesc <- ifelse(ukraine_exports$CmdCode == "26" | ukraine_exports$CmdCode == "72", "Iron, steel, and other ores", ukraine_exports$CmdDesc)

ukraine_exports_map <- left_join(world_coordinates, ukraine_exports, by = c("region" = "PartnerDesc"))
ukraine_exports_map <- na.omit(subset(ukraine_exports_map, select = -c(subregion)))

# Calculate Total Values for Each Category
ukr_exp_totals <- subset(ukraine_exports, PartnerDesc == "World") %>%
  group_by(CmdDesc) %>%
  summarise(TotalValue = sum(PrimaryValue))

# Create Plot
ggplot(data = ukraine_exports_map) +
  geom_map(data = world_coordinates, map = world_coordinates, aes(long, lat, map_id=region)) +
  geom_map(map = ukraine_exports_map, aes(long, lat, map_id = region, fill = PrimaryValue)) +
  geom_path(data = world_coordinates, aes(long, lat, group = group),
            color = "gray30", alpha = 0.5, linewidth = 0.1) +
  geom_path(data = ukraine_exports_map, aes(long, lat, group = group),
            color = "gray30", alpha = 0.5, linewidth = 0.1) +
  geom_label(data = ukr_exp_totals,
             aes(0, -45, label = paste("Total Value:",
                                        dollar_format()(round(TotalValue, -6)))),
             family = "Bahnschrift", color = "#fff0d5", fill = "gray30", alpha = 0.75,
             label.size = 0.5, size=5) +
  scale_fill_gradient(low = "darkblue", high = "firebrick1",
                       name = "Export Value (US$)", na.value = "gray20",
                       labels = dollar_format(), trans=log10_trans()) +
  labs(title = "Exports from Ukraine by Country in 2021",
       caption = "Data Source: United Nations Comtrade",
       subtitle = "Total Value of Exports for Largest Categories of Exports") + 
  theme_dark() + map.theme +
  theme(plot.background = element_rect(fill = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1.5, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=16),
        plot.title = element_text(size=30),
        strip.text = element_text(color="#fff0d5", size=16),
        plot.margin = margin(25, 25, 25, 25)) +
  facet_wrap(vars(CmdDesc))

# Difference in Exports Between 2021 and 2022
ukraine_exports21 <- subset(ukraine_exports, RefYear == "2021")
ukraine_exports22 <- subset(ukraine_exports, RefYear == "2022")
ukr_diff <- inner_join(ukraine_exports21, ukraine_exports22, by = c("PartnerDesc" = "PartnerDesc", "CmdDesc" = "CmdDesc"))
ukr_diff <- ukr_diff %>%
  group_by(PartnerDesc, CmdDesc) %>%
  summarise(diff = PrimaryValue.y - PrimaryValue.x)
ukr_diff_map <- left_join(world_coordinates, ukr_diff, by = c("region" = "PartnerDesc"))
ukr_diff_map <- na.omit(subset(ukr_diff_map, select = -c(subregion)))
ukr_diff_map$diff <- ifelse(ukr_diff_map$diff < 0, -log(abs(ukr_diff_map$diff), base = 10), log(ukr_diff_map$diff, base = 10))

# Calculate Total Differences for Each Category
ukr_diff_totals <- subset(ukr_diff, PartnerDesc == "World") %>%
  group_by(CmdDesc) %>%
  summarise(TotalValue = sum(diff))

ggplot(data = ukr_diff_map) +
  geom_map(data = world_coordinates, map = world_coordinates, aes(long, lat, map_id=region)) +
  geom_map(map = ukr_diff_map, aes(long, lat, map_id = region, fill = diff)) +
  geom_path(data = world_coordinates, aes(long, lat, group = group),
            color = "gray30", alpha = 0.5, linewidth = 0.1) +
  geom_path(data = ukr_diff_map, aes(long, lat, group = group),
            color = "gray30", alpha = 0.5, linewidth = 0.1) +
  geom_label(data = ukr_diff_totals,
             aes(0, -45, label = paste("Overall Change:",
                                       dollar_format()(round(TotalValue, -6)))),
             family = "Bahnschrift", color = "#fff0d5", fill = "gray30", alpha = 0.75,
             label.size = 0.5, size=5) +
  scale_fill_gradient2(low = "firebrick1", mid = "gray40", high = "springgreen3",
                      name = "Export Value (US$)", na.value = "gray20",
                      breaks = c(-8, -4, 0, 4, 8),
                      labels = dollar_format()(c(-10^8, -10^4, 0, 10^4, 10^8)),
                      midpoint = 0) +
  labs(title = "Change in Ukrainian Exports by Category from 2021 to 2022",
       caption = "Data Source: United Nations Comtrade",
       subtitle = "Difference in Amount Exported From Ukraine per Country from 2021 to 2022") +
  theme_dark() + map.theme +
  theme(plot.background = element_rect(fill = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1.5, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=16),
        plot.title = element_text(size=30),
        strip.text = element_text(color="#fff0d5", size=16),
        plot.margin = margin(25, 25, 25, 25)) +
  facet_wrap(vars(CmdDesc))

# Line Graph Showing Exports Over Time
# Load Data
ukraine_exports <- read.csv("UkraineAnnualExports.csv")
ukraine_exports$PartnerDesc <- ifelse(ukraine_exports$PartnerDesc == "USA", "United States", ukraine_exports$PartnerDesc)
ukraine_exports$CmdDesc <- ifelse(ukraine_exports$CmdCode == "26" | ukraine_exports$CmdCode == "72", "Iron, steel, and other ores", ukraine_exports$CmdDesc)

ukraine_exports_line <- subset(ukraine_exports, PartnerDesc == "World")
ukraine_exports_line$CmdDesc <- ifelse(ukraine_exports_line$CmdDesc == "Wood and articles of wood; wood charcoal", "Wood", ukraine_exports_line$CmdDesc)
ukraine_exports_line <- ukraine_exports_line %>%
  group_by(CmdDesc, RefYear) %>%
  summarise(PrimaryValue = sum(PrimaryValue))

ggplot(data = ukraine_exports_line) +
  geom_line(mapping = aes(x = RefYear, y = PrimaryValue, color = CmdDesc),
            linewidth = 6, alpha = 0.75) +
  geom_point(mapping = aes(x = RefYear, y = PrimaryValue, color = CmdDesc),
             size = 10) +
  labs(title = "Ukrainian Exports by Product Category",
       subtitle = "Exports from 2011 to 2022", x = "Year",
       y = "Total Value of Exports (US$)",
       caption = "Data Source: United Nations Comtrade") +
  scale_color_brewer(name = "Export Category", palette = "RdYlBu") +
  scale_x_continuous(breaks = seq(2011, 2022, 1), labels = seq(2011, 2022, 1)) +
  scale_y_continuous(labels = dollar_format()) +
  map.theme + theme_dark() +
  theme(plot.background = element_rect(fill = "gray8", color = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        panel.border = element_rect(color = "gray 20", fill = NA, linewidth = 4),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=20),
        plot.title = element_text(size=30),
        strip.text = element_text(color="#fff0d5", size=20),
        axis.text = element_text(color="#fff0d5"),
        plot.margin = margin(25, 25, 25, 25))


# Ukraine Total Exports/Imports Graph
# Import Data and Make Transformations
ukr_tot_exp <- read.csv("UkraineTotalAnnualExports.csv")
ukr_tot_imp <- read.csv("UkraineTotalAnnualImports.csv")
ukr_imp_totals <- ukr_tot_imp %>%
  group_by(RefYear) %>%
  summarize(TotalImports = sum(PrimaryValue))
ukr_exp_totals2 <- subset(ukr_tot_exp, PartnerDesc == "World")
ukr_exp_imp <- full_join(ukr_tot_exp, ukr_tot_imp,
                          by = c("RefYear" = "RefYear",
                                 "PartnerDesc" = "ReporterDesc"))
ukr_exp_imp$PrimaryValue.x <- ifelse(is.na(ukr_exp_imp$PrimaryValue.x), 0, ukr_exp_imp$PrimaryValue.x)
ukr_exp_imp$PrimaryValue.y <- ifelse(is.na(ukr_exp_imp$PrimaryValue.y), 0, ukr_exp_imp$PrimaryValue.y)
ukr_exp_imp <- left_join(ukr_exp_imp, country_to_continent, by = c("PartnerDesc" = "country"))
ukr_exp_imp$continent <- ifelse(is.na(ukr_exp_imp$continent), "None", ukr_exp_imp$continent)
ukr_exp_imp$continent <- ifelse(ukr_exp_imp$continent == "", "None", ukr_exp_imp$continent)
ukr_exp_imp$PrimaryValue.x <- ifelse(ukr_exp_imp$PartnerDesc == "World", 0, ukr_exp_imp$PrimaryValue.x)

# Create Plot
ggplot() +
  geom_bar(data = ukr_exp_imp, stat = "identity",
           mapping = aes(x = RefYear, y = PrimaryValue.x, fill = continent)) +
  geom_bar(data = ukr_exp_imp, stat = "identity",
           mapping = aes(x = RefYear, y = -PrimaryValue.y, fill = continent)) +
  geom_vline(xintercept = 2014.5, linewidth = 1.5, color = "firebrick", linetype = 5) +
  geom_label(data = ukr_exp_totals2, family = "Bahnschrift", color = "#fff0d5",
             fill = "gray30", alpha = 0.75, label.size = 0.5, size=8, nudge_y = 6e+9,
             mapping = aes(x = RefYear, y = PrimaryValue, 
                           label = paste0(str_remove(substr(
                             as.character(dollar_format()(round(PrimaryValue))),
                             1, 4), ","), "b"))) +
  geom_label(data = ukr_imp_totals, family = "Bahnschrift", color = "#fff0d5",
             fill = "gray30", alpha = 0.75, label.size = 0.5, size=8, nudge_y = -6e+9,
             mapping = aes(x = RefYear, y = -TotalImports, 
                           label = paste0(str_remove(substr(
                             as.character(dollar_format()(round(-TotalImports))),
                             1, 4), ","), "b"))) +
  annotate("text", x = 2017, y = 0.75e+11, label = "Exports", alpha = 0.5,
           family = "Bahnschrift", color = "#fff0d5", size = 20) +
  annotate("text", x = 2017, y = -0.75e+11, label = "Imports", alpha = 0.5,
           family = "Bahnschrift", color = "#fff0d5", size = 20) +
  annotate("text", x = 2014.5, y = 0.9e+11, label = "3/21/2014 - Crimea Annexed",
           alpha = 0.9, family = "Bahnschrift", color = "#fff0d5", size = 10,
           hjust = 0.368) +
  geom_hline(yintercept = 0, linewidth = 1.5, color = "#fff0d5", linetype = 5) +
  labs(title = "Ukrainian Trade by Continent from 2012 to 2022",
       subtitle = "Export and Import Values in US$ from 2012 to 2022", x = "Year",
       y = "Trade Balance (US$)",
       caption = "Data Source: United Nations Comtrade") +
  scale_fill_manual(values = c("#F29E4C", "#EFEA5A", "#16DB93",
                               "#048BA8", "#A4036F", "#FE64A3"),
                    name = "Continent") +
  scale_x_continuous(breaks = seq(2011, 2022, 1), labels = seq(2011, 2022, 1)) +
  scale_y_continuous(breaks = seq(-0.9e+11, 0.9e+11, 0.3e+11), labels = dollar_format(),
                     limits = c(-0.9e+11, 0.9e+11)) +
  theme_dark() +
  theme(plot.background = element_rect(fill = "gray8", color = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        panel.border = element_rect(color = "gray 20", fill = NA, linewidth = 4),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=20),
        plot.title = element_text(size=30),
        strip.text = element_text(color="#fff0d5", size=20),
        axis.text = element_text(color="#fff0d5"),
        plot.margin = margin(25, 25, 25, 25))


# Russia Total Imports Bar Graph
rus_tot_exp <- read.csv("RussiaTotalAnnualExports.csv")
rus_tot_exp <- subset(rus_tot_exp, RefYear != "2011")

# Add Estimated Export Number for 2022
rus2022_data <- rus_tot_exp[2,]
rus2022_data$RefYear <- 2022
rus2022_data$PartnerDesc <- "Saint Maarten"
rus2022_data$PrimaryValue <- (1-0.1598) * as.numeric(subset(rus_tot_exp, RefYear == "2021" & PartnerDesc == "World")[9])
rus_tot_exp <- rbind(rus_tot_exp, rus2022_data)
rus2022_data$PartnerDesc <- "World"
rus_tot_exp <- rbind(rus_tot_exp, rus2022_data)

rus_tot_imp <- read.csv("RussiaTotalAnnualImports.csv")
rus_imp_totals <- rus_tot_imp %>%
  group_by(RefYear) %>%
  summarize(TotalImports = sum(PrimaryValue))
rus_exp_totals <- subset(rus_tot_exp, PartnerDesc == "World")
rus_exp_imp <- full_join(rus_tot_exp, rus_tot_imp, by = c("RefYear" = "RefYear",
                                "PartnerDesc" = "ReporterDesc"))
rus_exp_imp$PrimaryValue.x <- ifelse(is.na(rus_exp_imp$PrimaryValue.x), 0, rus_exp_imp$PrimaryValue.x)
rus_exp_imp$PrimaryValue.y <- ifelse(is.na(rus_exp_imp$PrimaryValue.y), 0, rus_exp_imp$PrimaryValue.y)
rus_exp_imp <- left_join(rus_exp_imp, country_to_continent, by = c("PartnerDesc" = "country"))
rus_exp_imp$continent <- ifelse(is.na(rus_exp_imp$continent), "None", rus_exp_imp$continent)
rus_exp_imp$continent <- ifelse(rus_exp_imp$continent == "", "None", rus_exp_imp$continent)
rus_exp_imp$PrimaryValue.x <- ifelse(rus_exp_imp$PartnerDesc == "World", 0, rus_exp_imp$PrimaryValue.x)

# Create Plot
ggplot() +
  geom_bar(data = rus_exp_imp, stat = "identity",
           mapping = aes(x = RefYear, y = PrimaryValue.x, fill = continent)) +
  geom_bar(data = rus_exp_imp, stat = "identity",
           mapping = aes(x = RefYear, y = -PrimaryValue.y, fill = continent)) +
  geom_vline(xintercept = 2014.5, linewidth = 1.5, color = "firebrick", linetype = 5) +
  geom_label(data = rus_exp_totals, family = "Bahnschrift", color = "#fff0d5",
             fill = "gray30", alpha = 0.75, label.size = 0.5, size=8, nudge_y = 3e+10,
             mapping = aes(x = RefYear, y = PrimaryValue, 
                           label = paste0(str_remove(substr(
                             as.character(dollar_format()(round(PrimaryValue))),
                             1, 4), ","), "b"))) +
  geom_label(data = rus_imp_totals, family = "Bahnschrift", color = "#fff0d5",
             fill = "gray30", alpha = 0.75, label.size = 0.5, size=8, nudge_y = -3e+10,
             mapping = aes(x = RefYear, y = -TotalImports, 
                           label = paste0(str_remove(substr(
                             as.character(dollar_format()(round(-TotalImports))),
                             1, 5), ","), "b"))) +
  annotate("text", x = 2017, y = 0.55e+12, label = "Exports", alpha = 0.5,
           family = "Bahnschrift", color = "#fff0d5", size = 20) +
  annotate("text", x = 2017, y = -0.35e+12, label = "Imports", alpha = 0.5,
           family = "Bahnschrift", color = "#fff0d5", size = 20) +
  annotate("text", x = 2022, y = 0.25e+12, label = "Estimate", angle = 90,
           family = "Bahnschrift", color = "#fff0d5", size = 12) +
  geom_hline(yintercept = 0, linewidth = 1.5, color = "#fff0d5", linetype = 5) +
  labs(title = "Russian Trade by Continent from 2012 to 2022",
       subtitle = "Export and Import Values in US$ from 2012 to 2022", x = "Year",
       y = "Trade Balance (US$)",
       caption = "Data Source: United Nations Comtrade") +
  scale_fill_manual(values = c("#F29E4C", "#EFEA5A", "#16DB93",
                               "#048BA8", "#A4036F", "#FE64A3"),
                    name = "Continent") +
  scale_x_continuous(breaks = seq(2012, 2022, 1), labels = seq(2012, 2022, 1)) +
  scale_y_continuous(breaks = seq(-0.6e+12, 0.6e+12, 0.2e+12), labels = dollar_format(),
                     limits = c(-0.4e+12, 0.6e+12)) +
  theme_dark() +
  theme(plot.background = element_rect(fill = "gray8", color = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        panel.border = element_rect(color = "gray 20", fill = NA, linewidth = 4),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=20),
        plot.title = element_text(size=30),
        strip.text = element_text(color="#fff0d5", size=20),
        axis.text = element_text(color = "#fff0d5"),
        plot.margin = margin(25, 25, 25, 25))

# Russia Annual Energy Exports
# Load Data
russia_energy <- read.csv("RussianEnergyExports.csv")
russia_energy$PartnerDesc <- ifelse(russia_energy$PartnerDesc == "USA", "United States", russia_energy$PartnerDesc)
russia_energy <- russia_energy %>%
  group_by(PartnerDesc, RefYear)  %>%
  summarise(PrimaryValue = sum(PrimaryValue, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(PartnerDesc) %>%
  summarise(PrimaryValue = mean(PrimaryValue, na.rm = TRUE))
russia_energy_map <- left_join(world_coordinates, russia_energy, by = c("region" = "PartnerDesc"))
russia_energy_map <- left_join(russia_energy_map, country_to_continent,
                               by = c("region" = "country"))
russia_energy_map <- subset(russia_energy_map, continent == "Europe")
russia_energy_map$PrimaryValue <- ifelse(is.na(russia_energy_map$PrimaryValue), 2, russia_energy_map$PrimaryValue)

# Get countries with highest energy imports
top_5 <- russia_energy_map %>% 
  group_by(region, code_3) %>%
  top_n(1, PrimaryValue) %>%
  summarize(long = mean(long), lat = mean(lat),
            PrimaryValue=mean(PrimaryValue)) %>%
  ungroup() %>%
  top_n(5, PrimaryValue) %>%
  arrange(PrimaryValue)

top_5$lat <- seq(52, 68, 3.75)

# Create Plot 
ggplot(data = russia_energy_map) +
  geom_map(data = world_coordinates, map = world_coordinates, aes(long, lat, map_id=region)) +
  geom_map(map = russia_energy_map, aes(long, lat, map_id = region, fill = PrimaryValue)) +
  geom_path(data = russia_energy_map, aes(long, lat, group = group),
            color = "gray20", alpha = 0.5) +
  geom_label(data = top_5,
             aes(45.5, lat,
                 label = paste0(region, "\n ", dollar_format()(round(PrimaryValue, -6)))),
             nudge_x = -2,
             size = 8, label.padding = unit(0.3, "lines"), fill = "gray50", nudge_y = 0.75,
             color = "#fff0d5", alpha = 0.75, family="Bahnschrift", label.size=0.5) +
  annotate("text", label = "Top 5 Importers:", x = 43.5, y = 70.5, family = "Bahnschrift",
           size = 10, color = "#fff0d5") +
  scale_fill_gradient(low = "gray25", high = "goldenrod1", name = "Energy Imports (US$)",
                      na.value = "gray20", labels = dollar_format(), trans = "log10",
                      limits = c(1e+6, 24718623563)) +
  labs(title = "European Energy Imports from Russia by Country",
       subtitle = "Average Import Value of Coal, Oil, and Electricity in Europe from 2011 to 2021", caption = "Data Source: United Nations Comtrade") + 
  theme_dark() + map.theme + coord_map(proj = "gall", parameters = 30,
                                       orientation = c(90, 0, 0),
                                       xlim = c(-15, 50), ylim = c(35, 70)) +
  theme(plot.background = element_rect(fill = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        panel.border = element_rect(color = "gray 20", fill = NA, linewidth = 4),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=20),
        plot.title = element_text(size=30),
        strip.text = element_text(color="#fff0d5", size=20),
        plot.margin = margin(25, 25, 25, 25))


# Ukraine vs Russia Economic Overview
# Gather Data Together
agri_data <- read.csv("agriculture_percent_of_gdp.csv")
agri_data <- agri_data %>%
  filter(country == "Ukraine" | country == "Russia") %>%
  pivot_longer(cols = X1959:X2019, names_to = "year") %>%
  na.omit()
agri_data$measure <- "Percent of GDP from Agriculture"
agri_data$country <- recode(agri_data$country, Russia = "Russian Federation")

inco_data <- read.csv("gdp_per_capita.csv")
inco_data <- inco_data %>%
  filter(Country.Name == "Ukraine" | Country.Name == "Russian Federation") %>%
  pivot_longer(cols = X1960:X2021, names_to = "year") %>%
  na.omit() %>%
  mutate(measure = "GDP per Capita (PPP-Adjusted)", country = Country.Name) %>%
  select(c(country, year, value, measure))

infl_data <- read.csv("inflation_annual_percent.csv")
infl_data <- infl_data %>%
  filter(country == "Ukraine" | country == "Russia") %>%
  mutate(across(X1960:X2019, as.numeric)) %>%
  pivot_longer(cols = X1960:X2019, names_to = "year") %>%
  na.omit()
infl_data$measure <- "Inflation Rate (%)"
infl_data$country <- recode(infl_data$country, Russia = "Russian Federation")

inve_data <- read.csv("investments_percent_of_gdp.csv")
inve_data <- inve_data %>%
  filter(country == "Ukraine" | country == "Russia") %>%
  mutate(across(X1960:X2019, as.numeric)) %>%
  pivot_longer(cols = X1959:X2019, names_to = "year") %>%
  na.omit()
inve_data$measure <- "invest_percent"
inve_data$country <- recode(inve_data$country, Russia = "Russian Federation")

life_data <- read.csv("life_expectancy_years.csv")
life_data <- life_data %>%
  filter(country == "Ukraine" | country == "Russia") %>%
  mutate(across(X1799:X2099, as.numeric)) %>%
  pivot_longer(cols = X1799:X2099, names_to = "year") %>%
  na.omit()
life_data$measure <- "Life Expectancy"
life_data$country <- recode(life_data$country, Russia = "Russian Federation")

popu_data <- read.csv("population.csv")
popu_data <- popu_data %>%
  filter(Country.Name == "Ukraine" | Country.Name == "Russian Federation") %>%
  pivot_longer(cols = X1960:X2021, names_to = "year") %>%
  na.omit() %>%
  mutate(measure = "total_gdp", country = Country.Name) %>%
  select(c(country, year, value, measure))
popu_data$measure <- "Population"

gdpt_data <- read.csv("actual_total_gdp.csv")
gdpt_data <- gdpt_data %>%
  filter(Country.Name == "Ukraine" | Country.Name == "Russian Federation") %>%
  pivot_longer(cols = X1960:X2021, names_to = "year") %>%
  na.omit() %>%
  mutate(measure = "Annual GDP (Current US$)", country = Country.Name) %>%
  select(c(country, year, value, measure))

econ_factors_data <- rbind(life_data, infl_data, inco_data, agri_data)
#gdpt_data, popu_data, 
econ_factors_data$year <- as.numeric(str_remove(econ_factors_data$year, "X"))
econ_factors_data <- subset(econ_factors_data, year > 1994 & year < 2020)
econ_factors_data$country <- ifelse(econ_factors_data$country == "Russian Federation", "Russia", "Ukraine")

# Create Plot
econ <- ggplot(data = econ_factors_data) +
  geom_line(mapping = aes(x = year, y = value, color = country), linewidth = 2, alpha = 0.65) +
  geom_point(mapping = aes(x = year, y = value, color = country), size = 3) +
  labs(title = "Russia vs. Ukraine Over Time",
       subtitle = "Aspects of the Russian and Ukrainian Economies from 2000 to 2019",
       x="Year", y="Measure Value", caption = "Data Source: World Bank Open Data, Gapminder") +
  facet_wrap(vars(measure), scales="free") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_manual(values = c("firebrick", "deepskyblue4"), name = "Country") +
  theme_dark() +
  theme(plot.background = element_rect(fill = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        panel.border = element_rect(color = "gray 20", fill = NA, linewidth = 2),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=16),
        plot.title = element_text(size=16, hjust=0.5),
        strip.text = element_text(color="#fff0d5", size=16),
        axis.text = element_text(color = "#fff0d5"),
        panel.spacing = unit(0.25, "lines"),
        plot.margin = margin(25, 25, 25, 25))
econ

econ_interactive <- ggplotly(econ)
saveWidget(econ_interactive, "C:/Users/jackt/Documents/College Work/2022-2023/DSCI304/Final Project/RussiaUkraineEconInteractive.html")

# Create Summary Plot
summarize_econ_data <- econ_factors_data %>%
  group_by(country, measure) %>%
  summarize(`Median Value` = median(value)) %>%
  pivot_longer(cols = `Median Value`, names_to = "Type")

colnames(summarize_econ_data) <-c("Country", "measure", "Type", "Value")

sum_econ <- ggplot(data = summarize_econ_data) +
  geom_bar(mapping = aes(x = Type, y = Value, fill = Country), stat = "identity",
           position = position_dodge(), color = "#fff0d5", linewidth = 0.5, alpha = 0.7) +
  labs(title = "Russia vs. Ukraine Summarized",
       subtitle = "Median Aspects of the Russian and Ukrainian Economies",
       x="", y="Median Value", caption = "Data Source: World Bank Open Data, Gapminder") +
  facet_wrap(vars(measure), scales = "free") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_discrete(labels="") +
  scale_fill_manual(values = c("firebrick", "deepskyblue4"), name = "Country") +
  theme_dark() +
  theme(plot.background = element_rect(fill = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        strip.background = element_rect(fill = "gray8"),
        panel.border = element_rect(color = "gray 20", fill = NA, linewidth = 2),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=16),
        plot.title = element_text(size=16, hjust=0.5),
        strip.text = element_text(color="#fff0d5", size=16),
        axis.text = element_text(color = "#fff0d5"),
        plot.margin = margin(25, 25, 25, 25))
sum_econ

sum_econ_interactive <- ggplotly(sum_econ)
sum_econ_interactive
saveWidget(sum_econ_interactive, "C:/Users/jackt/Documents/College Work/2022-2023/DSCI304/Final Project/RussiaUkraineSumEconInteractive.html")


# Ukraine Military Aid Chart
ukr_aid <- read.csv("UkraineSupportTracker.csv")
ukr_aid <- ukr_aid %>%
  select(c("Country", "EU.member", "Financial.commitments", "Humanitarian.commitments",
           "Military.commitments", "Total.bilateral.commitments",
           "Total.bilateral.commitments.1")) %>%
  rename("Financial Aid" = "Financial.commitments",
         "Humanitarian Aid" = "Humanitarian.commitments", 
         "Military Aid" = "Military.commitments", "total_aid" = "Total.bilateral.commitments",
         "aid_perc_gdp" = "Total.bilateral.commitments.1") %>%
  pivot_longer(cols = c(`Financial Aid`:`Military Aid`), names_to = "Type of Aid") %>%
  filter(Country != "EU (Commission and Council)") %>%
  slice_max(n = 30, order_by = total_aid)

# Create Plot
ggplot(data = ukr_aid) +
  geom_bar(mapping = aes(y = reorder(Country, value), x = value, fill = `Type of Aid`),
           stat = "identity", color = "#fff0d5") +
  labs(title = "Foreign Aid for Ukraine by Country and Type of Aid",
       subtitle = "Top 10 Countries Giving Aid; Includes Committed Aid and Delivered Aid",
       y="Country", x="Amount of Aid",
       caption = 'Data Source: "The Ukraine Support Tracker: Which countries help Ukraine and how?" Kiel Working Paper, No. 2218, 1-75') +
  scale_x_continuous(limits = c(0, 75), labels = dollar_format(prefix = "€", scale = 1e+9)) +
  scale_fill_manual(values = c("firebrick", "goldenrod3", "deepskyblue4")) +
  theme_dark() +
  theme(plot.background = element_rect(fill = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        panel.border = element_rect(color = "gray 20", fill = NA, linewidth = 2),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=24),
        plot.title = element_text(size=30, hjust=0),
        axis.text = element_text(color = "#fff0d5"),
        plot.margin = margin(25, 25, 25, 25))

# Create Map
ukr_aid <- read.csv("UkraineSupportTracker.csv")
ukr_aid <- ukr_aid %>%
  select(c("Country", "EU.member", "Financial.commitments", "Humanitarian.commitments",
           "Military.commitments", "Total.bilateral.commitments",
           "Total.bilateral.commitments.1")) %>%
  rename("Financial Aid" = "Financial.commitments",
         "24718623563Humanitarian Aid" = "Humanitarian.commitments", 
         "Military Aid" = "Military.commitments", "total_aid" = "Total.bilateral.commitments",
         "aid_perc_gdp" = "Total.bilateral.commitments.1") %>%
  filter(Country != "EU (Commission and Council)")
ukr_aid$total_aid <- 1e+9 * ukr_aid$total_aid
ukr_aid_map <- left_join(world, ukr_aid, by = c("brk_name" = "Country"))

# Create plot
aid_map <- ggplot(ukr_aid_map) +
  geom_sf(aes(fill = total_aid, label = brk_name), col = "gray30", size = 0.05) +
  scale_fill_gradient2(low = "white", mid = "skyblue", high = "deepskyblue4",
                       name = "Amount of Aid", midpoint = 10e+9,
                      na.value = "gray20", labels = dollar_format(prefix = "€"),
                      limits = c(0, 72e+9)) +
  labs(title = "Foreign Aid for Ukraine by Country",
       subtitle = "Total Aid Committed and/or Delivered",
       y="Country", x="Amount of Aid",
       caption = 'Data Source: "The Ukraine Support Tracker: Which countries help Ukraine and how?" Kiel Working Paper, No. 2218, 1-75') +
  theme_dark() + map.theme +
  theme(plot.background = element_rect(fill = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        panel.border = element_rect(color = "gray 20", fill = NA, linewidth = 2),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=16),
        plot.title = element_text(size=16, hjust=0.5),
        axis.text = element_text(color = "#fff0d5"),
        plot.margin = margin(25, 25, 25, 25))
aid_map
aid_map_interactive <- ggplotly(aid_map)
aid_map_interactive
saveWidget(aid_map_interactive, "C:/Users/jackt/Documents/College Work/2022-2023/DSCI304/Final Project/UkraineAidMapInteractive.html")

# Percent GDP Map
aid_perc_map <- ggplot(ukr_aid_map) +
  geom_sf(aes(fill = aid_perc_gdp, label = brk_name), col = "gray30", size = 0.05) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "orangered",
                       name = "Percent of GDP", midpoint = 0.3,
                       na.value = "gray20", limits = c(0, 1.3)) +
  labs(title = "Foreign Aid as Percent of Country GDP",
       subtitle = "Total Aid Committed and/or Delivered",
       y="Country", x="Percent of GDP",
       caption = 'Data Source: "The Ukraine Support Tracker: Which countries help Ukraine and how?" Kiel Working Paper, No. 2218, 1-75') +
  theme_dark() + map.theme +
  theme(plot.background = element_rect(fill = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        panel.border = element_rect(color = "gray 20", fill = NA, linewidth = 2),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=16),
        plot.title = element_text(size=16, hjust=0.5),
        axis.text = element_text(color = "#fff0d5"),
        plot.margin = margin(25, 25, 25, 25))
aid_perc_map
aid_perc_map_interactive <- ggplotly(aid_perc_map)
aid_perc_map_interactive
saveWidget(aid_perc_map_interactive, "C:/Users/jackt/Documents/College Work/2022-2023/DSCI304/Final Project/UkraineAidPercMapInteractive.html")


# Unfriendly Countries Map
unfried_countries <- c("Albania", "Andorra", "Australia", "Bahamas", "Canada", "Croatia", "Czech Rep.", "Denmark", "Greece", "Hungary", "Iceland", "Japan", "Liechtenstein", "Micronesia", "Monaco", "Montenegro", "New Zealand", "North Macedonia", "Norway", "San Marino", "Singapore", "Slovakia", "Slovenia", "Republic of Korea", "Switzerland", "Taiwan", "Ukraine", "United Kingdom", "United States", "Austria", "Belgium", "Cyprus", "Estonia", "Finland", "France", "Germany", "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands", "Poland", "Portugal", "Romania", "Spain", "Sweden")
Status <- rep_len(1, length(unfried_countries))
unfried_countries <- as.data.frame(cbind(unfried_countries, Status))
unfried_map <- left_join(world, unfried_countries, by = c("brk_name" = "unfried_countries"))
unfried_map$Status <- as.factor(ifelse(is.na(unfried_map$Status), ifelse(unfried_map$brk_name == "Russia", "Russia", "Not Imposed Sanctions"), "Imposed Sanctions"))

# Create Map
unfried_chart <- ggplot(data = unfried_map) +
  geom_sf(aes(fill = Status, label = brk_name), col = "gray10", size = 0.25) +
  scale_fill_manual(values = c("deepskyblue4", "gray30", "firebrick"),
                    name = "Country Status") +
  labs(title = "Countries That Have Imposed Sanctions on Russia",
       caption = 'Data Source: Russian Government') +
  theme_dark() + map.theme +
  theme(plot.background = element_rect(fill = "gray8"),
        panel.background = element_rect(fill = "gray10"),
        panel.border = element_rect(color = "gray 20", fill = NA, linewidth = 2),
        legend.background = element_rect(fill = "gray8"),
        legend.key.height = unit(1, "cm"),
        text = element_text(family="Bahnschrift", color="#fff0d5", size=16),
        plot.title = element_text(size=16, hjust=0.5),
        axis.text = element_text(color = "#fff0d5"),
        plot.margin = margin(25, 25, 25, 25))
unfried_chart
unfried_chart_interactive <- ggplotly(unfried_chart)
unfried_chart_interactive
saveWidget(unfried_chart_interactive, "C:/Users/jackt/Documents/College Work/2022-2023/DSCI304/Final Project/UnfriedMapInteractive.html")
