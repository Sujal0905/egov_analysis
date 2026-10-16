# ==============================================================
# Week 1 Task - Data Acquisition & Preliminary Analysis in E-Governance
# Dataset : UN E-Government Development Index (EGDI) 2024
# Source  : UN DESA, https://publicadministration.un.org/egovkb/Data-Center
# ==============================================================

library(readr)
library(dplyr)
library(ggplot2)

dir.create("figures", showWarnings = FALSE)

# ---------- 1. DATA IMPORT ----------
egdi_raw <- read_csv("data/egdi_2024_raw.csv", show_col_types = FALSE)

glimpse(egdi_raw)
cat("Rows:", nrow(egdi_raw), " Columns:", ncol(egdi_raw), "\n")

# ---------- 2. DATA CLEANING ----------
# 2a. Standardise column names
egdi <- egdi_raw %>%
  rename(country = Country, group = Group, rating_class = `Rating Class`,
         rank = `Rank 2024`, egdi = `EGDI 2024`, msq = MSQ)

# 2b. Missing / inconsistent values
cat("\nMissing values per column:\n")
print(colSums(is.na(egdi)))

# MSQ is blank for some countries and "No*" for others -> recode to explicit labels
egdi <- egdi %>%
  mutate(
    msq_response = if_else(is.na(msq), "Responded / not flagged", "No response (No*)"),
    country      = trimws(country),
    country      = recode(country, "Georgia (Country)" = "Georgia")
  )

# 2c. Correct data types and ordered factors
egdi <- egdi %>%
  mutate(
    group = factor(group, levels = c("LEGDI", "MEGDI", "HEGDI", "VHEGDI"),
                   labels = c("Low", "Middle", "High", "Very High")),
    rating_class = factor(rating_class,
                          levels = c("L1","L2","L3","LM","M1","M2","M3","MH",
                                     "H1","H2","H3","HV","V1","V2","V3","VH")),
    msq_response = factor(msq_response)
  ) %>%
  select(-msq)

# 2d. Validation checks
stopifnot(!any(duplicated(egdi$country)))          # no duplicate countries
stopifnot(all(egdi$egdi >= 0 & egdi$egdi <= 1))    # EGDI must be in [0, 1]
cat("\nDuplicate countries:", sum(duplicated(egdi$country)), "\n")
cat("EGDI outside [0,1]:", sum(egdi$egdi < 0 | egdi$egdi > 1), "\n")

# Tied scores share a different 'rank' in the UN table -> check
ties <- egdi %>% group_by(egdi) %>% filter(n() > 1) %>% arrange(desc(egdi))
cat("\nCountries with tied EGDI scores:\n")
print(ties %>% select(country, rank, egdi), n = 20)

# Does the UN 'group' agree with the official EGDI thresholds?
egdi <- egdi %>%
  mutate(group_check = cut(egdi, breaks = c(0, 0.25, 0.50, 0.75, 1),
                           labels = c("Low","Middle","High","Very High"),
                           include.lowest = TRUE, right = FALSE))
cat("\nGroup vs threshold mismatches:", sum(egdi$group != egdi$group_check), "\n")
print(egdi %>% filter(group != group_check) %>% select(country, egdi, group, group_check))

# ---------- 3. SUMMARY STATISTICS ----------
cat("\n--- Summary of EGDI ---\n")
print(summary(egdi$egdi))
cat("SD:", round(sd(egdi$egdi), 4), "  IQR:", round(IQR(egdi$egdi), 4), "\n")

group_stats <- egdi %>%
  group_by(group) %>%
  summarise(countries = n(),
            mean   = round(mean(egdi), 3),
            median = round(median(egdi), 3),
            sd     = round(sd(egdi), 3),
            min    = min(egdi), max = max(egdi))
cat("\n--- EGDI by group ---\n")
print(group_stats)

msq_stats <- egdi %>%
  group_by(msq_response) %>%
  summarise(countries = n(), mean = round(mean(egdi), 3), median = round(median(egdi), 3))
cat("\n--- EGDI by questionnaire response status ---\n")
print(msq_stats)

# Where does India stand?
india <- egdi %>% filter(country == "India")
cat("\nIndia -> rank:", india$rank, " EGDI:", india$egdi, " group:", as.character(india$group), "\n")
cat("India percentile (share of countries scoring lower):",
    round(mean(egdi$egdi < india$egdi) * 100, 1), "%\n")
cat("Global mean EGDI:", round(mean(egdi$egdi), 4), "\n")

# ---------- 4. VISUALISATIONS ----------
theme_set(theme_minimal(base_size = 12))
pal <- c("Low" = "#d95f02", "Middle" = "#e6ab02", "High" = "#1b9e77", "Very High" = "#2c5aa0")

# 4.1 Histogram of EGDI
p1 <- ggplot(egdi, aes(x = egdi)) +
  geom_histogram(binwidth = 0.05, fill = "#2c5aa0", colour = "white") +
  geom_vline(xintercept = mean(egdi$egdi), linetype = "dashed", colour = "#d95f02") +
  labs(title = "Distribution of E-Government Development Index (2024)",
       subtitle = "Dashed line = global mean", x = "EGDI score", y = "Number of countries")
ggsave("figures/fig1_histogram.png", p1, width = 7, height = 4.2, dpi = 200, bg = "white")

# 4.2 Countries per EGDI group
p2 <- ggplot(egdi, aes(x = group, fill = group)) +
  geom_bar(show.legend = FALSE) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4) +
  scale_fill_manual(values = pal) +
  labs(title = "Number of countries in each EGDI group (2024)", x = NULL, y = "Countries")
ggsave("figures/fig2_groups.png", p2, width = 7, height = 4.2, dpi = 200, bg = "white")

# 4.3 Boxplot of EGDI by questionnaire response
p3 <- ggplot(egdi, aes(x = msq_response, y = egdi, fill = msq_response)) +
  geom_boxplot(show.legend = FALSE, alpha = 0.8) +
  scale_fill_manual(values = c("#9ecae1", "#fdae6b")) +
  labs(title = "EGDI by Member State Questionnaire (MSQ) status",
       x = NULL, y = "EGDI score")
ggsave("figures/fig3_msq_boxplot.png", p3, width = 7, height = 4.2, dpi = 200, bg = "white")

# 4.4 Scatter: rank vs score, India highlighted
p4 <- ggplot(egdi, aes(x = rank, y = egdi, colour = group)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_point(data = india, colour = "black", size = 4, shape = 1, stroke = 1.2) +
  annotate("text", x = india$rank + 6, y = india$egdi + 0.06, label = "India", fontface = "bold") +
  scale_colour_manual(values = pal, name = "EGDI group") +
  labs(title = "EGDI score against global rank (2024)", x = "Rank (1 = best)", y = "EGDI score")
ggsave("figures/fig4_rank_scatter.png", p4, width = 7, height = 4.2, dpi = 200, bg = "white")

# 4.5 Top 10 and bottom 10
extremes <- bind_rows(
  egdi %>% slice_min(rank, n = 10) %>% mutate(end = "Top 10"),
  egdi %>% slice_max(rank, n = 10) %>% mutate(end = "Bottom 10")
) %>% mutate(end = factor(end, levels = c("Top 10", "Bottom 10")))

p5 <- ggplot(extremes, aes(x = reorder(country, egdi), y = egdi, fill = end)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~end, scales = "free_y") +
  scale_fill_manual(values = c("#2c5aa0", "#d95f02")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, 22)) +
  labs(title = "Highest and lowest EGDI scores (2024)", x = NULL, y = "EGDI score")
ggsave("figures/fig5_top_bottom.png", p5, width = 9, height = 4.8, dpi = 200, bg = "white")

# ---------- 5. SIMPLE STATISTICAL TEST ----------
# Do countries that did not respond to the MSQ score differently?
wt <- wilcox.test(egdi ~ msq_response, data = egdi)
print(wt)

# ---------- 6. SAVE CLEAN DATA ----------
write_csv(egdi %>% select(-group_check), "data/egdi_2024_clean.csv")
cat("\nDone. Clean file and figures saved.\n")
