
R version 4.3.1 (2023-06-16 ucrt) -- "Beagle Scouts"
Copyright (C) 2023 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64 (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications.

Type 'demo()' for some demos, 'help()' for on-line help, or
'help.start()' for an HTML browser interface to help.
Type 'q()' to quit R.

[Previously saved workspace restored]

> 
> # BOYCOTT ANALYSIS
> # CSCI 4343 Assignment 1 - DATA SCIENCE
> # ALI ILHAN THANI BIN JALALUDDIN
> # 2222253
> # SETUP
> library(gtrendsR)
> library(tidyverse)
── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.4     ✔ readr     2.1.5
✔ forcats   1.0.1     ✔ stringr   1.6.0
✔ ggplot2   4.0.1     ✔ tibble    3.2.1
✔ lubridate 1.9.4     ✔ tidyr     1.3.1
✔ purrr     1.0.4     
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
Warning messages:
1: package ‘tidyverse’ was built under R version 4.3.3 
2: package ‘tibble’ was built under R version 4.3.3 
3: package ‘tidyr’ was built under R version 4.3.3 
4: package ‘readr’ was built under R version 4.3.3 
5: package ‘purrr’ was built under R version 4.3.3 
6: package ‘dplyr’ was built under R version 4.3.3 
7: package ‘lubridate’ was built under R version 4.3.3 
> library(ggplot2)
> print("SETUP COMPLETE")
[1] "SETUP COMPLETE"
> # 1) DATA COLLECTION
> # Strategy 1: Single country, multiple keywords
> malaysia_data <- gtrends(
+  keyword = c("boycott Israel", "BDS", "McDonald's boycott"),
+  geo = "MY", # Malaysia
+  time = "today 12-m"
+ )
> # Strategy 2: Single keyword, multiple countries
> country_data <- gtrends(
+  keyword = "boycott Israel",
+  geo = c("MY", "ID", "SG"), # Malaysia, Indonesia, Singapore
+  time = "today 12-m"
+ )
> print("REAL-WORLD DATA COLLECTED")
[1] "REAL-WORLD DATA COLLECTED"
> # 2) DATA PROCESSING
> cat("Processing and cleaning data...\n")
Processing and cleaning data...
> clean_data <- malaysia_data$interest_over_time %>%
+  filter(!is.na(hits)) %>%
+  mutate(
+  date = as.Date(date),
+  keyword = as.factor(keyword)
+  )
> print("DATA PROCESSING COMPLETE")
[1] "DATA PROCESSING COMPLETE"
> # 3) EDA
> cat("Performing exploratory analysis...\n")
Performing exploratory analysis...
> # Summary statistics
> summary_stats <- clean_data %>%
+  group_by(keyword) %>%
+  summarize(
+  avg_interest = round(mean(hits, na.rm = TRUE), 2),
+  max_interest = max(hits, na.rm = TRUE),
+  peak_date = date[which.max(hits)],
+  total_observations = n()
+  )
> print("SUMMARY STATISTICS")
[1] "SUMMARY STATISTICS"
> print(summary_stats)
# A tibble: 3 × 5
  keyword            avg_interest max_interest peak_date  total_observations
  <fct>                     <dbl>        <int> <date>                  <int>
1 BDS                       35.3            78 2025-10-05                 54
2 boycott Israel            15.4           100 2025-10-05                 54
3 McDonald's boycott         0.04            1 2026-03-29                 54
> # 4) MACHINE LEARNING
> print("Doing correlation and trend analysis\n")
[1] "Doing correlation and trend analysis\n"
> # 1. Correlation analysis
> all_keywords <- unique(clean_data$keyword)
> cat("Keywords found:", paste(all_keywords, collapse = ", "), "\n\n")
Keywords found: boycott Israel, BDS, McDonald's boycott 

> # Create empty matrix
> cor_matrix <- matrix(NA, nrow = length(all_keywords), ncol = length(all_keywords))
> rownames(cor_matrix) <- all_keywords
> colnames(cor_matrix) <- all_keywords
> # Fill in the matrix from above
> #for loop
> 
> for(i in 1:length(all_keywords)) {
+  for(j in 1:length(all_keywords)) {
+  # Get data for each keyword pair
+  data_i <- clean_data$hits[clean_data$keyword == all_keywords[i]]
+  data_j <- clean_data$hits[clean_data$keyword == all_keywords[j]]
+ 
+  # Calculate correlation
+  if(length(data_i) > 1 & length(data_j) > 1) {
+  cor_matrix[i,j] <- round(cor(data_i, data_j, use = "complete.obs"), 3)
+  }
+  }
+ }
> # Print the correlation table
> print("Correlation Matrix:")
[1] "Correlation Matrix:"
> print(cor_matrix)
                   boycott Israel    BDS McDonald's boycott
boycott Israel              1.000  0.663             -0.071
BDS                         0.663  1.000             -0.153
McDonald's boycott         -0.071 -0.153              1.000
> # 2. Trend analysis
> # Defining keywords
> all_keywords <- unique(clean_data$keyword)
> cat("Keywords to analyze:", paste(all_keywords, collapse = ", "), "\n\n")
Keywords to analyze: boycott Israel, BDS, McDonald's boycott 

> # Create empty data frame
> trend_results <- data.frame(
+  keyword = character(),
+  slope = numeric(),
+  p_value = numeric(),
+  trend = character(),
+  stringsAsFactors = FALSE
+ )
> # Rest of your code continues...
> for(term in all_keywords) {
+  term_data <- clean_data[clean_data$keyword == term, ]
+ 
+  if(nrow(term_data) > 1) {
+  # Simple linear model
+  model <- lm(hits ~ as.numeric(date), data = term_data)
+ 
+  # Get results
+  slope_val <- round(coef(model)[2], 4)
+  p_val <- round(summary(model)$coefficients[2,4], 4)
+  trend_dir <- ifelse(slope_val > 0, "INCREASING", "DECREASING")
+ 
+  # Add to results
+  trend_results <- rbind(trend_results,
+  data.frame(keyword = term,
+  slope = slope_val,
+  p_value = p_val,
+  trend = trend_dir))
+  }
+ }
> # Print trend table
> print("Trend Analysis:")
[1] "Trend Analysis:"
> print(trend_results)
                             keyword   slope p_value      trend
as.numeric(date)      boycott Israel -0.0172  0.4468 DECREASING
as.numeric(date)1                BDS -0.0109  0.4947 DECREASING
as.numeric(date)2 McDonald's boycott  0.0005  0.0205 INCREASING
> cat("\n")

> # 5) MULTIPLE GRAPH VISUALIZATIONS
> cat("Creating graph visualizations...\n")
Creating graph visualizations...
> # Graph 1: Time Series Analysis
> p1 <- ggplot(clean_data, aes(x = date, y = hits, color = keyword)) +
+  geom_line(size = 1.1, alpha = 0.9) +
+  geom_point(size = 1.1) +
+  labs(title = "BOYCOTT MOVEMENT SEARCH TRENDS IN MALAYSIA",
+  subtitle = "Time-Series Analysis of Public Interest (Jan 2025 - Oct 2025)",
+  x = "Timeline",
+ 
+  y = "Google Search Interest Index",
+  color = "Search Terms") +
+  theme_minimal() +
+  scale_color_brewer(palette = "Set1")
Warning message:
Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
ℹ Please use `linewidth` instead.
This warning is displayed once every 8 hours.
Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
generated. 
> print("Time Series Analysis complete")
[1] "Time Series Analysis complete"
> # Graph 2: Peak Interest Comparison
> peak_data <- clean_data %>%
+  group_by(keyword) %>%
+  filter(hits == max(hits, na.rm = TRUE))
> p2 <- ggplot(peak_data, aes(x = reorder(keyword, hits), y = hits, fill = keyword)) +
+  geom_col(alpha = 0.9) +
+  labs(title = "PEAK SEARCH INTEREST COMPARISON",
+  subtitle = "Maximum Engagement with Different Boycott Terms",
+  x = "Search Terms",
+  y = "Peak Search Interest Score",
+  fill = "Search Terms") +
+  theme_minimal() +
+  coord_flip() +
+  scale_fill_brewer(palette = "Set2")
> print("Peak Interest Comparison")
[1] "Peak Interest Comparison"
> # Graph 3: Monthly Average Comparison
> monthly_avg <- clean_data %>%
+  mutate(month = format(date, "%Y-%m")) %>%
+  group_by(month, keyword) %>%
+  summarize(avg_interest = mean(hits, na.rm = TRUE), .groups = 'drop')
> p3 <- ggplot(monthly_avg, aes(x = month, y = avg_interest, fill = keyword)) +
+  geom_col(position = "dodge") +
+  labs(title = "MONTHLY AVERAGE SEARCH INTEREST",
+  subtitle = "Comparative Analysis Over Time",
+  x = "Month",
+  y = "Average Search Interest",
+  fill = "Search Terms") +
+  theme_minimal() +
+  theme(axis.text.x = element_text(angle = 50, hjust = 1.1))
> print("Monthly Average comparison complete")
[1] "Monthly Average comparison complete"
> # Graph 4: Country Comparison
> p4 <- ggplot(country_data$interest_over_time,
+  aes(x = date, y = hits, color = geo)) +
+  geom_line(size = 1.42) +
+  labs(title = "BOYCOTT SEARCH INTEREST: COUNTRY COMPARISON",
+  subtitle = "Malaysia vs Indonesia vs Singapore - 'boycott Israel' Searches",
+  x = "Date",
+  y = "Search Interest Index",
+  color = "Country") +
+  theme_minimal() +
+  scale_color_brewer(palette = "Set1")
> print("Country Comparison completed")
[1] "Country Comparison completed"
> # 6) SAVE ALL OUTPUTS
> cat("Saving all graphs\n")
Saving all graphs
> # Save plots
> ggsave("time_series_analysis.png", p1, width = 12, height = 8, dpi = 300)
> ggsave("peak_analysis.png", p2, width = 10, height = 8, dpi = 300)
> ggsave("monthly_analysis.png", p3, width = 10, height = 8, dpi = 300)
> ggsave("country_comparison.png", p4, width = 12, height = 6, dpi = 300)
> # Save data summary
> write_csv(summary_stats, "analysis_summary_statistics.csv")

[1mwrote[0m [32m2.15GB[0m in [36m 0s[0m, [32m2.15GB/s[0m
                                                                              
> print(" 4 graphs are created")
[1] " 4 graphs are created"
> 
> # Display graphs on screen
> print(p1) # Time series plot
> print(p2) # Peak analysis plot
> print(p3) # Monthly analysis plot
> print(p4) # Country comparison
