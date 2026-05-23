# ==============================================================================
# MULTIVARIATE DATA ANALYSIS FOR WRESTLING: DESCRIPTIVE PLOTS & CORRELATIONS
# ==============================================================================

# 1. PACKAGE CHECK AND INSTALLATION
required_packages <- c("ggplot2", "dplyr", "tidyr", "ggcorrplot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)

# 2. DATA LOADING AND CLEANING
if (!file.exists("wrestling_stats.csv")) {
  stop("The file 'wrestling_stats.csv' was not found in the working directory! Please check the folder.")
}

wrestling_raw <- read.csv("wrestling_stats.csv", header = TRUE, sep = ",")

# Replace all NA values with 0 in technical columns
wrestling_clean <- wrestling_raw %>%
  mutate(across(Attack:Opponent.Penalty, ~replace_na(as.numeric(.), 0)))

# RENAME COLUMNS IN ENGLISH (Clean names without dots)
wrestling_clean <- wrestling_clean %>%
  rename(
    `Attacks`                        = Attack, 
    `Counter Attacks`                = Counter.Attack, 
    `Front Headlock Points`          = Points.From.Front.Head.Lock,
    `Pushout Points`                 = Points.From.Pushing.Out.Of.Bounds,
    `Opponent Inactivity`            = Opponent.Inactivity,
    `High Single`                    = High.Single, 
    `Low Single`                     = Low.Single, 
    `Double Leg`                     = Double.Leg, 
    `Go Behind`                      = Go.Behind, 
    `Duck Under`                     = Duck.Under,
    `Slide By / Throw By`            = Slide.By.And.Throw.By,
    `Ankle Pick`                     = Ankle.Pick,
    `Trips`                          = Trips,
    `Upper Body Throws`              = Upper.Body.Throws,
    `Gut Wrench`                     = Gut.Wrench, 
    `Leg Lace`                       = Leg.Lace, 
    `Opponent Penalty`               = Opponent.Penalty
  )

# Convert outcome to Factor with clear English labels
wrestling_clean$Win.Lose <- factor(wrestling_clean$Win.Lose, levels = c(0, 1), labels = c("Loss", "Win"))


# 3. WIDE TO LONG FORMAT CONVERSION & POINT CALCULATION
technical_moves <- wrestling_clean %>%
  select(Match.Id, Win.Lose, `Attacks`, `Counter Attacks`, `Front Headlock Points`, 
         `Pushout Points`, `Opponent Inactivity`, `High Single`, `Low Single`, 
         `Double Leg`, `Go Behind`, `Duck Under`, `Slide By / Throw By`, 
         `Ankle Pick`, `Trips`, `Upper Body Throws`, `Gut Wrench`, `Leg Lace`, `Opponent Penalty`) %>%
  gather(key = "Move", value = "Count", -Match.Id, -Win.Lose)

# Calculate Points Generated based on UWW Rules
technical_moves <- technical_moves %>%
  mutate(Points_Generated = case_when(
    Move == "Upper Body Throws" ~ Count * 4,
    Move %in% c("Pushout Points", "Opponent Inactivity", "Opponent Penalty") ~ Count * 1,
    TRUE ~ Count * 2  # Standard takedowns and parterre turns score 2 points
  ))


# ==============================================================================
# PLOT 1: FREQUENCY OF EXECUTION vs TOTAL POINTS GENERATED
# ==============================================================================
move_summary <- technical_moves %>%
  group_by(Move) %>%
  summarize(
    `Total Executions (Count)` = sum(Count),
    `Total Points Generated` = sum(Points_Generated),
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = c(`Total Executions (Count)`, `Total Points Generated`), 
               names_to = "Metric", values_to = "Value")

plot1 <- ggplot(move_summary, aes(x = reorder(Move, Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85, color = "black", size = 0.2) +
  coord_flip() +
  scale_fill_manual(values = c("Total Executions (Count)" = "#4A6572", "Total Points Generated" = "#F9AA33")) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, margin = margin(b=10)),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = "Technical Actions: Frequency vs. Total Points Generated",
    subtitle = "Analysis across all recorded matches",
    x = "Tactical / Technical Variable",
    y = "Total Value",
    fill = "Analysis Metric:"
  )

print(plot1)


# ==============================================================================
# PLOT 2: TACTICAL PROFILE: WINNERS vs LOSERS
# ==============================================================================
win_loss_summary <- technical_moves %>%
  group_by(Win.Lose, Move) %>%
  summarize(Avg_Per_Match = mean(Count), .groups = 'drop')

plot2 <- ggplot(win_loss_summary, aes(x = reorder(Move, Avg_Per_Match), y = Avg_Per_Match, fill = Win.Lose)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.85, color = "black", size = 0.2) +
  coord_flip() +
  scale_fill_manual(values = c("Loss" = "#E06666", "Win" = "#6AA84F"),
                    labels = c("Loss (0)", "Win (1)")) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, margin = margin(b=10)),
    legend.position = "bottom",
    panel.grid.major.y = element_blank()
  ) +
  labs(
    title = "Tactical Profile: Winners vs. Losers",
    subtitle = "Average number of successful executions per match",
    x = "Tactical / Technical Variable",
    y = "Average Executions per Match",
    fill = "Match Outcome:"
  )

print(plot2)


# ==============================================================================
# PLOT 3: LINEAR CORRELATION HEATMAP
# ==============================================================================
correlation_data <- wrestling_clean %>%
  select(`Attacks`, `Counter Attacks`, `Front Headlock Points`, `Pushout Points`, 
         `Opponent Inactivity`, `High Single`, `Low Single`, `Double Leg`, 
         `Go Behind`, `Duck Under`, `Slide By / Throw By`, `Ankle Pick`, 
         `Trips`, `Upper Body Throws`, `Gut Wrench`, `Leg Lace`, `Opponent Penalty`)

corr_matrix <- cor(correlation_data, use = "pairwise.complete.obs")

plot3 <- ggcorrplot(
  corr_matrix, 
  hc.order = FALSE, 
  type = "lower",
  lab = TRUE, 
  lab_size = 2.5,
  method = "square",
  colors = c("#4575B4", "#FFFFFF", "#D73027"),
  title = "Correlation Heatmap of Wrestling Variables",
  ggtheme = theme_minimal(base_size = 11)
) +
theme(
  plot.title = element_text(face = "bold", size = 12, margin = margin(b=10)),
  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
)

print(plot3)


# ==============================================================================
# AUTOMATIC IMAGE EXPORT
# ==============================================================================
ggsave("plot1_frequency_vs_points.png", plot = plot1, width = 11, height = 7, dpi = 300)
ggsave("plot2_winners_vs_losers.png", plot = plot2, width = 11, height = 7, dpi = 300)
ggsave("plot3_correlation_heatmap.png", plot = plot3, width = 10, height = 9, dpi = 300)

cat("\n[SUCCESS] All 3 plots exported successfully in English with clean labels!\n")