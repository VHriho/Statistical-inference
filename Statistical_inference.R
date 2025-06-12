
library(dplyr)
library(readr)
library(boot)
library(forcats)
library(ggplot2)
library(tidyr)
library(scales)
library(knitr)
library(stringr)
library(kableExtra)
library(showtext)
library(purrr)      # для map_* у бутстрепі


PED_Clean <- read_csv("PED_Clean.csv")

alpha <- 0.05
z     <- qnorm(1 - alpha/2)


boro_ci <- PED_Clean %>%
  filter(
    PERIOD == 3,
    YEAR   == 2025,
    startsWith(PSTATUS, "A"),
    CUREXMPTOT <= 1e9
  ) %>%
  mutate(
    CUREXMPTOT = as.numeric(CUREXMPTOT),
    BORO = factor(BORO,
                  levels = 1:5,
                  labels = c("Manhattan","Bronx","Brooklyn","Queens","Staten Island"))
  ) %>%
  group_by(BORO) %>%
  summarise(
    N    = n(),
    Mean = mean(CUREXMPTOT, na.rm = TRUE),
    SD   = sd(CUREXMPTOT,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    SE    = z * SD / sqrt(N),
    Lower = Mean - SE,
    Upper = Mean + SE
  )

# 2) Красива таблиця для середніх
tbl_mean <- boro_ci %>%
  mutate(
    Mean   = dollar(Mean, accuracy = 1),
    SD     = dollar(SD,   accuracy = 1),
    SE = dollar(SE, accuracy = 1),
    `95% CI` = paste0(
      "$",
      comma(Lower, accuracy = 1),
      " – ",
      comma(Upper, accuracy = 1)
    )
  ) %>%
  select(Borough = BORO, N, SD, SE, Mean, `95% CI`)

tbl_mean %>%
  kable(
    format   = "html",
    booktabs = TRUE,
    align    = c("l","r","r","r","c")
  ) %>%
  kable_styling(
    full_width        = FALSE,
    position          = "center",
    bootstrap_options = c("striped","hover")
  )

# 3) Графік середніх з 95% CI
boro_plot_df <- boro_ci %>%
  mutate(BORO = factor(BORO, levels = rev(levels(BORO))))

ggplot(boro_plot_df, aes(x = Mean, y = BORO)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                 height = 0.2, color = "steelblue") +
  scale_x_continuous(labels = dollar) +
  labs(
    title = "95% CI for Mean Per-Parcel Exemption by Borough (2025)",
    x     = "Mean exemption",
    y     = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

# 4) Bootstrap CI для дисперсії по борам
set.seed(123)  # для відтворюваності

var_ci_boot <- PED_Clean %>%
  filter(
    PERIOD == 3,
    YEAR   == 2025,
    startsWith(PSTATUS, "A"),
    CUREXMPTOT <= 1e9
  ) %>%
  mutate(
    CUREXMPTOT = as.numeric(CUREXMPTOT),
    BORO = factor(BORO,
                  levels = 1:5,
                  labels = c("Manhattan","Bronx","Brooklyn","Queens","Staten Island"))
  ) %>%
  group_by(BORO) %>%
  summarise(
    N    = n(),
    boot = list(
      boot(
        data      = CUREXMPTOT,
        statistic = function(dat, idx) var(dat[idx], na.rm = TRUE),
        R         = 2000
      )
    ),
    .groups = "drop"
  ) %>%
  mutate(
    # extract all CI types from a single boot.ci() call
    ci       = map(boot, ~ boot.ci(.x, type = c("norm","basic","perc","bca"))),
    Variance = map_dbl(boot, ~ mean(.x$t, na.rm = TRUE)),
    # normal approximation
    norm_lo  = map_dbl(ci, ~ .x$normal[2]),
    norm_hi  = map_dbl(ci, ~ .x$normal[3]),
    # basic bootstrap
    basic_lo = map_dbl(ci, ~ .x$basic[4]),
    basic_hi = map_dbl(ci, ~ .x$basic[5]),
    # percentile bootstrap
    perc_lo  = map_dbl(ci, ~ .x$percent[4]),
    perc_hi  = map_dbl(ci, ~ .x$percent[5]),
    # bias-corrected and accelerated
    bca_lo   = map_dbl(ci, ~ .x$bca[4]),
    bca_hi   = map_dbl(ci, ~ .x$bca[5])
  )

# 5) Table: show all CIs
tbl_var_boot_all <- var_ci_boot %>%
  transmute(
    Borough     = BORO,
    N           = N,
    Variance    = dollar(Variance, accuracy = 1),
    `Norm CI`   = sprintf("$%s – $%s",
                          comma(norm_lo,  accuracy = 1),
                          comma(norm_hi,  accuracy = 1)),
    `Basic CI`  = sprintf("$%s – $%s",
                          comma(basic_lo, accuracy = 1),
                          comma(basic_hi, accuracy = 1)),
    `Perc CI`   = sprintf("$%s – $%s",
                          comma(perc_lo,  accuracy = 1),
                          comma(perc_hi,  accuracy = 1)),
    `BCa CI`    = sprintf("$%s – $%s",
                          comma(bca_lo,   accuracy = 1),
                          comma(bca_hi,   accuracy = 1))
  )

tbl_var_boot_all %>%
  kable(
    format   = "html",
    booktabs = TRUE,
    align    = c("l","r","r","r","r","r","r")
  ) %>%
  kable_styling(
    full_width        = FALSE,
    position          = "center",
    bootstrap_options = c("striped","hover")
  )

boro_plot_var_all <- var_ci_boot %>%
  mutate(BORO = factor(BORO, levels = rev(levels(BORO))))

ggplot(boro_plot_var_all, aes(x = Variance, y = BORO)) +
  geom_errorbarh(aes(xmin = perc_lo, xmax = perc_hi, colour = "Percentile"),
                 height = 0.2, size = 1.2, alpha = 0.9) +
  geom_errorbarh(aes(xmin = basic_lo, xmax = basic_hi, colour = "Basic"),
                 height = 0.2, size = 1.2, alpha = 0.9) +
  geom_errorbarh(aes(xmin = norm_lo, xmax = norm_hi, colour = "Normal"),
                 height = 0.2, size = 1.2, alpha = 0.9) +
  geom_errorbarh(aes(xmin = bca_lo, xmax = bca_hi, colour = "BCa"),
                 height = 0.2, size = 1.2, alpha = 0.9) +
  geom_point(aes(colour = "Estimate"), size = 2, stroke = 1.5) +
  scale_colour_manual(
    name   = "Interval type",
    values = c(
      "Percentile" = "orange",
      "Basic"      = "red",
      "Normal"     = "purple",
      "BCa"        = "steelblue",
      "Estimate"   = "black"
    )
  ) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title   = "Variance with Multiple 95% Bootstrap CIs by Borough (2025)",
    x       = "Variance of CUREXMPTOT",
    y       = NULL,
    caption = "Thicker lines, vibrant palette"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y  = element_blank(),
    panel.grid.minor    = element_blank(),
    legend.position     = "bottom",
    legend.key.width    = unit(1.5, "lines"),
    legend.text         = element_text(size = 12, face = "bold"),
    legend.title        = element_text(size = 13, face = "bold")
  )

# 7) Аналіз по типу будівлі (з бутстрепом для дисперсії)

df_bt <- PED_Clean %>%
  filter(
    PERIOD == 3,
    YEAR   == 2025,
    startsWith(PSTATUS, "A"),
    CUREXMPTOT <= 1e9
  ) %>%
  mutate(
    CUREXMPTOT = as.numeric(CUREXMPTOT),
    BLDG_TYPE  = str_sub(`BLDG-CLASS`, 1, 1)
  ) %>%
  filter(grepl("^[A-Za-z]$", BLDG_TYPE)) %>%
  mutate(
    BLDG_TYPE = factor(BLDG_TYPE, levels = sort(unique(BLDG_TYPE)))
  )

# 7.2) Parametric CI for mean by building type
summary_bt <- df_bt %>%
  group_by(BLDG_TYPE) %>%
  summarise(
    N    = n(),
    Mean = mean(CUREXMPTOT),
    SD   = sd(CUREXMPTOT),
    .groups = "drop"
  ) %>%
  mutate(
    ME     = z * SD / sqrt(N),
    MeanLo = Mean - ME,
    MeanHi = Mean + ME
  )

mean_tbl <- summary_bt %>%
  transmute(
    `Building Type` = BLDG_TYPE,
    N               = comma(N),
    SD              = dollar(SD,  accuracy = 0.01),
    SE              = dollar(ME,  accuracy = 1),
    Mean            = dollar(Mean,accuracy = 0.01),
    `95% CI`        = paste0(
      "$",
      comma(MeanLo, accuracy = 1),
      " – ",
      comma(MeanHi, accuracy = 1)
    )
  )

mean_tbl %>%
  kable(
    booktabs = TRUE,
    align    = c("c","r","r","r","r","c"),
    col.names = c("Building Type","N","SD","SE","Mean","95% CI")
  ) %>%
  kable_styling(full_width = FALSE, position = "center")

# 7.3) Bootstrap CI for variance by building type
set.seed(123)

mean_bt_boot <- df_bt %>%
  group_by(BLDG_TYPE) %>%
  summarise(
    N      = n(),
    boot_m = list(
      boot(
        data      = CUREXMPTOT,
        statistic = function(dat, idx) mean(dat[idx], na.rm = TRUE),
        R         = 2000
      )
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Mean     = map_dbl(boot_m, ~ mean(.x$t, na.rm = TRUE)),
    MeanLo   = map_dbl(boot_m, ~ boot.ci(.x, type = "perc")$percent[4]),
    MeanHi   = map_dbl(boot_m, ~ boot.ci(.x, type = "perc")$percent[5])
  )

# 7.3) Table of bootstrap means with 95% CI
mean_tbl_boot <- mean_bt_boot %>%
  mutate(
    Mean     = dollar(Mean, accuracy = 1),
    `95% CI` = paste0(
      "$", comma(MeanLo, accuracy = 1),
      " – ", comma(MeanHi, accuracy = 1)
    )
  ) %>%
  select(`Building Type` = BLDG_TYPE, N, Mean, `95% CI`)

mean_tbl_boot %>%
  kable(
    booktabs = TRUE,
    align    = c("c","r","r","c"),
    col.names = c("Building Type","N","Mean","95% CI")
  ) %>%
  kable_styling(full_width = FALSE, position = "center")

# 7.4) Dot-and-whisker plot for bootstrap mean by building type

p1_vert_boot <- ggplot(mean_bt_boot, aes(x = BLDG_TYPE, y = Mean)) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbar(aes(ymin = MeanLo, ymax = MeanHi),
                width = 0.2, color = "steelblue") +
  scale_y_log10(labels = comma) +
  labs(
    title = "Mean Exemption ± 95% Bootstrap CI by Building Type (log scale)",
    x     = "Building Type",
    y     = "Mean exemption, $"
  ) +
  theme_classic(base_size = 13) +
  theme(panel.grid.major.x = element_blank())

print(p1_vert_boot)

var_bt_boot <- df_bt %>%
  group_by(BLDG_TYPE) %>%
  summarise(
    N    = n(),
    boot = list(
      boot(
        data      = CUREXMPTOT,
        statistic = function(dat, idx) var(dat[idx], na.rm = TRUE),
        R         = 2000
      )
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Variance = map_dbl(boot, ~ mean(.x$t,   na.rm = TRUE)),
    VarLo    = map_dbl(boot, ~ boot.ci(.x, type = "perc")$percent[4]),
    VarHi    = map_dbl(boot, ~ boot.ci(.x, type = "perc")$percent[5])
  )

tbl_bt_var_boot <- var_bt_boot %>%
  mutate(
    Variance = dollar(Variance, accuracy = 0.01),
    `95% CI` = paste0(
      "$",
      comma(VarLo, accuracy = 1),
      " – ",
      comma(VarHi, accuracy = 1)
    )
  ) %>%
  select(`Building Type` = BLDG_TYPE, N, Variance, `95% CI`)

tbl_bt_var_boot %>%
  kable(booktabs = TRUE, align = c("c","r","r","c")) %>%
  kable_styling(full_width = FALSE, position = "center")

p2_vert <- ggplot(var_bt_boot, aes(x = BLDG_TYPE, y = Variance)) +
  geom_point(color = "darkorange", size = 3) +
  geom_errorbar(aes(ymin = VarLo, ymax = VarHi),
                width = 0.2, color = "darkorange") +
  scale_y_log10() +
  labs(
    title = "Variance ± 95% Bootstrap CI by Building Type (log scale)",
    x     = "Building Type",
    y     = "Variance of exemption, $"
  ) +
  theme_classic(base_size = 13) +
  theme(panel.grid.major.x = element_blank())

print(p2_vert)


quantile_stat <- function(dat, idx) {
  qs <- quantile(dat[idx], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
  return(as.numeric(qs))
}

get_lo <- function(b, i) {
  t <- b$t[, i]
  if (length(unique(t)) == 1) {
    return(unique(t))
  }
  ci <- try(boot.ci(b, type = "perc", index = i), silent = TRUE)
  if (inherits(ci, "try-error")) {
    return(mean(t, na.rm = TRUE))
  }
  ci$percent[4]
}

get_hi <- function(b, i) {
  t <- b$t[, i]
  if (length(unique(t)) == 1) {
    return(unique(t))
  }
  ci <- try(boot.ci(b, type = "perc", index = i), silent = TRUE)
  if (inherits(ci, "try-error")) {
    return(mean(t, na.rm = TRUE))
  }
  ci$percent[5]
}

# 1) Зробимо бутстреп для кожного BLDG_TYPE із CI для квантилів
qt_boot <- df_bt %>%
  group_by(BLDG_TYPE) %>%
  summarise(
    boot_obj = list(
      boot(
        data      = CUREXMPTOT,
        statistic = quantile_stat,
        R         = 2000
      )
    ),
    .groups = "drop"
  ) %>%
  mutate(
    # точкові оцінки
    Q1_mean  = map_dbl(boot_obj, ~ mean(.x$t[,1], na.rm = TRUE)),
    Med_mean = map_dbl(boot_obj, ~ mean(.x$t[,2], na.rm = TRUE)),
    Q3_mean  = map_dbl(boot_obj, ~ mean(.x$t[,3], na.rm = TRUE)),
    
    # персентильні CI через зовнішні функції
    Q1_lo  = map2_dbl(boot_obj, 1, get_lo),
    Q1_hi  = map2_dbl(boot_obj, 1, get_hi),
    Med_lo = map2_dbl(boot_obj, 2, get_lo),
    Med_hi = map2_dbl(boot_obj, 2, get_hi),
    Q3_lo  = map2_dbl(boot_obj, 3, get_lo),
    Q3_hi  = map2_dbl(boot_obj, 3, get_hi)
  )

# 2) Складання таблиці
qt_ci_tbl <- qt_boot %>%
  transmute(
    `Building Type` = BLDG_TYPE,
    `Q1 (25%)`     = sprintf("%0.0f (%0.0f – %0.0f)", Q1_mean, Q1_lo, Q1_hi),
    `Median (50%)` = sprintf("%0.0f (%0.0f – %0.0f)", Med_mean, Med_lo, Med_hi),
    `Q3 (75%)`     = sprintf("%0.0f (%0.0f – %0.0f)", Q3_mean, Q3_lo, Q3_hi)
  )

qt_ci_tbl %>%
  knitr::kable(booktabs = TRUE, align = "lccc") %>%
  kableExtra::kable_styling(full_width = FALSE, position = "center")

qt_plot <- qt_boot %>%
  select(BLDG_TYPE,
         Q1_mean, Med_mean, Q3_mean,
         Q1_lo,   Med_lo,   Q3_lo,
         Q1_hi,   Med_hi,   Q3_hi) %>%
  pivot_longer(
    cols      = c(Q1_mean, Med_mean, Q3_mean),
    names_to  = "quantile",
    values_to = "estimate"
  ) %>%
  mutate(
    lo = case_when(
      quantile == "Q1_mean"  ~ Q1_lo,
      quantile == "Med_mean" ~ Med_lo,
      quantile == "Q3_mean"  ~ Q3_lo
    ),
    hi = case_when(
      quantile == "Q1_mean"  ~ Q1_hi,
      quantile == "Med_mean" ~ Med_hi,
      quantile == "Q3_mean"  ~ Q3_hi
    ),
    quantile = recode(quantile,
                      Q1_mean  = "Q1",
                      Med_mean = "Median",
                      Q3_mean  = "Q3")
  )
n <- length(unique(qt_plot$BLDG_TYPE))
ggplot(qt_plot, aes(x = BLDG_TYPE, y = estimate, color = quantile)) +
  geom_vline(xintercept = seq(1.5, n - 0.5, by = 1),
             colour    = "grey80",
             linetype  = "dashed") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = lo, ymax = hi),
                position = position_dodge(width = 0.5),
                width = 0.2) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(
    title = "Quantiles ± 95% Bootstrap CI by Building Type (log scale)",
    x     = "Building Type",
    y     = "Exemption amount, $ (log10 scale)",
    color = "Quantile"
  ) +
  theme_classic(base_size = 13) +
  theme(panel.grid.major.x = element_blank())

alpha <- 0.05
z     <- qnorm(1 - alpha/2)

all_res_paired <- do.call(rbind, lapply(levels(df_bt$BLDG_TYPE), function(type) {
  dat    <- subset(df_bt, BLDG_TYPE == type)
  tb     <- table(dat$BORO)
  ok     <- names(tb)[tb >= 2]           # boroughs with ≥2 observations
  if (length(ok) < 2) return(NULL)       # nothing to compare
  
  dat_ok <- subset(dat, BORO %in% ok)
  pairs  <- combn(ok, 2)
  
  # prepare output data.frame
  res <- data.frame(
    BLDG_TYPE = type,
    Boro1     = pairs[1, ],
    Boro2     = pairs[2, ],
    estimate  = NA_real_,
    SE        = NA_real_,
    p.raw     = NA_real_,
    CI.low    = NA_real_,
    CI.high   = NA_real_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(ncol(pairs))) {
    g1 <- pairs[1,i]; g2 <- pairs[2,i]
    x1 <- dat_ok$CUREXMPTOT[dat_ok$BORO == g1]
    x2 <- dat_ok$CUREXMPTOT[dat_ok$BORO == g2]
    
    # if unequal lengths, trim to the smaller
    nmin <- min(length(x1), length(x2))
    if (length(x1) != length(x2)) {
      x1 <- x1[1:nmin]
      x2 <- x2[1:nmin]
    }
    
    # paired t-test
    t_res <- t.test(x1, x2, paired = TRUE, conf.level = 1 - alpha)
    est    <- unname(t_res$estimate)       # mean of differences
    ci     <- t_res$conf.int               # c(lower, upper)
    se     <- (ci[2] - ci[1]) / (2 * z)     # derive SE from CI width
    
    # fill results
    res$estimate[i] <- est
    res$SE[i]       <- se
    res$p.raw[i]    <- t_res$p.value
    res$CI.low[i]   <- ci[1]
    res$CI.high[i]  <- ci[2]
  }
  
  # Holm adjustment per building type
  res$p.adj <- p.adjust(res$p.raw, method = "holm")
  res
}))

# order and display
all_res_paired <- all_res_paired[order(all_res_paired$BLDG_TYPE, all_res_paired$p.adj), ]
print(all_res_paired, row.names = FALSE)

sig_res2 <- all_res_paired %>%
  filter(p.adj < 0.05)
sig_res2

sig <- all_res_paired %>%
  filter(p.adj < 0.05)

# create a human-readable label for each contrast
boro_names <- c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")
sig <- sig %>%
  mutate(
    BLDG_TYPE = factor(BLDG_TYPE, levels = sort(unique(BLDG_TYPE))),
    Boro1_name = factor(Boro1, levels = 1:5, labels = boro_names),
    Boro2_name = factor(Boro2, levels = 1:5, labels = boro_names),
    contrast   = paste0(Boro1_name, " vs ", Boro2_name)
  )

# pagination parameters
page      <- 4       # change to 1 for first six, 2 for next six, etc.
page_size <- 6

# compute which types to include
all_types <- levels(sig$BLDG_TYPE)
n_types   <- length(all_types)
start     <- (page - 1) * page_size + 1
end       <- min(page * page_size, n_types)
selected  <- all_types[start:end]

# filter & re‐factor
sig_page <- sig %>%
  filter(BLDG_TYPE %in% selected) %>%
  mutate(BLDG_TYPE = factor(BLDG_TYPE, levels = selected))

if (nrow(sig_page) == 0) {
  stop("No data for building‐types ", paste(selected, collapse = ", "))
}

# now plot
ggplot(sig_page, aes(x = contrast, y = estimate)) +
  geom_pointrange(aes(ymin = CI.low, ymax = CI.high),
                  fatten = 1.2, color = "steelblue") +
  facet_wrap(~ BLDG_TYPE, scales = "free_x", ncol = 3) +
  coord_flip() +
  theme_minimal(base_size = 12) +
  theme(
    panel.border       = element_rect(color = "grey60", fill = NA, size = 0.4),
    panel.spacing      = unit(1, "lines"),
    strip.text         = element_text(face = "bold"),
    axis.text.y        = element_text(size = 9),
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed")
  ) +
  labs(
    x        = NULL,
    y        = "Mean difference (with 95% CI)",
    title    = paste0("Pairwise Borough Differences (building types ", start, "–", end, ")"),
    subtitle = "Only contrasts significant at Holm-adjusted α = 0.05"
  )

sig <- all_res_paired %>%
  filter(p.adj < alpha)

# 1) For each building type: how many significant borough‐pairs?
tbl_by_type <- sig %>%
  group_by(BLDG_TYPE) %>%
  summarise(n_significant = n()) %>%
  arrange(desc(n_significant))

# 2) For each borough: how many times it showed a significant difference?
tbl_by_boro <- sig %>%
  pivot_longer(c(Boro1, Boro2), values_to = "BORO") %>%
  group_by(BORO) %>%
  summarise(n_significant = n()) %>%
  arrange(desc(n_significant))

# Display
tbl_by_type
tbl_by_boro

counts <- all_res_paired %>%
  filter(p.adj < alpha) %>%
  count(Boro1, Boro2, name = "n")

# 2) Mirror each pair so the matrix is full (i→j and j→i)
counts_full <- counts %>%
  bind_rows(rename(counts, Boro1 = Boro2, Boro2 = Boro1)) %>%
  distinct()

# 3) Make sure you include zeroes for pairs that never differed
boro_levels <- sort(unique(c(counts_full$Boro1, counts_full$Boro2)))
counts_full <- counts_full %>%
  complete(Boro1 = boro_levels,
           Boro2 = boro_levels,
           fill     = list(n = 0))

# 4) (Optional) map numeric codes to names
boro_names <- c("1"="Manhattan","2"="Bronx","3"="Brooklyn","4"="Queens","5"="Staten Island")
counts_full <- counts_full %>%
  mutate(
    Boro1 = factor(Boro1, levels = names(boro_names), labels = boro_names),
    Boro2 = factor(Boro2, levels = names(boro_names), labels = boro_names)
  )

# 5) Plot the heatmap
ggplot(counts_full, aes(x = Boro1, y = Boro2, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  coord_fixed() +    # keep squares square
  labs(
    x     = NULL,
    y     = NULL,
    fill  = "Count of\nsignificant\ndifferences",
    title = "How often Borough i differed from Borough j\nacross all building types"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid  = element_blank()
  )


tbl_horizontal <- tbl_by_type %>%
  select(BLDG_TYPE, significant_tests) %>%      # drop total_tests
  pivot_longer(
    cols      = significant_tests,
    names_to  = "Metric",
    values_to = "Count"
  ) %>%
  pivot_wider(
    names_from  = BLDG_TYPE,
    values_from = Count
  ) %>%
  mutate(
    Metric = "Significant tests"
  )

# render as HTML
tbl_horizontal %>%
  kable(
    format     = "html",
    caption    = "Significant Pairwise Test Counts by Building Type",
    table.attr = "class='table table-striped table-hover'"
  ) %>%
  kable_styling(
    full_width        = FALSE,
    position          = "center",
    bootstrap_options = c("striped", "hover", "condensed")
  )


library(ggplot2)
library(tidyverse)
library(RColorBrewer)

# Завантаження даних
ped_data <- read_csv("PED_clean.csv")

# Перетворення першого стовпця у формат символів
ped_data <- ped_data %>%
  mutate(across(1, as.character))

# Фільтрація за періодом 3
ped_data <- ped_data %>%
  filter(PERIOD == 3)

# Групування даних з кількома пільгами
multi_exemptions <- ped_data %>%
  group_by(PARID, YEAR, BORO) %>%
  summarise(n_exemptions = n_distinct(EXMP_CODE), .groups = "drop") %>%
  filter(n_exemptions > 1)

# Розрахунок середнього числа пільг
average_exemptions <- multi_exemptions %>%
  summarise(average = mean(n_exemptions))
print(average_exemptions)

# Побудова графіка динаміки кількості об'єктів з кількома пільгами
plot_data <- multi_exemptions %>%
  group_by(YEAR, BORO) %>%
  summarise(count = n(), .groups = "drop")

plot_data <- plot_data %>%
  mutate(BORO = recode_factor(
    factor(BORO),
    `1` = "Манхеттен",
    `2` = "Бронкс",
    `3` = "Бруклін",
    `4` = "Квінс",
    `5` = "Стейтен-Айленд"
  ))

# Додання т-тесту для перевірки середнього числа пільг
t_test_result <- t.test(multi_exemptions$n_exemptions)
print(t_test_result)

p <- ggplot(plot_data, aes(x = factor(YEAR), y = count, color = factor(BORO), group = BORO)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_text(aes(label = count), vjust = -0.8, size = 3.5) + 
  labs(title = "Динаміка кількості об'єктів з кількома пільгами",
       x = "Рік", y = "Кількість об'єктів",
       color = "Район (BORO)") +
  theme_minimal(base_size = 13) +
  theme(aspect.ratio = 0.6, plot.margin = margin(5, 5, 5, 5, unit = "pt"))

ggsave("exemptions_plot.jpg", plot = p, width = 10, height = 4, dpi = 300)

# Збереження результатів т-тесту у файл
write.csv(data.frame(t_test_result$estimate, t_test_result$p.value), "t_test_results.csv")


# Математичне сподівання та довірчий інтервал (95%) для нього
mean_val <- mean(multi_exemptions$n_exemptions)
sd_val <- sd(multi_exemptions$n_exemptions)
n <- length(multi_exemptions$n_exemptions)
error_margin <- qt(0.975, df = n - 1) * sd_val / sqrt(n)
ci_mean <- c(mean_val - error_margin, mean_val + error_margin)

# Дисперсія та довірчий інтервал (95%) для неї
var_val <- var(multi_exemptions$n_exemptions)
chi2_lower <- qchisq(0.975, df = n - 1)
chi2_upper <- qchisq(0.025, df = n - 1)
ci_variance <- c((n - 1) * var_val / chi2_lower, (n - 1) * var_val / chi2_upper)

# Вивід результатів
cat("Довірчий інтервал для мат. сподівання:\n")
print(ci_mean)

cat("Довірчий інтервал для дисперсії:\n")
print(ci_variance)

# Збереження у файл
ci_results <- data.frame(
  Metric = c("Mean Lower", "Mean Upper", "Variance Lower", "Variance Upper"),
  Value = c(ci_mean[1], ci_mean[2], ci_variance[1], ci_variance[2])
)
write.csv(ci_results, "confidence_intervals.csv", row.names = FALSE)

ci_data <- data.frame(
  parameter = c("Mean", "Variance"),
  estimate = c(mean_val, var_val),
  lower = c(ci_mean[1], ci_variance[1]),
  upper = c(ci_mean[2], ci_variance[2])
)


library(ggplot2)
library(gridExtra)
library(grid)

# Дані
ci_data <- data.frame(
  parameter = c("Mean", "Variance"),
  estimate = c(mean_val, var_val),
  lower = c(ci_mean[1], ci_variance[1]),
  upper = c(ci_mean[2], ci_variance[2])
)

# Графік
p1 <- ggplot(ci_data, aes(y = parameter, x = estimate)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.2, linewidth = 1.2) +
  labs(title = "Довірчі інтервали для середнього значення і дисперсії",
       y = "Параметр", x = "Значення") +
  theme_minimal(base_size = 14)

# Таблиця
table_data <- ci_data %>%
  mutate(across(where(is.numeric), ~ round(., 4)))
table_plot <- tableGrob(table_data, rows = NULL)

# Об'єднуємо
combined <- arrangeGrob(p1, table_plot, ncol = 1, heights = c(3, 1))

# Вивід на екран у RStudio з правильним співвідношенням
grid.newpage()
grid.draw(combined)

# АБО — Збереження в PNG з розтягуванням
png("ci_plot_custom_wide.png", width = 1600, height = 1000, res = 150)
grid.draw(combined)
dev.off()

library(dplyr)
library(tibble)

# Отримуємо унікальні роки та впорядковуємо
years <- sort(unique(multi_exemptions$YEAR))

# Порожній список для результатів
results <- list()

# Параметри тесту
alpha <- 0.05
z_crit <- qnorm(1 - alpha / 2)

# Цикл по парах років
for (i in 1:(length(years) - 1)) {
  year1 <- years[i]
  year2 <- years[i + 1]
  
  # Дані по роках
  x1 <- multi_exemptions %>% filter(YEAR == year1) %>% pull(n_exemptions)
  x2 <- multi_exemptions %>% filter(YEAR == year2) %>% pull(n_exemptions)
  
  n1 <- length(x1)
  n2 <- length(x2)
  m1 <- mean(x1)
  m2 <- mean(x2)
  s1 <- sd(x1)
  s2 <- sd(x2)
  
  theta_hat <- m1 - m2
  se_theta <- sqrt((s1^2 / n1) + (s2^2 / n2))
  T <- theta_hat / se_theta
  p_val <- 2 * (1 - pnorm(abs(T)))
  decision <- ifelse(abs(T) > z_crit, "Відхиляємо H₀", "Не відхиляємо H₀")
  
  results[[i]] <- tibble(
    `Пара років` = paste(year1, "vs", year2),
    `Середнє 1` = round(m1, 4),
    `Середнє 2` = round(m2, 4),
    `Різниця θ̂` = round(theta_hat, 4),
    `SE(θ̂)` = round(se_theta, 6),
    `T` = round(T, 4),
    `p-value` = round(p_val, 6),
    `Висновок` = decision
  )
}

# Об’єднуємо результати в таблицю
wald_results <- bind_rows(results)

# Виводимо таблицю
print(wald_results)

library(tidyverse)
library(openxlsx)
library(scales) # для логорифмування по даним на графікуS
library(cowplot) # для побудови з двома легендами
# library(treemapify) # пакет що наслідує ggplot та розширює його
library(vcd)
library(msm)
library(boot)

# Зчитуємо датасет
df <- read_csv("PED_Clean.csv")


# ---------------------------------------------------------------------------------------------
# Перейменовуємо змінні та приводимо до факторних типів
df <- df %>%
  rename(Exemption_Classification_Codes = EXMP_CODE, 
         Status = STATUS, 
         Building_Class = 'BLDG-CLASS') %>%
  filter(PERIOD == 3) %>%
  filter(Status %in% c("A", "AC", "AI", "AK", "AM", "AR", "AS", "AT")) %>%
  mutate(across(Building_Class, as.character),
         Exemption_Classification_Codes = as.factor(Exemption_Classification_Codes),
         Status = as.factor(Status),
         Building_Class_Grouped = str_sub(Building_Class, 1, 1),
         Building_Class = as.factor(Building_Class),
         Building_Class_Grouped = as.factor(Building_Class_Grouped))      
# ---------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------
# Опис класів будівель
bldg_descriptions <- c(
  "A" = "A - ONE FAMILY DWELLINGS",
  "B" = "B - TWO FAMILY DWELLINGS",
  "C" = "C - WALK UP APARTMENTS",
  "D" = "D - ELEVATOR APARTMENTS",
  "E" = "E - WAREHOUSES",
  "F" = "F - FACTORIES AND INDUSTRIAL BUILDINGS",
  "G" = "G - GARAGES",
  "H" = "H - HOTELS",
  "I" = "I - HOSPITALS AND HEALTH FACILITIES",
  "J" = "J - THEATRES",
  "K" = "K - STORE BUILDINGS",
  "L" = "L - LOFTS",
  "M" = "M - RELIGIOUS FACILITIES",
  "N" = "N - ASYLUMS AND HOMES",
  "O" = "O - OFFICE BUILDINGS",
  "P" = "P - INDOOR PUBLIC ASSEMBLY & CULT. FACILITIES",
  "Q" = "Q - OUTDOOR RECREATIONAL FACILITIES",
  "R" = "R - CONDOMINIUMS",
  "S" = "S - PRIMARILY RES. - MIXED USE",
  "T" = "T - TRANSPORTATION FACILITIES",
  "U" = "U - UTILITY BUREAU PROPERTIES",
  "V" = "V - VACANT LAND",
  "W" = "W - EDUCATIONAL FACILITIES",
  "Y" = "Y - GOVERNMENT/CITY DEPARTMENTS",
  "Z" = "Z - MISC. BUILDING CLASSIFICATIONS"
)
# ---------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------
# Датафрейм з класом будівлі та кількістю відповідних об'єктів
df_counts <- df %>%
  group_by(Building_Class_Grouped) %>%
  summarize(Count = n())

df_counts <- df_counts %>%
  arrange(Count) %>%
  mutate(Building_Class_Grouped = fct_reorder(Building_Class_Grouped, Count, .desc = FALSE)) %>% # Reorder factor levels 
  mutate(BLDG_DESC = recode(Building_Class_Grouped, !!!bldg_descriptions, .default="Unknown"))
# ---------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------
# Датафрейм з класом будівлі, кількістю відповідних об'єктів та згрупованими пільгами
df_with_exemption_codes <- df %>% 
  select(Building_Class_Grouped, Exemption_Classification_Codes) %>%
  mutate(Grouped_Exemption_Classification_Codes = case_when(
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1010 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1019 ~ "Vulnerable",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1021 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1200 ~ "Religion",
    as.numeric(as.character(Exemption_Classification_Codes)) == 1301 ~ "Vulnerable",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1401 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1404 ~ "Med Centers",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1501 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1504 ~ "Charitable",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1505 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1572 ~ "NFP/Public Associations",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1601 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1604 ~ "University/Education",
    as.numeric(as.character(Exemption_Classification_Codes)) == 1605 ~ "Museum",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1606 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1660 ~ "Arts and Culture",
    as.numeric(as.character(Exemption_Classification_Codes)) == 1700 ~ "Private Cemetery",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1840 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1891 ~ "Socio-Cultural significance",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1901 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1961 ~ "Infrastructure",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1963 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1965 ~ "Green Energy",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 1971 & as.numeric(as.character(Exemption_Classification_Codes)) <= 1992 ~ "Assets",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 2120 & as.numeric(as.character(Exemption_Classification_Codes)) <= 2231 ~ "Departments",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 2232 & as.numeric(as.character(Exemption_Classification_Codes)) <= 2234 ~ "Public places",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 2251 & as.numeric(as.character(Exemption_Classification_Codes)) <= 3360 ~ "State and municipal inst.",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 3380 & as.numeric(as.character(Exemption_Classification_Codes)) <= 3800 ~ "On State payroll",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 4500 & as.numeric(as.character(Exemption_Classification_Codes)) <= 4650 ~ "Under Federal Controll",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 5090 & as.numeric(as.character(Exemption_Classification_Codes)) <= 5130 ~ "Federal Authority",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 6110 & as.numeric(as.character(Exemption_Classification_Codes)) <= 6800 ~ "Ports/Power plants",
    as.numeric(as.character(Exemption_Classification_Codes)) >= 7120 & as.numeric(as.character(Exemption_Classification_Codes)) <= 7170 ~ "International organisations",
    TRUE ~ as.character(Exemption_Classification_Codes)
  ),
  Grouped_Exemption_Classification_Codes = as.factor(Grouped_Exemption_Classification_Codes)
  )
# ---------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------
contingency_table <- df_with_exemption_codes %>%
  count(Building_Class_Grouped, Grouped_Exemption_Classification_Codes) %>%
  group_by(Building_Class_Grouped) %>%
  arrange(desc(n), .by_group = TRUE)

# Обчислює загальну кількість об'єктів для кожного класу будівлі
building_class_counts <- contingency_table %>%
  group_by(Building_Class_Grouped) %>%
  summarise(total_n = sum(n)) %>%
  arrange(desc(total_n))

total_by_building_class <- contingency_table %>%
  group_by(Building_Class_Grouped) %>%
  summarise(Total = sum(n))

contingency_table_with_percentage <- contingency_table %>%
  left_join(total_by_building_class, by = "Building_Class_Grouped") %>%
  mutate(Percentage = round((n / Total) * 100, 2))

# Визначає порядок рівнів фактора для осі X
building_class_order <- building_class_counts$Building_Class_Grouped
# ---------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------
# 5. Чи існують класи будівель, які непропорційно часто (або рідко) 
# отримують певну категорію податкових пільг порівняно 
# з загальним розподілом пільг

exemption <- "Vulnerable"
# exemption <- "University/Education"
# exemption <- "Religion"
# exemption <- "Assets"
# exemption <- "Federal Authority"

filtered_data_q5 <- contingency_table_with_percentage %>%
  filter(Grouped_Exemption_Classification_Codes == exemption)

class_p_hat_ci <- filtered_data_q5 %>%
  mutate(
    p_hat = n / Total,
    p_se = sqrt(p_hat * (1 - p_hat) / Total),
    odds_hat = p_hat / (1 - p_hat),
    log_odds_hat = log(odds_hat),
    odds_se = deltamethod(~ (x1 / (1 - x1)), mean = p_hat, cov = p_se^2),
    log_odds_se = deltamethod(~ log(x1 / (1 - x1)), mean = p_hat, cov = p_se^2)
  ) %>% 
  mutate(
    log_a_odds = log_odds_hat - 1.96 * log_odds_se,
    log_b_odds = log_odds_hat + 1.96 * log_odds_se,
    a_odds = exp(log_a_odds),
    b_odds = exp(log_b_odds)
  ) %>% 
  select(
    Building_Class_Grouped,
    Grouped_Exemption_Classification_Codes,
    n,
    p_hat,
    p_se,
    odds_hat,
    odds_se, 
    a_odds,  
    b_odds   
  )

total_objects <- sum(contingency_table_with_percentage$n)

overall_p_hats <- contingency_table_with_percentage %>%
  group_by(Grouped_Exemption_Classification_Codes) %>%
  summarise(Total_n_for_Exemption = sum(n), .groups = 'drop') %>%
  mutate(
    overall_p_hat = Total_n_for_Exemption / total_objects,
    overall_odds_hat = overall_p_hat / (1 - overall_p_hat)
  ) %>%
  select(Grouped_Exemption_Classification_Codes, overall_p_hat, overall_odds_hat)

q5_df <- class_p_hat_ci %>%
  left_join(overall_p_hats, by = "Grouped_Exemption_Classification_Codes")

plot_data <- q5_df %>%
  filter(Grouped_Exemption_Classification_Codes == exemption)

plot_data_selected <- plot_data %>%
  # select(Building_Class_Grouped, n, p_hat, p_se, odds_hat, odds_se, a_odds, b_odds, overall_odds_hat)
  select(Building_Class_Grouped, p_hat, p_se, odds_hat, odds_se, a_odds, b_odds)

print(plot_data_selected, n = Inf)

current_overall_prop <- unique(plot_data$overall_odds_hat )

# write.xlsx(plot_data_selected, file = "Vulnerable_CI.xlsx", rowNames = FALSE)
# write.xlsx(plot_data_selected, file = "University_Education_CI.xlsx", rowNames = FALSE)
# write.xlsx(plot_data_selected, file = "Religion_CI.xlsx", rowNames = FALSE)
# write.xlsx(plot_data_selected, file = "Assets_CI.xlsx", rowNames = FALSE)
# write.xlsx(plot_data_selected, file = "Federal_Authority_CI.xlsx", rowNames = FALSE)


ggplot(plot_data, aes(x = Building_Class_Grouped, y = odds_hat)) +
  geom_point(size = 2, color = "black") + 
  geom_errorbar(aes(ymin = a_odds, ymax = b_odds), width = 0.2, linewidth = 0.7, color = "black") +
  geom_hline(aes(yintercept = current_overall_prop, linetype = "Загальні Odds для пільги"), color = "lightgrey", linewidth = 1) + 
  scale_linetype_manual(name = "Загальні Odds для пільги",
                        values = c("Загальні Odds для пільги" = "dashed"),
                        labels = c(paste(round(current_overall_prop, 5)))) +
  scale_y_log10(labels = comma) + 
  labs(
    title = paste0("Odds пільги '", exemption, "' за класами будівель"),
    x = "Клас будівлі",
    y = paste("Odds об'єктів з пільгою ", exemption, sep="")
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))
# ---------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------
# 5. Нульова гіпотеза: (H0) Шанси отримати певну податкову пільгу 
#    в даному класі будівлі не відрізняються від загальних шансів 
#    отримати цю пільгу по всій сукупності будівель.

#    Альтернативна гіпотеза: (H1) Шанси отримати певну податкову пільгу 
#    в даному класі будівлі відрізняються (тобто більші або менші) 
#    від загальних шансів отримати цю пільгу по всій сукупності будівель.

filtered_data_q5_hipo <- filtered_data_q5 %>%
  mutate(without_exemption = Total - n)

# Шанси для кожної категорії податкових пільг по всіх каласах нерухомості
overall_exemption_stats <- filtered_data_q5_hipo %>%
  group_by(Grouped_Exemption_Classification_Codes) %>%
  summarise(
    overall_n = sum(n, na.rm = TRUE),
    overall_Total = sum(Total, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    overall_p_hat = ifelse(overall_Total == 0, NA_real_, overall_n / overall_Total),
    overall_odds_hat = case_when(
      is.na(overall_p_hat) ~ NA_real_,
      overall_p_hat == 1 ~ Inf,
      overall_p_hat == 0 ~ 0,
      TRUE ~ overall_p_hat / (1 - overall_p_hat)
    ),
    log_overall_odds_hat = case_when(
      overall_odds_hat == Inf ~ Inf,
      overall_odds_hat == 0 ~ -Inf,
      TRUE ~ log(overall_odds_hat)
    )
  )

data_q5_wald_test <- filtered_data_q5_hipo %>%
  left_join(overall_exemption_stats %>%
              select(Grouped_Exemption_Classification_Codes, overall_p_hat, overall_odds_hat, log_overall_odds_hat, overall_n, overall_Total),
            by = "Grouped_Exemption_Classification_Codes") %>%
  mutate(
    p_hat = ifelse(Total == 0, NA_real_, n / Total),
    odds_hat = case_when(
      is.na(p_hat) ~ NA_real_,
      p_hat == 1 ~ Inf,
      p_hat == 0 ~ 0,
      TRUE ~ p_hat / (1 - p_hat)
    ),
    log_odds_hat = case_when(
      is.na(odds_hat) ~ NA_real_,
      odds_hat == Inf ~ Inf,
      odds_hat == 0 ~ -Inf,
      TRUE ~ log(odds_hat)
    )
  ) %>%
  mutate(
    var_log_odds_group = case_when(
      Total == 0 ~ NA_real_,
      p_hat == 0 | p_hat == 1 ~ Inf, 
      TRUE ~ 1 / (Total * p_hat * (1 - p_hat))
    ),
    var_log_odds_overall = case_when(
      overall_Total == 0 ~ NA_real_,
      overall_p_hat == 0 | overall_p_hat == 1 ~ Inf,
      TRUE ~ 1 / (overall_Total * overall_p_hat * (1 - overall_p_hat))
    ),
    log_odds_se = case_when(
      is.na(var_log_odds_group) | is.na(var_log_odds_overall) ~ NA_real_,
      is.infinite(var_log_odds_group) | is.infinite(var_log_odds_overall) ~ Inf, 
      TRUE ~ sqrt(var_log_odds_group + var_log_odds_overall)
    )
  )

q5_wald_test_results <- data_q5_wald_test %>%
  mutate(
    z_stat = case_when(
      is.na(log_odds_hat) | is.na(log_overall_odds_hat) | is.na(log_odds_se) ~ NA_real_,
      log_odds_hat == Inf & log_overall_odds_hat < Inf ~ Inf,
      log_odds_hat == -Inf & log_overall_odds_hat > -Inf ~ -Inf,
      (log_odds_hat == Inf & log_overall_odds_hat == Inf) | (log_odds_hat == -Inf & log_overall_odds_hat == -Inf) ~ 0,
      is.infinite(log_odds_se) ~ (log_odds_hat - log_overall_odds_hat) / 1, 
      log_odds_se == 0 & (log_odds_hat - log_overall_odds_hat) != 0 ~ Inf * sign(log_odds_hat - log_overall_odds_hat),
      log_odds_se == 0 & (log_odds_hat - log_overall_odds_hat) == 0 ~ 0,
      TRUE ~ (log_odds_hat - log_overall_odds_hat) / log_odds_se
    ),
    p_value = case_when(
      is.infinite(z_stat) ~ 0, 
      is.nan(z_stat) ~ NA_real_, 
      is.na(z_stat) ~ NA_real_, 
      is.na(p_hat) | is.na(overall_p_hat) ~ NA_real_,
      TRUE ~ 2 * pnorm(abs(z_stat), lower.tail = FALSE)
    )
  )

adjusted_p_values <- q5_wald_test_results %>%
  filter(!is.na(p_value)) %>%
  mutate(
    p_value_BH = p.adjust(p_value, method = "BH"),
    p_value_bonf = p.adjust(p_value, method = "bonferroni")
  ) %>%
  right_join(q5_wald_test_results %>% select(Building_Class_Grouped, Grouped_Exemption_Classification_Codes, p_value),
             by = c("Building_Class_Grouped", "Grouped_Exemption_Classification_Codes", "p_value"))

final_wald_results <- q5_wald_test_results %>%
  left_join(adjusted_p_values %>%
              select(Building_Class_Grouped, Grouped_Exemption_Classification_Codes, p_value_BH, p_value_bonf),
            by = c("Building_Class_Grouped", "Grouped_Exemption_Classification_Codes")) %>%
  mutate(
    p_value_BH = replace_na(p_value_BH, 1.0),
    p_value_bonf = replace_na(p_value_bonf, 1.0)
  )

results_for_prints <- final_wald_results %>%
  mutate(
    reject = p_value < 0.05,
    reject_bonf = p_value_bonf < 0.05,
    reject_BH = p_value_BH < 0.05
  ) %>%
  select(
    Building_Class_Grouped,
    Grouped_Exemption_Classification_Codes,
    p_value, 
    p_value_bonf, 
    p_value_BH, 
    reject,
    reject_bonf,
    reject_BH
  ) %>%
  arrange(Grouped_Exemption_Classification_Codes, p_value_BH)

# Виведення результатів
print(results_for_prints, n = Inf, width = Inf)

# write.xlsx(results_for_prints, file = "Vulnerable_Wald_test.xlsx", rowNames = FALSE)
# write.xlsx(results_for_prints, file = "University_Education.xlsx", rowNames = FALSE)
# write.xlsx(results_for_prints, file = "Religion.xlsx" , rowNames = FALSE)
# write.xlsx(results_for_prints, file = "Assets.xlsx", rowNames = FALSE)
# write.xlsx(results_for_prints, file = "Federal Authority.xlsx", rowNames = FALSE)
# ---------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(scales)

status_period3 <- PED_Clean %>%
  filter(!is.na(PSTATUS)) %>%
  mutate(PSTATUS_TYPE = substr(PSTATUS, 1, 1),
         PSTATUS_LABEL = case_when(
           PSTATUS_TYPE == "A" ~ "Approved",
           PSTATUS_TYPE == "P" ~ "Pending",
           TRUE ~ "Denied"
         )) %>%
  mutate(CREATION_DATE = as.Date(`Create date`, format = "%m/%d/%Y")) %>%
  mutate(CREATION_YEAR = as.numeric(format(CREATION_DATE, "%Y"))) %>%
  filter(!is.na(CREATION_YEAR))  


counts <- status_period3 %>%
  count(PSTATUS_LABEL)


new_labels <- paste0(counts$PSTATUS_LABEL, "\n(n = ", counts$n, ")")

levels_order <- levels(factor(status_period3$PSTATUS_LABEL))


p <- ggplot(status_period3, aes(x = PSTATUS_LABEL, y = CREATION_YEAR, fill = PSTATUS_LABEL)) +
  geom_violin(trim = FALSE, color = "black", alpha = 0.7) +
  geom_boxplot(width = 0.1, outlier.size = 0.8, color = "black", fill = "white") +
  scale_y_continuous(
    breaks = seq(min(status_period3$CREATION_YEAR, na.rm = TRUE),
                 max(status_period3$CREATION_YEAR, na.rm = TRUE),
                 by = 5),
    minor_breaks = NULL
  ) +
  scale_x_discrete(labels = new_labels) + 
  scale_fill_manual(
    name = "Status",
    values = c(
      "Approved" = "#2ca25f",
      "Pending" = "#3182bd",
      "Denied" = "#e31a1c"
    )
  ) +
  labs(
    title = "Розподіл років створення заявок за статусами",
    x = "Статус заявки (кількість заявок)",
    y = "Рік створення заявки"
  ) +
  theme_minimal()

print(p)

#==============================================================
  library(dplyr)
library(ggplot2)


status_year_summary <- status_period3 %>%
  group_by(CREATION_YEAR, PSTATUS_LABEL) %>%
  summarise(n = n(), .groups = "drop")


ggplot(status_year_summary, aes(x = CREATION_YEAR, y = n, color = PSTATUS_LABEL)) +
  geom_line(size = 1, alpha = 0.6) +  
  geom_smooth(method = "lm", se = TRUE, aes(group = PSTATUS_LABEL),
              linetype = "dashed", size = 0.7, alpha = 0.4,
              show.legend = FALSE) +  
  scale_color_manual(values = c(
    "Approved" = "#238b45",
    "Pending" = "#08519c",
    "Denied" = "#a50f15"
  )) +
  labs(
    title = "Динаміка заявок за роками та статусами",
    x = "Рік створення заявки",
    y = "Кількість заявок",
    color = "Статус"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 13),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )
#===================================================
  library(dplyr)
library(ggplot2)


status_by_year <- PED_Clean %>%
  filter(!is.na(PSTATUS)) %>%
  mutate(PSTATUS_TYPE = substr(PSTATUS, 1, 1),
         PSTATUS_LABEL = case_when(
           PSTATUS_TYPE == "A" ~ "Approved",
           PSTATUS_TYPE == "P" ~ "Pending",
           TRUE ~ "Denied"
         ),
         CREATION_DATE = as.Date(`Create date`, format = "%m/%d/%Y"),
         CREATION_YEAR = as.numeric(format(CREATION_DATE, "%Y"))) %>%
  filter(!is.na(CREATION_YEAR)) %>%
  group_by(PSTATUS_LABEL, CREATION_YEAR) %>%
  summarise(count = n(), .groups = "drop")


ci_data <- status_by_year %>%
  group_by(PSTATUS_LABEL) %>%
  mutate(mean_count = mean(count),
         sd_count = sd(count),
         n = n(),
         se = sd_count / sqrt(n),
         ci_upper = mean_count + qt(0.975, df = n - 1) * se,
         ci_lower = mean_count - qt(0.975, df = n - 1) * se)


ggplot(ci_data, aes(x = CREATION_YEAR, y = count, color = PSTATUS_LABEL, fill = PSTATUS_LABEL)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  scale_color_manual(values = c(
    "Approved" = "#2ca25f",
    "Pending" = "#3182bd",
    "Denied" = "#e31a1c"
  )) +
  scale_fill_manual(values = c(
    "Approved" = "#2ca25f",
    "Pending" = "#3182bd",
    "Denied" = "#e31a1c"
  )) +
  labs(
    title = "Кількість заявок за статусами по роках з довірчими інтервалами",
    x = "Рік створення заявки",
    y = "Кількість заявок",
    color = "Статус",
    fill = "Статус"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )

#==========================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(Hmisc)


status_stats <- status_by_year %>%
  group_by(PSTATUS_LABEL) %>%
  summarise(
    mean_count = mean(count),
    median_count = median(count),
    sd_count = sd(count),
    min_count = min(count),
    max_count = max(count),
    n = n(),
    se = sd_count / sqrt(n),
    ci_low = mean_count - qt(0.975, df = n - 1) * se,
    ci_high = mean_count + qt(0.975, df = n - 1) * se,
    .groups = "drop"
  )
View(status_stats)


#=====================================================
  library(ggplot2)
ggplot(status_stats, aes(x = PSTATUS_LABEL, y = mean_count)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), 
                width = 0.2, color = "black", linewidth = 1) +
  labs(
    title = "Середня кількість заявок на рік за статусами",
    x = "Статус",
    y = "Середня кількість заявок",
    caption = "З довірчими інтервалами 95%"
  ) +
  theme_minimal(base_size = 14)

