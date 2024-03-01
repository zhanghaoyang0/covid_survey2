#====================================================================
# Packages and functions
#====================================================================
libs = c('dplyr', 'openxlsx', 'stringr', 'ggplot2', 'ggsci', 'ggpubr', 'scales')
lapply(libs, require, character.only = TRUE)
options(stringsAsFactors=F)

# filter data to study period
filter_period <- function(df, nweek = 2) {
  out <- df %>%
    filter(DT >= (adjust_day - nweek * 7) & DT < (adjust_day + nweek * 7)) %>%
    mutate(policy = ifelse(DT >= adjust_day, "After", "Before")) %>%
    mutate(policy = factor(policy, levels = c("Before", "After")))
  return(out)
}

# get items with matched pattern
# e.g, get('a', c('a1', 'a', 'c'))
get = function(key, vector, exact=F, v=F){
    if (exact==T){out =vector[grep(key, vector, invert=v)]}
    else {out = vector[grep(toupper(key), toupper(vector), invert=v)]}
    return(out)
}

# convert character vector to numeric 
# e.g, df = df%>%mutate_if(is_numeric,as.numeric)
is_numeric <- function(x) {
  !any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)
}

#====================================================================
# load data and clean
#====================================================================
## data
datas <- load('data/data.rdata')
datas
# "staff"   "outpat"  "inpat"   "weather"

# study period
days = seq(as.Date('2022/11/01'), by = "day", length.out = 61)
adjust_day = as.Date('2022-12-15')

# data clean 
outpat_respir <- outpat %>% filter(DIS %in% c("Pneumonia", "Bronchitis", "URTI", "Other respiratory"))
inpat_respir <- inpat %>% filter(DIS %in% c("Pneumonia", "Bronchitis", "URTI", "Other respiratory"))
outpat1 <- filter_period(outpat)
inpat1 <- filter_period(inpat)
outpat_respir1 <- filter_period(outpat_respir)
inpat_respir1 <- filter_period(inpat_respir)

#====================================================================
# characteristics
#====================================================================
# Define functions:
get_ttest_stat <- function(n1, n2, prefix = "", pair = F) { # input is two vectors
  mean1 <- sprintf("%.2f ± %.2f", mean(n1), sd(n1))
  mean2 <- sprintf("%.2f ± %.2f", mean(n2), sd(n2))
  if (all(sd(n1) == 0, sd(n2) == 0)) {
    test <- list()
    test$statistic <- NA
    test$p.value <- NA
  } else {
    test <- t.test(n1, n2, paired = pair)
  }
  stat <- c(mean1, mean2, test$statistic, test$p.value)
  names(stat) <- paste0(prefix, c("_mean1", "_mean2", "_t", "_p"))
  return(stat)
}

des_popChara <- function(df) {
  out <- c()
  for (nweek in c(-2:1, 9)) { # 9 mean full range
    if (nweek == 9) {
      start <- adjust_day - 2 * 7
      end <- adjust_day + (1 + 1) * 7
    } else {
      start <- adjust_day + nweek * 7
      end <- adjust_day + (nweek + 1) * 7
    }
    sub <- df %>% filter(DT >= start & DT < end)
    n <- nrow(sub)
    range <- paste0(start, " to ", end - 1)
    age <- sprintf("%.2f ± %.2f", mean(sub$age), sd(sub$age))
    n_male <- table(sub$SEX)[2]
    n <- sprintf("%.0f (%.2f%%)", n, 100 * n_male / n)
    out <- c(out, range, n, age)
  }
  out <- data.frame(matrix(out, ncol = 3, byrow = T))
  names(out) <- c("range", "n(male%)", "age")
  return(out)
}

compare_sex <- function(df) {
  chi <- chisq.test(df$SEX, df$policy)
  print(sprintf("chisquare test for sex: chi = %.2f, p = %.2f", chi$statistic, chi$p.value))
}

compare_age <- function(df, test_range) {
  start <- test_range[1]
  end <- test_range[2]
  x1 <- df %>%
    filter(DT < adjust_day) %>%
    select(age)
  x2 <- df %>%
    filter(DT >= start & DT < end) %>%
    select(age)
  stat <- get_ttest_stat(unlist(x1), unlist(x2))
  print(sprintf("t test for age:"))
  print(stat)
}

des_popChara <- function(df) {
  out <- c()
  for (nweek in c(-2:1, 9)) { # 9 mean full range
    if (nweek == 9) {
      start <- adjust_day - 2 * 7
      end <- adjust_day + (1 + 1) * 7
    } else {
      start <- adjust_day + nweek * 7
      end <- adjust_day + (nweek + 1) * 7
    }
    sub <- df %>% filter(DT >= start & DT < end)
    n <- nrow(sub)
    range <- paste0(start, " to ", end - 1)
    age <- sprintf("%.2f ± %.2f", mean(sub$age), sd(sub$age))
    n_male <- table(sub$SEX)[2]
    n <- sprintf("%.0f (%.2f%%)", n, 100 * n_male / n)
    out <- c(out, range, n, age)
  }
  out <- data.frame(matrix(out, ncol = 3, byrow = T))
  names(out) <- c("range", "n(male%)", "age")
  return(out)
}


# Description:
## age and sex of outpatient
des_popChara(outpat)
#                      range       n(male%)         age
# 1 2022-12-01 to 2022-12-07 21904 (56.25%) 5.38 ± 3.74
# 2 2022-12-08 to 2022-12-14 19174 (54.71%) 5.68 ± 3.87
# 3 2022-12-15 to 2022-12-21 13817 (55.97%) 5.13 ± 4.24
# 4 2022-12-22 to 2022-12-28 13700 (55.74%) 4.04 ± 3.94
# 5 2022-12-01 to 2022-12-28 68595 (55.66%) 5.15 ± 3.96

## age and sex of inpatient
des_popChara(inpat)
#                      range      n(male%)         age
# 1 2022-12-01 to 2022-12-07  463 (55.29%) 5.36 ± 4.01
# 2 2022-12-08 to 2022-12-14  393 (52.67%) 5.19 ± 3.73
# 3 2022-12-15 to 2022-12-21  238 (60.08%) 4.49 ± 4.49
# 4 2022-12-22 to 2022-12-28  169 (57.99%) 3.14 ± 4.25
# 5 2022-12-01 to 2022-12-28 1263 (55.74%) 4.85 ± 4.12

## age and sex of respiratory outpatient
des_popChara(outpat_respir)
#                      range       n(male%)         age
# 1 2022-12-01 to 2022-12-07  7935 (55.17%) 4.95 ± 3.14
# 2 2022-12-08 to 2022-12-14  7821 (52.93%) 5.54 ± 3.49
# 3 2022-12-15 to 2022-12-21  7824 (55.00%) 4.79 ± 4.07
# 4 2022-12-22 to 2022-12-28  8226 (55.11%) 3.89 ± 3.86
# 5 2022-12-01 to 2022-12-28 31806 (54.56%) 4.78 ± 3.71

## age and sex of respiratory inpatient
des_popChara(inpat_respir)
#                      range     n(male%)         age
# 1 2022-12-01 to 2022-12-07 203 (51.23%) 4.79 ± 3.00
# 2 2022-12-08 to 2022-12-14 190 (51.05%) 4.94 ± 2.89
# 3 2022-12-15 to 2022-12-21 114 (64.04%) 3.89 ± 3.83
# 4 2022-12-22 to 2022-12-28  96 (54.17%) 2.30 ± 3.35
# 5 2022-12-01 to 2022-12-28 603 (54.06%) 4.27 ± 3.32

compare_sex(outpat1)
# chisquare test for sex: chi = 0.66, p = 0.42
compare_sex(inpat1)
# chisquare test for sex: chi = 2.73, p = 0.10
compare_sex(outpat_respir1)
# chisquare test for sex: chi = 3.11, p = 0.08
compare_sex(inpat_respir1)
# chisquare test for sex: chi = 3.54, p = 0.06

compare_age(outpat1, test_range = c(as.Date("2022-12-15"), as.Date("2022-12-29")))
# "t test for age:"
# _mean1                  _mean2                      _t                     _p
# "5.52 ± 3.80"           "4.59 ± 4.13"      "29.7490660244621"         "5.86064473240061e-193"
compare_age(inpat1, test_range = c(as.Date("2022-12-15"), as.Date("2022-12-29")))
# "t test for age:"
# _mean1                  _mean2                      _t                     _p
# "5.28 ± 3.88"         "3.93 ± 4.43"    "5.28227466933176"         "1.6975197048346e-07"
compare_age(outpat_respir1, test_range = c(as.Date("2022-12-15"), as.Date("2022-12-29")))
# "t test for age:"
# _mean1                  _mean2                      _t                     _p
# "5.25 ± 3.33"           "4.33 ± 3.99"      "22.2462338418588"         "8.72270663548571e-109"
compare_age(inpat_respir1, test_range = c(as.Date("2022-12-15"), as.Date("2022-12-29")))
# "t test for age:"
# _mean1                  _mean2                      _t                     _p
# "4.86 ± 2.94"          "3.16 ± 3.69"     "5.76030352731877"         "1.82849199890289e-08"


#====================================================================
# Time series of hospital activity
#====================================================================
# Daily number of patient visit:
get_nvisit_bygroup <- function(df, date_col, group_col, dates, groups) {
  out <- c()
  for (day in dates) {
    sub <- df[df[, date_col] == day, group_col]
    for (group in groups) {
      if (group == "All") {
        num <- length(sub)
      } else if (group == "All COVID") {
        num <- sum(sub %in% c("posi", "contact_posi"))
        group <- "All"
      } else if (group == "All respiratory") {
        num <- sum(sub %in% c("Pneumonia", "Bronchitis", "URTI", "Other respiratory"))
        group <- "All"
      } else if (group == "Other") {
        num <- sum(!sub %in% groups)
      } else if (group == "Other respiratory") {
        num <- sum(sub %in% c("Other respiratory"))
        group <- "Other"
      } else if (group == "COVID-19 positive") {
        num <- sum(sub == "posi")
      } else if (group == "COVID-19 contact history") {
        num <- sum(sub == "contact_posi")
      } else {
        num <- sum(sub == group)
      }
      out <- c(out, day, group, num)
    }
  }
  nvisit <- data.frame(matrix(out, ncol = 3, byrow = T)) %>%
    rename(DT = X1, group = X2, num = X3) %>%
    mutate_if(is_numeric, as.numeric) %>%
    mutate(DT = as.Date(DT, origin = "1970-01-01"))
  return(nvisit)
}

sort(table(outpat$DPT_NAME), decreasing = T)
sort(table(inpat$DPT_NAME), decreasing = T)

# nvist of patient
groups1 <- c("All", "Other", "Emergency", "Respiratory / Infectious")
groups2 <- c("All COVID", "COVID-19 positive", "COVID-19 contact history")
groups3 <- c("All", "Other", "Respiratory / Infectious")
groups4 <- c("All respiratory", "Pneumonia", "Bronchitis", "URTI", "Other respiratory")

nvisit_outpat <- get_nvisit_bygroup(outpat, "DT", "DPT_NAME", days, groups1)
nvisit_outpat[nvisit_outpat == 0] <- NA # r/i dpt not open
nposi_outpat <- get_nvisit_bygroup(outpat, "DT", "epi", days, groups2)
nvisit_inpat <- get_nvisit_bygroup(inpat, "DT", "DPT_NAME", days, groups3)
nres_outpat <- get_nvisit_bygroup(outpat, "DT", "DIS", days, groups4)
nres_inpat <- get_nvisit_bygroup(inpat, "DT", "DIS", days, groups4)


# Daily number of healthcare provider on covid leave:
out <- c()
for (day in days) {
  sub <- staff %>% filter(start <= day & end >= day)
  for (group in c("All", "Doctor", "Nurse", "Technician", "Other")) {
    if (group == "All") {
      n <- nrow(sub)
    } else {
      n <- sum(sub$group == group)
    }
    out <- c(out, day, group, n)
  }
}
ncovid_staff <- data.frame(matrix(out, ncol = 3, byrow = T)) %>%
  rename(DT = X1, group = X2, num = X3) %>%
  mutate_if(is_numeric, as.numeric) %>%
  mutate(DT = as.Date(DT, origin = "1970-01-01"))


# Reshape:

nvisit_outpat1 <- reshape(nvisit_outpat, idvar = "DT", timevar = "group", direction = "wide")
nposi_outpat1 <- reshape(nposi_outpat, idvar = "DT", timevar = "group", direction = "wide")
nvisit_inpat1 <- reshape(nvisit_inpat, idvar = "DT", timevar = "group", direction = "wide")
ncovid_staff1 <- reshape(ncovid_staff, idvar = "DT", timevar = "group", direction = "wide")

# Correlation between number of COVID-patient and COVID-healthcare:

t1 <- ncovid_staff1%>%filter(DT>=as.Date('2022-12-15')&DT<as.Date('2022-12-29'))%>%pull(num.All)
t2 <- nposi_outpat1%>%filter(DT>=as.Date('2022-12-15')&DT<as.Date('2022-12-29'))%>%pull(num.All)
cor.test(t1, t2)
# Pearson's product-moment correlation
# data:  t1 and t2
# t = 2.5486, df = 12, p-value = 0.02553
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.09048147 0.85450950
# sample estimates:
#       cor
# 0.5926111


## Time series of number patient visit and healthcare provider on COVID leave:
plot_nvist <- function(
    df, groups, ylab = "Number", xlab = "Date", title = "", lab = "",
    legend_pos, legend_col = 1, re_level = F, y_inflat = 1, x_text = adjust_day, hjust_x = 0.5, add_period = F) {
  df_p <- df %>% filter(group %in% groups)
  if (re_level == T) {
    df_p$group <- factor(df_p$group, levels = groups)
  } # level group as groups
  ymax <- ceiling(max(df_p$num, na.rm = T) / 100) * y_inflat * 100
  df_text <- df_p %>%
    filter(DT == as.Date(x_text)) %>%
    filter(num == max(num)) %>%
    mutate(num = ymax * 0.9)
  df_text <- df_text[1, ]
  days1 <- seq(as.Date("2022-11-03"), as.Date("2022-12-29"), by = "1 week")
  p <- ggplot(df_p, aes(x = DT, y = num, group = group)) +
    geom_point(aes(color = group)) +
    geom_line(aes(color = group)) +
    geom_vline(xintercept = as.Date("2022-12-15"), linetype = "dashed", color = "gray", size = 1) +
    ylim(0, ymax) +
    scale_x_date(breaks = days1, date_labels = "%m-%d") +
    geom_text(data = df_text, label = " Policy \n adjustment", vjust = 0.5, hjust = 0.3, size = 3.5) +
    ylab(ylab) +
    xlab(xlab) +
    theme_bw() +
    ggtitle(title) +
    labs(color = lab) +
    theme(
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = hjust_x, color = "black"),
      axis.title.y = element_text(size = 10), panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), # remove grid
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      legend.title = element_text(size = 10, hjust = 0), legend.position = c(legend_pos[1], legend_pos[2])
    ) +
    guides(color = guide_legend(ncol = legend_col)) + # legend row
    scale_color_manual(values = pal_npg("nrc")(5))
  if (add_period == T) {
    x_text2 <- "2022-12-10"
    df_text2 <- df_p %>%
      filter(DT == as.Date(x_text2)) %>%
      filter(num == max(num)) %>%
      mutate(num = ymax * 0.7)
    df_text2 <- df_text2[1, ]
    p <- p +
      geom_vline(xintercept = as.Date("2022-12-01"), linetype = "dashed", color = "gray", size = 1) +
      geom_vline(xintercept = as.Date("2022-12-29"), linetype = "dashed", color = "gray", size = 1) +
      geom_text(data = df_text2, label = " Before              After", vjust = 0.5, hjust = 0.3, size = 4.5)
  }
  return(p)
}

title1 <- "COVID-19-related patient visit"
title2 <- "COVID-19-related healthcare provider"
title3 <- "Outpatient visit"
title4 <- "Inpatient visit"
title5 <- "Respiratory outpatient visit"
title6 <- "Respiratory inpatient visit"

group1 <- c("All", "COVID-19 positive", "COVID-19 contact history")
group2 <- c("All", "Doctor", "Nurse", "Technician", "Other")
group3 <- c("All", "Respiratory / Infectious", "Emergency", "Other")
group4 <- c("All", "Respiratory / Infectious", "Other")
group5 <- c("All", "Pneumonia", "Bronchitis", "URTI", "Other") # the other is rename from 'other respiratory'

p1 <- plot_nvist(nposi_outpat, group1,
  x_text = "2022-12-14", lab = "COVID-19 status",
  legend_pos = c(0.23, 0.82), y_inflat = 1.4, title = title1, add_period = T
)
p2 <- plot_nvist(ncovid_staff, group2,
  x_text = "2022-12-14", lab = "Profession",
  legend_pos = c(0.13, 0.74), re_level = T, y_inflat = 1.7, title = title2, add_period = T
)
p3 <- plot_nvist(nvisit_outpat, group3,
  x_text = "2022-12-14", lab = "Attending department",
  legend_pos = c(0.2, 0.78), re_level = T, y_inflat = 2, title = title3, add_period = T
)
p4 <- plot_nvist(nvisit_inpat, group4,
  x_text = "2022-12-14", lab = "Attending department",
  legend_pos = c(0.2, 0.83), re_level = T, y_inflat = 1.65, title = title4, add_period = T
)
p5 <- plot_nvist(nres_outpat, group5,
  x_text = "2022-12-14", lab = "Diagnosis",
  legend_pos = c(0.14, 0.74), re_level = T, y_inflat = 2.3, title = title5, add_period = T
)
p6 <- plot_nvist(nres_inpat, group5,
  x_text = "2022-12-14", lab = "Diagnosis",
  legend_pos = c(0.14, 0.74), re_level = T, y_inflat = 1, title = title6, add_period = T
)
p <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3, common.legend = F, align = "hv", hjust = 0.1, vjust = 0.1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

# save
png("./plot/ts_nvist.png", height = 1800, width = 1600, res = 150)
print(p)
dev.off()


## Number of patient visit, predicted (assuming the policyadjustment is not implemented) VS actual:
lm_predict_nvist <- function(df, group) {
  df1 <- filter_period(df) %>%
    filter(group %in% UQ(group)) %>%
    mutate(holiday = ifelse(weekdays(DT) %in% c("Saturday", "Sunday"), 1, 0)) %>%
    merge(weather %>% select(temp_ave, humi_ave, DT), by = "DT")
  df2 <- df1 %>% filter(policy == "Before")
  df3 <- df1 %>% filter(policy == "After")
  reg <- lm(num ~ temp_ave + humi_ave + holiday, df2)
  df3 <- df3 %>% mutate(num = predict(reg, newdata = df3))
  sub1 <- df1 %>%
    select(DT, num) %>%
    mutate(group = "Actual")
  sub2 <- df3 %>%
    select(DT, num) %>%
    mutate(group = "Predicted")
  df_p <- rbind(sub1, sub2)
  df_p <- rbind(df_p, c("2022-12-29", NA, "Actual"), c("2022-12-29", NA, "Predicted")) # empty row for plot
  df_p$num <- as.numeric(df_p$num)
  return(df_p)
}

title1 <- "Outpatient visit"
title2 <- "Inpatient visit"
title3 <- "Respiratory outpatient visit"
title4 <- "Respiratory inpatient visit"

nvisit_outpat1 <- lm_predict_nvist(nvisit_outpat, "All")
nvisit_inpat1 <- lm_predict_nvist(nvisit_inpat, "All")
nres_outpat1 <- lm_predict_nvist(nres_outpat, "All")
nres_inpat1 <- lm_predict_nvist(nres_inpat, "All")

p1 <- plot_nvist(nvisit_outpat1, c("Actual", "Predicted"), lab = "Type", legend_pos = c(0.15, 0.76), y_inflat = 2.4, x_text = "2022-12-14", title = title1)
p2 <- plot_nvist(nvisit_inpat1, c("Actual", "Predicted"), lab = "Type", legend_pos = c(0.15, 0.76), y_inflat = 1.5, x_text = "2022-12-14", title = title2)
p3 <- plot_nvist(nres_outpat1, c("Actual", "Predicted"), lab = "Type", legend_pos = c(0.15, 0.76), y_inflat = 2.4, x_text = "2022-12-14", title = title3)
p4 <- plot_nvist(nres_inpat1, c("Actual", "Predicted"), lab = "Type", legend_pos = c(0.15, 0.76), y_inflat = 0.8, x_text = "2022-12-14", title = title4)

p <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, common.legend = F, align = "hv", hjust = 0.1, vjust = 0.1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

png("./plot/ts_prednvist.png", height = 800, width = 1300, res = 150)
print(p)
dev.off()

#====================================================================
# Impact on healthcare providers
#====================================================================
cal_workload <- function(df, col) {
  n_staff <- c()
  n_pat <- c()
  for (day in unique(df$DT)) {
    df1 <- df %>% filter(DT == day)
    tab <- table(df1[, col])
    n_staff <- c(n_staff, length(tab))
    n_pat <- c(n_pat, sum(tab))
  }
  res <- data.frame(DT = unique(df$DT), n_staff = n_staff, n_pat = n_pat) %>%
    mutate(n_workload = n_pat / n_staff, policy = ifelse(DT >= adjust_day, "After", "Before")) %>%
    arrange(DT)
  return(res)
}

workload <- data.frame()
for (dpt in c("All", "Respiratory", "Non-respiratory")) {
  if (dpt == "All") {
    outpat2 <- outpat1
    inpat2 <- inpat1
  } else if (dpt == "Respiratory") {
    outpat2 <- outpat1 %>% filter(DIS %in% c("Pneumonia", "Bronchitis", "URTI", "Other respiratory"))
    inpat2 <- inpat1 %>% filter(DIS %in% c("Pneumonia", "Bronchitis", "URTI", "Other respiratory"))
  } else if (dpt == "Non-respiratory") {
    outpat2 <- outpat1 %>% filter(!DIS %in% c("Pneumonia", "Bronchitis", "URTI", "Other respiratory"))
    inpat2 <- inpat1 %>% filter(!DIS %in% c("Pneumonia", "Bronchitis", "URTI", "Other respiratory"))
  }
  x1 <- cal_workload(outpat2, "DOC_NAME")
  x2 <- cal_workload(inpat2, "HPHY_NAME")
  x3 <- cal_workload(inpat2, "PRIMARY_NUR")
  if (nrow(x1) > 0) {
    sub1 <- data.frame(DT = x1$DT, group = dpt, num = x1$n_workload, type = "outpatient")
  }
  if (nrow(x2) > 0) {
    sub2 <- data.frame(DT = x2$DT, group = dpt, num = x2$n_workload, type = "inpatient_doc")
  }
  if (nrow(x3) > 0) {
    sub3 <- data.frame(DT = x3$DT, group = dpt, num = x3$n_workload, type = "inpatient_nur")
  }
  sub <- as.data.frame(rbind(sub1, sub2, sub3)) %>% mutate(DT = as.Date(DT))
  workload <- as.data.frame(rbind(workload, sub))
}

workload_p1 <- workload %>% filter(type == "outpatient")
workload_p2 <- workload %>% filter(type == "inpatient_doc")
workload_p3 <- workload %>% filter(type == "inpatient_nur")
title1 <- "Outpatients treated per doctor"
title2 <- "Inpatients treated per doctor"
title3 <- "Inpatients treated per nurse"

p1 <- plot_nvist(workload_p1, unique(workload_p1$group), legend_pos = c(0.22, 0.78), lab = "Diagnosis", re_level = T, x_text = "2022-12-14", y_inflat = 0.8, title = title1, hjust_x = 0.6)
p2 <- plot_nvist(workload_p2, unique(workload_p2$group), legend_pos = c(0.22, 0.78), lab = "Diagnosis", re_level = T, x_text = "2022-12-14", y_inflat = 0.1, title = title2, hjust_x = 0.6)
p3 <- plot_nvist(workload_p3, unique(workload_p3$group), legend_pos = c(0.22, 0.78), lab = "Diagnosis", re_level = T, x_text = "2022-12-14", y_inflat = 0.15, title = title3, hjust_x = 0.6)

p <- ggarrange(p1, p2, p3, ncol = 3, nrow = 1, common.legend = F, align = "hv", hjust = 0.1, vjust = 0.1) +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))


png("./plot/ts_workload.png", height = 600, width = 1800, res = 150)
print(p)
dev.off()


# Compare workload:
compare_n <- function(df, prefixCol = "NA") {
  df <- filter_period(df) %>%
    merge(weather %>% select(DT, temp_ave, humi_ave), by = "DT") %>%
    mutate(holiday = ifelse(weekdays(DT) %in% c("Saturday", "Sunday"), 1, 0)) # add policy and weather, holiday
  out <- c()
  for (i in unique(df$group)) {
    sub <- df %>% filter(group == i)
    mod <- lm(num ~ policy + temp_ave + humi_ave + holiday, data = sub)
    coef <- summary(mod)$coefficients
    coef <- coef[2, c(1, 2, 4)]
    coef1 <- sprintf("%.2f ± %.2f", coef[1], coef[2])
    coef2 <- sprintf("%.2f", coef[3])
    temp1 <- sub %>%
      filter(policy == "Before") %>%
      pull(num)
    temp2 <- sub %>%
      filter(policy == "After") %>%
      pull(num)
    mean1 <- sprintf("%.2f ± %.2f", mean(temp1), sd(temp1))
    mean2 <- sprintf("%.2f ± %.2f", mean(temp2), sd(temp2))
    out <- c(out, prefixCol, i, mean1, mean2, coef1, coef2)
  }
  res <- data.frame(matrix(out, ncol = 6, byrow = T))
  names(res) <- c("prefix", "group", "mean_before", "mean_after", "beta", "p")
  return(res)
}

## compare nvist and workload
compare <- data.frame()
#  nvist
compare <- rbind(
  compare,
  compare_n(nvisit_outpat %>% filter(group == "All"), "outpat_nvist"),
  compare_n(nvisit_inpat %>% filter(group == "All"), "inpat_nvist"),
  compare_n(nres_outpat %>% filter(group == "All"), "outpat_nresvist"),
  compare_n(nres_inpat %>% filter(group == "All"), "inpat_nresvist")
)
# workload
for (type in unique(workload$type)) {
  sub <- workload %>% filter(type == UQ(type))
  compare <- rbind(compare, compare_n(sub, type))
}
compare
#         prefix           group      mean_before       mean_after              beta    p
# 1     outpat_nvist             All 2934.14 ± 398.21 1965.50 ± 106.51  -844.38 ± 102.36 0.00
# 2      inpat_nvist             All     61.14 ± 8.36    29.07 ± 14.27     -33.78 ± 5.86 0.00
# 3  outpat_nresvist             All  1125.43 ± 88.83 1146.43 ± 133.63     41.07 ± 56.61 0.48
# 4   inpat_nresvist             All     28.07 ± 3.52     15.00 ± 8.55     -13.11 ± 3.26 0.00
# 5       outpatient             All     26.40 ± 5.92     23.63 ± 5.52      -3.11 ± 2.29 0.19
# 6       outpatient     Respiratory     17.34 ± 3.27     21.24 ± 5.18       2.99 ± 2.03 0.15
# 7       outpatient Non-respiratory     18.68 ± 3.90     11.73 ± 2.20     -6.81 ± 0.92 0.00
# 8    inpatient_doc             All      2.17 ± 0.21      1.95 ± 0.47    -0.17 ± 0.18 0.36
# 9    inpatient_doc     Respiratory      2.16 ± 0.35      2.23 ± 0.86       0.15 ± 0.32 0.66
# 10   inpatient_doc Non-respiratory      1.67 ± 0.17      1.37 ± 0.34     -0.26 ± 0.13 0.07
# 11   inpatient_nur             All      3.30 ± 0.40      3.09 ± 1.10     -0.52 ± 0.39 0.20
# 12   inpatient_nur     Respiratory      2.74 ± 0.28      3.20 ± 1.30      0.36 ± 0.47 0.46
# 13   inpatient_nur Non-respiratory      2.46 ± 0.41      1.87 ± 0.59     -0.76 ± 0.24 0.00


## Proportion of visiting reason:
get_prop <- function(df, keep_dis, split = T) {
  out <- c()
  is <- list("Before", "After", c("Before", "After"))
  for (i in is) {
    sub <- df %>% filter(policy %in% i & DIS %in% keep_dis)
    for (j in keep_dis) {
      n <- sum(sub$DIS == j)
      prop <- n / nrow(sub)
      out <- c(out, paste(i, collapse = "|"), j, n, prop)
    }
  }
  res <- data.frame(matrix(out, ncol = 4, byrow = T)) %>%
    mutate_if(is_numeric, as.numeric) %>%
    rename(policy = X1, group = X2, n = X3, prop = X4)
  return(res)
}


plot_prop <- function(df, title, lab_title, re_level = NA, nrow_legend = 2) {
  if (all(!is.na(re_level))) {
    df$group <- factor(df$group, levels = re_level)
  } # level group as groups
  p <- ggplot(df, aes(x = policy, weight = prop, fill = group)) +
    geom_bar(position = "stack") +
    xlab("") +
    ylab("Percentage") +
    theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      axis.text.y = element_text(color = "black"),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7.5)
    ) +
    ggtitle(title) +
    coord_flip() +
    guides(fill = guide_legend(title = lab_title, nrow = nrow_legend)) + # legend row
    scale_fill_nejm()
  return(p)
}

keep1 <- c("Pneumonia", "URTI", "Bronchitis", "Other respiratory")
keep2 <- c("ENT", "Dermatology", "Healthcare", "Ophthalmology", "General surgery", "Other")
keep3 <- c("Neurology", "Neonatology", "Nephropathy", "Hematology", "General surgery", "Other")

outpat2 <- get_prop(outpat1, keep1)
outpat2[outpat2 == "Other respiratory"] <- "Other"
inpat2 <- get_prop(inpat1, keep1)
inpat2[inpat2 == "Other respiratory"] <- "Other"
outpat3 <- get_prop(outpat1, keep2)
inpat3 <- get_prop(inpat1, keep3)

## proportion of visiting reason
temp <- rbind(outpat2, inpat2, outpat3, inpat3)
temp <- temp %>% mutate(prop1 = sprintf("%.0f (%.2f%%)", n, prop * 100))
temp
#       policy           group     n       prop          prop1
# 1        Before       Pneumonia  1728 0.10967251  1728 (10.97%)
# 2        Before            URTI  7993 0.50729881  7993 (50.73%)
# 3        Before      Bronchitis  5448 0.34577304  5448 (34.58%)
# 4        Before           Other   587 0.03725565    587 (3.73%)
# 5         After       Pneumonia  1867 0.11632399  1867 (11.63%)
# 6         After            URTI 10522 0.65557632 10522 (65.56%)
# 7         After      Bronchitis  3470 0.21619938  3470 (21.62%)
# 8         After           Other   191 0.01190031    191 (1.19%)
# 9  Before|After       Pneumonia  3595 0.11302899  3595 (11.30%)
# 10 Before|After            URTI 18515 0.58212287 18515 (58.21%)
# 11 Before|After      Bronchitis  8918 0.28038735  8918 (28.04%)
# 12 Before|After           Other   778 0.02446079    778 (2.45%)
# 13       Before       Pneumonia   331 0.84223919   331 (84.22%)
# 14       Before            URTI     4 0.01017812      4 (1.02%)
# 15       Before      Bronchitis    41 0.10432570    41 (10.43%)
# 16       Before           Other    17 0.04325700     17 (4.33%)
# 17        After       Pneumonia   136 0.64761905   136 (64.76%)
# 18        After            URTI    34 0.16190476    34 (16.19%)
# 19        After      Bronchitis    36 0.17142857    36 (17.14%)
# 20        After           Other     4 0.01904762      4 (1.90%)
# 21 Before|After       Pneumonia   467 0.77446103   467 (77.45%)
# 22 Before|After            URTI    38 0.06301824     38 (6.30%)
# 23 Before|After      Bronchitis    77 0.12769486    77 (12.77%)
# 24 Before|After           Other    21 0.03482587     21 (3.48%)
# 25       Before             ENT  6347 0.25065161  6347 (25.07%)
# 26       Before     Dermatology  3144 0.12416081  3144 (12.42%)
# 27       Before      Healthcare  2603 0.10279599  2603 (10.28%)
# 28       Before   Ophthalmology  2499 0.09868889   2499 (9.87%)
# 29       Before General surgery  2118 0.08364268   2118 (8.36%)
# 30       Before           Other  8611 0.34006003  8611 (34.01%)
# 31        After             ENT  2596 0.22638877  2596 (22.64%)
# 32        After     Dermatology  1105 0.09636348   1105 (9.64%)
# 33        After      Healthcare   737 0.06427139    737 (6.43%)
# 34        After   Ophthalmology   757 0.06601552    757 (6.60%)
# 35        After General surgery  1110 0.09679951   1110 (9.68%)
# 36        After           Other  5162 0.45016133  5162 (45.02%)
# 37 Before|After             ENT  8943 0.24308897  8943 (24.31%)
# 38 Before|After     Dermatology  4249 0.11549648  4249 (11.55%)
# 39 Before|After      Healthcare  3340 0.09078801   3340 (9.08%)
# 40 Before|After   Ophthalmology  3256 0.08850472   3256 (8.85%)
# 41 Before|After General surgery  3228 0.08774362   3228 (8.77%)
# 42 Before|After           Other 13773 0.37437821 13773 (37.44%)
# 43       Before       Neurology    52 0.11231102    52 (11.23%)
# 44       Before     Neonatology    51 0.11015119    51 (11.02%)
# 45       Before     Nephropathy    44 0.09503240     44 (9.50%)
# 46       Before      Hematology    38 0.08207343     38 (8.21%)
# 47       Before General surgery    36 0.07775378     36 (7.78%)
# 48       Before           Other   242 0.52267819   242 (52.27%)
# 49        After       Neurology    47 0.23857868    47 (23.86%)
# 50        After     Neonatology    38 0.19289340    38 (19.29%)
# 51        After     Nephropathy    23 0.11675127    23 (11.68%)
# 52        After      Hematology    22 0.11167513    22 (11.17%)
# 53        After General surgery    12 0.06091371     12 (6.09%)
# 54        After           Other    55 0.27918782    55 (27.92%)
# 55 Before|After       Neurology    99 0.15000000    99 (15.00%)
# 56 Before|After     Neonatology    89 0.13484848    89 (13.48%)
# 57 Before|After     Nephropathy    67 0.10151515    67 (10.15%)
# 58 Before|After      Hematology    60 0.09090909     60 (9.09%)
# 59 Before|After General surgery    48 0.07272727     48 (7.27%)
# 60 Before|After           Other   297 0.45000000   297 (45.00%)

## compare
chisq.test(outpat1 %>% filter(DIS %in% keep1) %>% pull(DIS), outpat1 %>% filter(DIS %in% keep1) %>% pull(policy))
# X-squared = 988.46, df = 3, p-value < 2.2e-16
chisq.test(inpat1 %>% filter(DIS %in% keep1) %>% pull(DIS), inpat1 %>% filter(DIS %in% keep1) %>% pull(policy))
# X-squared = 63.821, df = 3, p-value = 8.963e-14
chisq.test(outpat1 %>% filter(DIS %in% keep2) %>% pull(DIS), outpat1 %>% filter(DIS %in% keep2) %>% pull(policy))
# X-squared = 567.28, df = 5, p-value < 2.2e-16
chisq.test(outpat1 %>% filter(DIS %in% keep3) %>% pull(DIS), outpat1 %>% filter(DIS %in% keep3) %>% pull(policy))
# X-squared = 10.61, df = 1, p-value = 0.001125

## plot
plots <- list()
levels <- c("Pneumonia", "URTI", "Bronchitis", "Other")
plots[[1]] <- plot_prop(outpat2 %>% filter(policy != "Before|After"), title = "Respiratory disease (outpatient)", lab_title = "Disease", re_level = levels, nrow_legend = 4)
plots[[2]] <- plot_prop(inpat2 %>% filter(policy != "Before|After"), title = "Respiratory disease (inpatient)", lab_title = "Disease", re_level = levels, nrow_legend = 4)
plots[[3]] <- plot_prop(outpat3 %>% filter(policy != "Before|After"), title = "Non-respiratory disease (outpatient)", lab_title = "Disease", nrow_legend = 6)
plots[[4]] <- plot_prop(inpat3 %>% filter(policy != "Before|After"), title = "Non-respiratory disease (inpatient)", lab_title = "Disease", nrow_legend = 6)

p <- ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], hjust = 0.1, vjust = 0.1, ncol = 2, nrow = 2, common.legend = F, legend = "right")

png("./plot/dis_prop.png", height = 700, width = 1500, res = 150)
p
dev.off()


#====================================================================
# Impact on pediatric patients
#====================================================================
## Proportion of hospitalization expense:

inpat_fee <- inpat1 %>% mutate(DPT_NAME = ifelse(DPT_NAME == "Respiratory / Infectious", "Respiratory / Infectious", "Other"))
inpat_fee$OTHER_FEE <- inpat_fee$OTHER_FEE + inpat_fee$GEN_MED_FEE + inpat_fee$NON_OP_T_FEE # combine some fee to other
inpat_fee[, c("GEN_MED_FEE", "NON_OP_T_FEE")] <- NULL

out <- c()
for (dpt in c("Respiratory / Infectious", "Other")) {
  sub <- inpat_fee %>% filter(DPT_NAME == dpt)
  sub1 <- sub %>% filter(policy == "Before")
  sub2 <- sub %>% filter(policy == "After")
  fee_cols <- get("FEE", names(sub))
  fee_cols1 <- fee_cols[fee_cols != "OTHER_FEE"]

  for (col in fee_cols) {
    prop1 <- sum(sub1[, col]) / sum(sub1[, get("FEE", names(sub1))]) # update fee_cols
    prop2 <- sum(sub2[, col]) / sum(sub2[, get("FEE", names(sub2))]) # update fee_cols
    if (prop1 < 0.05 & prop2 < 0.05) {
      sub[, "OTHER_FEE"] <- sub[, "OTHER_FEE"] + sub[, col]
      sub[, col] <- NULL
    }
  }

  fee_cols <- get("FEE", names(sub))
  for (col in fee_cols) {
    prop1 <- sum(sub1[, col]) / sum(sub1[, fee_cols])
    prop2 <- sum(sub2[, col]) / sum(sub2[, fee_cols])
    mean1 <- sprintf("%.2f±%.2f", mean(sub1[, col]), sd(sub1[, col]))
    mean2 <- sprintf("%.2f±%.2f", mean(sub2[, col]), sd(sub2[, col]))
    out <- c(out, dpt, col, mean1, prop1, mean2, prop2)
  }
}

fee_prop <- data.frame(matrix(out, ncol = 6, byrow = T)) %>%
  mutate_if(is_numeric, as.numeric) %>%
  rename(dpt = X1, group = X2, mean_before = X3, prop_before = X4, mean_after = X5, prop_after = X6)

key <- c("NURSING_FEE", "LAB_DIAG_FEE", "IMAG_DIAG_FEE", "W_MED_FEE", "OP_T_FEE", "DMM_FEE", "OTHER_FEE")
value <- c("Nursing", "Laboratory", "Imaging", "Medication", "Surgery", "Material", "Other")
map <- data.frame(group = key, group_new = value)
fee_prop <- fee_prop %>%
  merge(map, "group") %>%
  mutate(group = group_new) %>%
  select(-group_new) %>%
  mutate(group = factor(group, levels = c("Laboratory", "Imaging", "Nursing", "Medication", "Surgery", "Material", "General", "Other"))) # rename fee
#    group                      dpt     mean_before prop_before     mean_after prop_after
# 1    Material Respiratory / Infectious   261.51±318.36  0.04789479    233.03±480.54 0.06344033
# 2    Material                    Other 1642.37±3993.31  0.21222855   514.88±1567.33 0.10386379
# 3     Imaging                    Other   495.83±626.10  0.06407115    285.17±489.93 0.05752500
# 4     Imaging Respiratory / Infectious   315.09±292.72  0.05770728   79.29±106.65 0.02158685
# 5  Laboratory Respiratory / Infectious  2488.87±920.98  0.45582241   1768.20±637.19 0.48137804
# 6  Laboratory                    Other 1809.18±1237.35  0.23378306   1654.53±892.01 0.33375897
# 7     Nursing Respiratory / Infectious   316.04±106.80  0.05788163     260.05±75.39 0.07079636
# 8     Nursing                    Other   397.64±504.35  0.05138387    297.66±310.67 0.06004425
# 9     Surgery                    Other  795.99±1862.20  0.10285841    186.21±714.80 0.03756294
# 10      Other Respiratory / Infectious  1165.92±701.07  0.21353239   855.94±557.65 0.23302320
# 11      Other                    Other 1377.76±1781.99  0.17803472  1107.68±1018.38 0.22344503
# 12 Medication Respiratory / Infectious   912.73±841.48  0.16716150   476.69±398.47 0.12977523
# 13 Medication                    Other 1219.93±1871.07  0.15764025  911.15±1745.27 0.18380002


## Compare hospitalization expense:

dpts <- c("Total", "Gastroenterology", "Neonatology", "Neurology", "Nephropathy", "Cardiology", "Respiratory / Infectious") # dpt with patients > 100
inpat2 <- rbind(inpat1, inpat1 %>% mutate(DPT_NAME = "Total")) # double df to add total
inpat2 <- inpat2 %>% mutate(DPT_NAME = ifelse(DPT_NAME %in% dpts, DPT_NAME, "Other"))

desReg <- function(df, col) {
  df$y <- df[, col]
  var1 <- df %>%
    filter(policy == "Before") %>%
    pull(y)
  var2 <- df %>%
    filter(policy == "After") %>%
    pull(y)
  mean1 <- sprintf("%.2f ± %.2f", mean(var1), sd(var1))
  mean2 <- sprintf("%.2f ± %.2f", mean(var2), sd(var2))
  lm <- lm(y ~ policy + age + SEX, df)
  coef <- summary(lm)$coefficients[2, ]
  beta <- sprintf("%.2f ± %.2f", coef[1], coef[2])
  p <- sprintf("%.2f", coef[4])
  out <- c(col, mean1, mean2, beta, p)
  return(out)
}

out <- c()
for (dpt in c(dpts, "Other")) {
  for (col in c("TOTAL_COST", "hosp_day")) {
    sub <- inpat2 %>%
      filter(DPT_NAME == dpt) %>%
      select(policy, age, SEX, TOTAL_COST, hosp_day)
    reg <- desReg(sub, col)
    out <- c(out, dpt, reg)
  }
}

reg <- data.frame(matrix(out, ncol = 6, byrow = T))
names(reg) <- c("dpt", "col", "mean_before", "mean_after", "beta", "p")
#                       dpt        col         mean_before        mean_after              beta    p
# 1                     Total TOTAL_COST   7448.68 ± 7822.60 4913.57 ± 3799.15   -2533.01 ± 414.37 0.00
# 2                     Total   hosp_day         5.76 ± 3.24       4.66 ± 2.04        -1.15 ± 0.18 0.00
# 3          Gastroenterology TOTAL_COST   5170.11 ± 2325.58 3733.01 ± 1945.17   -1493.85 ± 663.43 0.03
# 4          Gastroenterology   hosp_day         5.25 ± 1.94       3.53 ± 1.92        -1.40 ± 0.52 0.01
# 5               Neonatology TOTAL_COST 10189.81 ± 10282.71 5583.74 ± 3229.29  -4730.11 ± 1305.04 0.00
# 6               Neonatology   hosp_day         7.32 ± 5.87       4.38 ± 1.67        -2.92 ± 0.75 0.00
# 7                 Neurology TOTAL_COST   4807.38 ± 1992.39 4866.90 ± 3876.15      76.61 ± 638.45 0.90
# 8                 Neurology   hosp_day         5.72 ± 2.19       4.86 ± 2.47      -0.81 ± 0.55 0.14
# 9               Nephropathy TOTAL_COST   4421.27 ± 2122.92 5158.64 ± 3404.96     735.95 ± 566.63 0.20
# 10              Nephropathy   hosp_day         4.98 ± 3.16       4.94 ± 3.00       -0.07 ± 0.63 0.92
# 11               Cardiology TOTAL_COST   5963.29 ± 5276.26 6742.02 ± 3824.11   861.19 ± 1649.54 0.60
# 12               Cardiology   hosp_day         5.52 ± 1.57       5.45 ± 1.63       -0.06 ± 0.51 0.91
# 13 Respiratory / Infectious TOTAL_COST   5646.29 ± 2359.46 3744.23 ± 1391.98  -1748.31 ± 279.00 0.00
# 14 Respiratory / Infectious   hosp_day         5.59 ± 1.74       4.87 ± 1.53       -0.68 ± 0.22 0.00
# 15                    Other TOTAL_COST 10658.80 ± 11294.60 5143.25 ± 4725.47  -5419.71 ± 913.95 0.00
# 16                    Other   hosp_day         5.97 ± 4.09       4.64 ± 2.12       -1.26 ± 0.34 0.00


## Hospitalization expense, predicted (assuming the policyadjustment is not implemented) VS actual:
lm_predict_cost <- function(df, group_col, group) {
  if (all(group == "all")) {
    df1 <- df
  } else {
    df1 <- df[df[, group_col] %in% group, ]
  }
  df2 <- df1 %>% filter(policy == "Before")
  df3 <- df1 %>% filter(policy == "After")
  reg <- lm(TOTAL_COST ~ age + SEX + hosp_day, df2)
  df3 <- df3 %>% mutate(TOTAL_COST = predict(reg, newdata = df3))
  sub1 <- df1 %>%
    select(DT, TOTAL_COST) %>%
    mutate(group = "Actual")
  sub2 <- df3 %>%
    select(DT, TOTAL_COST) %>%
    mutate(group = "Predicted")
  temp <- data.frame(DT = as.Date(setdiff(c(df$DT, "2022-12-29"), df$DT), origin = "1970-01-01"), TOTAL_COST = NA)
  sub3 <- rbind(temp %>% mutate(group = "Actual"), temp %>% mutate(group = "Predicted"))
  df_p <- rbind(sub1, sub2, sub3)
  return(df_p)
}

plot_fee <- function(df_p, ylab_text = "Hospitalization expense (RMB)", legend_pos, y_inflat = 1, x_text = "12-14", title = "") {
  df_p$DT <- as.factor(format(as.Date(df_p$DT), format = "%m-%d"))
  ymax <- ceiling(median(df_p$TOTAL_COST, na.rm = T) / 100) * y_inflat * 100
  df_text <- data.frame(DT = x_text, TOTAL_COST = ymax * 0.85, group = "Actual")
  p <- ggplot(df_p, aes(x = DT, y = TOTAL_COST, fill = group)) +
    geom_boxplot(outlier.color = NA) +
    ylim(0, ymax) +
    scale_x_discrete(breaks = c("12-01", "12-08", "12-15", "12-22", "12-29")) +
    geom_vline(xintercept = "12-15", linetype = "dashed", color = "gray", size = 1) +
    geom_text(data = df_text, label = " Policy \n adjustment", vjust = 0.5, hjust = 0.3, size = 3.5) +
    ylab(ylab_text) +
    xlab("") +
    ggtitle(title) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
      axis.title.y = element_text(size = 8),
      plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # remove grid
      legend.title = element_blank(), legend.position = c(legend_pos[1], legend_pos[2])
    ) +
    scale_fill_nejm()
  return(p)
}

title1 <- "All diseases"
title2 <- "Respiratory or infectious diseases"

fee1 <- lm_predict_cost(inpat1, group_col = "DPT_NAME", group = "all")
fee2 <- lm_predict_cost(inpat1, group_col = "DIS", group = c("Bronchitis", "Pneumonia", "URTI", "Other respiratory"))

p1 <- plot_fee(fee1, title = title1, legend_pos = c(0.13, 0.78), y_inflat = 6)
p2 <- plot_fee(fee2, title = title2, legend_pos = c(0.13, 0.78), y_inflat = 3)

p <- ggarrange(p1, p2, ncol = 1, nrow = 2, common.legend = F, align = "v", hjust = 0.1, vjust = 0.1)

png("./plot/ts_predfee.png", height = 900, width = 800, res = 150)
print(p)
dev.off()


# ==========================================
# Session info  
# ==========================================
# R version I used (not necessary to be the same, but check if you have error)
session = c(unname(unlist(R.version['version.string'])))
for (lib in libs) {session = c(session, sprintf('%s: %s', lib, packageVersion(lib)))}
paste(session, collapse='; ')
# "R version 4.2.3 (2023-03-15); dplyr: 1.1.4; openxlsx: 4.2.5.2; stringr: 1.5.1; ggplot2: 3.4.4; ggsci: 3.0.0; ggpubr: 0.6.0; scales: 1.3.0"