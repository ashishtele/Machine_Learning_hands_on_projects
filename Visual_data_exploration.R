
# trying GGally package
if(!require(GGally)) install.packages("GGally")
if(!require(outliers)) install.packages("outliers")
if(!require(forcats)) install.packages("forcats")
if(!require(ggridges)) install.packages("ggridges")
if(!require(extracat)) install.packages("extracat")



ames <- AmesHousing::make_ames()
names(ames)

ggplot(ames, aes(x=Sale_Price)) +
  geom_histogram(bins = 2000) + #binwidth
  scale_x_continuous()

outlier(ames$Sale_Price, opposite = TRUE)
library(mlr)
mlr::summarizeColumns(ames)

ames %>%
  count(cut_width(Sale_Price, 5000)) %>%                     #cut_width: bins creation
  arrange(-n)

# Log transformed series 

ggplot(ames, aes(x=Sale_Price)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(alpha = 0.2, color = "red") +
  scale_x_log10(breaks = c(50000,125000,300000))

## Normality of variables

par(mfrow = c(1,2))

qqnorm(ames$Sale_Price)
qqline(ames$Sale_Price)

qqnorm(log(ames$Sale_Price))
qqline(log(ames$Sale_Price))

# Box-plot
library(ggplot2)

p1 <- ggplot(ames, aes("var", Sale_Price)) +
  geom_boxplot(outlier.alpha = 0.25, color = "red") +
  scale_y_log10(
    labels = scales::dollar,
    breaks = quantile(ames$Sale_Price))

p1

# Violin Plot

p2 <- ggplot(ames, aes("var", Sale_Price)) +
  geom_point(color = "red") +
  geom_violin(color = "blue") +
  scale_y_log10(
    labels = scales::dollar,
    breaks = quantile(ames$Sale_Price)
  )
p2

gridExtra::grid.arrange(p1,p2,ncol = 2)

# outlier package: outliers and scores functions
outliers <- scores(log(ames$Sale_Price), type = "iqr", lim= 1.5)
sort(ames$Sale_Price[outliers])
stem(ames$Sale_Price[outliers])

################################## categorical variable ####################################
# zoning
library(dplyr)
ames %>%
  count(MS_Zoning) %>%
  ggplot(aes(reorder(MS_Zoning,n),n)) +
  geom_col() +
  coord_flip()

# fct_lump to combine other (keep only top 2)
ames %>% 
  mutate(MS_Zoning = fct_lump(MS_Zoning, n = 2)) %>% 
  count(MS_Zoning) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(reorder(MS_Zoning, pct), pct)) +
  geom_col() +
  coord_flip()

# lollipop chart
ames %>%  
  count(Neighborhood) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(pct, reorder(Neighborhood, pct))) +
  geom_point() +
  geom_segment(aes(x = 0, xend = pct, y = Neighborhood, yend = Neighborhood), size = .15)


# fct_relevel to arrange the order
ames %>%
  mutate(Kitchen_Qual = fct_relevel(Kitchen_Qual, "Poor", "Fair", "Typical", "Good")) %>%
  ggplot(aes(Kitchen_Qual)) + 
  geom_bar()

library(tidyverse)
MASS::survey %>%
  select(Sex, Exer, Smoke, Fold, Clap, M.I) %>%
  gather(var, value, Sex:M.I) %>%
  ggplot(aes(value)) +
  geom_bar() +
  facet_wrap(~ var, scales = "free")
  
######################### Bivariate relationships and associations #############################

# 2 continuous variables
ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_point(alpha = .3)

p1 <- ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", lty = "dashed") +
  geom_smooth(se = FALSE, lty = "dashed") +
  ggtitle("Non-transformed variables")

p2 <- ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_point(alpha = .3) +
  geom_smooth(method = "lm", se = FALSE, color = "red", lty = "dashed") +
  geom_smooth(se = FALSE, lty = "dashed") +
  scale_x_log10() +
  scale_y_log10() +
  ggtitle("log-transformed variables")

gridExtra::grid.arrange(p1, p2, nrow = 1)

# 2 continuous variables
# clustering or gaps

p1 <- ggplot(ames, aes(x = Garage_Area, y = Sale_Price)) + 
  geom_point(alpha = .2)

p2 <- ggplot(ames, aes(x = Garage_Area, y = Sale_Price)) + 
  geom_point(alpha = .2) + 
  geom_density2d()

p3 <- ggplot(ames, aes(x = Garage_Area, y = Sale_Price)) + 
  geom_hex(bins = 50, show.legend = FALSE)

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)


# 1 continuous variable vs. 1 categorical variable
# jitter,violin, boxplot
p1 <- ggplot(ames, aes(x = factor(Bedroom_AbvGr), y = Sale_Price)) +
  geom_point(alpha = .2)

p2 <- ggplot(ames, aes(x = factor(Bedroom_AbvGr), y = Sale_Price)) +
  geom_jitter(alpha = .2, width = .2)

p3 <- ggplot(ames, aes(x = factor(Bedroom_AbvGr), y = Sale_Price)) +
  geom_boxplot()

p4 <- ggplot(ames, aes(x = factor(Bedroom_AbvGr), y = Sale_Price)) +
  geom_violin()

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

# 1 continuous variable vs. 1 categorical variable
# continuous variable across multiple categories (overlaid plots)
# freqpoly
p1 <- ggplot(ames, aes(x = Sale_Price, color = Overall_Qual)) +
  geom_freqpoly() +
  scale_x_log10(breaks = c(50, 150, 400, 750) * 1000, labels = scales::dollar)

# density 
p2 <- ggplot(ames, aes(x = Sale_Price, color = Overall_Qual, fill = Overall_Qual)) +
  geom_density(alpha = .15) +
  scale_x_log10(breaks = c(50, 150, 400, 750) * 1000, labels = scales::dollar)

gridExtra::grid.arrange(p1, p2, nrow = 2)

# 1 continuous variable vs. 1 categorical variable
# many level in categorical variable (partial overlapping)

#geom_density_ridges
ggplot(ames, aes(x = Sale_Price, y = Overall_Qual)) + 
  ggridges::geom_density_ridges() +
  scale_x_continuous(labels = scales::dollar)


# 1 continuous variable vs. 1 categorical variable
# facet_wrap 
ames %>%
  mutate(
    Above_Avg = ifelse(Sale_Price > mean(Sale_Price), "Above", "Below"),
    Kitchen_Qual = fct_relevel(Kitchen_Qual, "Poor", "Fair", "Typical", "Good")
  ) %>%
  ggplot(aes(Kitchen_Qual)) + 
  geom_bar() +
  facet_wrap(~ Above_Avg) +
  theme_bw()


# 1 continuous variable vs. 2 categorical variable
# facet_grid 
# tally: wrapper for summarise- call either n() or sum(n)
ames %>%
  mutate(
    Above_Avg = ifelse(Sale_Price > mean(Sale_Price), "Above", "Below"),
    Kitchen_Qual = fct_relevel(Kitchen_Qual, "Poor", "Fair", "Typical", "Good")
  ) %>%
  group_by(Neighborhood, Above_Avg, Kitchen_Qual) %>%
  tally() %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(Kitchen_Qual, pct)) + 
  geom_col() +
  facet_grid(Neighborhood ~ Above_Avg) +
  theme(strip.text.y = element_text(angle = 0, hjust = 0))


############################# Multivariate relationship #######################################

# 2 continuous and 1 categorical(less classes)  
# two types in central_Air(Categorical type)
ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price, color = Central_Air, shape = Central_Air)) +
  geom_point(alpha = .3) +
  scale_x_log10() +
  scale_y_log10()

# 2 continuous and 1 categorical(multiple classes) 
ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price)) +
  geom_point(alpha = .3) +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  facet_wrap(~ House_Style, nrow = 2) +
  theme_bw()

# 2 continuous and 2 categorical(two classes/multiple classes)
ggplot(ames, aes(x = Gr_Liv_Area, y = Sale_Price, color = Central_Air, shape = Central_Air)) +
  geom_point(alpha = .3) +
  geom_density2d(alpha = .5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  scale_y_log10(labels = scales::dollar) +
  facet_wrap(~ House_Style, nrow = 2) +
  ggtitle("Sale Price vs. Above Ground Sq.Ft",
          subtitle = "How does central air and house style influence this relationship?") +
  theme_bw()


## parallel coordinate plots (PCP)

# across continuous and categorical variables
# 1 continuous variable across multiple variables 
# Z-score standardization

# ggparcoord

variables <- c("Sale_Price", "Year_Built", "Year_Remod_Add", "Overall_Qual")

ames %>%
  select(variables) %>%
  ggparcoord(alpha = .05, scale = "center")

# ggparcoord with group column
ames %>%
  select(variables) %>%
  mutate(Above_Avg = Sale_Price > mean(Sale_Price)) %>%
  ggparcoord(
    alpha = .05,
    scale = "center",
    columns = 1:4,
    groupColumn = "Above_Avg"
  )

## Mosaic plots: visualizing data from two or more qualitative variables
# 1 continuous variable across multiple variables

ames2 <- ames %>%
  mutate(
    Above_Avg = Sale_Price > mean(Sale_Price),
    Garage_Type = abbreviate(Garage_Type),
    Garage_Qual = abbreviate(Garage_Qual)
  )

par(mfrow = c(1, 2))
mosaicplot(Above_Avg ~ Garage_Type, data = ames2, las = 1)
mosaicplot(Above_Avg ~ Garage_Type + Garage_Cars, data = ames2, las = 1)


## Correlation matrix and heatmap:(Numerical data)
# clusters of strongly correlated values

ames %>%
  select_if(is.numeric) %>%
  cor() %>%
  heatmap()

ames %>%
  select(Sale_Price, contains("sf")) %>%
  map_df(scale) %>%
  ggpairs()

############################# Data quality ###############################

sum(is.na(AmesHousing::ames_raw))

# missing values
heatmap(1 * is.na(AmesHousing::ames_raw), Rowv = NA, Colv = NA)


# missing pattern
# ordered by numbers of missings on both rows and columns
extracat::visna(AmesHousing::ames_raw, sort = "b")


# Amelia package: missingness map
Amelia::missmap(AmesHousing::ames_raw)










