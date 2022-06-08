#### Libraries --------------

library(tidyverse) 
library(skimr)
library(psych)
library(GGally)
library(janitor)
library(ggExtra)

#### setup -----------

set.seed(seed = 42)

#### Read data --------

df_sidr <- readRDS("df_sidr.rds")

# https://legilexi.org/media/1736/legilexi-la-rarhandledning-version-3.pdf


#### understand your data ----------

view(df_sidr) #view the object

head(df_sidr) #just show the top rows of the data

str(df_sidr) #get structure of object

summary(df_sidr) #summary of an object

psych::describe(df_sidr) # get common summary measures

skimr::skim(df_sidr)

GGally::ggpairs(df_sidr)

GGally::ggpairs(df_sidr[,5:10])

vignette(package = "skimr")

vignette(package = "skimr", topic = "skimr")

#### data wrangling ------------
#### select ------------

df_sidr_dt1_11_to_dt9_13 <- df_sidr %>%
  dplyr::select(dt1_11, dt1_12, dt1_13, dt2_11, dt2_12, dt2_13, 
                dt3_11, dt3_12, dt3_13, dt4_11, dt4_12, dt4_13, 
                dt5_11, dt5_12, dt5_13, dt6_11, dt6_12, dt6_13, 
                dt7_11, dt7_12, dt7_13, dt8_11, dt8_12, dt8_13, 
                dt9_11, dt9_12, dt9_13 )

df_sidr_dt1_11_to_dt9_13 <- df_sidr %>%
  dplyr::select(dt1_11:dt9_13)

df_sidr_dt1_11_to_dt9_13 <- df_sidr %>%
  dplyr::select(contains("dt"))

df_sidr_dt1_to_dt9_occasion_11 <- df_sidr %>%
  dplyr::select(contains("dt")) %>%
  dplyr::select(contains("11"))


GGally::ggpairs(df_sidr_dt1_to_dt9_occasion_11)


df_sidr_childid_to_schoolclass <- df_sidr %>%
  dplyr::select(childID, Kommun, SchoolID, SchoolClass)


#### Names -------------

colnames(df_sidr)

janitor::clean_names(df_sidr)

?clean_names
# case = c("snake", "small_camel", "big_camel",
# "screaming_snake", "parsed", "mixed", "lower_upper", "upper_lower",
# "swap", "all_caps", "lower_camel", "upper_camel", "internal_parsing",
# "none", "flip", "sentence", "random", "title")

janitor::clean_names(df_sidr, case = "small_camel")

df_sidr_snake <- janitor::clean_names(df_sidr)

colnames(df_sidr_snake)

### wide to long format -----------
?pivot_longer

df_sidr_snake_long <- df_sidr_snake %>%
  pivot_longer(cols = dt1_11:dt9_13, names_to = "variable_name", values_to = "value")


### separate variables into several columns ----------

df_sidr_snake_long_separated <- df_sidr_snake_long %>%
  separate(col = variable_name, into = c("test_nr", "occasion"), sep = "_")

?tidyr::separate

#### long to wide format --------------

?pivot_wider

df_sidr_snake_wide <- df_sidr_snake_long %>%
  pivot_wider(names_from = variable_name, values_from = value)


#### make new variables ----------

?dplyr::mutate

df_sidr_snake_wide <- df_sidr_snake_wide %>%
  mutate(kommun2 = as.factor(kommun)) %>%
  mutate(dt1_11_and_dt2_11 = dt1_11 + dt2_11)

summary(df_sidr_snake_wide)

#do all of your variables have the right format? If not, change them with mutate
# as.factor, as.integer, as.character
# factor to number needs to go via character. if x is a factor, use as.integer(as.character(x))


#### plot with ggplot --------------

# https://github.com/rstudio/cheatsheets/raw/master/data-visualization-2.1.pdf

#requires long format

df_sidr_snake_wide %>%
  ggplot(aes(x = dt1_11, y = dt2_11)) + 
  geom_point() +
  geom_smooth(method = "lm")


df_sidr_snake_wide %>%
  ggplot(aes(x = dt1_11, y = dt2_11)) + 
  geom_jitter() +
  geom_smooth()

df_sidr_snake_wide %>%
  ggplot(aes(x = dt5_11, y = dt8_11)) + 
  geom_jitter() +
  geom_smooth() 

plot1 <- df_sidr_snake_wide %>%
  ggplot(aes(x = dt5_11, y = dt8_11)) + 
  geom_jitter() +
  geom_smooth() 
  
ggMarginal(plot1, type = "histogram")

?ggMarginal



df_sidr_snake_long_separated %>%
  ggplot(aes(y = value, fill = test_nr)) + 
  geom_boxplot() +
  facet_wrap(facets = vars(occasion)
             #, scales = "free_y"
             )

# reverse figure so that each test is in one facet

df_sidr_snake_long_separated %>%
  ggplot(aes(y = value, fill = occasion)) + 
  geom_boxplot() +
  facet_wrap(facets = vars(test_nr)
             , scales = "free_y"
  )

#### combine above ---------

#make a plot of only dt5 and dt8 for all occasions   

df_sidr_snake_wide %>%
  dplyr::select(child_id:school_class, contains("dt5"), contains("dt8")) %>%
  pivot_longer(cols = contains("dt"), names_to = "variable_name", values_to = "value") %>%
  separate(col = variable_name, into = c("test_nr", "occasion"), sep = "_") %>%
  pivot_wider(names_from = test_nr, values_from = value) %>%
  ggplot(aes(x = dt5, y = dt8)) + 
  geom_jitter() +
  geom_smooth() 
  

df_sidr_snake_wide %>%
  dplyr::select(child_id:school_class, contains("dt5"), contains("dt8")) %>%
  pivot_longer(cols = contains("dt"), names_to = "variable_name", values_to = "value") %>%
  separate(col = variable_name, into = c("test_nr", "occasion"), sep = "_") %>%
  pivot_wider(names_from = test_nr, values_from = value) %>%
  ggplot(aes(x = dt5, y = dt8, group = occasion)) + 
  geom_jitter() +
  geom_smooth() 

df_sidr_snake_wide %>%
  dplyr::select(child_id:school_class, contains("dt5"), contains("dt8")) %>%
  pivot_longer(cols = contains("dt"), names_to = "variable_name", values_to = "value") %>%
  separate(col = variable_name, into = c("test_nr", "occasion"), sep = "_") %>%
  pivot_wider(names_from = test_nr, values_from = value) %>%
  ggplot(aes(x = dt5, y = dt8, group = occasion, color = occasion)) + 
  geom_jitter() +
  geom_smooth() 


#### other alternatives ---------


#corrmorant https://github.com/r-link/corrmorant

# install remotes package if necessary
install.packages("remotes")
# install corrmorant from the github repository
remotes::install_github("r-link/corrmorant")
# Afterwards, the package can be loaded regularly via library():


#ggstatsplot
# https://indrajeetpatil.github.io/ggstatsplot/index.html

library(ggstatsplot)

df_sidr_snake_wide %>%
  dplyr::select(contains("dt5"), contains("dt8")) %>% 
  ggcorrmat()

df_sidr_snake_wide %>%
  ggscatterstats(dt5_11, dt8_11)
