##################################################################################################
##################################################################################################
################################# Mid Course Assessment ##########################################
################################## Vince Weber ###################################################
##################################################################################################

library(here)
library(tidyverse)

# load data
ww <- read_csv(here("data/MCA/WastewaterDataSC.csv"))

# A. First, read in the datafile that contains information on viruses detected in wastewater (WastewaterDataSC.csv). 
# 
# ww=read.csv("WastewaterDataSC.csv")
# 
# C. Before you analyze the data, there are two measurements that were anomalous that you should remove with this code:
#   
 ww$RSV_gc_g_dry_weight[ww$RSV_gc_g_dry_weight>1e6]=NA #remove one outlier
 ww$Influenza_A_gc_g_dry_weight[ww$Influenza_A_gc_g_dry_weight>3e5]=NA #remove one outlier
 
# B. The file has 160 columns (!!) so it’s a bit cumbersome to work with. 
# You’ll only need 3 columns for the analysis: 
# the amounts of RNA of the two viruses are in the columns: "RSV_gc_g_dry_weight" and "Influenza_A_gc_g_dry_weight" 
# and you’ll need the Collection_Date column.

# drop columns in df
ww <- ww %>% 
  select(RSV_gc_g_dry_weight, 
         Influenza_A_gc_g_dry_weight, 
         Collection_Date)

# (hint: you can use Tidyverse piping and select, or column indexing with df[,c("col1","col2") ).
# 
# 
# D. Next, plot both Influenza RNA and RSV RNA vs Collection Date. 
# Think carefully about which variable should be on the x axis, and which should be on the why axis. 
# Make the plot your own (change the colors, point or line size, labels, title, etc) 
# and save it as a high resolution .png file. Before you do this, you should convert the Collection_Date column into Date format using:

# convert to date format   
ww$Collection_Date=as.Date(ww$Collection_Date,format="%m/%d/%Y") #Date format - 4 digit yr, dashes

# You can make this plot either in ggplot by making a new “longer” dataframe that has RNA of the two viruses 
# (Influenza and RSV) in a single column, or by using base plot (there are multiple ways to do this, as always!).

# long form for viruses
ww_long <- pivot_longer(ww, c("RSV_gc_g_dry_weight", "Influenza_A_gc_g_dry_weight"), names_to = "virus", values_to = "weight")

# plot virus by date
v_dat <- ggplot(ww_long, aes(Collection_Date, weight, fill = virus)) +
  theme_minimal() +
  geom_point(aes(color=virus)) +
  scale_x_date(date_breaks="1 month",date_labels = "%b-%d") +
  scale_fill_manual(values = c("RSV_gc_g_dry_weight" = "brown", "Influenza_A_gc_g_dry_weight" = "blue"),# not my favorite color scheme
                    labels = c("RSV_gc_g_dry_weight" = "RSV", "Influenza_A_gc_g_dry_weight" = "Influenza A")) +
  scale_color_manual(values = c("RSV_gc_g_dry_weight" = "brown", "Influenza_A_gc_g_dry_weight" = "blue"), 
                     labels = c("RSV_gc_g_dry_weight" = "RSV", "Influenza_A_gc_g_dry_weight" = "Influenza A")) +
  labs(x = "Collection Date (April 2022 - August 2023)", y = "Virus Dry Weight (grams dry weight)") # not exactly sure what the specific unit of measure is
  
  
v_dat

ggsave("VirusWeight_RSV_InfA.png", v_dat, path = here("output/VirusWeight_RSV_InfA.png"))

# In ggplot, one nice trick for making Date axes have the writing spacing and format is to use:  scale_x_date(date_breaks="1 month",date_labels = "%b-%d")
# 
# Google “strptime r” to see some different format options (e.g. “%b-%d”).
# 
# E. Do the amounts of Influenza RNA and RSV RNA show similar patterns or rising and falling over time? Add a comment in your code with your observations.
#
# The rise and fall of RSV and Influenza share a similar spike around Nov 1, 2022. RSV has a less dramatic spike over time than Influenza, and started to rise around Sep 1, a few months earlier.
# 
# F. Next, plot the data, with the RNA of Influenza on the x-axis and the RNA of RSV on the y-axis (you will need your data frame to be in the original “wide” format for this!). 
# Please add a fitted linear regression model line to the plot and the 95% confidence interval. 
# Make the plot your own (change the colors, point or line size, labels, title, etc) and save it as a high resolution .png file.
#
rsv_inf <- ggplot(ww, aes(Influenza_A_gc_g_dry_weight, RSV_gc_g_dry_weight)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +  # Customize point color and transparency
  geom_smooth(method = "lm", color = "red", fill = "pink", se = TRUE, size = 1.2) +  # Linear model with confidence interval
  labs(
    title = "Relationship Between Influenza A and RSV RNA",
    x = "Influenza A RNA (gc/g dry weight)",
    y = "RSV RNA (gc/g dry weight)"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

rsv_inf

# Save as high-resolution PNG
ggsave("output/Virus_RNA_Relationship.png", rsv_inf, width = 8, height = 6, dpi = 300)

# G. Analyze the data using a linear model with the RNA of Influenza as the predictor and RNA of RSV as the response variable 
# (think carefully about model structure, which should be lm(y~x)). 
# What does the analysis suggest about the relationship between RNA of Influenza and RNA of RSV? 
#
# The relationship between RSV and Inf_A suggests that they are positively correlated, rising at relatively the same time in the year. When one rises so does the other. 
#
# Is there a clear relationship between the two viruses or is it likely due to chance?

# There is a clear positive relationship that is very likely is not due random chance.
#   

m1 <- lm(RSV_gc_g_dry_weight ~ Influenza_A_gc_g_dry_weight, data = ww); summary(m1)

# Call:
#   lm(formula = RSV_gc_g_dry_weight ~ Influenza_A_gc_g_dry_weight, 
#      data = ww)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -62235  -5557  -5557   1776 255471 
# 
# Coefficients:
#                                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                   5.557e+03  1.003e+03   5.538 5.46e-08 ***
#   Influenza_A_gc_g_dry_weight 5.113e-01  3.603e-02  14.190  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 19240 on 411 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.3288,	Adjusted R-squared:  0.3272 
# F-statistic: 201.4 on 1 and 411 DF,  p-value: < 2.2e-16


#   H. How much of the variation in RSV RNA is explained by variation in Influenza RNA? Most of it? A small amount?
#
#       Most of the relationship in RSV is explained by variation in Inf_A.
#   
#   I. What is the slope of the relationship between RNA of Influenza and RNA of RSV? 

#        slope = 5.113e-01

#      What is the uncertainty in the slope? 

#        uncertainty = 3.603e-02 = SE
#       
#      What is the approximate 95% CI of the slope? 
5.113e-01 + (2 * 3.603e-02) # 0.58336
5.113e-01 - (2 * 3.603e-02) # 0.43924
confint(m1, level = 0.95) # Influenza_A_gc_g_dry_weight    0.44046    0.5821131, I like this way better because there's less likelihood for a simple math error.

#   
#   J. If you wanted to have a more precise measurement of the slope, what are two things you could do to make the uncertainty in the slope smaller?

#          More data points
#          Remove outliers
#   
#   K. What are the four assumptions of the regression analysis you did and does the analysis meet or likely not meet each of these assumptions? Add a comment with your assessments.

#          Linearity - there is a linear relationship between x and y
#          Independence - there are differences in the residuals
#          Homoscedasticity - there is constant variance in the data
#          Normality - data follow a normal distribution
#
# L. Add the slope, intercept, and R2 to the plot you made of Influenza vs RSV RNA. Here’s one way to add these elements if you’re using ggplot (add this after a +):
#   
#   #Note: after_stat() pulls the calculated values (rr.label is R2, p.labl is P-value), the “sep=” separates the R2 and P-value by a comma and a space (the “~”).
#   ggpubr::stat_cor(label.x=1e5,label.y=2.5e5,size=5, #put R2 and P-value at x,y location
#                    aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~"))) +
#   ggpubr::stat_regline_equation(label.x=1e5,label.y=2.2e5,size=5) #put equation at x,y location
# 
# If you are using base plot and want to add the regression equation and R2, see the Part 2: Regression slides which show how to do this by extracting and plotting model components.
# 
library(ggpubr)

rsv_inf_R2 <- ggplot(ww, aes(Influenza_A_gc_g_dry_weight, RSV_gc_g_dry_weight)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) + 
  geom_smooth(method = "lm", color = "red", fill = "pink", se = TRUE, size = 1.2) +  # Linear model with confidence interval
  labs(
    title = "Relationship Between Influenza A and RSV RNA",
    x = "Influenza A RNA (gc/g dry weight)",
    y = "RSV RNA (gc/g dry weight)"
  ) +
  ggpubr::stat_cor(label.x=1e5,label.y=2.5e5,size=5, #put R2 and P-value at x,y location
                   aes(label = paste(after_stat(rr.label), after_stat(p.label), sep = "~`,`~"))) +
  ggpubr::stat_regline_equation(label.x=1e5,label.y=2.2e5,size=5) + #put equation at x,y location 
  theme_minimal() +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10) +
      
    
  )

rsv_inf_R2

# Save as high-resolution PNG
ggsave("output/Virus_RNA_Relationship_R2.png", rsv_inf, width = 8, height = 6, dpi = 300)

# M. Load in the dataset from the last lecture (NSFRoD2022_Deployment_Summary_RSB.csv). 
df <- read_csv(here("data/Lectures/NSFRoD2022_Deployment_Summary_RSB.csv"))

# Let’s repeat the exercise that we did in class – 
# to see if size increases with age and sex in elephant seals – 
# but this time, use Standard Length (cm) instead of mass. 
# Run an additive multiple regression of Standard Length with Age and Sex as predictors. 
# Then, run an interactive multiple regression of Standard Length with Age and Sex as predictors. 
# Write R code that produces/prints the intercepts and slopes of each model 
# (feel free to either fill in a single number, or include multiple numbers in each cell to show us how you calculated e.g., intercept for males in the interactive model). 

# Additive model
m2 <- lm(StandardLength ~ Age + Sex, data = df)
summary(m2)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -23.481  -5.009   1.019   4.969  15.306 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    143.744      2.961  48.548   <2e-16 ***
#   Age           31.213      1.359  22.975   <2e-16 ***
#   SexM           6.313      3.038   2.078   0.0467 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8.592 on 29 degrees of freedom
# Multiple R-squared:  0.9483,	Adjusted R-squared:  0.9448 
# F-statistic: 266.1 on 2 and 29 DF,  p-value: < 2.2e-16

# Slope for Males: 31.213, both sexes parallel in slope
# Slope for Females: 31.213
# Intercept for Males: 150.057, males are bigger from the get go
# Intercept for Females: 143.744
# 
# Interactive model
m2 <- lm(StandardLength ~ Age * Sex, data = df)
summary(m2)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -23.825  -4.850   0.675   5.731  14.275 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  144.775      3.642  39.754  < 2e-16 ***
#   Age           30.525      1.947  15.681 2.14e-15 ***
#   SexM           4.250      5.150   0.825    0.416    
# Age:SexM       1.375      2.753   0.499    0.621    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 8.705 on 28 degrees of freedom
# Multiple R-squared:  0.9488,	Adjusted R-squared:  0.9433 
# F-statistic: 172.9 on 3 and 28 DF,  p-value: < 2.2e-16

# Slope for Males: 31.9
# Slope for Females: 30.525
# Intercept for Males: 149.025
# Intercept for Females: 144.775
# 
# 
# N. Make a ggplot of Standard Length ~ Age with color=Sex. Manually add two best fit lines (one for males and one for females) 
# FROM THE ADDITIVE MODEL using the slopes and intercepts in the table (hint: use geom_abline() with inputs slope=…, intercept=…, color=…).  
# 
# O. Make a ggplot of Standard Length ~ Age with color=Sex. Manually add two best fit lines (one for males and one for females) FROM THE INTERACTIVE MODEL using the slopes and intercepts in the table (hint: use geom_abline() with inputs slope=…, intercept=…, color=…).  
# P. Make a ggplot of Standard Length ~ Age with color=Sex. Add best fit lines by adding geom_smooth(method=”lm”) to the plot. Does this plot look the same as your additive model plot or your interactive model plot? 
#   Q. How do the male and female lines differ in the additive model? How do they differ in the interactive model? What do these two models suggest about the increase in length with age and sex?
#   R. Which model (additive or interactive) should you use and why? Please save your plot for that model as a high resolution .png file. 
# S. In your code, include a commented line with this sentence (which could be a sentence in the results section of a paper), but with the blanks filled: “The __________ [additive or interactive] model suggests that males are ______cm (95% CI: ______ to ______ cm) ___________ [longer or shorter] across all ages. Growth rates for both males and females are __________ centimeters per year (95% CI __________ to ______ cm) (Figure 1). 
# T. Upload your three .png figures (virus time series, virus comparison, seal length model) to the Canvas Discussion Board (link is in the Canvas Assignment). 
# 
