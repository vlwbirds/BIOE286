### PrePost Assessment BIOE 286
library(here)
library(tidyverse)

df <- read.csv(here("data/Rabbits3Areas.csv"))
str(df)

## means
mean(df$Pogonip)
# pogo mean 3.15
mean(df$UCSC)
# ucsc mean 6.7
mean(df$Wilder)
# wilder mean 7.8

## standard deviation
sd(df$Pogonip)
# pogo sd 1.871532
sd(df$UCSC)
# ucsc sd 2.65766
sd(df$Wilder)
# wild sd 3.607011


## Put Rabbits into long format for sites
dfL <- as.data.frame(df %>% pivot_longer(cols=c("Pogonip","UCSC", "Wilder"), 
                                    names_to = "sites", 
                                    values_to = "rabbits"))
head(dfL)

### Load slug and fog data
S_df <- read_csv(here("data/SlugsAndFog.csv"))
head(S_df)

ggplot(data = S_df, mapping = aes(x = fogdays, y = slugs)) +
  geom_point()
# not sure how to plot the trend line

lm(slugs ~ fogdays, data = S_df)


set.seed(1)
y <- rnorm(100)
df <- data.frame(
  x = 1,
  y0 = min(y = slugs),
  y25 = quantile(y = slugs, 0.25),
  y50 = median(y = slugs),
  y75 = quantile(y = slugs, 0.75),
  y100 = max(y = slugs)
)
ggplot(S_df, aes(x = fogdays)) +
  geom_boxplot(
    aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
    stat = "identity"
  )

ggplot(S_df, aes(x = fogdays)) +
  geom_boxplot(S_df, aes(y = slugs)

                