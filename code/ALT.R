library(readxl)
library(tidyverse)
library(Microsoft365R)

#list_teams()
#team <- get_team("INSULATE")

#conn <- odbcConnect("CDataMSTeams INSULATE")

ALT_season <- read_xlsx("./Data/thaw_depth_seasonal.xlsx")

str(ALT_season)
ALT_season <- rename(ALT_season, "DOY"="Date(DOY2024)")

ggplot(ALT_season, aes(x=DOY, y=as.numeric(Thaw_depth), color=Site))+
  geom_point()+
  geom_smooth()

ggplot(ALT_season, aes(x=DOY, y=as.numeric(Thaw_depth), color=Site))+
  geom_point()+
  geom_smooth(se=F)+
  facet_grid(cols=vars(Plot))


# Calculate the mean thaw depth height for each Site, Plot, and Date
thaw_depth_mean <- ALT_season %>%
  group_by(Site, Plot, DOY) %>%    # Group by Site, Plot, and Date
  summarise(Mean_ALT = mean(as.numeric(Thaw_depth), na.rm = TRUE))  # Calculate the mean height

# Display the first few rows of the summarized data
head(thaw_depth_mean)

# Plot the data
ggplot(subset(thaw_depth_mean, DOY>"225"), aes(x = DOY, y = Mean_ALT, color=Site)) +
  geom_point() +
  geom_smooth(method="lm")+
  labs(title = "Mean Vegetation Height Over Time",
       x = "Date",
       y = "Mean Vegetation Height") +
  theme_minimal()

ggplot(thaw_depth_mean, aes(x = DOY, y = Mean_ALT, color=Site)) +
  geom_point() +
  geom_smooth(se=F)+
  labs(title = "Mean Vegetation Height Over Time",
       x = "Date",
       y = "Mean Vegetation Height") +
  theme_minimal()


ggplot(subset(thaw_depth_mean, DOY>"225"), aes(x = DOY, y = Mean_ALT, color=Site)) +
  geom_point() +
  geom_smooth()+
  labs(title = "Mean Vegetation Height Over Time",
       x = "Date",
       y = "Mean Vegetation Height") +
  theme_minimal()

# Perform a linear regression on the entire dataset
model <- lm(Mean_ALT ~ DOY, data = thaw_depth_mean)

# Summary of the linear model
summary(model)

# Late period
late_model <- lm(Mean_ALT ~ DOY + Site, data = subset(thaw_depth_mean, DOY > "225"))
summary(late_model)

# Perform linear regression for each Site
site_trends <- subset(ALT_season,DOY > "200") %>%
  group_by(Site) %>%
  do(model = lm(Thaw_depth ~ DOY, data = .))

# Extract the slope and p-value for each site
site_trends_summary <- site_trends %>%
  summarise(Site = Site,
            Slope = coef(model)[2],
            p_value = summary(model)$coefficients[2, 4])

# Display the results
print(site_trends_summary)

