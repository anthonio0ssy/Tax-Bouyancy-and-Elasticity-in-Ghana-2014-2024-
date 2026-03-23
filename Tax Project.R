#Tax buoyancy and elasticity of revenue in Ghana
#Working with data from 2014 to 2024 with all figures in Ghana Cedis
# Model (log-log OLS regression)

#importing our data
library(readxl)
library(dplyr)
library(lmtest)
library(sandwich)
Project <- read_excel("C:/Users/HP 15/Desktop/Tax Project.xlsx")

#checking the dataset and getting the summary
print(Project)
summary(Project)

#run the log of the variables
 Project <- Project %>%
   mutate(
     ln_tax = log(TaxRevenue),
     ln_gdp = log(GDP)
   )

 #run the log-log OLS regression
 model <- lm(ln_tax ~ ln_gdp, data = Project )
 
 # summary of the coefficients or the model
 summary(model)
 coefficients(model)

 #recompute regression due to the violation of constant variance of errors assumption
 coeftest(model, vcovHC(model, type = "HC1"))
 
#Trend of GDP and Tax Revenue
 plot(Project$GDP / 1e6, Project$TaxRevenue / 1e6,
      xlab = "GDP (Million GHS)",
      ylab = "Tax Revenue (Million GHS)",
      main = "Relationship Between GDP and Tax Revenue in Ghana (2014–2024)",
      pch = 16)
 
 # Rescale to millions
 x_vals <- Project$GDP / 1e6
 y_vals <- Project$TaxRevenue / 1e6
 
 # Create plot
 plot_data <- Project%>%
   filter(!is.na(ln_gdp), !is.na(ln_tax))
 
ggplot(plot_data,aes(x = ln_gdp, y = ln_tax)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "GDP- Tax Revenue in Ghana (2014-2024)",
    x = "Log of GDP (GHS)",
    y = "Log of Tax Revenue(GHS)"
  ) +
  theme_minimal()
 
 #Generate fitted values
 fitted_values <- predict(MModel, newdata = plot_data)
  
 #Add regression line
 lines(
   plot_data$ln_gdp,
   fitted_values,
   lwd = 2
 )
 
 # Add regression line
 abline(model, lwd = 3, col = "red")
 
 
  # Time Series Plot (simple line plot)
 library(ggplot2)
ggplot(Project, aes(x = Year, y = `Tax to GDP Ratio`)) +
  geom_line(color = "blue", linewidth = 1.5) +
  geom_point(color= "red", linewidth = 4) +
   scale_x_continuous(
     breaks = Project$Year
   ) +
  labs(title = "Tax to GDP Ratio (2014-2024)",
       x = "Year",
       y = "Tax to GDP Ratio(%)") +
  theme_minimal()

#save plots
ggsave("Tax to GDP Ratio (2014-2024) plot.png", width = 8, height = 6, dpi = 300)
ggsave("GDP Vs Tax Revenue in Ghana plot.png", width = 470, height = 360, dpi = 300)

#save final dataset
write.csv(Project, "Ghana_tax_GDP_final.csv", row.names = FALSE)

