# Sales-Data-Viz
Visualization of sales data using R 
library(readr)
X5000_sales_ <- read_csv("5000 sales .csv")
View(X5000_sales_)

packages <- c("ggplot2", "dplyr", "tidyr",
              "ggthemes", "hrbrthemes", "ggpol")

library(dplyr)
library(ggplot2)
install.packages(packages)

barchart <- X5000_sales_%>%
 count(`Item Type`)
ggplot (barchart,aes(x= `Item Type`, y=n))+
  geom_bar (stat= "identity" , fill = "cornflowerblue", color="black") +
  geom_text (aes(label=n), vjust=-0.5)+
  labs(x= "Item Type", y= "Frequency", title= "Total Number Of Items Available In Store") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


piedata <- X5000_sales_%>%
  count(Region)%>%
  arrange(desc(Region))%>%
  mutate(prop=round(n * 100/ sum(n), 1), lab.ypos= cumsum(prop) - 0.5 * prop)

  piedata$label <- paste0(piedata$Region, "\n",
                          round(piedata$prop), 
                          "%")
  ggplot(piedata,
         aes(x= "",
             y= prop,
             fill= Region))+
    geom_bar(width = 1,
             stat = "identity",
             color = "black") +
    geom_text(aes(y= lab.ypos, label= label),
              color= "black") +
    coord_polar("y",
                start = 0,
                direction = -1) +
    theme_void() +
    theme(legend.position = "TRUE") +
    labs(title = "Sales By Region")
  
 
  
  ggplot(X5000_sales_, aes(x= `Item Type`)) +
    geom_density(fill= "indianred3") +
    labs(title = "Most Item Sold") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
