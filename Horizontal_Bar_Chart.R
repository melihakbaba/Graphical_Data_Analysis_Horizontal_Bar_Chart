library(grid)
library(tidyverse)
library(shadowtext)
library(sysfonts)
library(showtext)

data <- read.csv("C:/Users/MELİH/Desktop/GRAFİKSEL VERİ ANALİZİ DÖKÜMAN/final ödevi/salaries_2.csv")




salary_in_peryear <- as.data.frame(data[data$job_title =="Data Scientist",] %>% 
  group_by(work_year) %>% 
  summarise(avg_salary=mean(salary_in_usd)))

salary_in_peryear$work_year <- as.character(salary_in_peryear$work_year)

font_add_google("Noto Serif")


plt <- ggplot(salary_in_peryear) +
  geom_col(aes(x=avg_salary,y = work_year), fill = "#FFC464", width = 0.6)

plt <- plt + 
  scale_x_continuous(
    limits = c(0, 200000,10000),
    breaks = seq(0, 180000, by = 60000), 
    expand = c(0, 0),
    position = "top"
     
    ) +
 
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(
   
    panel.background = element_rect(fill = "white"),
    
    panel.grid.major.x = element_line(color = "#D38A00", size = 0.7),
    
    axis.ticks.length = unit(0, "mm"),
   
    axis.title = element_blank(),
    
    axis.line.y.left = element_line(color = "black",size=0.7),
    
    axis.text.y = element_blank(),
    
    axis.text.x = element_text(family="Noto Serif",size = 14)
  )

plt <- plt + 
  geom_text(
    data = subset(salary_in_peryear),
    aes(0, y = work_year, label = work_year),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family="Noto Serif",
    size = 7
  ) + 
  geom_text(
    data = subset(salary_in_peryear),
    aes(x = avg_salary, y = work_year, label = round(avg_salary)),
    hjust = 1, 
    nudge_x = 0.3, 
    colour = "white",
    family = "Noto Serif",
    size = 7
  )

plt <- plt +
  labs(
    title = "Veri Bilimci Maaşlarının Değişimi", 
    subtitle = "Yıllara Göre Ortalama Veri Bilimci Maaşı (USD), 2020-2024"
  ) + 
  theme(
    plot.title = element_text(
      family = "Noto Serif", 
      face = "bold",
      size = 22
    ),
    plot.subtitle = element_text(
      family = "Noto Serif",
      size = 20
    )
  )



plt <- plt + 
  theme(
    plot.margin = margin(0.05, 0, 0.1, 0.01, "npc")
  )

plt

grid.lines(x = unit(0, "npc"), y = c(0.85, 0.95), gp = gpar(lwd = 10, col = "red",linejoin = "mitre", lineend = "butt"))

grid.text(
  "Kaynak:Data Scientist Salary in 2024; Kaggle", 
  x = 0.005, 
  y = 0.06, 
  just = c("left", "bottom"),
  gp = gpar(
    col = "#8A8989",
    fontsize = 16,
    fontfamily = "Noto Serif"
  )
)


