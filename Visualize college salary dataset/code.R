library(ggplot2)
library('readr')
library('ggthemes')
library('ggrepel')
setwd("C:\\Users\\tsengxt\\Desktop\\大四上课程\\Big Data and Data Mining\\midterm present")

# excel 1
degreedata <-  read_csv("C:\\Users\\tsengxt\\Desktop\\大四上课程\\Big Data and Data Mining\\midterm present\\degrees-that-pay-back.csv")
#1
ggplot(data = degreedata, mapping = aes(x = Starting_Median_Salary, y = Mid_Career_Median_Salary))+
  geom_point()+
  geom_smooth(se = FALSE)+
  labs(x = "Starting Median Salary($)", y = "Mid-Career Median Salary($)",
       title = "Starting median salary and Mid-Career Median Salary by major",
       caption = "Source:payscale")
ggsave(filename = "figure1.png",width = 20, height = 15, units = "cm")

#2
ggplot(data = degreedata, mapping = aes(x = Starting_Median_Salary, y = Percent_change_from_Starting_to_Mid_Career_Salary))+
  geom_point()+
  labs(x = "Starting Median Salary($)", y = "Percent change from Starting to Mid Career Salary($)",
       title = "Does Starting Median Salary relate to Percent change from Starting to Mid Career Salary?",
       caption = "Source:payscale")
ggsave(filename = "figure2.png",width = 20, height = 15, units = "cm")

#3
ggplot(degreedata, aes(x=reorder(Undergraduate_Major,Mid_Career_Median_Salary), 
                 ymin = Mid_Career_10th_Percentile_Salary,
                 lower = Mid_Career_25th_Percentile_Salary, 
                 middle = Mid_Career_Median_Salary, 
                 upper = Mid_Career_75th_Percentile_Salary, 
                 ymax = Mid_Career_90th_Percentile_Salary,
                 fill = Undergraduate_Major)) +
  geom_boxplot(stat = "identity")+
  guides(fill = FALSE)+
  coord_flip()
ggsave(filename = "figure3.png",width = 20, height = 15, units = "cm")


typedata <- read_csv("C:\\Users\\tsengxt\\Desktop\\大四上课程\\Big Data and Data Mining\\midterm present\\salaries-by-college-type.csv")
#4
ggplot(data = typedata, mapping = aes(x = School_Type,fill = School_Type))+
  geom_bar()+
  labs(x = "School Type", y = "Number of schools",
       title = "School number of different types",
       caption = "Source:Payscale")+
  guides(fill = FALSE)
ggsave(filename = "figure4.png",width = 20, height = 15, units = "cm")

#5
ggplot(data = typedata, mapping = aes(x = School_Type,
                                      y = Percentage_increase,
                                      fill = School_Type))+
  geom_boxplot()+
  labs(x = "School Type", y = "Percentage increase of salary from starting to mid-career",
       title = "Percentage increase of salary from starting to mid-career by different types of school",
       caption = "Source:Payscale")
ggsave(filename = "figure5.png",width = 20, height = 15, units = "cm")

#7
ggplot(typedata, mapping = aes(x = School_Type,
                                 y = Starting_Median_Salary,
                                 fill = School_Type))+
  geom_boxplot(varwidth = TRUE)+
  labs(x = "School Type", y = "Starting Median Salary",
       title = "Starting Median Salary by School Type",
       caption = "Source:Payscale")+
  guides(color = FALSE)
ggsave(filename = "figure7.png",width = 20, height = 15, units = "cm")

regiondata <- read_csv("C:\\Users\\tsengxt\\Desktop\\大四上课程\\Big Data and Data Mining\\midterm present\\salaries-by-region.csv")

#6
p <- ggplot(data = regiondata, mapping = aes(x = Region,fill = Region))
p+  geom_bar()+
  labs(x = "Region", y = "Number of schools",
       title = "Number of schools in different regions",
       caption = "Source:Payscale")+
  guides(fill = FALSE)
ggsave(filename = "figure6.png",width = 20, height = 15, units = "cm")



#8
ggplot(regiondata, aes(x = Starting_Median_Salary,
                       group = Region,
                       fill = Region,
                       color = Region))+
  geom_density(alpha = 0.3)+
  facet_wrap(~Region)+
  guides(fill = FALSE,color = FALSE)+
  labs(x = "Starting Median Salary",
       title = "Distribution of Starting Median Salary by region")
ggsave(filename = "figure8.png",width = 20, height = 15, units = "cm")

library(dplyr)
top20_start <- arrange(regiondata,desc(Starting_Median_Salary))[1:20,]
bottom20_start <- arrange(regiondata,Starting_Median_Salary)[1:20,]
top_bottom_start <- rbind(top20_start, bottom20_start)
top_bottom_start$type <- ifelse(top_bottom_start$Starting_Median_Salary  < 50000, "below", "above")


#9
top20_median <- arrange(regiondata,desc(Mid_Career_Median_Salary))[1:20,]
ggplot(top20_median, aes(x=reorder(School_Name,Mid_Career_Median_Salary), 
                       ymin = Mid_Career_10th_Percentile_Salary,
                       lower = Mid_Career_25th_Percentile_Salary, 
                       middle = Mid_Career_Median_Salary, 
                       upper = Mid_Career_75th_Percentile_Salary, 
                       ymax = Mid_Career_90th_Percentile_Salary,
                       fill = School_Name)) +
  geom_boxplot(stat = "identity")+
  guides(fill = FALSE)+
  coord_flip()+
  labs(x = "Mid-Career Salary",y = "School Name",
       title = "Top20 Schools in terms of Mid-Career Salary",
       caption = "Source:Payscale")
ggsave(filename = "figure9.png",width = 20, height = 15, units = "cm")

#9-10
ggplot(top_bottom_start, aes(x=reorder(School_Name,Starting_Median_Salary),
                                       y=Starting_Median_Salary, label=Starting_Median_Salary)) + 
  geom_bar(stat='identity', aes(fill=type), width=.5)  +
  scale_fill_manual(name="type", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(x = "Starting Median Salary",
       y = "School Name",
       title= "Top20 vs Bottom20 Schools in Starting Median Salary")+
       coord_flip()
ggsave(filename = "figure_new_1.png",width = 20, height = 15, units = "cm")

#10
ggplot(top_bottom_start, aes(x=reorder(School_Name,Starting_Median_Salary),
                             y=Starting_Median_Salary, label=Starting_Median_Salary)) + 
  geom_bar(stat='identity', aes(fill=Region), width=.5)  +
  labs(x = "Starting Median Salary",
       y = "School Name",
       title= "Top20 vs Bottom20 Schools in Starting Median Salary",
       subtitle = "by region") + 
  coord_flip()
ggsave(filename = "figure10.png",width = 20, height = 15, units = "cm")

#11
merge40 <- merge(top_bottom_start,typedata,by="School_Name",all.x = TRUE)
ggplot(merge40, aes(x=reorder(School_Name,Starting_Median_Salary.x),
                    y = Starting_Median_Salary.x, 
                    label=Starting_Median_Salary.x)) + 
  geom_bar(stat='identity', aes(fill=School_Type), width=.5)  +
  labs(x = "Starting Median Salary",
       y = "School Name",
       title= "Top20 vs Bottom20 Schools in Starting Median Salary",
       subtitle = "by school type") + 
  coord_flip()
ggsave(filename = "figure11.png",width = 20, height = 15, units = "cm")

#12
ggplot(regiondata, mapping = aes(x = Region,
                                 y = Percentage_increase,
                                 fill = Region))+
  geom_boxplot()+
  labs(x = "Region", y = "Percentage increase of salary from starting to mid-career",
       title = "Percentage increase of salary from starting to mid-career by Region",
       caption = "Source:Payscale")
ggsave(filename = "figure12.png",width = 20, height = 15, units = "cm")