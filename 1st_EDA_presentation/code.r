#CAMPUS PLACEMENT DATA FOR ENGINEERING COLLEGES
#Data Importing

getwd()
setwd("c://Users//PC//Desktop//1st_EDA_presentation")
data_set <- read.csv("placement.csv")
fix(data_set)

#####################################################################

#Do students with more internships have a higher chance of getting placed or not?


#Internship Effect On Number Of Placement of the Students

placement_data <- data_set
Internships_effect <- table(placement_data$Internships, placement_data$PlacementStatus)
print(Internships_effect)

levels(factor(data_set$Internships))
number_of_placements=
  matrix(c(1320,3733,750,663,1809,1725),
         nrow = 2,ncol = 3,byrow = TRUE)
colors=c("red","blue")                          
number_of_internships=c("0","1","2") 
categories=c("Not Placed","Placed")  
barplot(number_of_placements,main = "Internship's Effect On Placement Status",
        names.arg =number_of_internships,xlab = "Number Of Internship",
        ylab = "Number Of Student",ylim=c(0,5000),col = colors,beside = TRUE)
legend("topleft", categories, cex = .6, fill = colors)
title(sub='multiple bar diagram')
box()

#CONCLUSION :-
#from this table we Concludes that:
#(1) If you don't have any internships Your chances will be less.
#(2) One internship is not enough.
#(3) Two internships make your chances greater.

#############################################################################

#Do students with more projects have a higher chance of getting placed or not?

#Project Effect On Number Of Placement of the Students

placement_data <- data_set
Projects_effect <- table(placement_data$Projects, placement_data$PlacementStatus)
print(Projects_effect)

levels(factor(data_set$Projects))
number_of_placements=
  matrix(c(12,2949,1750,1092,18,598,800,2781),
         nrow = 2,ncol = 4,byrow = TRUE)
colors=c("yellow","violet")
number_of_projects=c("0","1","2","3") 
categories=c("Not Placed","Placed")  
barplot(number_of_placements,main = "Project's Effect On Placement Status",
        names.arg =number_of_projects,xlab = "Number Of Project",
        ylab = "Number Of Students",ylim=c(0,3500),col = colors,beside = TRUE) 
legend("topleft", categories, cex = .6, fill = colors)
title(sub='multiple bar diagram')
box()

#CONCLUSION :-
#from this table we Concludes that:
#(1) Most people do projects
#(2) Three projects make your chances greater

#############################################################################

#Do students with more workshop have a higher chance of getting placed or not?

#Workshop Effect On Number Of Placement of the Students

placement_data <- data_set
Workshops_effect <- table(placement_data$Workshops, placement_data$PlacementStatus)
print(Workshops_effect)

levels(factor(data_set$Workshops))
number_of_placements=
  matrix(c(2678,2111,890,124,1061,563,2413,160),
         nrow = 2,ncol = 4,byrow = TRUE)
colors=c("green","grey")
number_of_workshops=c("0","1","2","3") 
categories=c("Not Placed","Placed")  
barplot(number_of_placements,main = "Workshop's Effect On Placement Status",
        names.arg =number_of_workshops,xlab = "Number Of Workshop",
        ylab = "Number Of Placement",ylim=c(0,4000),col = colors,beside = TRUE) 
legend("topleft", categories, cex = .6, fill = colors)
title(sub='multiple bar diagram')
box()

#CONCLUSION:
#from this table we Concludes that:
#(1) Few people take three workshops
#(2) More than one workshop makes your chances greater

##############################################################################

library(ggplot2)

#Do students with higher aptitude score have a higher chance of placement?

#Aptitude Test Score Effect on Placement Status

ggplot(data_set, aes(x = PlacementStatus, y = AptitudeTestScore)) +
  geom_boxplot() +
  labs(x = "Placement Status", y = "Aptitude Test Score") +
  ggtitle("Box Plot of Aptitude Test Score by Placement Status")

#CONCLUSION:
#Placed students generally tend to have higher aptitude test scores. 
#The upper quartile (top of the box) for placed students is higher than
#the upper quartile for not placed students, indicating that a significant
#portion of placed students scored well in the aptitude test.

#The median aptitude test score for placed students is higher, indicating that 
#half of the placed students scored above this value.

#The median aptitude test score for not placed students is lower than that of 
#placed students. This means that half of the not placed students scored below
#this value.

#Not placed students have more low outliers, indicating that there are students
#with very low aptitude test scores who are not placed.

#The aptitude test score appears to be a significant factor in determining placement
#status. Students with higher scores are more likely to be placed, while students
#with lower scores face a higher risk of not being placed. 
##############################################################################



#Do students with higher SSC marks have a higher chance of placement?

#SSC marks Effect on Placement Status

ggplot(data_set, aes(x = PlacementStatus, y = SSC_Marks)) +
  geom_boxplot() +
  labs(x = "Placement Status", y = "SSC_Marks") +
  ggtitle("Box Plot of SSC Marks by Placement Status")

#CONCLUSION:
# Placed students tend to have higher median SSC marks compared to not placed 
#students. This suggests that students with relatively higher SSC marks are more
#likely to be placed.

#Not placed students have a lower median SSC mark compared to placed students.
#This implies that a significant portion of not placed students has lower SSC marks.

#SSC marks appear to be a crucial factor in determining placement status. Higher
#SSC marks increase the chance of being placed, while lower marks decrease 
#the chances of placement.

##############################################################################

#Do students with higher HSC marks have a higher chance of placement?

#HSC marks Effect on Placement Status

ggplot(data_set, aes(x = PlacementStatus, y = HSC_Marks)) +
  geom_boxplot() +
  labs(x = "Placement Status", y = "HSC Marks") +
  ggtitle("Box Plot of HSC Marks by Placement Status")

#CONCLUSION:
#Placed students tend to have higher median HSC marks compared to not placed
#students. This indicates that students with relatively higher HSC marks are more
#likely to be placed.

#Not placed students have a lower median HSC mark compared to placed students. 
#This implies that a significant portion of not placed students has lower HSC marks.

# HSC marks appear to be a crucial factor in determining placement status. Higher 
#HSC marks increase the likelihood of being placed, while lower marks decrease
#the chances of placement.

##############################################################################

#What is the effect of Extracurricular Activity On placement status?

#Effect Of Extracurricular Activity On Placement Status

placement_data <- data_set
Extracurricular_Activity_effect <- table(placement_data$ExtracurricularActivities, placement_data$PlacementStatus)
print(Extracurricular_Activity_effect)

library(ggplot2)
library(gridExtra)  

data_set <- read.csv("placement.csv")

with_extracurricular <- subset(data_set, ExtracurricularActivities == "Yes")
without_extracurricular <- subset(data_set, ExtracurricularActivities == "No")

calculate_percentages <- function(data) {
  placement_counts <- table(data$PlacementStatus)
  percentages <- prop.table(placement_counts) * 100
  return(percentages)
}

with_extracurricular_percentages <- calculate_percentages(with_extracurricular)
without_extracurricular_percentages <- calculate_percentages(without_extracurricular)

draw_pie_chart <- function(percentages, title) {
  ggplot(data=NULL, aes(x = "", y = percentages, fill = names(percentages))) +
    geom_bar(width = 1, stat = "identity") +
    geom_text(aes(label = paste0(round(percentages, 2), "%")), position = position_stack(vjust = 0.5)) +
    coord_polar("y") +
    labs(fill = "Placement Status", title = title) +
    theme_void()
}

p1 <- draw_pie_chart(with_extracurricular_percentages, "With Extracurricular Activities: Placement Status")
p2 <- draw_pie_chart(without_extracurricular_percentages, "Without Extracurricular Activities: Placement Status")

grid.arrange(p1, p2, ncol = 2)


#CONCLUsion :-

#The pie chart for students with extracurricular activities shows a larger portion
#of the pie as placed. This suggests that students who are involved in
#extracurricular activities have a higher likelihood of being placed.

#The pie chart for students without extracurricular activities shows a smaller
#portion of the pie as placed. This indicates that students without
#extracurricular involvement have a comparatively lower placement rate.

###################################################################################

#What is the effect of Placement Training On placement status?

#Effect Of Placement Training On Placement Status

placement_data <- data_set
Placement_Training_effect <- table(placement_data$PlacementTraining, placement_data$PlacementStatus)
print(Placement_Training_effect)

library(ggplot2)
library(gridExtra)  

data_set <- read.csv("placement.csv")

with_placement_training <- subset(data_set, PlacementTraining == "Yes")
without_placement_training <- subset(data_set, PlacementTraining == "No")

calculate_percentages <- function(data) {
  placement_counts <- table(data$PlacementStatus)
  percentages <- prop.table(placement_counts) * 100
  return(percentages)
}

with_placement_training_percentages <- calculate_percentages(with_placement_training)
without_placement_training_percentages <- calculate_percentages(without_placement_training)

draw_pie_chart <- function(percentages, title) {
  ggplot(data=NULL, aes(x = "", y = percentages, fill = names(percentages))) +
    geom_bar(width = 1, stat = "identity") +
    geom_text(aes(label = paste0(round(percentages, 2), "%")), position = position_stack(vjust = 0.5)) +
    coord_polar("y") +
    labs(fill = "Placement Status", title = title) +
    theme_void()
}

p1 <- draw_pie_chart(with_placement_training_percentages, "With placement Training: Placement Status")
p2 <- draw_pie_chart(without_placement_training_percentages, "Without placement Training: Placement Status")

grid.arrange(p1, p2, ncol = 2)

#CONCLUSION:
#The pie chart for students with placement training shows a larger portion of 
#the pie as placed. This suggests that students who received placement training
#have a higher likelihood of being placed.

#The pie chart for students without placement training shows a smaller portion
#of the pie as placed. This indicates that students without placement training 
#have a comparatively lower placement rate.

# The data indicates a positive correlation between participation in placement
#training programs and placement rates.
##################################################################################

#Is there any relation between soft skill rating and placement status?

#effect of soft skill rating on placement status:-

library(ggplot2)
ggplot(data_set, aes(x = SoftSkillsRating, fill = PlacementStatus)) +
  geom_bar(position = "dodge") +
  labs(x = "Soft Skills Rating", y = "Count", fill = "Placement Status") +
  ggtitle("Relationship between Soft Skills Rating and Placement Status") +
  theme_minimal()+
  scale_x_continuous(breaks = seq(0, 5, by = 0.1)) +
  scale_y_continuous(breaks = seq(0, 2000, by = 500), limits = c(0, 2000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#CONCLUSION :-
#The students who are having soft skill rating below 4 are very less likely to be
#placed.... but the students who are having higher soft skill rating especially
#those who have a soft skill rating between 4.5 to 5 have a greater chance to be 
#to be placed.This indicates that students with better-developed soft skills are 
#more likely to be placed.

