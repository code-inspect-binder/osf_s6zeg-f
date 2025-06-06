# R Visualization code for Attachment and Criticism manuscript figures.
# Manuscript submitted to Scientific Reports, May 2020.
# Code written by Jeffrey J. Kim. jeffrey.kim@uqconnect.edu.au

setwd("/Users/jefferykim/DROPBOX/4. JAffetive - Criticism") # specify your own working directory.

library("ggplot2")
library("DEGreport") # for an obselete correlation function: not used in the current plots.
library("grid")
library("cowplot")
library("dplyr")
library("readr")

# Import the dataset and print the names of variables
mydata = read.csv("dataset.csv")
names(mydata)

mydata.cor = cor(mydata,use ="complete.obs")

   # Graph the correlation between secure attachment and visual cortex response
   ggplot(mydata, aes(SECURE.ATTACHMENT, LINGUAL.GYRUS..VISUAL.CORTEX.), scale="globalminmax") +
   geom_smooth(method = "lm", fill = "green", alpha = 0.6)+
   geom_point(size =5 ) +
   theme_minimal()+
   theme_bw() + 
   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
   labs(title = "Visual Cortex & Secure Attachment", x = "Secure Attachment", y = "Lingual Gyrus Activation")
   scale_x_continuous(limits = c(1,5), breaks = c(1,2,3,4,5))
   
   # Graph the correlation between avoidant attachment and visual cortex response
   ggplot(mydata, aes(AVOIDANT.ATTACHMENT, LINGUAL.GYRUS..VISUAL.CORTEX.), scale="globalminmax") +
   geom_smooth(method = "lm", fill = "red")+
   geom_point(size = 5) + 
   theme_minimal()+
   theme_bw() + 
   theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
   labs(title = "Visual Cortex & Avoidant Attachment", x = "Avoidant Attachment", y = "Lingual Gyrus Activation")+
   scale_x_continuous(limits = c(1,5), breaks = c(1,2,3,4,5))
   
   # This plot should not be overly interpreted: anxious subscale had a poor internal consistency.
   ggplot(mydata, aes(ANXIOUS.ATTACHMENT, LINGUAL.GYRUS..VISUAL.CORTEX.), scale="globalminmax") +
      geom_smooth(method = "lm", fill = "darkblue", alpha = 0.3)+
      geom_point(size = 5) + 
      theme_minimal()+
      theme_bw() + 
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
      labs(title = "Visual Cortex & Anxious Attachment", x = "Anxious Attachment", y = "Lingual Gyrus Activation")+
      scale_x_continuous(limits = c(1,5), breaks = c(1,2,3,4,5))
   
   
 # https://cran.r-project.org/web/packages/interactions/vignettes/interactions.html
   install.packages("interactions")
   library(interactions)
   
   library(jtools) # for summ() function
   
   # Build and print regression models to use for interaction plots.
   states <- as.data.frame(mydata)
   fiti <- lm(LINGUAL.GYRUS..VISUAL.CORTEX. ~ AMYGDALA * AVOIDANT.ATTACHMENT, data = states)
   summ(fiti)
   
   fiti2 <- lm(LINGUAL.GYRUS..VISUAL.CORTEX. ~ AMYGDALA * SECURE.ATTACHMENT, data = states)
   summ(fiti2)
   
   # Graph the interaction plot of amygdala and visual cortex activation, with AVOIDANT attachment as the moderator.
  p1 = interact_plot(fiti, pred = AMYGDALA, modx = AVOIDANT.ATTACHMENT,robust = FALSE,
                 x.label = "Amygdala", y.label = "LG Visual Cortex",
                 main.title = "Avoidant Attachment",  legend.main = "Avoidant Levels",
                colors = "red",interval = TRUE,
                int.width = 0.8)+
     theme_bw() + 
     theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  p1

  # Graph the interaction plot of amygdala and visual cortex activation, with SECURE attachment as the moderator.
  p2 = interact_plot(fiti2, pred = AMYGDALA, modx = SECURE.ATTACHMENT,robust = FALSE,
                     x.label = "Amygdala", y.label = "LG Visual Cortex",
                     main.title = "Secure Attachment",  legend.main = "Secure Levels",
                     colors = "green",interval = TRUE,
                     int.width = 0.8)+
     theme_bw() + 
     theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  p2
  
  