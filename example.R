# install IPAQlong

install.packages("devtools")
devtools::install_github("Mariana-plr/IPAQlong")
library("IPAQlong")

# generate example data (IPAQ long form): participants in rows, questions 1:25 (parts 1-4) in columns

data <- matrix(NA,nrow = 3, ncol = 25)

data[,1] <- c(1,1,1) # response to question 1 is either yes-1 or no-0

for (i in 2:25){ 
  if (i%%2 == 0) {
  data[,i] <- sample(1:7,3) # questions with an even number refer to days per week
  } else {
    data[,i] <- sample(1:200,3) # questions with an odd number refer to minutes or hours per day. Hours should be converted to minutes
  }
}

data <- as.data.frame(data) # input to IPAQlong functions should be a dataframe object

# Calculate scores using ipaq_scores function and save results in an object called "ipaq_scores"

ipaq_scores <- IPAQlong::ipaq_scores(data = data, truncate = F)  

# Calculate subscores using ipaq_subscores function and save results in an object called "ipaq_subscores"

ipaq_subscores <- IPAQlong::ipaq_subScores(data = data)





