#' @title IPAQlong
#'
#' @description Calculates the domain and intensity sub scores for the 'International Physical Activity Questionnaire (IPAQ)'
#' long Form <http://www.ipaq.ki.se>
#'
#' @param ipaqdata A data frame object containing 25 columns with the replies to the IPAQ long format (parts 1-4).
#' Yes/no replies should be categorized as yes-1, no-0. Time should be in minutes. Sub scores are calculated for all cases, even when there are missing values.
#'
#' @param truncate Logical vector. If TRUE all walking, moderate and vigorous time variables are truncated following the IPAQ short rule.
#' Variables exceeding 180 minutes are truncated to be equal to 180 minutes.
#'
#' @return A data frame object with the domain and intensity sub scores in metabolic equivalent of task (MET)/week. Domain sub scores: work, transportation, domestic and leisure.
#' Intensity sub scores: walking, moderate and vigorous.
#'
#' @export
#'
#' @references
#' The IPAQ Group (2005). Guidelines for Data Processing and Analysis of the International Physical Activity Questionnaire. Retrieved from (\url{http://www.ipaq.ki.se})
#'
#'
ipaq_subScores <- function(data, truncate= F){
library(dplyr)

if (length(names(data)) != 25) {
  stop("Number of columns needs to be 25")
}

scores_data <- as.data.frame(data)
scores_data[,1:25] <- lapply(scores_data[,1:25], as.numeric)
scores_data <- scores_data[which(complete.cases(scores_data)),]

if (truncate== T) {
  for (i in c(7,13,21,5,11,15,17,19,25,3,23)){
    scores_data[which(scores_data[,i]>=180), i] <- 180
  }
}

else {
  scores_data$total_walk <- rowSums(scores_data[, c(7,13,21)], na.rm = T)
  scores_data$total_mod <- rowSums(scores_data[, c(5,11,15,17,19,25)], na.rm = T)
  scores_data$total_vig <- rowSums(scores_data[, c(3,23)], na.rm = T)
  scores_data$total_min <- rowSums(scores_data[, c("total_walk", "total_mod", "total_vig")], na.rm = T)
  scores_data$total_mod_walk <- rowSums(scores_data[, c("total_walk", "total_mod")], na.rm = T)
}

#work

scores_data$w_walk <- ifelse(scores_data[,1]==2,0,
                             ifelse(scores_data[,6]==0, 0,
                                    ifelse(scores_data[,6]>0 & scores_data[,7]>0, 3.3*scores_data[,6]*scores_data[,7], NA)))

scores_data$w_mod <- ifelse(scores_data[,1]==2,0,
                            ifelse(scores_data[,4]==0,0,
                                   ifelse(scores_data[,4]>0 & scores_data[,5]>0, 4*scores_data[,4]*scores_data[,5],NA)))

scores_data$w_vig <- ifelse(scores_data[,1]==2,0,
                            ifelse(scores_data[,2]==0, 0,
                                   ifelse(scores_data[,2]>0 & scores_data[,3]>0, 8*scores_data[,2]*scores_data[,3], NA)))

scores_data$work_total <- rowSums(scores_data[, c("w_walk","w_mod", "w_vig")], na.rm = T)


#transport

scores_data$t_walk <- ifelse(scores_data[,12]==0,0,
                             ifelse(scores_data[,12]>0 & scores_data[,13]>0, 3.3*scores_data[,12]*scores_data[,13], NA))

scores_data$t_bike <- ifelse(scores_data[,10]==0, 0,
                             ifelse(scores_data[,10]>0 & scores_data[, 11]>0, 6*scores_data[,10]*scores_data[,11],NA))

scores_data$transportation_total <- rowSums(scores_data[, c("t_walk","t_bike")], na.rm = T)

#domestic

scores_data$d_vig <- ifelse(scores_data[,14]==0, 0,
                            ifelse(scores_data[,14]>0 & scores_data[,15]>0, 5.5*scores_data[,14]*scores_data[,15],NA))

scores_data$d_mod_in <- ifelse(scores_data[,18]==0,0,
                               ifelse(scores_data[,18]>0 & scores_data[,19]>0, 3*scores_data[,18]*scores_data[,19],NA))

scores_data$d_mod_out <- ifelse(scores_data[, 16]==0,0,
                                ifelse(scores_data[,16]>0 & scores_data[,17]>0, 4*scores_data[,16]*scores_data[,17],NA))

scores_data$domestic_total <- rowSums(scores_data[, c("d_vig","d_mod_in", "d_mod_out")], na.rm = T)

#leisure

scores_data$l_walk <- ifelse(scores_data[,20]==0, 0,
                             ifelse(scores_data[,20]>0 & scores_data[,21]>0, 3.3*scores_data[,20]*scores_data[,21],NA))

scores_data$l_mod <- ifelse(scores_data[,24]==0,0,
                            ifelse(scores_data[,24]>0 & scores_data[,25]>0, 4*scores_data[,24]*scores_data[,25],NA))

scores_data$l_vig <- ifelse(scores_data[,22]==0,0,
                            ifelse(scores_data[,22]>0 & scores_data[,23]>0, 8*scores_data[,22]*scores_data[,23],NA))

scores_data$leisure_total <- rowSums(scores_data[, c("l_walk","l_mod","d_vig")], na.rm = T)

# continuous score (MET-minutes/week)

scores_data$walking_total <- rowSums(scores_data[, c("w_walk", "t_walk", "l_walk")], na.rm = T) # truncate to 4158?
scores_data$moderate_total <- rowSums(scores_data[, c("w_mod", "t_bike", "d_mod_in", "d_mod_out", "d_vig", "l_mod")], na.rm = T) # to 5040
scores_data$vigorous_total <- rowSums(scores_data[, c("w_vig", "l_vig")], na.rm = T) # to 10080


sub_scores <- scores_data[,c("work_total", "transportation_total", "domestic_total", "leisure_total", "walking_total", "moderate_total", "vigorous_total")]


return(sub_scores)
}
