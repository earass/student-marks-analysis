# importing ggplot2 library for plotting graphs
library("ggplot2")
library('glue')

# creating a list of categorical variables
cat_columns <- c("gender", "race.ethnicity", "parental.level.of.education", 
                 "lunch.fee", "test.preparation.course")

# creating a list of exam score columns
exam_score_columns <- c("science.score", "history.score", "english.score")
exams <- c('science', 'history', 'english')

sample_size = 150


# loading data
data <- read.csv('StudentsMarksData.csv')

# generating bar plots for each categorical variable
for (col in cat_columns){
  # creating freq table
  freq_df = data.frame(sort(table(data[col]), decreasing = TRUE))
  # creating plot object
  p <- ggplot(freq_df, aes(x=Var1, y=Freq)) + geom_bar(stat = "identity")
  # adding axis labels and styling
  p <- p + xlab(col) + ylab("Frequency") + theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold")
  )
  print(p)
}


# creating box plots for numeric columns
for (num_col in exam_score_columns){
  d <- c(data[[num_col]])
  boxplot(fivenum(d),
          horizontal = TRUE)
  lines(rep(mean(d), 2), c(0.8, 1.2), col="red", lwd = 2)
  text(x=fivenum(d), labels =fivenum(d), y=1.25)
  title(num_col)
  hist(d, main=glue("Histogram of {num_col}"), xlab = num_col)
}

# all exams box plots in one chart
boxplot(fivenum(data$science.score),
        fivenum(data$history.score),
        fivenum(data$english.score),
        names = c("Science", "History", "Engish"),
        horizontal = TRUE)


passing_marks <- 40
total_records <- nrow(data)

# iterate over the score columns to find the percent of students passed in each exam
for (exam in exams){
  col_name <- glue("{exam}.score")
  pass_cond <- data[col_name] >= passing_marks
  passed_perc <- (nrow(data[pass_cond, ]) * 100)/total_records
  print(glue("{passed_perc} percent students passed in the {exam} exam"))
}

# defining condition for students passing in all three exams
pass_cond_all_exams <- data$science.score >= passing_marks & 
  data$history.score >= passing_marks & data$english.score >= passing_marks
passed_all <- (nrow(data[pass_cond_all_exams, ]) * 100)/total_records
print(glue("{passed_all} percent students passed in all the exams"))


# finding stats for the score columns
for (exam in exams){
  col_name <- glue("{exam}.score")
  scores <- data[[col_name]]
  sample_scores <- sample(scores, sample_size)
  mean_val <- mean(scores, na.rm = TRUE)
  mean_val_samp <- mean(sample_scores, na.rm = TRUE)
  print(glue("Population Mean for {exam} exam is {mean_val}"))
  print(glue("Sample Mean for {exam} exam is {mean_val_samp}"))
  std_samp <- sd(scores)
  print(glue("Sample Standard deviation for {exam} exam is {std_samp}"))
  percentile_95 <- quantile(scores, .95)[[1]]
  print(glue("95th percentile for {exam} exam is {percentile_95}"))
  cat("\n")
}