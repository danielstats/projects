#unit 1
unit_1_analysis <- read_csv("data/Unit 1 Exam (Remotely Proctored) Quiz Student Analysis Report.csv")
#unit 2
unit_2_analysis <- read_csv("data/unit2_student_analysis.csv")
#unit 1 outcomes which I randomly assigned to each questions
unit_1_outcomes <- read_csv("data/unit1outcomes.csv")

#generate data frame of all the questions and their associated point values (for future use)
questions_from_analysis <- function(student_analysis) {
  col_names <- names(student_analysis)
  questions <- data.frame(question = "NA", points = 0)
  for(i in 1:length(col_names)) {
    if(stringr::str_detect(col_names[i], "\\d{8}:") == TRUE) {
      q <- data.frame(question = col_names[i], #question col is name of question
                      #next item in column names list is the number of points associated with the question
                      points = as.numeric(stringr::str_extract(col_names[i+1], "\\d[:punct:]\\d{1,}")))
      questions <- rbind(questions, q)
    }
  }
  questions <- tidyr::separate(questions, question, into = c("id", "question"), 
                               sep = ": ", extra = "merge", fill = "right")
  return(questions[-1,])
}

#generate data frame of questions each student missed
student_missed_questions <- function(st_name, student_analysis, outcomes = NULL) {
  cols <- names(student_analysis)
  # filter to only one student
  student <- dplyr::filter(student_analysis, name == st_name)
  #create list of missed questions
  missed_questions <- data.frame(id = rep(NA, length(cols)))
  for(i in 1:length(cols)){
    #check if column is a question
    if(stringr::str_detect(cols[i], "\\d{8}:") == TRUE){
      #pull the number of points available for that question (next item in column names)
      points = as.numeric(stringr::str_extract(cols[i+1], "\\d[:punct:]\\d{1,}"))
      #check if student scored that number of points
      if(points != student[1,i+1]) {
        #if not, add this column's id to the list of questions missed
        missed_questions$id[i] <- str_sub(cols[i], 1,8) #all question ids are 8 digits
        }
      }
    } #If the student got 100%, return string (prevents breaking during join)
  if(is.logical(missed_questions$id) == TRUE){
    return("Student missed 0 questions")
  } #otherwise, add the details of the questions and return a data frame of each question missed
    missed_questions <- missed_questions |> 
      tidyr::drop_na() |> #only return the missed questions, not all Qs
      dplyr::left_join(questions_from_analysis(student_analysis), by = "id")
    if(is.null(outcomes) == FALSE) { #where outcomes are provided, join them to the questions
      missed_questions$id <- as.numeric(missed_questions$id) #id needs to be double because blah
      return(dplyr::left_join(missed_questions, outcomes, by = c("id", "points", "question")))
    }
    return(missed_questions) 
}

#generate list of all student's missed questions for the given quiz
student_missed_report <- function(student_analysis){
  tibble(name = student_analysis$name,
         missed = purrr:map(student_analysis$name, student_missed_questions, student_analysis))
} 


#create data frame of most-missed questions
most_missed_questions <- function(student_analysis, outcomes = NULL) {
  missed_questions <- NULL
  for(i in student_analysis$name) {
    indv_missed <- student_missed_questions(i, student_analysis)
    if(is.vector(indv_missed) == FALSE){
      missed_questions <- c(missed_questions, indv_missed$id)
      }
  }
  ques <- sort(table(missed_questions), decreasing = TRUE)
  questions <- data.frame(id = names(ques), #question id
                          num_missed = as.vector(ques), #number of students who missed this question
                          percent_missed = (as.vector(ques)/nrow(student_analysis) * 100)) |>
    dplyr::left_join(questions_from_analysis(student_analysis), by = "id") |>  #add question details
    select(question, percent_missed, num_missed, points, id)
  if(is.null(outcomes) == FALSE) {
    return(dplyr::left_join(questions, outcomes, by = c("id", "points", "question")))
  }
 return(questions) 
}




