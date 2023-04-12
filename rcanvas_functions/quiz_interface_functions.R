

library(httr)



#returns a specific quiz id or all quizzes in the course if quiz ID not specified.
get_quizzes <- function(course_id, domain = "byui.instructure.com", quiz_title = NULL) {
  quizzes_url <- sprintf("https://%s/api/v1/courses/%s/quizzes/", domain, course_id)
  
  quiz <- GET(quizzes_url, add_headers("Authorization" = paste0("Bearer ", API_token)))
  if(is.null(quiz_title)) {
   return(content(quiz)) 
  }
  quizzes <- content(quiz)
  for(i in 1:length(quizzes)) {
    title <- quizzes[[i]]$title
    if(stringr::str_detect(title, quiz_title) == TRUE) {
      return(quizzes[[i]]$id)
    }
    print(title)
  }
  return("No such quiz title found. Check spelling and capitalization.")
}

#returns the id of either the student or item analyis for a given quiz
get_report_id <- function(course_id, quiz_id, type = "student_analysis", domain = "byui.instructure.com") {
  reports_url <- sprintf("https://%s/api/v1/courses/%s/quizzes/%s/reports", domain, course_id, quiz_id)
  reports <- content(GET(reports_url, add_headers("Authorization" = paste0("Bearer ", API_token))))

  if(type == "student_analysis") {
    return(reports[[1]]$id)
  }
  else if(type == "item_analysis") {
    return(reports[[2]]$id)
  }
  return("type not valid. Reports are either 'student_analysis' or 'item_analysis'.")
}

#returns the student analysis (as dataframe) for a given quiz
get_student_analysis <- function(course_id, quiz_id, report_id = NULL, domain = "byui.instructure.com") {
  if(is.null(report_id) == TRUE) {
    report_id = get_report_id(course_id, quiz_id)
  }
  report_url <- sprintf("https://%s/api/v1/courses/%s/quizzes/%s/reports/%s?include[]=file&include[]=progress",domain, course_id, quiz_id, report_id)
  report <- GET(report_url, add_headers("Authorization" = paste0("Bearer ", API_token)))
  reports <- content(report)
  
  analysis <- GET(reports$file$url, add_headers("Authorization" = paste0("Bearer ", API_token)))
  content(analysis)
}









