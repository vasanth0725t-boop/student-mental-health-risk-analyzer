set.seed(2025)

student_count <- 300
departments <- c("CSE", "AI&DS", "ECE", "EEE", "MECH")
department <- sample(departments, student_count, replace = TRUE)
gender <- sample(c("Female", "Male"), student_count, replace = TRUE, prob = c(0.46, 0.54))
year <- sample(1:4, student_count, replace = TRUE, prob = c(0.24, 0.25, 0.25, 0.26))

cgpa <- round(pmin(9.9, pmax(5.1, rnorm(student_count, mean = 7.65, sd = 0.9))), 2)
attendance <- round(pmin(100, pmax(55, rnorm(student_count, mean = 82, sd = 9))), 1)
study_hours <- round(pmin(9, pmax(1, rnorm(student_count, mean = 4.8, sd = 1.5))), 1)
internship <- sample(c("Yes", "No"), student_count, replace = TRUE, prob = c(0.38, 0.62))
backlogs <- sample(0:4, student_count, replace = TRUE, prob = c(0.57, 0.22, 0.12, 0.06, 0.03))
technical_score <- round(pmin(100, pmax(35, rnorm(student_count, mean = 72 + (cgpa - 7.5) * 3, sd = 11))))
communication_score <- round(pmin(100, pmax(35, rnorm(student_count, mean = 70, sd = 12))))

department_effect <- c(CSE = 0.10, `AI&DS` = 0.08, ECE = 0.03, EEE = -0.02, MECH = -0.08)
placement_probability <- plogis(
  -1.5 +
    (cgpa - 7) * 0.75 +
    (technical_score - 70) * 0.035 +
    (communication_score - 70) * 0.02 +
    (attendance - 80) * 0.018 +
    ifelse(internship == "Yes", 0.55, 0) -
    backlogs * 0.55 +
    department_effect[department]
)
placement_status <- ifelse(
  runif(student_count) < placement_probability,
  "Placed",
  "Not Placed"
)

salary <- ifelse(
  placement_status == "Placed",
  round(pmax(
    2.4,
    3.2 + (cgpa - 7) * 0.8 + (technical_score - 70) * 0.045 +
      rnorm(student_count, mean = 0, sd = 0.8)
  ), 1),
  NA_real_
)

student_names <- c(
  "Aarav Sharma", "Aanya Patel", "Aditya Reddy", "Ananya Iyer", "Arjun Nair",
  "Diya Mehta", "Ishaan Rao", "Kavya Menon", "Rohan Das", "Saanvi Gupta",
  "Vihaan Kumar", "Zoya Khan", "Devika Joshi", "Karthik Bose", "Meera Shah",
  "Nikhil Verma", "Priya Singh", "Rahul Pillai", "Tara Krishnan", "Yash Malhotra"
)

students <- data.frame(
  Student_ID = sprintf("STU%04d", seq_len(student_count)),
  Name = sample(student_names, student_count, replace = TRUE),
  Gender = gender,
  Department = department,
  Year = year,
  CGPA = cgpa,
  Attendance = attendance,
  Study_Hours = study_hours,
  Internship = internship,
  Backlogs = backlogs,
  Technical_Score = technical_score,
  Communication_Score = communication_score,
  Placement_Status = placement_status,
  Salary = salary,
  stringsAsFactors = FALSE
)

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grepl("^--file=", script_args)][1])
project_dir <- if (!is.na(script_file) && nzchar(script_file)) {
  dirname(normalizePath(script_file))
} else {
  getwd()
}
output_file <- file.path(project_dir, "data", "students.csv")
write.csv(students, output_file, row.names = FALSE, na = "")
message("Created ", nrow(students), " student records at ", output_file)
