EDA
================
Atshaya Suresh

``` r
setwd("/Users/p/Downloads/R-ND")
getwd()
```

    ## [1] "/Users/p/Downloads/R-ND"

``` r
install.packages("tidyverse")
```

    ## Warning: unable to access index for repository [https://cloud.r-project.org](https://cloud.r-project.org)/src/contrib:
    ##   cannot open URL '[https://cloud.r-project.org](https://cloud.r-project.org)/src/contrib/PACKAGES'

    ## Warning: package 'tidyverse' is not available for this version of R
    ## 
    ## A version of this package for your version of R might be available elsewhere,
    ## see the ideas at
    ## https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

    ## Warning: unable to access index for repository [https://cloud.r-project.org](https://cloud.r-project.org)/bin/macosx/big-sur-x86_64/contrib/4.4:
    ##   cannot open URL '[https://cloud.r-project.org](https://cloud.r-project.org)/bin/macosx/big-sur-x86_64/contrib/4.4/PACKAGES'

``` r
library(tidyverse)
```

    ## Warning: package 'dplyr' was built under R version 4.4.3

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
advisor_roster <- tibble(
  Advisor_ID = 1:6,
  Advisor_Name = c("Alex Miller", "Sam Rivera", "Jordan Chen", 
                   "Taylor Reed", "Casey Morgan", "Jamie Bell"),
  
  
  Specialty = c("General", "General", "Career", 
                "Financial Aid", "General", "International"),
  
  
  Max_Weekly_Hours = c(40, 40, 20, 40, 30, 40), 
  
  
  Employee_Type = c("Full-Time", "Full-Time", "Part-Time", 
                    "Full-Time", "Contract", "Full-Time")
)

write_csv(advisor_roster, "advisor_roster.csv")
```

``` r
install.packages("lubridate")
```

    ## Warning: unable to access index for repository [https://cloud.r-project.org](https://cloud.r-project.org)/src/contrib:
    ##   cannot open URL '[https://cloud.r-project.org](https://cloud.r-project.org)/src/contrib/PACKAGES'

    ## Warning: package 'lubridate' is not available for this version of R
    ## 
    ## A version of this package for your version of R might be available elsewhere,
    ## see the ideas at
    ## https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

    ## Warning: unable to access index for repository [https://cloud.r-project.org](https://cloud.r-project.org)/bin/macosx/big-sur-x86_64/contrib/4.4:
    ##   cannot open URL '[https://cloud.r-project.org](https://cloud.r-project.org)/bin/macosx/big-sur-x86_64/contrib/4.4/PACKAGES'

``` r
install.packages("dplyr")
```

    ## Warning: unable to access index for repository [https://cloud.r-project.org](https://cloud.r-project.org)/src/contrib:
    ##   cannot open URL '[https://cloud.r-project.org](https://cloud.r-project.org)/src/contrib/PACKAGES'

    ## Warning: package 'dplyr' is not available for this version of R
    ## 
    ## A version of this package for your version of R might be available elsewhere,
    ## see the ideas at
    ## https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

    ## Warning: unable to access index for repository [https://cloud.r-project.org](https://cloud.r-project.org)/bin/macosx/big-sur-x86_64/contrib/4.4:
    ##   cannot open URL '[https://cloud.r-project.org](https://cloud.r-project.org)/bin/macosx/big-sur-x86_64/contrib/4.4/PACKAGES'

``` r
install.packages("readr")
```

    ## Warning: unable to access index for repository [https://cloud.r-project.org](https://cloud.r-project.org)/src/contrib:
    ##   cannot open URL '[https://cloud.r-project.org](https://cloud.r-project.org)/src/contrib/PACKAGES'

    ## Warning: package 'readr' is not available for this version of R
    ## 
    ## A version of this package for your version of R might be available elsewhere,
    ## see the ideas at
    ## https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages

    ## Warning: unable to access index for repository [https://cloud.r-project.org](https://cloud.r-project.org)/bin/macosx/big-sur-x86_64/contrib/4.4:
    ##   cannot open URL '[https://cloud.r-project.org](https://cloud.r-project.org)/bin/macosx/big-sur-x86_64/contrib/4.4/PACKAGES'

``` r
library(lubridate)
library(dplyr) # This contains case_when
library(readr) # This contains write_csv

# 1. Set the date range for a full Fall semester
start_date <- as.Date("2025-08-01")
end_date <- as.Date("2025-12-20")
all_dates <- seq(start_date, end_date, by="day")

# 2. Define "Weights" to create peak seasons
# case_when checks conditions in order
date_weights <- case_when(
  month(all_dates) == 8 ~ 0.5,  # August Rush
  month(all_dates) == 10 ~ 0.4, # Midterm Planning
  TRUE ~ 0.1                    # Everything else
)

# 3. Generate 1,500 Requests
set.seed(42) 
student_requests <- data.frame(
  Request_ID = 1001:2500,
  Student_ID = sample(5000:9000, 1500, replace = TRUE),
  Request_Date = sample(all_dates, 1500, replace = TRUE, prob = date_weights),
  Appt_Type = sample(c("General", "Career", "Financial Aid", "International"), 
                     1500, replace = TRUE, prob = c(0.5, 0.2, 0.2, 0.1)),
  Urgency = sample(1:5, 1500, replace = TRUE)
)

# Arrange by date to make it look like a log
student_requests <- student_requests %>% arrange(Request_Date)

# Save the file
write.csv(student_requests, "student_requests.csv", row.names = FALSE)

# Check the first few rows
head(student_requests)
```

    ##   Request_ID Student_ID Request_Date     Appt_Type Urgency
    ## 1       1221       8739   2025-08-01       General       4
    ## 2       1247       6405   2025-08-01 Financial Aid       2
    ## 3       1512       6602   2025-08-01       General       2
    ## 4       1667       6272   2025-08-01        Career       1
    ## 5       1756       5721   2025-08-01       General       1
    ## 6       1799       7721   2025-08-01        Career       2

``` r
# 1. Load libraries
library(dplyr)
library(lubridate)

# 2. Read the previous tables (or use the objects if still in environment)
# student_requests <- read.csv("student_requests.csv")
# advisor_roster <- read.csv("advisor_roster.csv")

# 3. Generate Outcomes with a "Full Schedule" penalty
historical_appointments <- student_requests %>%
  mutate(
    # Create a wait time based on the month (Seasonality)
    # August (8) and October (10) get long wait times
    wait_penalty = case_when(
      month(Request_Date) == 8  ~ sample(10:15, n(), replace = TRUE),
      month(Request_Date) == 10 ~ sample(7:12, n(), replace = TRUE),
      TRUE                      ~ sample(1:4, n(), replace = TRUE)
    ),
    
    # Calculate the actual Appointment Date
    Appt_Date = Request_Date + days(wait_penalty),
    
    # Assign a random Advisor (We'll optimize this assignment later!)
    Advisor_ID = sample(1:6, n(), replace = TRUE),
    
    # Satisfaction drops as wait time increases
    Satisfaction_Score = pmax(1, 10 - (wait_penalty / 1.5) + rnorm(n(), 0, 1))
  ) %>%
  select(Request_ID, Student_ID, Advisor_ID, Appt_Type, 
         Request_Date, Appt_Date, Wait_Time_Days = wait_penalty, Satisfaction_Score)

# 4. Save the final "Current State" data
write.csv(historical_appointments, "historical_appointments.csv", row.names = FALSE)

# Check the 'Pain Points'
summary(historical_appointments$Wait_Time_Days)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   7.000  10.000   9.061  12.000  15.000

``` r
# Load libraries
library(dplyr)

# 1. Join the Appointments with the Advisor details
# This lets us see if the 'Specialty' matched the 'Appt_Type'
final_dataset <- historical_appointments %>%
  left_join(advisor_roster, by = "Advisor_ID") %>%
  mutate(
    # Create a flag: Was the student seen by the RIGHT specialist?
    Specialist_Match = ifelse(Appt_Type == Specialty, "Match", "Mismatched"),
    
    # Create a 'Peak Season' label for easy Tableau filtering
    Season = ifelse(month(Request_Date) %in% c(8, 10), "Peak", "Off-Peak")
  )

# 2. Save this as your "Tableau_Ready_Data.csv"
write.csv(final_dataset, "Tableau_Ready_Data.csv", row.names = FALSE)

# Check the result
print(head(final_dataset))
```

    ##   Request_ID Student_ID Advisor_ID     Appt_Type Request_Date  Appt_Date
    ## 1       1221       8739          5       General   2025-08-01 2025-08-13
    ## 2       1247       6405          4 Financial Aid   2025-08-01 2025-08-16
    ## 3       1512       6602          1       General   2025-08-01 2025-08-12
    ## 4       1667       6272          6        Career   2025-08-01 2025-08-14
    ## 5       1756       5721          5       General   2025-08-01 2025-08-11
    ## 6       1799       7721          2        Career   2025-08-01 2025-08-12
    ##   Wait_Time_Days Satisfaction_Score Advisor_Name     Specialty Max_Weekly_Hours
    ## 1             12           2.232979 Casey Morgan       General               30
    ## 2             15           1.000000  Taylor Reed Financial Aid               40
    ## 3             11           2.592016  Alex Miller       General               40
    ## 4             13           2.332026   Jamie Bell International               40
    ## 5             10           3.975936 Casey Morgan       General               30
    ## 6             11           3.625846   Sam Rivera       General               40
    ##   Employee_Type Specialist_Match Season
    ## 1      Contract            Match   Peak
    ## 2     Full-Time            Match   Peak
    ## 3     Full-Time            Match   Peak
    ## 4     Full-Time       Mismatched   Peak
    ## 5      Contract            Match   Peak
    ## 6     Full-Time       Mismatched   Peak
