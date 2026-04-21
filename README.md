Advising Optimization Project
================
Atshaya Suresh

``` r
options(repos = c(CRAN = "https://cran.rstudio.com/"))
```

``` r
# Install the optimization suite
install.packages("ompr")
```

    ## Error in install.packages : Updating loaded packages

``` r
install.packages("ompr.roi")
```

    ## Error in install.packages : Updating loaded packages

``` r
install.packages("ROI.plugin.glpk") # The solver engine
```

    ## Error in install.packages : Updating loaded packages

``` r
install.packages("dplyr")
```

    ## Error in install.packages : Updating loaded packages

``` r
install.packages("lubridate")
```

    ## Error in install.packages : Updating loaded packages

``` r
install.packages("rlang")
```

    ## Error in install.packages : Updating loaded packages

``` r
# Load libraries
library(dplyr)
library(lubridate)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)
```

``` r
setwd("/Users/p/Downloads/R-ND")
library(dplyr)
library(lubridate)
library(dplyr)
library(lubridate)

# 1. Load data
requests <- read.csv("student_requests.csv")

# 2. Convert and Clean Dates (The Fix)
requests_clean <- requests %>%
  #Making sure Request_Date data type is set to date
  mutate(Request_Date = as.Date(Request_Date)) %>%
  # Create a Week column based on Sunday start
  mutate(Week = floor_date(Request_Date, unit = "week"))

# 3. Identify the real "Super Week" 
# Instead of guessing the date, let's let R find the one with the most rows in August
super_week_date <- requests_clean %>%
  filter(month(Request_Date) == 8) %>%
  group_by(Week) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(Week)

print(paste("The actual busiest week starts on:", super_week_date))
```

    ## [1] "The actual busiest week starts on: 2025-08-24"

``` r
# 4. Now filter using that dynamic date
peak_week_data <- requests_clean %>%
  filter(Week == super_week_date)

# 5. Calculate Demand
demand_summary <- peak_week_data %>%
  group_by(Appt_Type) %>%
  tally(name = "Student_Requests") %>%
  mutate(Estimated_Labor_Hours = case_when(
    Appt_Type == "Career" ~ Student_Requests * 1.0,
    Appt_Type == "Financial Aid" ~ Student_Requests * 1.0,
    Appt_Type == "International" ~ Student_Requests * 0.75,
    Appt_Type == "General" ~ Student_Requests * 0.5,
    TRUE ~ Student_Requests * 0.5
  ))

print(demand_summary)
```

    ## # A tibble: 4 × 3
    ##   Appt_Type     Student_Requests Estimated_Labor_Hours
    ##   <chr>                    <int>                 <dbl>
    ## 1 Career                      30                    30
    ## 2 Financial Aid               28                    28
    ## 3 General                     78                    39
    ## 4 International               16                    12

``` r
install.packages("janitor")
```

    ## 
    ## The downloaded binary packages are in
    ##  /var/folders/r8/9svs0zzs5vsc59yzys847ldh0000gn/T//RtmpD4OrFt/downloaded_packages

``` r
library(dplyr)
library(lubridate)

# 1. Load data
hist_appts <- read.csv("historical_appointments.csv")

# 2. Convert date and create a week column
# We'll use a standard format and let R find the busiest week
hist_clean <- hist_appts %>%
  mutate(Appt_Date = as.Date(Appt_Date)) %>%
  mutate(Week_Start = floor_date(Appt_Date, unit = "week"))

# 3. DYNAMIC FIND: What is the actual busiest week in your data?
busiest_week <- hist_clean %>%
  count(Week_Start) %>%
  arrange(desc(n)) %>%
  slice(1) %>%
  pull(Week_Start)

print(paste("The data says the busiest week actually starts on:", busiest_week))
```

    ## [1] "The data says the busiest week actually starts on: 2025-08-17"

``` r
# 4. GET BASELINE FOR THAT WEEK
# (Using 'Wait_Time_Days' - update if your column name is different!)
baseline_report <- hist_clean %>%
  filter(Week_Start == busiest_week) %>%
  group_by(Appt_Type) %>%
  summarise(
    Appointments = n(),
    Avg_Wait = mean(Wait_Time_Days, na.rm = TRUE)
  ) %>%
  arrange(desc(Avg_Wait))

print("--- Baseline for the Real Super Week ---")
```

    ## [1] "--- Baseline for the Real Super Week ---"

``` r
print(baseline_report)
```

    ## # A tibble: 4 × 3
    ##   Appt_Type     Appointments Avg_Wait
    ##   <chr>                <int>    <dbl>
    ## 1 Financial Aid           28     12.5
    ## 2 General                 73     12.4
    ## 3 Career                  38     12.1
    ## 4 International           17     11.8

``` r
print(requests)
```

    ##     Request_ID Student_ID Request_Date     Appt_Type Urgency
    ## 1         1221       8739   2025-08-01       General       4
    ## 2         1247       6405   2025-08-01 Financial Aid       2
    ## 3         1512       6602   2025-08-01       General       2
    ## 4         1667       6272   2025-08-01        Career       1
    ## 5         1756       5721   2025-08-01       General       1
    ## 6         1799       7721   2025-08-01        Career       2
    ## 7         1836       8760   2025-08-01       General       3
    ## 8         1883       7018   2025-08-01       General       2
    ## 9         1942       5237   2025-08-01        Career       3
    ## 10        2035       6134   2025-08-01        Career       5
    ## 11        2071       5152   2025-08-01 International       5
    ## 12        2161       8015   2025-08-01       General       3
    ## 13        2170       6693   2025-08-01       General       5
    ## 14        2198       5416   2025-08-01       General       2
    ## 15        2215       8173   2025-08-01       General       5
    ## 16        2254       6788   2025-08-01        Career       2
    ## 17        2263       8477   2025-08-01        Career       2
    ## 18        2293       8131   2025-08-01 International       5
    ## 19        2392       6478   2025-08-01        Career       2
    ## 20        1171       5798   2025-08-02       General       5
    ## 21        1193       8649   2025-08-02        Career       3
    ## 22        1203       5161   2025-08-02       General       2
    ## 23        1245       6292   2025-08-02 Financial Aid       1
    ## 24        1494       5842   2025-08-02       General       3
    ## 25        1640       5825   2025-08-02       General       5
    ## 26        1659       7173   2025-08-02       General       2
    ## 27        1704       5863   2025-08-02 Financial Aid       4
    ## 28        1744       6034   2025-08-02       General       1
    ## 29        1780       6009   2025-08-02       General       5
    ## 30        1913       6376   2025-08-02        Career       1
    ## 31        2043       8462   2025-08-02        Career       4
    ## 32        2273       7608   2025-08-02 Financial Aid       2
    ## 33        2282       5405   2025-08-02 International       3
    ## 34        2342       8158   2025-08-02 International       3
    ## 35        2423       8209   2025-08-02 International       3
    ## 36        1007       5633   2025-08-03       General       1
    ## 37        1181       7811   2025-08-03 International       4
    ## 38        1495       6413   2025-08-03        Career       3
    ## 39        1549       6191   2025-08-03        Career       3
    ## 40        1648       5750   2025-08-03       General       2
    ## 41        1716       6335   2025-08-03       General       2
    ## 42        1800       8519   2025-08-03 Financial Aid       1
    ## 43        1882       5488   2025-08-03       General       1
    ## 44        1899       5029   2025-08-03       General       2
    ## 45        1952       5070   2025-08-03       General       4
    ## 46        2102       5373   2025-08-03       General       3
    ## 47        2291       6686   2025-08-03 Financial Aid       3
    ## 48        2319       7974   2025-08-03       General       3
    ## 49        2401       6325   2025-08-03        Career       3
    ## 50        2479       7523   2025-08-03       General       1
    ## 51        2492       6416   2025-08-03       General       3
    ## 52        1108       8043   2025-08-04        Career       4
    ## 53        1121       6781   2025-08-04        Career       2
    ## 54        1168       7805   2025-08-04        Career       4
    ## 55        1249       8134   2025-08-04        Career       4
    ## 56        1266       5704   2025-08-04       General       5
    ## 57        1318       8008   2025-08-04 International       2
    ## 58        1358       8497   2025-08-04 International       4
    ## 59        1387       8489   2025-08-04       General       1
    ## 60        1556       6142   2025-08-04       General       5
    ## 61        1643       7472   2025-08-04 International       4
    ## 62        1691       5614   2025-08-04 International       5
    ## 63        1712       6894   2025-08-04       General       3
    ## 64        1735       8772   2025-08-04       General       1
    ## 65        1828       5020   2025-08-04       General       2
    ## 66        1894       6689   2025-08-04        Career       2
    ## 67        1908       6949   2025-08-04 International       4
    ## 68        1960       8204   2025-08-04 Financial Aid       5
    ## 69        1980       8545   2025-08-04       General       3
    ## 70        2278       5658   2025-08-04       General       5
    ## 71        2292       5212   2025-08-04 Financial Aid       4
    ## 72        2398       7643   2025-08-04 Financial Aid       1
    ## 73        1138       6741   2025-08-05       General       2
    ## 74        1231       6578   2025-08-05       General       4
    ## 75        1265       6035   2025-08-05       General       3
    ## 76        1448       7178   2025-08-05       General       2
    ## 77        1661       7203   2025-08-05 International       5
    ## 78        1675       6956   2025-08-05 Financial Aid       2
    ## 79        1797       6171   2025-08-05       General       4
    ## 80        1966       7306   2025-08-05       General       4
    ## 81        2089       7237   2025-08-05 Financial Aid       4
    ## 82        2154       6760   2025-08-05       General       3
    ## 83        1091       8803   2025-08-06       General       4
    ## 84        1113       7449   2025-08-06 Financial Aid       4
    ## 85        1182       7830   2025-08-06 International       5
    ## 86        1339       8987   2025-08-06       General       5
    ## 87        1425       6077   2025-08-06 International       1
    ## 88        1490       8416   2025-08-06        Career       4
    ## 89        1532       5002   2025-08-06       General       2
    ## 90        1569       7917   2025-08-06       General       2
    ## 91        1591       8529   2025-08-06       General       5
    ## 92        1607       5742   2025-08-06        Career       3
    ## 93        1668       5764   2025-08-06 Financial Aid       5
    ## 94        1710       5102   2025-08-06       General       3
    ## 95        1801       6300   2025-08-06       General       5
    ## 96        1871       5479   2025-08-06 Financial Aid       1
    ## 97        1909       5174   2025-08-06       General       5
    ## 98        1950       8779   2025-08-06        Career       5
    ## 99        1987       7738   2025-08-06       General       2
    ## 100       2025       6271   2025-08-06 International       1
    ## 101       2180       8681   2025-08-06       General       4
    ## 102       2237       7029   2025-08-06        Career       1
    ## 103       2290       6095   2025-08-06 Financial Aid       5
    ## 104       2400       6238   2025-08-06       General       4
    ## 105       2408       8173   2025-08-06 International       1
    ## 106       1043       8719   2025-08-07 Financial Aid       1
    ## 107       1209       5905   2025-08-07       General       2
    ## 108       1263       5740   2025-08-07       General       1
    ## 109       1281       6944   2025-08-07 Financial Aid       4
    ## 110       1282       8586   2025-08-07       General       1
    ## 111       1289       7652   2025-08-07 International       1
    ## 112       1344       5731   2025-08-07        Career       5
    ## 113       1411       8429   2025-08-07       General       1
    ## 114       1429       7976   2025-08-07       General       4
    ## 115       1499       6215   2025-08-07        Career       2
    ## 116       1635       6789   2025-08-07 Financial Aid       4
    ## 117       1705       8970   2025-08-07        Career       5
    ## 118       1826       6725   2025-08-07       General       5
    ## 119       1834       7419   2025-08-07        Career       5
    ## 120       1924       8733   2025-08-07       General       4
    ## 121       1964       5488   2025-08-07 Financial Aid       4
    ## 122       1978       7579   2025-08-07       General       1
    ## 123       1995       6790   2025-08-07 International       5
    ## 124       2095       7058   2025-08-07       General       2
    ## 125       2131       5689   2025-08-07       General       5
    ## 126       2148       8957   2025-08-07       General       3
    ## 127       2159       7121   2025-08-07 Financial Aid       4
    ## 128       2184       6808   2025-08-07 Financial Aid       1
    ## 129       2339       8533   2025-08-07       General       2
    ## 130       2384       6499   2025-08-07 Financial Aid       2
    ## 131       1216       8056   2025-08-08 Financial Aid       1
    ## 132       1219       5077   2025-08-08       General       2
    ## 133       1400       6427   2025-08-08       General       5
    ## 134       1480       8058   2025-08-08        Career       1
    ## 135       1516       7684   2025-08-08       General       1
    ## 136       1542       7916   2025-08-08        Career       3
    ## 137       1560       8640   2025-08-08 Financial Aid       1
    ## 138       1613       5658   2025-08-08        Career       1
    ## 139       1622       5157   2025-08-08        Career       3
    ## 140       1630       5819   2025-08-08        Career       5
    ## 141       1634       7730   2025-08-08       General       3
    ## 142       1678       7280   2025-08-08 International       2
    ## 143       1779       5759   2025-08-08       General       2
    ## 144       1831       5958   2025-08-08 Financial Aid       5
    ## 145       1983       8045   2025-08-08 Financial Aid       5
    ## 146       2026       6828   2025-08-08       General       2
    ## 147       2032       8371   2025-08-08        Career       1
    ## 148       2046       7987   2025-08-08       General       4
    ## 149       2158       6295   2025-08-08       General       4
    ## 150       2190       8238   2025-08-08       General       1
    ## 151       2347       5431   2025-08-08       General       5
    ## 152       2427       8478   2025-08-08        Career       4
    ## 153       2473       8039   2025-08-08       General       4
    ## 154       1072       5649   2025-08-09       General       4
    ## 155       1087       8618   2025-08-09        Career       1
    ## 156       1179       8589   2025-08-09       General       2
    ## 157       1199       7154   2025-08-09 Financial Aid       4
    ## 158       1232       8763   2025-08-09        Career       2
    ## 159       1320       7287   2025-08-09       General       3
    ## 160       1347       7504   2025-08-09       General       4
    ## 161       1423       6401   2025-08-09        Career       4
    ## 162       1460       5093   2025-08-09 Financial Aid       5
    ## 163       1540       8667   2025-08-09       General       1
    ## 164       1620       6884   2025-08-09 Financial Aid       1
    ## 165       1621       8575   2025-08-09        Career       1
    ## 166       1644       7299   2025-08-09 International       4
    ## 167       1946       5568   2025-08-09       General       5
    ## 168       2010       6533   2025-08-09        Career       1
    ## 169       2096       6882   2025-08-09 International       3
    ## 170       2115       6066   2025-08-09 Financial Aid       2
    ## 171       2155       6794   2025-08-09        Career       2
    ## 172       2431       8162   2025-08-09        Career       3
    ## 173       2498       5391   2025-08-09       General       2
    ## 174       1010       6326   2025-08-10       General       4
    ## 175       1020       6902   2025-08-10       General       3
    ## 176       1041       7453   2025-08-10       General       3
    ## 177       1047       7193   2025-08-10 Financial Aid       2
    ## 178       1054       8426   2025-08-10       General       2
    ## 179       1062       7437   2025-08-10       General       1
    ## 180       1067       8476   2025-08-10       General       2
    ## 181       1151       6692   2025-08-10       General       5
    ## 182       1164       8517   2025-08-10 Financial Aid       5
    ## 183       1252       8699   2025-08-10 International       2
    ## 184       1404       7253   2025-08-10       General       5
    ## 185       1445       5334   2025-08-10       General       5
    ## 186       1520       7749   2025-08-10        Career       3
    ## 187       1522       8233   2025-08-10       General       2
    ## 188       1525       7040   2025-08-10 International       4
    ## 189       1580       8865   2025-08-10 International       2
    ## 190       1650       6056   2025-08-10        Career       1
    ## 191       1939       5822   2025-08-10        Career       3
    ## 192       1992       8206   2025-08-10       General       1
    ## 193       2041       7408   2025-08-10 Financial Aid       5
    ## 194       2189       5007   2025-08-10 International       4
    ## 195       2205       7481   2025-08-10       General       2
    ## 196       2211       6100   2025-08-10 International       3
    ## 197       2271       6706   2025-08-10 International       3
    ## 198       2371       8190   2025-08-10        Career       3
    ## 199       2376       8320   2025-08-10 International       2
    ## 200       2380       8153   2025-08-10 Financial Aid       4
    ##  [ reached 'max' / getOption("max.print") -- omitted 1300 rows ]

``` r
# Load the advisor roster
advisors <- read.csv("advisor_roster.csv")

# Quick check: Make sure we have the hours column
if(!"Max_Weekly_Hours" %in% names(advisors)) {
  print("Warning: Column 'Max_Weekly_Hours' not found. Checking for similar names...")
  # Rename common variations to the one our LPP expects
  names(advisors)[names(advisors) == "Weekly_Capacity"] <- "Max_Weekly_Hours"
  names(advisors)[names(advisors) == "Hours"] <- "Max_Weekly_Hours"
}

print("Advisors loaded successfully. Ready to optimize.")
```

    ## [1] "Advisors loaded successfully. Ready to optimize."

``` r
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

# --- 1. DATA PREP ---
model_data <- baseline_report %>%
  inner_join(
    requests_clean %>% 
      group_by(Appt_Type) %>% 
      summarise(Avg_Urgency = mean(as.numeric(Urgency), na.rm = TRUE)),
    by = "Appt_Type"
  ) %>%
  filter(!is.na(Avg_Wait) & Appointments > 0)

appt_types    <- as.character(model_data$Appt_Type)
demand_values <- as.numeric(model_data$Appointments)
baseline_avg  <- as.numeric(model_data$Avg_Wait)

# Urgency → priority weight
p_weights <- 6 - as.numeric(model_data$Avg_Urgency)

num_types    <- length(appt_types)
num_advisors <- nrow(advisors)
advisor_hrs  <- as.numeric(advisors$Max_Weekly_Hours)

# --- 2. MODEL ---
model <- MIPModel() %>%
  
  # Allocation decision
  add_variable(x[i, j], i = 1:num_advisors, j = 1:num_types, lb = 0) %>%
  
  # Unmet demand
  add_variable(shortage[j], j = 1:num_types, lb = 0) %>%
  
  # OBJECTIVE:
  # Serve urgent requests + minimize unmet demand
  set_objective(
    sum_expr(x[i, j] * p_weights[j], i = 1:num_advisors, j = 1:num_types) -
    sum_expr(shortage[j] * 20, j = 1:num_types),   # strong penalty
    "max"
  ) %>%
  
  # HARD capacity constraint (no overtime)
  add_constraint(
    sum_expr(x[i, j], j = 1:num_types) <= advisor_hrs[i],
    i = 1:num_advisors
  ) %>%
  
  # Demand balance
  add_constraint(
    sum_expr(x[i, j], i = 1:num_advisors) + shortage[j] == demand_values[j],
    j = 1:num_types
  )

# --- 3. SOLVE ---
result <- solve_model(model, with_ROI(solver = "glpk"))

solution_x <- get_solution(result, x[i, j])
solution_short <- get_solution(result, shortage[j])

# --- 4. RESULTS ---
allocation_summary <- solution_x %>%
  group_by(j) %>%
  summarise(Allocated = sum(value), .groups = "drop") %>%
  mutate(
    Shortage = solution_short$value,
    Demand = demand_values[j],
    Coverage = Allocated / Demand,
    Appt_Type = appt_types[j],
    Wait_Baseline = baseline_avg[j],
    
    # Wait time reduction model
    Wait_Optimized = round(
      Wait_Baseline * (1 - Coverage * 0.9), 1
    )
  ) %>%
  select(Appt_Type, Demand, Allocated, Shortage, Coverage,
         Wait_Baseline, Wait_Optimized)

print("--- OPTIMIZATION RESULTS ---")
```

    ## [1] "--- OPTIMIZATION RESULTS ---"

``` r
print(allocation_summary)
```

    ## # A tibble: 4 × 7
    ##   Appt_Type     Demand Allocated Shortage Coverage Wait_Baseline
    ##   <chr>          <dbl>     <dbl>    <dbl>    <dbl>         <dbl>
    ## 1 Financial Aid     28        28        0        1          12.5
    ## 2 General           73        73        0        1          12.4
    ## 3 Career            38        38        0        1          12.1
    ## 4 International     17        17        0        1          11.8
    ## # ℹ 1 more variable: Wait_Optimized <dbl>

``` r
# --- 5. IMPACT ---
avg_before <- mean(allocation_summary$Wait_Baseline)
avg_after  <- mean(allocation_summary$Wait_Optimized)

cat("\nAverage Wait Time Before:", round(avg_before, 2), "days\n")
```

    ## 
    ## Average Wait Time Before: 12.19 days

``` r
cat("Average Wait Time After :", round(avg_after, 2), "days\n")
```

    ## Average Wait Time After : 1.2 days

``` r
cat("Average Reduction       :", round(avg_before - avg_after, 2), "days\n")
```

    ## Average Reduction       : 10.99 days
