####
# IPF for single target 
###

library(dplyr);


# get target 
tar1 <- read.csv("Target1.csv", stringsAsFactors = FALSE)
tar1 <- filter(tar1, Year == 2015)


###
# Seed
###

u.country <- unique(tar1$Country)
u.measure <- unique(tar1$Measure)
u.product <- unique(tar1$Product)
u.segment <- unique(tar1$Segment)
u.year <- unique(tar1$Year)


seed <- expand.grid(Measure = u.measure, Country = u.country, Segment = u.segment, Product = u.product,
                    Year = u.year)

seed$value <- 1 #Unary seed
seed$value <- rnorm(nrow(seed), 100, 20) #Random seed

####
# Looping the IPF
####

tolerance <- 0.001

#Iterations
iterations <- 0

#Error starting
error <- 100000 + tolerance

#Set seed
k0 <- seed

#While loop

while( (error > tolerance) & (iterations < 20)) { #start loop
  
  k1 <- k0 %>%
    left_join(tar1) %>% #Bring in tar 1
    group_by(Measure, Country, Product, Segment, Year) %>% #Group by dimensions in Target 1
    mutate(share = value / sum(value, na.rm=TRUE)) %>% #Get share for each group member
    ungroup() %>%
    mutate(value = share * target) %>%
    select(-share, -target)
  
   #Check our errors
  e1 <- k1 %>%
    group_by(Measure, Country, Product, Segment, Year) %>% #Group by dimensions in Target 1
    summarize(value = sum(value, na.rm=TRUE)) %>%
    ungroup() %>%
    left_join(tar1) %>%
    mutate(error = target - value)
  
  error <- sum(abs(e1$error), na.rm=TRUE)
  
  #Difference 1 from 1-iteration above - reset K0
  k0 <- k1
  
  #Difference 2
  iterations <- iterations + 1
  
  #Difference 3
  print(paste("Iteration:", iterations, "       ", "Error:", error))
  
} # end loop


rm(e1, k1, k0)

