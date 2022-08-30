# Converts CSV to google input

library( tidyverse )
library( lubridate )

read_csv("2023.csv") |>
  mutate( Location = paste( BLD, ROOM) ) |>
  mutate( Location = ifelse( is.na(ROOM), NA, Location) ) -> tmp 

data.frame( Subject = NA,
            `Start Date` = NA,
            `End Date` = NA,
            `Start Time` = NA,
            `End Time` = NA,
            Description = NA,
            Location = NA
            ) -> schedule

for( i in 1:nrow(tmp) ) { 
  
  start <- as.numeric(tmp$START[i])
  
  
  # Classes with times have start time
  if( !is.na(start) ) {
    end <- as.numeric(tmp$END[i])
    apstart <- ifelse( start > 1200, "PM", "AM" )
    
    if( apstart == "PM" ) { 
      start <- start - 1200
    }
    start <- start/100
    
    
    apend <- ifelse( end > 1200, "PM","AM")
    if( apend  == "PM" ) { 
      end <- end - 1200
    }
    end <- end/100
    
    description <- tmp$TITLE[i]
    instructor <- tmp$PROFESSOR[i]
    location <- tmp$Location[i]
    
    
    if( !.is.na( tmp$MON[i]) ) { 
      
    }
    
    
    
  }

  
  
}


View(schedule)



