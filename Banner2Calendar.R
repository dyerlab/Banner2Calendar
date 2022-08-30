# Converts CSV to google input

library( tidyverse )
library( lubridate )


FirstMonday <- mdy("01-23-2023")

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



makeTime <- function( raw ) { 
  tm <- as.numeric( raw ) 
  am.pm <- ifelse( tm > 1200, " PM"," AM")
  corr <- ifelse( am.pm == " PM", 12, 0 )
  ret <- c( floor(tm / 100) - corr,
            ":",
            str_sub(raw,-2),
            am.pm )
  return( paste( ret, collapse = "" ) )
}





for( i in 1:nrow(tmp) ) { 
  
  start <- tmp$START[i]
  
  if( !is.na(start) ) {
    
    stime <- makeTime( start )
    etime <- makeTime( tmp$END[i] )
    description <- paste( tmp$COURSE[i], tmp$TITLE[i], sep=": ")
    instructor <- tmp$PROFESSOR[i]
    location <- tmp$Location[i]
    
    if( !is.na( tmp$MON[i]) ) { 
      schedule <- rbind( schedule, 
                         data.frame( Subject = description,
                                     `Start Date` = as.character(FirstMonday),
                                     `End Date` = as.character(FirstMonday),
                                     `Start Time` = stime,
                                     `End Time` = etime,
                                     Description = instructor,
                                     Location = location )
                         )
    }
    
    if( !is.na( tmp$TUE[i]) ) { 
      schedule <- rbind( schedule, 
                         data.frame( Subject = description,
                                     `Start Date` = as.character(FirstMonday + 1),
                                     `End Date` = as.character(FirstMonday + 1),
                                     `Start Time` = stime,
                                     `End Time` = etime,
                                     Description = instructor,
                                     Location = location )
      )
    }
    
    if( !is.na( tmp$WED[i]) ) { 
      schedule <- rbind( schedule, 
                         data.frame( Subject = description,
                                     `Start Date` = as.character(FirstMonday + 2),
                                     `End Date` = as.character(FirstMonday + 2),
                                     `Start Time` = stime,
                                     `End Time` = etime,
                                     Description = instructor,
                                     Location = location )
      )
    }
    
    if( !is.na( tmp$THU[i]) ) { 
      schedule <- rbind( schedule, 
                         data.frame( Subject = description,
                                     `Start Date` = as.character(FirstMonday + 3),
                                     `End Date` = as.character(FirstMonday + 3),
                                     `Start Time` = stime,
                                     `End Time` = etime,
                                     Description = instructor,
                                     Location = location )
      )
    }
    
    if( !is.na( tmp$FRI[i]) ) { 
      schedule <- rbind( schedule, 
                         data.frame( Subject = description,
                                     `Start Date` = as.character(FirstMonday + 4),
                                     `End Date` = as.character(FirstMonday + 4),
                                     `Start Time` = stime,
                                     `End Time` = etime,
                                     Description = instructor,
                                     Location = location )
      )
    }
    
    
    
        
    
  }

  
  
}

schedule |>
  filter( !is.na(Subject) ) -> schedule

View(schedule)



