# Converts CSV to google input

library( tidyverse )
library( lubridate )



FirstMonday <- mdy("08-29-2022")
read_csv("202301.csv") |>
  select( TERM, COURSE, SECT, CRN, CRED = `MAX CREDITS`, 
          TITLE, SEATS=`ACTUAL ENROLLMENT`, 
          INSTRUCTOR = `PRIMARY INSTRUCTOR FIRST NAME`,
          PROFESSOR = `PRIMARY INSTRUCTOR LAST NAME`,
          MON = `MON-IND`, TUE=`TUE-IND`, WED=`WED-IND`, THU = `THU-IND`, FRI=`FRI-IND`,
          START = `BEGIN TIME`, END = `END TIME`,
          BLD, ROOM, MODALITY = `MODALITY CODE` ) |> 
  mutate( Location = paste( BLD, ROOM) ) |>
  mutate( Location = ifelse( is.na(ROOM), NA, Location) ) -> tmp 
#FirstMonday <- mdy("01-23-2023")
#read_csv("2023.csv") |>
#  mutate( Location = paste( BLD, ROOM) ) |>
#  mutate( Location = ifelse( is.na(ROOM), NA, Location) ) -> tmp 



tribble( 
  ~Subject, ~`Start Date`, ~`Start Time`, ~`End Date`, ~`End Time`, ~Description, ~Location 
) -> schedule



makeTime <- function( raw ) { 
  tm <- as.numeric( raw ) 
  am.pm <- ifelse( tm < 1200, " AM", " PM")
  corr <- ifelse( tm > 1300, 12, 0 )
  ret <- c( floor(tm / 100) - corr,
            ":",
            str_sub(raw,-2),
            am.pm )
  return( paste( ret, collapse = "" ) )
}


makeEntry <- function( subject, date, stime, etime, desc, loc) { 
  day <- format( date, format="%m/%d/%Y" )
  return( tribble( 
                ~Subject, ~`Start Date`, ~`Start Time`, ~`End Date`, ~`End Time`, ~Description, ~Location,
                subject, day, stime, day, etime, desc, loc )   )
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
                         makeEntry( description,
                                    FirstMonday,
                                    stime, etime, instructor, location) 
                         )
    }
    
    if( !is.na( tmp$TUE[i]) ) { 
      schedule <- rbind( schedule, 
                         makeEntry( description,
                                    FirstMonday+1,
                                    stime, etime, instructor, location) 
      )
    }
    
    if( !is.na( tmp$WED[i]) ) { 
      schedule <- rbind( schedule, 
                         makeEntry( description,
                                    FirstMonday+2,
                                    stime, etime, instructor, location) 
      )
    }
    
    if( !is.na( tmp$THU[i]) ) { 
      schedule <- rbind( schedule, 
                         makeEntry( description,
                                    FirstMonday+3,
                                    stime, etime, instructor, location) 
      )
    }
    
    if( !is.na( tmp$FRI[i]) ) { 
      schedule <- rbind( schedule, 
                         makeEntry( description,
                                    FirstMonday+4,
                                    stime, etime, instructor, location) 
      )
    }
    
    
    
        
    
  }

  
  
}

schedule %>%
  select( Subject,`Start Date`, `Start Time`, 
          `End Date`, `End Time`,
          Description, Location  ) |>
  filter( !is.na(Subject) ) -> schedule

View(schedule)

write_csv( schedule, file="202301.cal.csv")

