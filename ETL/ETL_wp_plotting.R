# Load the RMariaDB package to interact with MySQL database
library(RMariaDB)

# Create a seasons vector to perform ETL operations by season
seasons <- 1999:2020

# Loop through each season
for (season in seasons) {
    # Create connection to database
    con <- dbConnect(RMariaDB::MariaDB(),
                   host="win-prob.cdihizlh8lu5.us-east-2.rds.amazonaws.com",
                   port=3306,
                   dbname="winProb",
                   user="admin",
                   password="79d2JAiyr8Czi3zGlfMs")
    

    # Query the db to get the raw play-by-play data for a season
    pbp_season <- dbGetQuery(con,paste0("SELECT * FROM pbpDataRaw WHERE season=",season))
    
    # Transform pbp data for win probability plotting
    wp_season <- export_wp(wp_model,ov_wp_model,pbp_season)
    
    # Load the data in the wpPlotting table
    dbWriteTable(con,"wpPlotting",wp_season,append=TRUE)
    
    # Add print statement to track progress
    print(paste(season,"complete!"))
    
    # Disconnect from DB to save progress
    dbDisconnect(con)
}