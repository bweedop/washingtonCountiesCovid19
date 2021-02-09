    # List the counties in WA
    wash_counties <- c("Adams", "Asotin", "Benton", "Chelan", "Clallam", "Clark", "Columbia", "Cowlitz", "Douglas", "Ferry", "Franklin", "Garfield", "Grant", "Grays Harbor", "Island", "Jefferson", "King", "Kitsap", "Kittitas", "Klickitat", "Lewis", "Lincoln", "Mason", "Okanogan", "Pacific", "Pend Oreille", "Pierce", "San Juan", "Skagit", "Skamania", "Snohomish", "Spokane", "Stevens", "Thurston", "Wahkiakum", "Walla Walla", "Whatcom", "Whitman", "Yakima")
    # Append "County" at the end to be consistent with the data
    wash_counties <- unlist(lapply(wash_counties, function(x) paste(x, "County")))

    # Load data
    epiData <- read.csv(file = "../data/rawData/WA_COVID19_Cases_Hospitalizations_Deaths.csv", header = TRUE)
    # Changing the date to date data type
    epiData$WeekStartDate <- as.Date(epiData$WeekStartDate, format = "%m/%d/%Y")
    # Removing some rows with "Unassigned" in county
    epiData <- epiData[-which(!epiData$County %in% wash_counties), ]

    ggplot(epiData, aes(x = WeekStartDate, y = ConfirmedCases, group=County, color=County)) + 
      geom_line()

![](epiDataExploration_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
