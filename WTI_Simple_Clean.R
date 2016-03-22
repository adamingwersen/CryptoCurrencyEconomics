

## Cleaning & Transposing WTI-Oil price data as preparation for analysis in OxMetrics
## OxMetrics does not import sc-delim. csv's

# Read excel file as sc-delim.
wtiprice = read.csv("C:/Users/Adam/Downloads/WTI.oil.daily.2010_2016.csv", sep = ";")

# Replace commas with dots - in alignment with existing data
wtiprice$DCOILWTICO = as.numeric(gsub(",", ".", gsub("\\.", "", wtiprice$DCOILWTICO)))

# Write prepped file to folder - ready for import to Ox
write.csv(wtiprice, "C:/Users/Adam/Downloads/WTI.oil.daily.prep.2010_2016.csv")

