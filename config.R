# Database Configuration File
# Copy this file to config.R and update with your actual credentials
# NEVER commit config.R to version control

# Database connection parameters
DB_HOST <- "host_here"
DB_NAME <- "db_name_here"
DB_USER <- "your_username_here"
DB_PASSWORD <- "your_password_here"
DB_PORT <- 3306

# Set as environment variables
Sys.setenv(
  DB_USER = DB_USER,
  DB_PASSWORD = DB_PASSWORD
)
