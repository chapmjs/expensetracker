# Family Expense Tracker

A comprehensive Shiny application for tracking family expenses with MySQL backend.

## Features

### Current Version (1.0)
- ✅ Add new expenses with date, vendor, amount, category, buyer, and notes
- ✅ View and filter all expenses
- ✅ Generate reports (Monthly, Weekly, Category, Buyer analysis)
- ✅ Interactive visualizations using Plotly
- ✅ Manage categories and buyers
- ✅ Real-time database status monitoring
- ✅ Export capabilities for reports

### Future Version (2.0)
- 🔄 User authentication and login system
- 🔄 Session management
- 🔄 Role-based access control
- 🔄 Budget planning and alerts
- 🔄 Receipt image uploads
- 🔄 Mobile-responsive design enhancements

## Setup Instructions

### 1. Database Setup

First, create the database on your MySQL server (mexico.bbfarm.org):

```bash
mysql -h mexico.bbfarm.org -u your_username -p < expense_tracker_schema.sql
```

Or manually run the SQL commands in `expense_tracker_schema.sql` using your preferred MySQL client.

### 2. Configure Database Credentials

1. Copy the configuration template:
```bash
cp config_template.R config.R
```

2. Edit `config.R` with your actual database credentials:
```r
DB_USER <- "your_actual_username"
DB_PASSWORD <- "your_actual_password"
```

3. **Important:** Add `config.R` to your `.gitignore` file to prevent committing credentials:
```bash
echo "config.R" >> .gitignore
```

### 3. Install Required R Packages

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "DBI",
  "RMySQL",
  "DT",
  "dplyr",
  "lubridate",
  "ggplot2",
  "plotly",
  "tidyr"
))
```

### 4. Run the Application

#### Local Development
```r
# Load configuration
source("config.R")

# Run the app
shiny::runApp("expense_tracker_app.R")
```

#### Deploy to Posit Connect

1. Create a manifest file:
```r
rsconnect::writeManifest()
```

2. Deploy to Posit Connect:
```r
library(rsconnect)

# Set up account (first time only)
rsconnect::setAccountInfo(
  name = "your-account-name",
  token = "your-token",
  secret = "your-secret",
  server = "your-posit-connect-url"
)

# Deploy
rsconnect::deployApp(
  appDir = ".",
  appFiles = c("expense_tracker_app.R", "config.R"),
  appName = "family-expense-tracker",
  account = "your-account-name",
  server = "your-posit-connect-url"
)
```

#### Deploy to Your Own Server (app.bbfarm.org)

1. Install Shiny Server on your server if not already installed
2. Copy application files to the Shiny Server directory:
```bash
sudo cp -r /path/to/expense-tracker /srv/shiny-server/expense-tracker
```

3. Set proper permissions:
```bash
sudo chown -R shiny:shiny /srv/shiny-server/expense-tracker
```

4. Access the app at: `http://app.bbfarm.org/expense-tracker/`

## Usage Guide

### Adding Expenses
1. Navigate to "Add Expense" tab
2. Fill in the required fields:
   - Date (defaults to today)
   - Vendor name
   - Amount
   - Category (select from dropdown)
   - Buyer (select existing or add new)
   - Optional notes
3. Click "Add Expense"
4. Recent expenses appear below

### Viewing Expenses
1. Go to "View Expenses" tab
2. Use filters to narrow results:
   - Date range
   - Category (multi-select)
   - Buyer (multi-select)
3. Click "Apply Filters"
4. Export data using the table's built-in export buttons

### Generating Reports
1. Open "Reports" tab
2. Select report type:
   - Monthly Summary: Expenses grouped by month and category
   - Weekly Summary: Expenses grouped by week and category
   - Category Breakdown: Total spending per category
   - Buyer Analysis: Spending patterns by buyer
3. Set date range
4. Click "Generate Report"
5. View summary statistics, interactive charts, and detailed tables

### Settings
1. Access "Settings" tab to:
   - Add new categories
   - Add new buyers
   - View database connection status
   - Monitor system health

## Database Schema

### Tables
- `expenses`: Main transaction table
- `categories`: Expense categories
- `buyers`: Family members/buyers
- `monthly_summary`: View for monthly reporting
- `weekly_summary`: View for weekly reporting

## Troubleshooting

### Database Connection Issues
1. Check network connectivity to mexico.bbfarm.org
2. Verify MySQL user has proper privileges
3. Ensure firewall allows MySQL port (3306)
4. Check credentials in config.R

### Common Errors
- **"Database connection failed"**: Check credentials and network
- **"Table doesn't exist"**: Run the schema SQL file
- **"Permission denied"**: Grant proper MySQL privileges to user

### MySQL User Privileges
The database user needs these privileges:
```sql
GRANT SELECT, INSERT, UPDATE, DELETE, CREATE, DROP, INDEX, ALTER 
ON expense_tracker.* TO 'your_username'@'%';
FLUSH PRIVILEGES;
```

## Security Considerations

1. **Never commit credentials** to version control
2. Use environment variables for production deployments
3. Implement HTTPS on your server
4. Regular database backups recommended
5. Consider IP whitelisting for MySQL access

## Performance Tips

1. Database indexes are already created on frequently queried columns
2. Consider archiving old expenses (>2 years) to a separate table
3. Use date filters to limit data retrieval
4. Monitor slow queries and optimize as needed

## Backup Strategy

Regular backups recommended:
```bash
# Daily backup script
mysqldump -h mexico.bbfarm.org -u username -p expense_tracker > backup_$(date +%Y%m%d).sql
```

## Support & Maintenance

For issues or feature requests, consider:
1. Checking application logs
2. Reviewing database connection status in Settings tab
3. Monitoring Shiny Server logs (if self-hosted)
4. Testing with minimal data first

## License

This is a private family application. Modify as needed for your use case.

## Credits

Built with:
- R Shiny for the web framework
- MySQL for data persistence
- Plotly for interactive visualizations
- DT for advanced data tables

## Version History

- v1.0 (Current): Basic expense tracking with reporting
- v2.0 (Planned): User authentication and advanced features
