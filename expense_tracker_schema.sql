-- Create database for expense tracker
CREATE DATABASE IF NOT EXISTS expense_tracker;
USE expense_tracker;

-- Create main expenses table
CREATE TABLE IF NOT EXISTS expenses (
    id INT AUTO_INCREMENT PRIMARY KEY,
    expense_date DATE NOT NULL,
    vendor VARCHAR(255) NOT NULL,
    amount DECIMAL(10, 2) NOT NULL,
    category VARCHAR(100) NOT NULL,
    buyer VARCHAR(100) NOT NULL,
    note TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    INDEX idx_date (expense_date),
    INDEX idx_category (category),
    INDEX idx_buyer (buyer)
);

-- Create categories table for future dropdown consistency
CREATE TABLE IF NOT EXISTS categories (
    id INT AUTO_INCREMENT PRIMARY KEY,
    category_name VARCHAR(100) UNIQUE NOT NULL,
    description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Insert some default categories
INSERT INTO categories (category_name, description) VALUES
    ('Groceries', 'Food and household supplies'),
    ('Transportation', 'Gas, maintenance, public transit'),
    ('Utilities', 'Electric, water, internet, phone'),
    ('Healthcare', 'Medical, dental, pharmacy'),
    ('Entertainment', 'Movies, games, subscriptions'),
    ('Dining Out', 'Restaurants and takeout'),
    ('Clothing', 'Apparel and accessories'),
    ('Education', 'Books, courses, school supplies'),
    ('Home', 'Maintenance, repairs, improvements'),
    ('Insurance', 'Home, auto, health insurance'),
    ('Personal Care', 'Haircuts, hygiene products'),
    ('Gifts', 'Presents and donations'),
    ('Savings', 'Emergency fund, investments'),
    ('Other', 'Miscellaneous expenses')
ON DUPLICATE KEY UPDATE category_name=category_name;

-- Create buyers table for consistency
CREATE TABLE IF NOT EXISTS buyers (
    id INT AUTO_INCREMENT PRIMARY KEY,
    buyer_name VARCHAR(100) UNIQUE NOT NULL,
    email VARCHAR(255),
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Create view for monthly summary
CREATE OR REPLACE VIEW monthly_summary AS
SELECT 
    YEAR(expense_date) as year,
    MONTH(expense_date) as month,
    MONTHNAME(expense_date) as month_name,
    category,
    SUM(amount) as total_amount,
    COUNT(*) as transaction_count
FROM expenses
GROUP BY YEAR(expense_date), MONTH(expense_date), category
ORDER BY year DESC, month DESC, category;

-- Create view for weekly summary
CREATE OR REPLACE VIEW weekly_summary AS
SELECT 
    YEAR(expense_date) as year,
    WEEK(expense_date) as week,
    DATE_SUB(expense_date, INTERVAL WEEKDAY(expense_date) DAY) as week_start,
    category,
    SUM(amount) as total_amount,
    COUNT(*) as transaction_count
FROM expenses
GROUP BY YEAR(expense_date), WEEK(expense_date), category
ORDER BY year DESC, week DESC, category;
