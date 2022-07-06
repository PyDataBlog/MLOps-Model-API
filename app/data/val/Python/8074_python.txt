from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.select import Select

# Configure the baseURL
baseUrl = "https://www.expedia.es"

# Create a webDriver instance and maximize window
driver = webdriver.Firefox()
driver.maximize_window()

# Navigage to URL and put a 10 seconds implicit wait
driver.get(baseUrl)
driver.implicitly_wait(10)

# Find and click on element "Flights"



# Find departure textbox and type "Barcelona"



# Find departure textbox and type "Madrid"



# Find departure time and type "23/11/2017"


# Close Calendar


# Find the "Find" button and click on



# Quit driver



