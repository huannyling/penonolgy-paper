import requests
import csv
from datetime import datetime

# OpenWeatherMap API Key (Replace with your own key)
API_KEY = "d1275d80cfd59643dc32b5c3d696ad9a"

# Base URL for OpenWeatherMap One Call API
BASE_URL = "https://api.openweathermap.org/data/3.0/onecall"

# List of cities with coordinates
cities = [
    {"name": "Kyoto, Japan", "latitude": 35.0120, "longitude": 135.6761},
    {"name": "Liestal-Weideli, Switzerland", "latitude": 47.4814, "longitude": 7.730519},
    {"name": "Washington, D.C., USA", "latitude": 38.8853, "longitude": -77.0386},
    {"name": "Vancouver, BC, Canada", "latitude": 49.2237, "longitude": -123.1636},
    {"name": "New York City, NY, USA", "latitude": 40.73040, "longitude": -73.99809}
]

# CSV file to store results
csv_filename = "weather_forecast.csv"

# Headers for CSV
csv_headers = ["City", "Date", "Temperature (°C)", "Min Temp (°C)", "Max Temp (°C)",
               "Humidity (%)", "Weather Condition", "Wind Speed (m/s)", "Precipitation Probability"]


# Function to fetch weather data
def get_weather_forecast(lat, lon, city_name):
    params = {
        "lat": lat,
        "lon": lon,
        "exclude": "current,minutely,hourly,alerts",
        "appid": API_KEY,
        "units": "metric"  # Change to "imperial" for Fahrenheit
    }

    response = requests.get(BASE_URL, params=params)

    if response.status_code == 200:
        data = response.json()
        daily_forecast = data.get("daily", [])

        weather_data = []
        for day in daily_forecast[:16]:  # OpenWeatherMap's free plan allows only 16-day forecast
            date = datetime.utcfromtimestamp(day["dt"]).strftime('%Y-%m-%d')
            temp = day["temp"]["day"]
            temp_min = day["temp"]["min"]
            temp_max = day["temp"]["max"]
            humidity = day["humidity"]
            weather = day["weather"][0]["description"]
            wind_speed = day["wind_speed"]
            precipitation_prob = day["pop"]  # Probability of precipitation (0 to 1)

            weather_data.append([city_name, date, temp, temp_min, temp_max,
                                 humidity, weather, wind_speed, precipitation_prob])
        return weather_data
    else:
        print(f"Failed to get weather data for {city_name}: {response.status_code}")
        return []


# Fetch data and save to CSV
all_weather_data = []

for city in cities:
    city_weather = get_weather_forecast(city["latitude"], city["longitude"], city["name"])
    all_weather_data.extend(city_weather)

# Write to CSV file
with open(csv_filename, mode='w', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)
    writer.writerow(csv_headers)  # Write header
    writer.writerows(all_weather_data)  # Write data

print(f"Weather data saved successfully in {csv_filename}")
