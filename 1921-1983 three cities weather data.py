import requests
import pandas as pd
from datetime import datetime
import json

# Define locations (only three cities)
cities = [
    {"name": "Kyoto, Japan", "latitude": 35.0120, "longitude": 135.6761},
    {"name": "Liestal-Weideli, Switzerland", "latitude": 47.4814, "longitude": 7.730519},
    {"name": "Washington, D.C., USA", "latitude": 38.8853, "longitude": -77.0386}
]

# Define years and date range
start_month_day = "01-01"
end_month_day = "03-31"
years = list(range(2023, 2025))  # From 1984 to 2024

# Open-Meteo API URL
base_url = "https://archive-api.open-meteo.com/v1/archive"

# Store results
historical_weather_data = []
no_data_cities = []

for year in years:
    for city in cities:
        print(f"Fetching weather data for {city['name']} in {year}")

        # Define start and end date for each year
        start_date = f"{year}-{start_month_day}"
        end_date = f"{year}-{end_month_day}"

        # Historical data request
        params_past = {
            "latitude": city["latitude"],
            "longitude": city["longitude"],
            "start_date": start_date,
            "end_date": end_date,
            "daily": [
                "temperature_2m_max", "temperature_2m_min", "precipitation_sum", "wind_speed_10m_max",
                "weathercode", "sunshine_duration", "et0_fao_evapotranspiration"
            ],
            "timezone": "auto"
        }

        response_past = requests.get(base_url, params=params_past)
        data_past = response_past.json()

        if "daily" in data_past and data_past["daily"]:
            df_past = pd.DataFrame(data_past["daily"])
            df_past["city"] = city["name"]
            df_past["year"] = year
            historical_weather_data.append(df_past)
        else:
            print(f"No data found for {city['name']} in {year}")
            no_data_cities.append((city["name"], year))

# Combine and save data
if historical_weather_data:
    historical_df = pd.concat(historical_weather_data, ignore_index=True)
    historical_df.to_csv("historical_weather60Days_2024.csv", index=False)
    print("Historical weather data for 1984-2024 downloaded successfully!")
else:
    print("No historical weather data was retrieved.")

# Report cities with missing data
if no_data_cities:
    print("Warning: No data available for the following locations:")
    for city, year in no_data_cities:
        print(f"- {city}, {year}")
