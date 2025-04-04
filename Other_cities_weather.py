import requests
import pandas as pd
import time

# Load the city data
#file_path_cities = "other_cities_combined_location_year_summary.csv"
file_path_cities = "USA-NPN_filtered.csv"
df_cities = pd.read_csv(file_path_cities, encoding='utf-8-sig')

# Define date range for end_year only
start_month_day = "01-01"
end_month_day = "03-31"

# Open-Meteo API URL
base_url = "https://archive-api.open-meteo.com/v1/archive"

# Store results
historical_weather_data = []
no_data_cities = []

# API retry configuration
MAX_RETRIES = 5  # Maximum retry attempts for 429 errors
SAVE_INTERVAL = 80  # Save data every 30 locations

# Iterate over cities
for index, row in df_cities.iterrows():
    city_name = row['location']
    latitude = row['lat']
    longitude = row['long']
    end_year = row['end_year']  # Only retrieve the last year's data

    print(f"Fetching weather data for {city_name} in {end_year}")

    start_date = f"{end_year}-{start_month_day}"
    end_date = f"{end_year}-{end_month_day}"

    # Historical data request
    params_past = {
        "latitude": latitude,
        "longitude": longitude,
        "start_date": start_date,
        "end_date": end_date,
        "daily": [
            "temperature_2m_max", "temperature_2m_min", "precipitation_sum", "wind_speed_10m_max",
            "weathercode", "sunshine_duration", "et0_fao_evapotranspiration"
        ],
        "timezone": "auto"
    }

    retries = 0
    while retries < MAX_RETRIES:
        try:
            response_past = requests.get(base_url, params=params_past)
            if response_past.status_code == 429:
                wait_time = (retries + 1) * 5  # Exponential backoff: 5s, 10s, 15s...
                print(f"Rate limit reached for {city_name} in {end_year}. Retrying in {wait_time} seconds...")
                time.sleep(wait_time)
                retries += 1
                continue
            response_past.raise_for_status()
            data_past = response_past.json()

            if "daily" in data_past and data_past["daily"]:
                df_past = pd.DataFrame(data_past["daily"])
                df_past["city"] = city_name
                df_past["year"] = end_year
                historical_weather_data.append(df_past)
            else:
                print(f"No data found for {city_name} in {end_year}")
                no_data_cities.append((city_name, end_year))
            break  # Exit retry loop on success
        except requests.exceptions.RequestException as e:
            print(f"Error fetching data for {city_name} in {end_year}: {e}")
            retries += 1
            time.sleep(5)  # Wait before retrying

    # Sleep to avoid hitting API limits
    time.sleep(3)

    # Save data every 30 locations
    if (index + 1) % SAVE_INTERVAL == 0 and historical_weather_data:
        temp_output_path = f"weather_data_batch_{index + 1}.csv"
        pd.concat(historical_weather_data, ignore_index=True).to_csv(temp_output_path, index=False,
                                                                     encoding='utf-8-sig')
        print(f"Saved interim data: {temp_output_path}")
        historical_weather_data = []  # Clear the list after saving

# Final save
if historical_weather_data:
    output_path = "final_historical_weather_data.csv"
    pd.concat(historical_weather_data, ignore_index=True).to_csv(output_path, index=False, encoding='utf-8-sig')
    print("Final historical weather data saved successfully!")
else:
    print("No historical weather data was retrieved.")

# Report cities with missing data
if no_data_cities:
    print("Warning: No data available for the following locations:")
    for city, year in no_data_cities:
        print(f"- {city}, {year}")
