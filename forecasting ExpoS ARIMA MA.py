import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.tsa.holtwinters import ExponentialSmoothing
from statsmodels.tsa.arima.model import ARIMA
from sklearn.metrics import mean_absolute_error, mean_absolute_percentage_error

# Function to calculate MAE and MAPE
def evaluate_forecast(y_true, y_pred):
    mae = mean_absolute_error(y_true, y_pred)
    mape = mean_absolute_percentage_error(y_true, y_pred)
    return mae, mape

# Function to forecast using Exponential Smoothing
def forecast_exponential_smoothing(data, alpha, beta, phi, periods):
    model = ExponentialSmoothing(data, trend="add", seasonal="add", seasonal_periods=phi)
    fit = model.fit(smoothing_level=alpha, smoothing_slope=beta)
    forecast = fit.forecast(periods)
    return forecast

# Function to forecast using ARIMA
def forecast_arima(data, order, periods):
    model = ARIMA(data, order=order)
    fit = model.fit()
    forecast, stderr, conf_int = fit.forecast(steps=periods)
    return forecast

# Function to forecast using Moving Average
def forecast_moving_average(data, window, periods):
    forecast = data.rolling(window=window).mean().iloc[-1] * np.ones(periods)
    return forecast

# Input parameters
file_path = input("Enter the path to the Excel file: ")
periods = int(input("Enter the number of periods to forecast: "))

# Load data from an Excel file
try:
    data = pd.read_excel(file_path)
except Exception as e:
    print("Error loading data from Excel:", str(e))
    exit(1)

# Default parameter values
alpha = 0.3
beta = 0.1
phi = 12
p = 1
d = 1
q = 1
window = 3

# Ask the user if they want to use default values or provide custom ones
use_default = input("Use default parameter values? (Y/n): ").strip().lower()

if use_default == "n":
    # If not using defaults, ask the user to provide custom values
    alpha = float(input("Enter alpha for Exponential Smoothing: "))
    beta = float(input("Enter beta for Exponential Smoothing: "))
    phi = int(input("Enter seasonal period (phi) for Exponential Smoothing: "))
    p = int(input("Enter the autoregressive order (p) for ARIMA: "))
    d = int(input("Enter the differencing order (d) for ARIMA: "))
    q = int(input("Enter the moving average order (q) for ARIMA: "))
    window = int(input("Enter the window size for Moving Average: "))

# Perform forecasts
exp_smooth_forecast = forecast_exponential_smoothing(data, alpha, beta, phi, periods)
arima_forecast = forecast_arima(data, order=(p, d, q), periods=periods)
ma_forecast = forecast_moving_average(data, window, periods)

# Evaluate the forecasts
true_values = data[-periods:]
mae_exp_smooth, mape_exp_smooth = evaluate_forecast(true_values, exp_smooth_forecast)
mae_arima, mape_arima = evaluate_forecast(true_values, arima_forecast)
mae_ma, mape_ma = evaluate_forecast(true_values, ma_forecast)

# Print evaluation metrics
print("Exponential Smoothing - MAE: {}, MAPE: {}".format(mae_exp_smooth, mape_exp_smooth))
print("ARIMA - MAE: {}, MAPE: {}".format(mae_arima, mape_arima))
print("Moving Average - MAE: {}, MAPE: {}".format(mae_ma, mape_ma))

# Plot the forecasts
plt.figure(figsize=(12, 6))
plt.plot(true_values, label="True Values")
plt.plot(exp_smooth_forecast, label="Exponential Smoothing Forecast")
plt.plot(arima_forecast, label="ARIMA Forecast")
plt.plot(ma_forecast, label="Moving Average Forecast")
plt.legend()
plt.title("Forecast Comparison")
plt.show()
