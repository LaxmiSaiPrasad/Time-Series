# Time-Series
Time Series Analysis of Stock Data (MMM)

Time Series Analysis of S&P 500 stock by ticker name 'MMM' was done. 
Box-Jenkins modeling (Autoregressive Integrated Moving Average), or more popularly ARIMA(p,d,q) was done on the data.
ACF (Auto-correlation factor plots to find parameter 'p') and PACF (Partical Auto-correlation factor plots for finding parameter 'q') were evaluated to find best (p,q). Dickey-Fuller test and Unit-root test were also done

Procedure:
1) Raw data was plotted. Observations of trend and constant variance were nmade (If the variance wasn't constant, an initial step would be to apply 'log' transformation on it). 
2) To eliminate 'trand' in data, differencing was done, i.e a new series which was difference of 2 adjacent time series points/data was genrated. It satisfied stationarity conditions (constant mean, constant variance ) i.e Dickey-Fuller test was done on it (Null Hypothesis: Time Series is not Stationary. Thus we have to reject null hypothesis to proceed further), and null hypothesis was rejected. Thus parameter 'd' was found.
3) Then ACF and PACF plots of Single Order Differencing were plotted to estimate 'p' and 'q', respectively
4) Various models formed by permutation of [0,p] and [0,q] and 'd' were done, and model with least 'AIC' was chosen as representative model
5) After identifying best model, it was fitted on original data, and, Ljung-Box test was done the residuals (statistical test for checking if there is auto-correlation factor (acf) such that |acf| >= 1.96/sqrt(n), where 'n' is number of time series points considered). Also ACF and PACF plots were plotted for residuals, and found that no lags were significant, establishing lack of coorelation between the residuals.

Thus time series modeling was done. The script is in 'R'.

Good Resources: 
1) Forecasting - Methods and Application, Authors: Spyros Makridakis, Steven C Wheelright, Rob J Hyndman
2) Time Series Analysis and its Applications with R Examples , Authors: Robert H Shumway, David S Stoffer
