# Senior_Capstone_Spring_18
Exponential gradient algorithm on stock data

For this project, we applied the exponential gradient machine learning algorithm to historical stock market data.

The purpose of this project was to observe how the algorithm performed in a handful of distinct industries and to determine the scale of capital which would allow for such a high volume of trades to pay off as a realistically smart investment.

We compared the exponential gradient algorithm with two benchmarks: a sit-and-wait method (investing all capital into one company and selling all shares at the end of the investment period) and the gold-standard Best Constant Rebalanced Portfolio (which isn't a strategy in itself because it is a function of retroactive data).

The BCRP and EG algorithms can be viewed as BCRP.R and descriptive_LePage EG.R. These functions were found on github.

The data sampled for this project is under the name Master_Stocks.RData.

I modified the EG function to include the other two benchmark comparisons. This function is viewable under stock_algorithm_20180421.R.

The presentation slides are found under the title Capstone Presentation.pdf.
