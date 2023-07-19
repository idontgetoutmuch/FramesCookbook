Maybe this one has some inspiring things we could add: https://pandas.pydata.org/docs/user_guide/cookbook.html ?

chatGPT's answer for the 100 most important operations on dataframes: :wink:
Working with data in a DataFrame can involve numerous operations, and the importance of each operation can vary depending on the specific use case and data analysis requirements. However, I can provide a list of 100 essential DataFrame operations that cover a wide range of common tasks. These operations are typically performed in data manipulation, exploration, cleaning, and analysis. Keep in mind that this list is not exhaustive, but it should give you a solid foundation to work with DataFrame data:

Creating a DataFrame from various sources (CSV, Excel, JSON, etc.).
Viewing the first/last few rows of the DataFrame.
Inspecting data types and general information using dtypes, info(), shape.
Extracting basic statistics with describe() method.
Selecting columns by name.
Filtering rows based on conditions.
Adding new columns.
Renaming columns.
Removing duplicate rows with drop_duplicates().
Handling missing data with fillna() and dropna().
Sorting the DataFrame using sort_values().
Grouping data with groupby().
Aggregating data with sum(), mean(), count(), etc.
Pivoting data using pivot_table().
Melting data using melt().
Reshaping data using stack() and unstack().
Combining DataFrames using concat() and merge().
Joins and merges using merge() and join().
Creating cross-tabulations using crosstab().
Changing data types with astype().
Applying custom functions with apply() and applymap().
Vectorized operations with arithmetic and logical operators.
Handling string data with str accessor methods.
Extracting data from datetime columns with dt accessor methods.
Applying aggregation functions to specific columns using agg().
Handling categorical data with pd.Categorical and cut().
Encoding categorical data using pd.get_dummies().
Handling large datasets using chunking with chunksize parameter in read_* functions.
Resampling time series data using resample().
Shifting data in time series with shift().
Handling time zones using tz_localize() and tz_convert().
Plotting data with matplotlib and pandas built-in plotting.
Subsetting data using .loc[] and .iloc[] indexing.
Assigning new values to specific cells or rows.
Chaining operations using method chaining.
Handling hierarchical indexing with MultiIndex.
Dropping columns or rows using drop().
Iterating over rows with iterrows() (caution: slow for large DataFrames).
Sampling random rows with sample().
Finding unique values with unique() and counting them with value_counts().
Replacing values with replace().
Renaming index labels using rename_axis().
Changing the order of columns with indexing.
Transposing the DataFrame with .T.
Handling infinities and NaNs with np.inf, np.nan, and np.isinf().
Changing the display options using pd.set_option().
Converting a DataFrame to a NumPy array with to_numpy().
Handling duplicate column labels using pd.MultiIndex.from_tuples().
Reversing the DataFrame using .iloc[::-1].
Handling boolean indexing with isin().
Changing the index using set_index().
Resetting the index with reset_index().
Handling multi-level index with .xs() and .xs(key, level=...).
Merging cells horizontally or vertically using pd.merge().
Using cut() and qcut() to bin continuous data.
Applying rolling and cumulative functions with rolling() and expanding().
Using nlargest() and nsmallest() to get top N rows.
Applying conditions to create a new column with np.where().
Handling JSON data with json_normalize().
Using min(), max(), and idxmin(), idxmax() for finding minimum and maximum values and their indices.
Handling scientific notation with pd.options.display.float_format.
Selecting rows based on index conditions with .loc[].
Querying data with query() method.
Handling time intervals with pd.Interval.
Handling time series data with pd.Period and pd.PeriodIndex.
Using pd.melt() for unpivoting data.
Handling data in wide and long formats with pd.wide_to_long().
Handling complex data transformations with groupby().apply().
Using cut() and qcut() for binning data into intervals.
Handling multi-index sorting using sort_index().
Applying shift() to create time lagged features in time series data.
Using where() for conditional replacement of values.
Converting text to lowercase/uppercase using str.lower() and str.upper().
Handling regular expressions with str.contains() and str.extract().
Creating data filters with .filter() method.
Using merge_asof() for approximate joins on non-unique keys.
Using pd.concat() to merge DataFrames with different columns.
Handling multi-level columns with pd.MultiIndex.from_product().
Handling time series data with pd.to_datetime().
Using rank() for calculating ranks of data.
Applying mode() to find the mode of a column.
Calculating the cumulative sum and product using cumsum() and cumprod().
Applying .reindex() for changing the order of rows or columns.
Handling data with non-standard date formats using pd.to_datetime().
Converting DataFrame to a dictionary using to_dict().
Finding the index of a value using idxmax() and idxmin().
Handling chained assignments with .loc[].
Handling negative values with abs() or custom functions.
Using merge() with indicator=True for tracking the origin of rows.
Using explode() to expand lists or series within cells.
Applying a rolling window function with rolling().apply().
Using query() method to filter rows based on complex conditions.
Handling cyclic data with pd.Categorical and pd.CategoricalDtype.
Calculating covariance and correlation using cov() and corr().
Applying a function element-wise with map().
Encoding ordinal data using pd.Categorical and cat.codes.
Converting DataFrame to CSV, Excel, or JSON using to_csv(), to_excel(), and to_json().
Using searchsorted() to find insertion points for values.
Applying a custom function to rows or columns using .apply() with axis=0 or `axis=
