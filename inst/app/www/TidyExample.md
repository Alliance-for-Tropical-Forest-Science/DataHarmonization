# Tidying your files

At this stage, we want to make sure your data has one row per observation and one column per variable.

So if you collected the same type of information in several columns (e.g. you added a column each time you visited a tree, or for each stem of the tree etc...), we need to "tidy" that.

First, you need to indicated what was the reason you added new columns for a new observation. 

In this example, our dataset has multiple columns for the dbh measurements: one per census. It also has multiple columns for year, to indicate the year in which each census happened.

Because we added a new column for each census, we select `Census ID` in the first part of the form.

Next, we need to select what columns are the ones we added for each observation, and what variable they represent. The app tries to help by detecting the columns that have similar names (like in our case, columns `dbh1`, `dbh2`... indicate the columns for dbh and `year1`, `year2`... indicate the year of each census ) but your columns may have very different names so you may need to indicate them yourself.
