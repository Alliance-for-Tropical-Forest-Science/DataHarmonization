# Tidying your files

At this stage, we want to make your data has one row per observation. So if you collected the same type of information in several columns (e.g. you added a column each time you visited a tree, or for each stem of the tree etc...), we need to "tidy" that.

First, you need to indicated what was the reason you added new columns for a new observation. In our example, we added a new column for each census, so we select `Census ID` in stem 1.

Next, you need to indicate what columns are the ones you added for each observation, and what they represent. We tried to help you with that, by detecting the columns that have similar names (like in our case, columns `dbh1`, `dbh2`... indicate the columns for dbh and `year1`, `year2`... indicate the year of each census ) but your columns may have very different names so you may need to indicate them yourself.

