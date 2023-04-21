# Merging tables

In this stage, you can bring data from different tables together. 

You will need to do that if, e.g., your species or your plot information is stored in a different table than your inventory data.

In this example, we have our stacked tables (`StackedTables` - we stacked data from 3 censuses at the step before), and our species table(`SpeciesTable`), which has information that we want to merge in our inventory data.

Because we want to keep all the rows of the longer data set, with all the inventory data, we first select`StackedTables`. We then indicate what column in that dataset is the `key` column that should be used to link it to our species table. We then do the same to our species table.

NOTE 1: you need to click on the little arrow to be able to see the columns in your tables.

NOTE 2: If you need multiple columns to indicate the `key` that links the two tables together, you can select multiple columns. If you are doing so, make sure that you select them in the right order.

NOTE 3: If you have more than 2 tables to merge. Start with two. When you click on "Merge", you will be prompt to proceed to anoter merging. After this stage, you should be done to 1 table. 
