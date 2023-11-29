import pandas as pd
import ast

# Step 1: Read the CSV file
df = pd.read_csv('recipes.csv')  # Replace 'your_file.csv' with your file name

# Step 2: Select the second column (index 1)
second_column = df.iloc[:, 1]  # iloc[:, 1] selects the second column

# Step 3: Evaluate string-like lists
second_column = second_column.apply(ast.literal_eval)

# Step 4: Aggregate the lists
aggregated_list = []
for lst in second_column:
    aggregated_list.extend(lst)

# Step 5: Remove duplicates, if needed
aggregated_list = list(set(aggregated_list))

# Step 6: Sort the list alphabetically
aggregated_list.sort()

# Step 7: Save to CSV or Output as List
# Save to CSV
aggregated_list_df = pd.DataFrame(aggregated_list, columns=['Ingradients'])
aggregated_list_df.to_csv('Ingradients_list.csv', index=False)