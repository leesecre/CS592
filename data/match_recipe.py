import pandas as pd

def filter_csv(base_file_path, second_file_path, output_file_path, column_name):
    """
    Filters the second CSV file to only include rows that have matching values in the specified column with the base file.

    Parameters:
    base_file_path (str): Path to the base CSV file.
    second_file_path (str): Path to the second CSV file to be filtered.
    output_file_path (str): Path where the filtered CSV file will be saved.
    column_name (str): The name of the column to match.
    """
    try:
        # Load the base file and create a set of unique values in the specified column
        base_df = pd.read_csv(base_file_path, encoding='ISO-8859-1')
        base_values = set(base_df[column_name])

        # Load the second file
        second_df = pd.read_csv(second_file_path, encoding='ISO-8859-1')

        # Filter the second file to only include rows with values in the base file
        filtered_df = second_df[second_df[column_name].isin(base_values)]

        # Save the filtered data to a new CSV file
        filtered_df.to_csv(output_file_path, index=False)
        print(f"Filtered data saved to {output_file_path}")
    except Exception as e:
        print(f"An error occurred: {e}")

# Example usage
filter_csv('recipes.csv', 'recipe_step_description.csv', 'desc_Recipe.csv', 'name')