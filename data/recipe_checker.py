import csv

def check_csv(file_path):
    with open(file_path, 'r', newline='') as csvfile:
        reader = csv.reader(csvfile)
        for row in reader:
            try:
                eval(row[1])
            except SyntaxError:
                    print(f"Row {row.count}: Unmatched quote in string - {row[1]}")
            try:
                eval(row[2])
            except SyntaxError:
                    print(f"Row {row.count}: Unmatched quote in string - {row[2]}")

# Replace 'your_file.csv' with the path to your CSV file
check_csv('recipes.csv')