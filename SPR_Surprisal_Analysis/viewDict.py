import pickle

# Assuming you have a serialized pickle file named 'your_file.pickle'
#file_path = 'wordDict.pickle'
file_path = 'gramDict_combined.pickle'

# Open the pickle file for reading in binary mode ('rb')
with open(file_path, 'rb') as file:
    # Load the contents of the pickle file using pickle.load()
    data = pickle.load(file)

# Now 'data' contains the contents of the serialized pickle object
print(data)