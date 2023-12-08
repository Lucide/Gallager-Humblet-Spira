import os
import json
import yaml



def load(filename):
    fileextension = os.path.splitext(filename)[1]
    if fileextension == ".json":
        try:
            with open(filename, 'r') as file:
                return json.load(file)
        except FileNotFoundError:
            print(f"File {filename} not found.")
            exit()
    elif fileextension == ".yaml":
        try:
            with open(filename, 'r') as file:
                return yaml.safe_load(file)
        except FileNotFoundError:
            print(f"File {filename} not found.")
            exit()
    else:
        assert False, f"File extension {fileextension} not recognized."


def save(filename, data):
    fileextension = os.path.splitext(filename)[1]
    if fileextension == ".json":
        with open(filename, 'w') as file:
            return json.dump(data, file, indent=2)
    elif fileextension == ".yaml":
        with open(filename, 'w') as file:
            return yaml.dump(data, file, default_flow_style=False)
    else:
        assert False, f"File extension {fileextension} not recognized."
