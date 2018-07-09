# importing csv module
import csv

# initializing the titles and rows list
headers = []
rows = []
cloumn_dict = {}

def readCSVFile(filename, headers_list):
    global headers
    with open(filename, 'r') as csvfile:
        # creating a csv reader object
        csvreader = csv.reader(csvfile)
        # extracting field names through first row
        headers = csvreader.next()
        print headers
        # extracting each data row one by one
        for row in csvreader:
            rows.append(row)
        print("Total no. of rows: %d" % (csvreader.line_num))
    # printing the field names
    print('Field names are:' + ', '.join(field for field in headers))

    if not rows:
        print 'CSV file is Empty!!!'
    return rows, getCloumnIndex(headers_list)

def getAllRows():
    print rows

def getHeaders():
    print headers
    return headers

def getColumnNumbers():
    # Read the first row
    print headers

def getCloumnIndex(headers_list):
    global cloumn_dict
    for arg in headers_list:
        print arg
        if arg in headers:
            # print headers.index(arg)
            cloumn_dict[arg] = headers.index(arg)
        else:
            print 'Provided Cloumn didnt found on CSV headers!! You cant work with the varibale name: {}'.format(arg)
    return cloumn_dict
    # getColumns()

# Testing
def getColumns():
    for row in rows:
        for column_name in cloumn_dict.keys():
            print row[cloumn_dict[column_name]]

if __name__ == "__main__":
    test = ["header1","header2","header3"]
    filename =""
    readCSVFile(filename,test)
    # getAllRows()
    # print(getHeaders())
