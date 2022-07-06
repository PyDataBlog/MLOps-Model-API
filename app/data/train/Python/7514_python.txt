#http://pandas.pydata.org/pandas-docs/stable/tutorials.html
#file='pand.py'
#exec(compile(open(file).read(), file, 'exec'))

from pandas import DataFrame, read_csv
import matplotlib.pyplot as plt
import pandas as pd
#import sys
#import matplotlib 


names = ['Bob','Jessica','Mary','John','Mel']
births = [968, 155, 77, 578, 973]

BabyDataSet = list(zip(names,births)) # zip pairs entries together and list combines the entries to a list

print(BabyDataSet)

#The DataFrame attribute of pandas reorganizes the list into a tabular panda object 
#similar to an sql table or an excel spreadsheet. 
df = pd.DataFrame(data = BabyDataSet, columns=['Names', 'Births'])
print(df)

wait = input("PRESS ENTER TO CONTINUE.")

#We can now save the content as a standard tabular data format (csv)
df.to_csv('births1880.csv',index=False,header=False)

#We can also read back from the same file into a panda object
df = pd.read_csv(r'births1880.csv')

print(df)

print('Wrong header. read_cvs treated the first record as the header')
print('set the header to null')
wait = input("PRESS ENTER TO CONTINUE.")

df = pd.read_csv(r'births1880.csv',header=None)
print(df)

print('Now we have the right data but no header')
print('Label the headers')
wait = input("PRESS ENTER TO CONTINUE.")

df = pd.read_csv(r'births1880.csv', names=['Names','Births'])

print(df)
print('This looks like the table we need')
print('Numbers of 0,1,2,3,4 are row numbers similar to an Excel spreadsheet')
wait = input("PRESS ENTER TO CONTINUE.")
print('Lets do something with this tabulated data')
print('Sort the dataframe and select the top row')
Sorted1=df.sort_values(['Births'], ascending=False)
#Sorted2=df.sort_values(by='Births', ascending=False)
#Sorted.head(1)
print(Sorted1)

wait = input("PRESS ENTER TO CONTINUE.")

print('Use the max() attribute to find the maximum value')
MaxValue=df['Births'].max()
print('MaxValue is ',MaxValue)


wait = input("PRESS ENTER TO CONTINUE.")

print('Convert a column to an array')
print(df['Names'].values)
print('Reference the second entry')
print(df['Names'][1:2].values)
print('Apply a booleen mask on the Births column when compared to the MaxValue')
mask = df['Births']==MaxValue
print(mask)
print('Find the name associated with the maximum value')
MaxName = df['Names'][mask].values
print('Name at Max Value is ',MaxName)
wait = input("PRESS ENTER TO CONTINUE.")

#Create a graph object
print('Create a graph object')
df['Births'].plot()

# Text to display on graph
print('Construct a string to display on the graph')
Text = str(MaxValue) + " - " + MaxName
print(Text)

# Add text to graph
print('Annonate the graph')
plt.annotate(Text, xy=(1, MaxValue), xytext=(8, 0), 
                 xycoords=('axes fraction', 'data'), textcoords='offset points')

print('Show the graph')
plt.show()
#Uncomment the following to save it as a png file
#plt.savefig('mygraph.png')
