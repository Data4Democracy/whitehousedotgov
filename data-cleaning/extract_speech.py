import pandas as pd
import re

df = pd.read_csv('../../data/TrumpWhiteHouseNewsPosts.csv')

remarks = df[df['subType'] == 'Remarks']

remarks = remarks[remarks['text'].str.contains("THE PRESIDENT:")]

for i, row in remarks.iterrows():
    text = row.text

    #Removing unwanted substrings
    text = re.sub('\(.*?\)', '', text)
    text = re.sub("\xa0", "", text)
    text = re.sub('END.*', '', text)

    #Split text into statements by the president
    text_list = text.split("THE PRESIDENT:")

    #Removing all introductory material before first statement
    del text_list[0]

    #Removing all other speakers
    for j, line in enumerate(text_list):
        line = re.sub('[A-Z -]+:.*', '', line)
        line = re.sub('MR..*', '', line)
        line = re.sub('MRS.*', '', line)
        line = re.sub('MS.*', '', line)
        line = re.sub('Q .*', '', line)
        text_list[j] = line

    newtext = ''.join(text_list)
    remarks.at[i, 'text'] = newtext

remarks.to_csv('../../TrumpSpokenRemarks.csv', index = False)
