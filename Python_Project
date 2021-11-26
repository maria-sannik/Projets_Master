'''list = ["Crops  Surface(in ha)  Yield(in t/ha)  Pdtion Cost(euro/t)  Sell Price (euro/t)",
        "DurumWheat  3 5.9  360  272",
        "SoftWheat  3  7.9  214  183",
        "Rapeseed  1  3.5  338  430",
        "WinterBarley  4  7.1  216  185",
        "Sunflower  6  2.3  497  408",
        "Peas  5  5.1  304  213",
        "Maize  2  10.5  206  170"]'''

#Part 1
#%%
list = ["DurumWheat 3 5.9 360 272",
        "SoftWheat 3 7.9 214 183",
        "Rapeseed  1  3.5  338  430",
        "WinterBarley  4  7.1  216  185",
        "Sunflower  6  2.3  497  408",
        "Peas  5  5.1  304  213",
        "Maize  2  10.5  206  170"]
for i in range(len(list)):
    list[i]=list[i]+"\n"
    print(list[i])

myfile = open('Tom_Crops.txt','w')
myfile.writelines(list)
myfile.close()

l=[]
for line in list:
    Crop_Surface_Yield_PdtionCost_SellPrice = line.split()
    Surface = float(Crop_Surface_Yield_PdtionCost_SellPrice [1])
    Yield = float(Crop_Surface_Yield_PdtionCost_SellPrice [2])
    PdtionCost = float(Crop_Surface_Yield_PdtionCost_SellPrice[3])
    Selling = float(Crop_Surface_Yield_PdtionCost_SellPrice[4])
    Tot_Yield = round(Surface * Yield)
    Tot_Cost = round(Tot_Yield * PdtionCost)
    Turnover = round(Tot_Yield * Selling)
    Profit = round(Turnover-Tot_Cost)

    l.append(Tot_Yield)
    l.append(Tot_Cost)
    l.append(Turnover)
    l.append(Profit)
print( "Display of Total yield, Total cost of production and Turnover:")
print( "DurumWheat ", l[0:4])
print( "SoftWheat ", l[4:8])
print( "Rapeseed", l[8:12])
print( "WinterBarley", l[12:16])
print( "Sunflower", l[16:20])
print( "Peas", l[20:24])
print( "Maize", l[24:28])

myfile2 = open('conventional_agriculture','w')
myfile2.writelines(str(l))
myfile2.close()

with open('Tom_Crops.txt','w') as myfile:
    myfile.writelines(list)

list_Tom_selling_price = []
list_Tom_profits = []
for i, crop in enumerate(list):
    crop = crop.split(',')
    crop_surface = float(list[i].split()[1])
    crop_yield = float(list[i].split()[2])
    cost_per_ton = float(list[i].split()[3])
    price_per_ton = float(list[i].split()[4])
    crop_Tom_selling_price = crop_surface * crop_yield * price_per_ton
    list_Tom_selling_price.append(crop_Tom_selling_price)
    print('Selling Price ' + crop[0] +': '+ str(crop_Tom_selling_price))
    crop_Tom_profit = round(crop_Tom_selling_price - crop_surface * crop_yield * cost_per_ton)
    list_Tom_profits.append([crop[0], crop_Tom_profit])
    print(list_Tom_profits)

def display_not_profitable(list_Tom_profits, threshold_profitable=0):
    print('All unprofitable crops')
    for name, profit in list_Tom_profits:
        if profit <= threshold_profitable:
            print('Crop Name: '+ name + 'Profit: '+ str(profit))

display_not_profitable(list_Tom_profits, threshold_profitable=10)
list_sorted_profits = sorted(list_Tom_profits, key=lambda x:x[1], reverse=False)

#%%
#PART2:
#It is considered that the areas for each crop are identical and the production costs are unchanged.
# We admit that the selling prices in organic farming are 30% higher than those in conventional farming.
#The table above summarizes the yields of each crop in organic farming.

#- Create the table in a .txt file and import these data in Python.
# Keep in mind you need data from the previous table.

#- With Python, create a .txt file to save your results. You can name it “organic_farming.txt”
#- Display the selling prices of each crop in organic farming.
#- Calculate the profit for each crop of Sébastien
#- Using a function, display crops that are not profitable. (expected: crops names and
#   their profits)
#- Sort crops in ascending order of profit.

print("\n PART 2\n")
list1 = ["DurumWheat 3 5.9 360 272",
        "SoftWheat 3 7.9 214 183",
        "Rapeseed  1  3.5  338  430",
        "WinterBarley  4  7.1  216  185",
        "Sunflower  6  2.3  497  408",
        "Peas  5  5.1  304  213",
        "Maize  2  10.5  206  170"]


result = []
for line in list1:
    Crop_Surface_Yield_PdtionCost_SellPrice = line.split()
    Surface = float(Crop_Surface_Yield_PdtionCost_SellPrice[1])
    Yield = float(Crop_Surface_Yield_PdtionCost_SellPrice[2])
    PdtionCost = float(Crop_Surface_Yield_PdtionCost_SellPrice[3])
    Selling = float(Crop_Surface_Yield_PdtionCost_SellPrice[4])
    Tot_Yield = Surface*Yield
    Tot_Cost = Tot_Yield*PdtionCost
    Tot_Sell = Tot_Yield*Selling
    Turnover = Tot_Yield*Selling
    Profit = Tot_Sell-Tot_Cost
    result.append([Tot_Yield,Tot_Cost,Tot_Sell,Turnover,Profit])


list_organic_farming = ['Durum wheat,3,2.8\n',
                        'Soft wheat,3,3,2\n',
                        'Rapeseed,1,2.5\n',
                        'Winter barley,4,3.0\n',
                        'Sunflower,6,2.5\n',
                        'Peas,5,2.0\n',
                        'Maize,2,5.4\n']

with open('organic_farming.txt','w') as myfile:
    myfile.writelines(list_organic_farming)

list_selling_price = []
list_profits = []
for i, crop in enumerate(list_organic_farming):
    crop = crop.split(',')
    crop_surface = float(crop[1])
    crop_yield = float(crop[2])
    cost_per_ton = float(list[i].split()[3])
    price_per_ton = float(list[i].split()[4])
# calculation
    crop_selling_price = crop_surface * crop_yield * price_per_ton * 1.3
    list_selling_price.append(crop_selling_price)
# Display
    print('Selling Price ' + crop[0] +': '+ str(crop_selling_price))

# Calculate Profit
    crop_profit = round(crop_selling_price - crop_surface * crop_yield * cost_per_ton)
    list_profits.append([crop[0], crop_profit])
    print(list_profits)

# Define function
def display_not_profitable(list_profits, threshold_profitable=0):
    print('All unprofitable crops')
    for name, profit in list_profits:
        if profit <= threshold_profitable:
            print('Crop Name: '+ name + 'Profit: '+ str(profit))

# Use the funcion
display_not_profitable(list_profits, threshold_profitable=10)
# Sorted the list by its second element/column
list_sorted_profits = sorted(list_profits, key=lambda x:x[1], reverse=False)

#Part 3: comparison between conventional and organic agriculture
# We want to compare the results between Tom and Sébastien.
# - Calculate and display the total profit of all crops for Tom and Sébastien.
# - Compare Tom's results to Sébastien's. Display a message indicating who, between
# Tom or Sebastian, has the best results
# - (optional) Create a button with the message “Who has better results?”. When you click
# on it, it displays the related name.
print("\n PART 3\n")

total_profit_Tom = sum([x[4] for x in result])
total_profit_Seb = sum([x[1] for x in list_profits])

print('Total Profit of Tom:', total_profit_Tom)
print('Total Profit of Sébastien:', total_profit_Seb)
if total_profit_Tom < total_profit_Seb:
    print('Sébastien has better results')
else:
    print('Tom has better results')
#%%
# button
from tkinter import *

def info():
    winner = 'Sébastien' if total_profit_Tom < total_profit_Seb else 'Tom'
    lab = Label(root, text=winner)
    lab.pack()

root = Tk()

but = Button(root, text ="Who has better results ?", command = info,width=40,height=20, foreground="purple",)

but.pack()
root.mainloop()

#%%
# For further
# Preparation

# redefine the two table (list) to a list of lists
# crops, surface, yield, production cost, selling price
table_conv_agri  = []
for line in list1:
    x = line.split()
    table_conv_agri.append([x[0],float(x[1]),float(x[2]),float(x[3]),float(x[4])])

# crops, surface, yield, production cost, selling price
table_org_agri = []
for i,line in enumerate(list_organic_farming):
    x = line.split(',')
    table_org_agri.append([x[0],float(x[1]),float(x[2]),table_conv_agri[i][3],table_conv_agri[i][4]*1.3])

table_intrans_agri = []
for i,line in enumerate(table_conv_agri):
    table_intrans_agri.append(table_org_agri[i][0:4] + [table_conv_agri[i][4]])

def func_profit_for_one_crops(line):
    return round(line[1]*line[2]*(line[4]-line[3]),1)


#This part is optional.
#In France, it takes 3 years to be certified in organic farming. During these years, the farmer has the same cost production. He sells his production at the conventional agriculture price but with the organic yield.
#In this last part, we want to determine the cost of transition.
#- In a new .txt file, calculate yield loss between conventional agriculture and organic farming. Display it in percentage.
#- Calculate the profit for one year of change.
#- Display the total profit for 3 years.
#- Try to display a graphic with all the data between conventional agriculture, organic
#farming and in transition agriculture.


# In a new .txt file, calculate yield loss between conventional agriculture and organic farming. Display it in percentage.
yield_loss = []
print('\nYield Loss:')
for i in range(len(table_conv_agri)):
    yield_loss.append((table_conv_agri[i][0], round(table_conv_agri[i][2]-table_org_agri[i][2],1)))
    print(yield_loss[i])

with open('yield_loss.txt','w') as txtfile:
    txtfile.writelines(map(str,yield_loss))
 
# Calculate the profit for one year of change.
profit_one_year_in_trans = []
print('\nProfit for one year in transition:')
for i in range(len(table_conv_agri)):
    profit_one_year_in_trans.append(round(yield_loss[i][1]*table_conv_agri[i][1]*(table_conv_agri[i][4]-table_conv_agri[i][3]),1))
    print(table_conv_agri[i][0], profit_one_year_in_trans[i])

# Display the total profit for 3 years.
print('\nTotal profit for 3 years in transition:')
print(sum(profit_one_year_in_trans)*3)

# Define the table with all data
table_data_conv_agri = []
table_data_org_agri = []
table_data_intrans_agri = []
table_data_profits_compare = []

for i in range(len(table_conv_agri)):
    table_data_conv_agri.append(table_conv_agri[i] + [func_profit_for_one_crops(table_conv_agri[i])])
    table_data_org_agri.append(table_org_agri[i] + [func_profit_for_one_crops(table_org_agri[i])])
    table_data_intrans_agri.append(table_intrans_agri[i] + [func_profit_for_one_crops(table_intrans_agri[i])])
    table_data_profits_compare.append([table_conv_agri[i][0], table_data_conv_agri[i][5], table_data_org_agri[i][5], table_data_intrans_agri[i][5]])

#%%
# Display
print("\n{:-^76}".format('Data conventional agriculture'))
print('{:^14}|{:^8}|{:^8}|{:^16}|{:^16}|{:^8}'.format('Crops', 'Surface', 'Yield', 'Production Costs', 'Selling Price', 'Profit'))
print('{:-<76}'.format(''))
for line in table_data_conv_agri:
    print('{:<14}|{:<8}|{:<8}|{:<16}|{:<16}|{:<8}'.format(line[0],line[1],line[2],line[3],line[4],line[5]))
print('{:<14}|{:<8}|{:<8}|{:<16}|{:<16}|{:<8.1f}'.format('Total','-','-','-','-',sum([line[5] for line in table_data_conv_agri])))
print('{:-<76}'.format(''))

print("\n{:-^76}".format('Data organic agriculture'))
print('{:^14}|{:^8}|{:^8}|{:^16}|{:^16}|{:^8}'.format('Crops', 'Surface', 'Yield', 'Production Costs', 'Selling Price', 'Profit'))
print('{:-<76}'.format(''))
for line in table_data_org_agri:
    print('{:<14}|{:<8}|{:<8}|{:<16}|{:<16.1f}|{:<8}'.format(line[0],line[1],line[2],line[3],line[4],line[5]))
print('{:<14}|{:<8}|{:<8}|{:<16}|{:<16}|{:<8}'.format('Total','-','-','-','-',sum([line[5] for line in table_data_org_agri])))
print('{:-<76}'.format(''))

print("\n{:-^76}".format('Data in transition agriculture'))
print('{:^14}|{:^8}|{:^8}|{:^16}|{:^16}|{:^8}'.format('Crops', 'Surface', 'Yield', 'Production Costs', 'Selling Price', 'Profit'))
print('{:-<76}'.format(''))
for line in table_data_intrans_agri:
    print('{:<14}|{:<8}|{:<8}|{:<16}|{:<16}|{:<8}'.format(line[0],line[1],line[2],line[3],line[4],line[5]))
print('{:<14}|{:<8}|{:<8}|{:<16}|{:<16}|{:<8}'.format('Total','-','-','-','-',sum([line[5] for line in table_data_intrans_agri])))
print('{:-<76}'.format(''))


print("\n{:-^100}".format('Profit comparison'))
print('{:^14}|{:^25}|{:^25}|{:^25}'.format('Crops', 'Profit Conventional Agri', 'Profit Organic Agri', 'Profit In Transition Agri'))
print('{:-<100}'.format(''))
for line in table_data_profits_compare:
    print('{:^14}|{:^25}|{:^25}|{:^25}'.format(line[0],line[1],line[2],line[3]))
print('{:^14}|{:^25.1f}|{:^25}|{:^25}'.format('Total',sum([line[5] for line in table_data_conv_agri]), sum([line[5] for line in table_data_org_agri]), sum([line[5] for line in table_data_intrans_agri])))
print('{:-<100}'.format(''))


# install pandas
#install matplotlib

import pandas as pd
table_data_profits_compare.append(['Total',sum([line[5] for line in table_data_conv_agri]), sum([line[5] for line in table_data_org_agri]), sum([line[5] for line in table_data_intrans_agri])])
df = pd.DataFrame(table_data_profits_compare,columns=['Crops', 'Profit Conventional Agri', 'Profit Organic Agri', 'Profit In Transition Agri'])
ax = df.plot(kind='bar')
ax.set_xticklabels(df.Crops, rotation=90)
from matplotlib import pyplot
pyplot.show()
# 
