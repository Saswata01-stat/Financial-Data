# Financial-Data
#Sorting a financial data in a presentable format
# codes relating to the sorting of the data
data_1=read.csv("Financial_sample.csv",header=T)
data_1
View(data_1)

segment=data_1$Segment
unique_segment=table(segment)
sorted_unique_segment=sort(unique_segment,decreasing=TRUE)
print(sorted_unique_segment)

country=data_1$Country
unique_country=table(country)
sorted_unique_country=sort(unique_country,decreasing=TRUE)
print(sorted_unique_country)

product=data_1$Product
unique_product=table(product)
sorted_unique_product=sort(unique_product,decreasing=TRUE)
print(sorted_unique_product)

discount=data_1$Discount.Band
unique_discount=table(discount)
sorted_unique_discount=sort(unique_discount,decreasing=TRUE)
print(sorted_unique_discount)

u=data_1$Units.Sold
u_1=sum(u>0&u<500);u_1
u_2=sum(u>499&u<1000);u_2
u_3=sum(u>999&u<1500);u_3
u_4=sum(u>1499&u<2000);u_4
u_5=sum(u>1999&u<2500);u_5
u_6=sum(u>2499&u<3000);u_6
u_7=sum(u>2999&u<3500);u_7
u_8=sum(u>3499&u<4000);u_8
u_9=sum(u>3999&u<4500);u_9
u_10=sum(u>4499&u<5000);u_10

mfp=data_1$Manufacturing.Price
unique_mfp=table(mfp)
sorted_unique_mfp=sort(unique_mfp,decreasing=TRUE)
print(sorted_unique_mfp)


str_mod=gsub("\\$","",data_1$Sale.Price);View(str_mod)
as.numeric(as.vector(str_mod[1]))
str_mod=gsub("\\,","",data_1$Sale.Price);str_mod
str_mod2=gsub("\\$","",str_mod);str_mod2
sum(as.numeric(str_mod2)>5)

str_mod3=gsub("\\$","",data_1$Gross.Sales);View(str_mod3)
as.numeric(as.vector(str_mod3[1]))
str_mod3=gsub("\\,","",data_1$Gross.Sales);str_mod3
str_mod4=gsub("\\$","",str_mod3);str_mod4
sum(as.numeric(str_mod4)>0&as.numeric(str_mod4)<100000)
sum(as.numeric(str_mod4)>99999&as.numeric(str_mod4)<200000)
sum(as.numeric(str_mod4)>199999&as.numeric(str_mod4)<300000)
sum(as.numeric(str_mod4)>299999&as.numeric(str_mod4)<400000)
sum(as.numeric(str_mod4)>399999&as.numeric(str_mod4)<500000)
sum(as.numeric(str_mod4)>499999)

str_mod5=gsub("\\$","",data_1$Sales);View(str_mod5)
as.numeric(as.vector(str_mod5[1]))
str_mod5=gsub("\\,","",data_1$Sales);str_mod5
str_mod6=gsub("\\$","",str_mod5);str_mod6
sum(as.numeric(str_mod6)>0&as.numeric(str_mod6)<100000)
sum(as.numeric(str_mod6)>99999&as.numeric(str_mod6)<200000)
sum(as.numeric(str_mod6)>199999&as.numeric(str_mod6)<300000)
sum(as.numeric(str_mod6)>299999&as.numeric(str_mod6)<400000)
sum(as.numeric(str_mod6)>399999&as.numeric(str_mod6)<500000)
sum(as.numeric(str_mod6)>499999&as.numeric(str_mod6)<600000)
sum(as.numeric(str_mod6)>599999&as.numeric(str_mod6)<700000)
sum(as.numeric(str_mod6)>699999)

str_mod7=gsub("\\$","-","",data_1$Discounts);View(str_mod7)
as.numeric(as.vector(str_mod7[1]))
str_mod7=gsub("\\,","",data_1$Discounts);str_mod7
str_mod8=gsub("\\$","",str_mod7);str_mod8
str_mod9=gsub("\\-","",str_mod8);str_mod9
sum(as.numeric(str_mod9)<1)
sum(as.numeric(str_mod9)>0&as.numeric(str_mod9)<5000)
sum(as.numeric(str_mod9)>4999&as.numeric(str_mod9)<10000)
sum(as.numeric(str_mod9)>9999&as.numeric(str_mod9)<15000)
sum(as.numeric(str_mod9)>14999&as.numeric(str_mod9)<20000)
sum(as.numeric(str_mod9)>19999&as.numeric(str_mod9)<25000)
sum(as.numeric(str_mod9)>24999)

str_mod10=gsub("\\$","",data_1$COGS);View(str_mod10)
as.numeric(as.vector(str_mod10[1]))
str_mod10=gsub("\\,","",data_1$COGS);str_mod10
str_mod11=gsub("\\$","",str_mod10);str_mod11
sum(as.numeric(str_mod11)>0&as.numeric(str_mod11)<100000)
sum(as.numeric(str_mod11)>99999&as.numeric(str_mod11)<200000)
sum(as.numeric(str_mod11)>199999&as.numeric(str_mod11)<300000)
sum(as.numeric(str_mod11)>299999&as.numeric(str_mod11)<400000)
sum(as.numeric(str_mod11)>399999&as.numeric(str_mod11)<500000)
sum(as.numeric(str_mod11)>499999&as.numeric(str_mod11)<600000)
sum(as.numeric(str_mod11)>599999&as.numeric(str_mod11)<700000)
sum(as.numeric(str_mod11)>699999)

str_mod12=gsub("\\$","",data_1$Profit);View(str_mod12)
as.numeric(as.vector(str_mod12[1]))
str_mod12=gsub("\\,","",data_1$Profit);str_mod12
str_mod13=gsub("\\$","",str_mod12);str_mod13
str_mod14=gsub("-","",str_mod13);str_mod14
str_mod15=gsub("[()]","",str_mod14);str_mod15
sum(as.numeric(str_mod15)>0&as.numeric(str_mod15)<100000)
