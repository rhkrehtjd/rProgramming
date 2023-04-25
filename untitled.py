from bs4 import BeautifulSoup
import urllib.request
import pandas as pd

result = []

for page in range(1,3):
    lotte = 'https://company.lottemart.com/shop/shop_search.asp?page=%d&list_num=10&schType=&schWord=&schDisplay=&schStartDate=&schEndDate='%page 
    print(lotte)
    html = urllib.request.urlopen(lotte)
    soup = BeautifulSoup(html, 'html.parser')
    tag_tbody = soup.find_all('tbody')[1]
    for store in tag_tbody.find_all('tr'):
        store_td = store.find_all('td')
        store_name = store_td[1].string
        store_address = store_td[4].find_all('div')[0].string
        store_num = store_td[2].find_all('div')[2].find_all('span')[1].string
        store_time = store_td[2].find_all('div')[2].find_all('span')[0].string
        result.append([store_name]+[store_address]+[store_num] + [store_time])


lotte_mart = pd.DataFrame(result, columns = ('store_name', 'store_address', 'store_num', 'store_time' ))

lotte_mart.to_csv("/Users/gwagdoseong/Documents/Rprogramming/lotte.csv", encoding = "cp949", mode = "w", index = True)