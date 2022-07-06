import requests
from bs4 import BeautifulSoup

def trade_spider(max_pages):
    page = 1
    while page <= max_pages:
        url = "https://thenewboston.com/videos.php?cat=98&video=20144" #+ str(page)
        source_code = request.get(url)
        plain_text = source_code.text
        soup = BeautifulSoup(plain_text)
        for link in soup.findAll("a", {"class": "itemname"}):
            href = link.get("href")
            print("href")
trade_spider(1)
def get_single_item_data(item_url):
    source_code = request.get(item_url)
    plain_text = source_code.text
    soup = BeautifulSoup(plain_text)
    for item_name in soup.findAll("a", {"class": "i-name"}):
        print(item_name.string)
    
