#!env /usr/bin/python3

import sys
import urllib.parse
import urllib.request

def main():
    search = sys.argv[1]
    url = 'http://rarbg.to/torrents.php?order=seeders&by=DESC&search='
    url = url + search
    print(url)
    req = urllib.request.Request(url, headers={'User-Agent' : "Magic Browser"})
    resp = urllib.request.urlopen(req)
    respData = resp.read()

if __name__ == '__main__':
    main()
