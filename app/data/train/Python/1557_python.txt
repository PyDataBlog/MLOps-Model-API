# Python Code From Book

# This file consists of code snippets only
# It is not intended to be run as a script
raise SystemExit


####################################################################
# 3. Thinking in Binary
####################################################################


import magic
print magic.from_file("my_image.jpg")
# JPEG image data, Exif standard: [TIFF image data, big-endian,
# direntries=16, height=3264, bps=0, PhotometricIntepretation=RGB],
# baseline, precision 8, 2378x2379, frames 3



if magic.from_file("upload.jpg", mime=True) == "image/jpeg":
    continue_uploading("upload.jpg")
else:
    alert("Sorry! This file type is not allowed")



import imghdr
print imghdr.what("path/to/my/file.ext")



import binascii
  
def spoof_file(file, magic_number):
    magic_number = binascii.unhexlify(magic_number)
    with open(file, "r+b") as f:
        old = f.read()
        f.seek(0)
        f.write(magic_number + old)
  


def to_ascii_bytes(string):
    return " ".join(format(ord(char), '08b') for char in string)



string = "my ascii string"
"".join(hex(ord(char))[2:] for char in string)
# '6d7920617363696920737472696e67'



hex_string = "6d7920617363696920737472696e67"
hex_string.decode("hex")
# 'my ascii string'
"".join(chr(int(hex_string[i:i+2], 16)) for i in range(0, len(hex_string), 2))
# 'my ascii string'



# adapted from https://code.activestate.com/recipes/142812-hex-dumper/
def hexdump(string, length=8):
    result = []
    digits = 4 if isinstance(string, unicode) else 2

    for i in xrange(0, len(string), length):
        s = string[i:i + length]
        hexa = "".join("{:0{}X}".format(ord(x), digits) for x in s)
        text = "".join(x if 0x20 <= ord(x) < 0x7F else '.' for x in s)
        result.append("{:04X}   {:{}}   {}".format(i, hexa, length * (digits + 1), text))

    return '\n'.join(result)



with open("/path/to/my_file.ext", "r") as f:
    print hexdump(f.read())



import struct

num = 0x103e4
struct.pack("I", 0x103e4)
# '\xe4\x03\x01\x00'



string = '\xe4\x03\x01\x00'
struct.unpack("i", string)
 # (66532,)



bytes = '\x01\xc2'
struct.pack("<h", struct.unpack(">h", bytes)[0])
 # '\xc2\x01'



import base64

base64.b64encode('encodings are fun...')
 # 'ZW5jb2RpbmdzIGFyZSBmdW4uLi4='
base64.b64decode(_)
 # 'encodings are fun...'



string = "hello\x00"
binary_string = ' '.join('{:08b}'.format(ord(char)) for char in string)
" ".join(binary_string[i:i+6] for i in range(0, len(binary_string), 6))
 # '011010 000110 010101 101100 011011 000110 111100 000000'



bin_string = '011010 000110 010101 101100 011011 000110 111100 000000'
[int(b, 2) for b in bin_string.split()]
 # [26, 6, 21, 44, 27, 6, 60, 0]



u'◑ \u2020'.encode('utf8')
# '\xe2\x97\x91 \xe2\x80\xa0'



'\xe2\x97\x91 \xe2\x80\xa0'.decode('utf8')
# u'\u25d1 \u2020'



unicode('\xe2\x97\x91 \xe2\x80\xa0', encoding='utf8')
 # u'\u25d1 \u2020'



utf8_string = 'Åêíòü'
utf8_string
# '\xc3\x85\xc3\xaa\xc3\xad\xc3\xb2\xc3\xbc'
unicode_string = utf8_string.decode('utf8')
unicode_string
# u'\xc5\xea\xed\xf2\xfc'
unicode_string.encode('mac roman')
# '\x81\x90\x92\x98\x9f'


'Åêíòü'.decode('utf8').encode('ascii')
 # Traceback (most recent call last):
 #   File "<stdin>", line 1, in <module>
 # UnicodeEncodeError: 'ascii' codec can't encode characters in position 0-4: ordinal not in range(128)




 file = """潍楪慢敫椠⁳桴⁥慧扲敬⁤整瑸琠慨⁴獩琠敨爠獥汵⁴景琠硥⁴敢湩⁧敤潣敤⁤獵湩⁧湡甠楮瑮湥敤⁤档
 牡捡整⁲湥潣楤杮楷桴挠浯汰瑥汥⁹湵敲慬整⁤湯獥景整⁮牦浯愠搠晩敦敲瑮眠楲楴杮猠獹整⹭‧⠊慔敫⁮
 牦浯攠⹮楷楫数楤⹡牯⥧"""


print file.decode('utf8').encode('utf16')
 # ??Mojibake is the garbled text that is the result of text being decoded using an
 # unintended character encoding with completely unrelated ones, often from a
 # different writing system.' (Taken from en.wikipedia.org)


import ftfy
ftfy.fix_text(u"â€œMojibakeâ€œ can be fixed.")
# u'"Mojibake" can be fixed.'


bin(0b1010 & 0b1111110111)
# '0b10'

bin(0b1010 | 0b0110)
# '0b1110'

bin(0b10111 | 0b01000)
# '0b11111'

 bin(0b100 ^ 0b110)
 # '0b10'


bin(-0b1010 >> 0b10)
 # '-0b11'


x = 0b1111
y = 0b1010
bin(int("{:b}{:b}".format(x, y), 2))
 # '0b11111010'


bin(x << 4 | y)
 # '0b11111010'



####################################################################
# 4. Cryptography
####################################################################


import random
import string
  
r = random.SystemRandom()

# Get a random integer between 0 and 20
r.randint(0, 20)
# 5
  
# Get a random number between 0 and 1
r.random()
# 0.8282475835972263
  
# Generate a random 40-bit number
r.getrandbits(40)
# 595477188771L

# Choose a random item from a string or list
chars = string.printable
r.choice(chars)
# 'e'

 # Randomize the order of a sequence
seq = ['a', 'b', 'c', 'd', 'e']
r.shuffle(seq)

print seq
# ['c','d', 'a', 'e', 'b']



"ALLIGATOR".encode('rot13')
# 'NYYVTNGBE'
"NYYVTNGBE".encode('rot13')
# 'ALLIGATOR'



plaintext = "A secret-ish message!"
"".join(chr((ord(c) + 20) % 256) for c in plaintext)
# 'U4\x87yw\x86y\x88A}\x87|4\x81y\x87\x87u{y5'


ciphertext = 'U4\x87yw\x86y\x88A}\x87|4\x81y\x87\x87u{y5'
"".join(chr((ord(c) - 20) % 256) for c in ciphertext)
 # 'A secret-ish message!'


plaintext = 0b110100001101001
one_time_pad = 0b110000011100001
bin(plaintext ^ one_time_pad)
 # '0b100010001000'



decrypted = 0b100010001000 ^ one_time_pad
format(decrypted, 'x').decode('hex')
 # 'hi'


import os
import binascii

# ASCII-encoded plaintext
plaintext = "this is a secret message"
plaintext_bits = int(binascii.hexlify(plaintext), 16)

print "plaintext (ascii):", plaintext
print "plaintext (hex):", plaintext_bits

# Generate the one-time pad
onetime_pad = int(binascii.hexlify(os.urandom(len(plaintext))), 16)

print "one-time pad: (hex):", onetime_pad

# Encrypt plaintext using XOR operation with one-time pad
ciphertext_bits = plaintext_bits ^ onetime_pad

print "encrypted text (hex):", ciphertext_bits

# Decrypt using XOR operation with one-time pad
decrypted_text = ciphertext_bits ^ onetime_pad
decrypted_text = binascii.unhexlify(hex(decrypted_text)[2:-1])

print "decrypted text (ascii):", decrypted_text




import random
import binascii

p1 = "this is the part where you run away"
p2 = "from bad cryptography practices."

# pad plaintexts with spaces to ensure equal length
p1 = p1.ljust(len(p2))
p2 = p2.ljust(len(p1))

p1 = int(binascii.hexlify(p1), 16)
p2 = int(binascii.hexlify(p2), 16)

# get random one-time pad
otp = random.SystemRandom().getrandbits(p1.bit_length())

# encrypt
c1 = p1 ^ otp
c2 = p2 ^ otp  # otp reuse...not good!

print "c1 ^ c2 == p1 ^ p2 ?", c1 ^ c2 == p1 ^ p2
print "c1 ^ c2 =", hex(c1 ^ c2)

# the crib
crib = " the "
crib = int(binascii.hexlify(crib), 16)

xored = c1 ^ c2

print "crib =", hex(crib)

cbl = crib.bit_length()
xbl = xored.bit_length()

print
mask = (2**(cbl + 1) - 1)
fill = len(str(xbl / 8))

# crib dragging
for s in range(0, xbl - cbl + 8, 8):
    xor = (xored ^ (crib << s)) & (mask << s)
    out = binascii.unhexlify(hex(xor)[2:-1])

    print "{:>{}}   {}".format(s/8, fill, out)





from cryptography.fernet import Fernet
key = Fernet.generate_key()
f = Fernet(key)
ciphertext = f.encrypt("this is my plaintext")
decrypted = f.decrypt(ciphertext)
print decrypted
# this is my plaintext



import os
from cryptography.hazmat.primitives import padding
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.backends import default_backend

pt = "my plaintext"

backend = default_backend()
key = os.urandom(32)
iv = os.urandom(16)

padder = padding.PKCS7(128).padder()
pt = padder.update(pt) + padder.finalize()

cipher = Cipher(algorithms.AES(key), modes.CBC(iv), backend=backend)
encryptor = cipher.encryptor()
ct = encryptor.update(pt) + encryptor.finalize()
decryptor = cipher.decryptor()
out = decryptor.update(ct) + decryptor.finalize()

unpadder = padding.PKCS7(128).unpadder()
out = unpadder.update(out) + unpadder.finalize()
print out



import hashlib
hashlib.md5("hash me please").hexdigest()
# '760d92b6a6f974ae11904cd0a6fc2e90'


hashlib.sha1("hash me please").hexdigest()
 # '1a58c9b3d138a45519518ee42e634600d1b52153'


import os
from cryptography.hazmat.primitives.kdf.scrypt import Scrypt
from cryptography.hazmat.backends import default_backend

backend = default_backend()
salt = os.urandom(16)

kdf = Scrypt(salt=salt, length=64, n=2**14, r=8, p=1, backend=backend)
key = kdf.derive("your favorite password")
key



import hmac
import hashlib

secret_key = "my secret key"
ciphertext = "my ciphertext"

# generate HMAC
h = hmac.new(key=secret_key, msg=ciphertext, digestmod=hashlib.sha256)
print h.hexdigest()

# verify HMAC
hmac.compare_digest(h.hexdigest(), h.hexdigest())




p = 9576890767
q = 1299827
n = p * q
print n
# 12448301194997309

e = 65537
phi = (p - 1) * (q - 1)
phi % e != 0
# True

import sympy
d = sympy.numbers.igcdex(e, phi)[0]
print d
# 1409376745910033

m = 12345
c = pow(m, e, n)
print c
 # 3599057382134015

pow(c, d, n)
# 12345



m = 0
 while pow(m, e, n) != c:
     m += 1
 print m



from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives.asymmetric import rsa
from cryptography.hazmat.primitives import serialization

private_key = rsa.generate_private_key(public_exponent=65537, key_size=2048, b
    ackend=default_backend())

public_key = private_key.public_key()

private_pem = private_key.private_bytes(encoding=serialization.Encoding.PEM,
    format=serialization.PrivateFormat.PKCS8,
    encryption_algorithm=serialization.BestAvailableEncryption('your password here'))

public_pem = public_key.public_bytes(encoding=serialization.Encoding.PEM,
    format=serialization.PublicFormat.SubjectPublicKeyInfo)

print public_pem
print private_pem



from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import padding
import base64

with open("path/to/public_key.pem", "rb") as key_file:
    public_key = serialization.load_pem_public_key(key_file.read(),
                                                   backend=default_backend())

message = "your secret message"
ciphertext = public_key.encrypt(message,
                                padding.OAEP(mgf=padding.MGF1(algorithm=hashes.SHA256()),
                                             algorithm=hashes.SHA256(),
                                             label=None))
b64_ciphertext = base64.urlsafe_b64encode(ciphertext)
print b64_ciphertext




plaintext = private_key.decrypt(ciphertext,
                                padding.OAEP(mgf=padding.MGF1(algorithm=hashes.SHA256()),
                                             algorithm=hashes.SHA256(),
                                             label=None))
print plaintext




from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.asymmetric import padding

signer = private_key.signer(padding.PSS(mgf=padding.MGF1(hashes.SHA256()), salt_length=padding.PSS.MAX_LENGTH), hashes.SHA256())
message = "A message of arbitrary length"
signer.update(message)
signature = signer.finalize()




public_key = private_key.public_key()
verifier = public_key.verifier(signature, padding.PSS(mgf=padding.MGF1(hashes.SHA256()), salt_length=padding.PSS.MAX_LENGTH), hashes.SHA256())
verifier.update(message)
verifier.verify()



####################################################################
# 5. Networking
####################################################################


import requests
r = requests.get('https://www.google.com/imghp')
r.content[:200]

 # View status code
 r.status_code
 # 200
  
# View response header fields
r.headers
#  {'Alt-Svc': 'quic=":443"; ma=2592000; v="36,35,34"',
#   'Cache-Control': 'private, max-age=0',
#   'Content-Encoding': 'gzip',
#   'Content-Type': 'text/html; charset=ISO-8859-1',
#  'Expires': '-1',
#   'P3P': 'CP="This is not a P3P policy! See https://www.google.com/support/accounts/answer/151657?hl=en for more info."',
#   'Server': 'gws',
# path=/; domain=.google.com; HttpOnly',
#  'Transfer-Encoding': 'chunked',
#  'X-Frame-Options': 'SAMEORIGIN',
#  'X-XSS-Protection': '1; mode=block'}


# Get content length in bytes
len(r.content)
# 10971

# Encoding
r.apparent_encoding
# 'ISO-8859-2'

# Time elapsed during request
r.elapsed
# datetime.timedelta(0, 0, 454447)

r.request.headers
 # {'Accept': '*/*',
 #  'Accept-Encoding': 'gzip, deflate',
 #  'Connection': 'keep-alive',
 #  'User-Agent': 'python-requests/2.12.4'}


custom_headers = {"user-agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36"}
r = requests.get("https://www.google.com/imghp", headers=custom_headers)
r.request.headers
 # {'Accept': '*/*',
 #  'Accept-Encoding': 'gzip, deflate',
 #  'Connection': 'keep-alive',
 #  'user-agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36'}




import requests
import logging

import http.client as http_client
http_client.HTTPConnection.debuglevel = 1

logging.basicConfig()
logging.getLogger().setLevel(logging.DEBUG)
requests_log = logging.getLogger("requests.packages.urllib3")
requests_log.setLevel(logging.DEBUG)
requests_log.propagate = True
r = requests.get('https://www.google.com/')

#  send: 'GET / HTTP/1.1\r\nHost: www.google.com\r\nConnection: keep-alive\r\nAccept-Encoding: gzip, deflate\r\nAccept: */*\r\nUser-Agent: python-requests/2.12.4\r\n\r\n'
#  reply: 'HTTP/1.1 200 OK\r\n'
#  header: Expires: -1
#  header: Cache-Control: private, max-age=0
#  header: Content-Type: text/html; charset=ISO-8859-1
#  header: P3P: CP="This is not a P3P policy! See https://www.google.com/support/accounts/answer/151657?hl=en for more info."
#  header: Content-Encoding: gzip
#  header: Server: gws
#  header: X-XSS-Protection: 1; mode=block
#  header: X-Frame-Options: SAMEORIGIN


import urlparse
simple_url = "http://www.example.com/path/to/my/page"
parsed = urlparse.urlparse(simple_url)
parsed.scheme
parsed.hostname
parsed.path


url_with_query = "http://www.example.com/?page=1&key=Anvn4mo24"
query = urlparse.urlparse(url_with_query).query
urlparse.parse_qs(query)
# {'key': ['Anvn4mo24'], 'page': ['1']}


import urllib
url = 'https://www.example.com/%5EA-url-with-%-and-%5E?page=page+with%20spaces'
urllib.unquote(url)
# 'https://www.example.com/^A-url-with-%-and-^?page=page+with spaces'

chars = '!@#$%^%$#)'
urllib.quote(chars)
# '%21%40%23%24%25%5E%25%24%23%29'


urllib.unquote_plus(url)
# 'https://www.example.com/^A-url-with-%-and-^?page=page with spaces'
urllib.quote_plus('one two')
'one+two'


import requests
from bs4 import BeautifulSoup
r = requests.get("http://www.google.com")
soup = BeautifulSoup(r.content, "lxml")

soup.find_all('p')

soup.find_all('a')
# [<a class="gb1" href="http://www.google.com/imghp?hl=en&amp;tab=wi">Images</a>,
#  <a class="gb1" href="http://maps.google.com/maps?hl=en&amp;tab=wl">Maps</a>,
#  <a class="gb1" href="https://play.google.com/?hl=en&amp;tab=w8">Play</a>,
#  <a class="gb1" href="http://www.youtube.com/?tab=w1">YouTube</a>,
#  <a class="gb1" href="http://news.google.com/nwshp?hl=en&amp;tab=wn">News</a>,
# …]



for link in soup.find_all('a'):
    print link.text, link["href"]
# Images http://www.google.com/imghp?hl=en&tab=wi
# Maps http://maps.google.com/maps?hl=en&tab=wl
# Play https://play.google.com/?hl=en&tab=w8
# YouTube http://www.youtube.com/?tab=w1



import dryscrape
from bs4 import BeautifulSoup
session = dryscrape.Session()
session.visit("http://www.google.com")
r = session.body()
soup = BeautifulSoup(r, "lxml")


from selenium import webdriver
driver = webdriver.Chrome("/path/to/chromedriver")
driver.get("http://www.google.com")
html = driver.page_source
driver.save_screenshot("screenshot.png")
driver.quit()



import smtplib

server = smtplib.SMTP('localhost', port=1025)
server.set_debuglevel(True)
server.sendmail("me@localhost", "you@localhost", "This is an email message")
server.quit()
