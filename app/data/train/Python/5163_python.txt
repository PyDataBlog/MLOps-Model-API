# Numbers 数字
print(2 + 2)            # 4
print(50 - 5*6)         # 20
print((50 - 5*6) / 4)   # 5.0
print(8/5)              # 1.6
print(17 / 3)           # 5.666666666666667 float
print(17 // 3)          # 5 取整
print(17 % 3)           # 2 取模
print(5*3+2)            # 17 先乘除，后加减
print(2+5*3)            # 17 先乘除，后加减

print(5**2)             # 5的平方 25
print(5**3)             # 5的立方 125
print(2**7)             # 2的7次方128
print("--华丽的分割线--")

# 给变量赋值,使用“ = ”号，不需要定义变量的类型
width=50
height=10*10
print(width*height)     # 5000

# n                       # n 没有定义  NameError: name 'n' is not defined
print(4 * 3.75 - 1)

tax = 12.5 / 100
price = 100.50
print(price * tax)

# print(price+_);      #在控制台可行，但是在本文件中不行，提示ameError: name '_' is not defined
# round(_, 2)          #在控制台可行，但是在本文件中不行，提示ameError: name '_' is not defined

print("--华丽的分割线--")
# Strings 字符串
print('spam eggs')
print( 'doesn\'t')      # \' 会进行转义
print("doesn't")        # 也可使用双引号来输出，此时‘ 就不需要转义
print('"Yes," he said.')  # 被单引号包含的双引号会被当成字符处理
print("\"Yes,\" he said.") # 被双引号包含中的双银行需要转义
print('"Isn\'t," she said.') #被单引号包含的单引号需要进行转义，不是用print函数打印时'"Isn\'t," she said.'

s = 'First line.\nSecond line.'
print(s)                       # 使用print打印\n会被转义换行 ，使用命令行是\n不会被转义 First line.\nSecond line.
print("----")
print('C:\some\name')    # 这里 \n 会被转义
print(r'C:\some\name' )  # 声明 r' 后面的字符串不会被转义

print("""\
Usage: thingy [OPTIONS]
     -h                        Display this usage message
     -H hostname               Hostname to connect to
""")
                            # """...""" or '''...''' 相当于html中p标签的作用，允许多行，排列格式
                            #  不加\ : 空一行
print(3 * 'un' + 'ium')   # 字符串可以跟数字进行相乘
print('Py' 'thon')        # Python ,在同一个print方法中，用多个空格隔开，最终会拼接成一个

prefix = 'Py'
#prefix 'thon'             #不能连接变量和字符串文字

#print(('un' * 3)* 'ium')

print(prefix + 'thon')     # 字符串用+ 进行拼接

text = ('Put several strings within parentheses to have them joined together.')
print(text)


word = 'Python'
print(word[0])          # 字符串也可以当成数组来取值
print(word[5])

print(word[-1])         # 截取最后一个字符
print(word[-2])         # 截取最后第二个字符
print(word[-6])         # 截取最后第六个字符

print(word[0:2])        # 从第一个字符所在索引截取2个字符
print(word[2:5])        # 从第三个字符所在索引截取5个字符

print(word[:2] + word[2:]) #  s[:i] + s[i:] = s ，不管i为某个整数
print(word[:4] + word[4:])
print(word[:70] + word[70:])

print(word[:2])         # 从第一个字符开始取2个 字符
print(word[4:])         # 从第四个字符取到最后的 字符
print(word[-2:])        # 从最后第二个字符取到最后的 字符
print("---------");
#   +---+---+---+---+---+---+
#   | P | y | t | h | o | n |
#   +---+---+---+---+---+---+
#   0   1   2   3   4   5   6
#  -6  -5  -4  -3  -2  -1
print( word[4:42])      #从第四个字符取到最后的 字符
print(word[42:])        #从第42个字符取到最后的字符 空

# word[0] = 'J'
# word[2:] = 'py'    不允许修改字符串

print('J' + word[1:])  #可以重新拼接新字符串
print(word[:2] + 'py')

s = 'supercalifragilisticexpialidocious'
print(len(s))       # 获取字符串的长度


