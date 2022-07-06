import datetime

def suffix(d):
    return 'th' if 11<=d<=13 else {1:'st',2:'nd',3:'rd'}.get(d%10, 'th')

def custom_strftime(format, t):
    return t.strftime(format).replace('{S}', str(t.day) + suffix(t.day))


print "Welcome to GenerateUpdateLines, the nation's favourite automatic update line generator."

start = int(raw_input("Enter initial day number: "))
stop = int(raw_input("Enter final day number: "))

t0 = datetime.date(2018, 3, 24)

for d in range(start, stop+1):
    date = t0 + datetime.timedelta(d-1)

    print "| "+str(d)+" | "+custom_strftime("%a {S} %B", date)+" |  |  |"



# from datetime import datetime as dt
#
# def suffix(d):
#     return 'th' if 11<=d<=13 else {1:'st',2:'nd',3:'rd'}.get(d%10, 'th')
#
# def custom_strftime(format, t):
#     return t.strftime(format).replace('{S}', str(t.day) + suffix(t.day))
#
# print custom_strftime('%B {S}, %Y', dt.now())
