import sys

def main():
    with open(sys.argv[1]) as input_file:
        for line in input_file.readlines():
            input_list = list(reversed(line.strip().split(' ')))
            index = int(input_list[0])
            del input_list[0]

            print(input_list[index - 1])

if __name__ == '__main__':
    main()
