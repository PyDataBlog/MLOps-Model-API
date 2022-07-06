#! /usr/bin/env python3

'''
given a list of stock price ticks for the day, can you tell me what
trades I should make to maximize my gain within the constraints of the
market? Remember - buy low, sell high, and you can't sell before you
buy.

Sample Input
19.35 19.30 18.88 18.93 18.95 19.03 19.00 18.97 18.97 18.98

'''

import argparse


def parse_args():
    parser = argparse.ArgumentParser(description='easy 249')
    parser.add_argument('stock_prices', action='store', nargs='+',
                        help='prices of a given stock')
    return parser.parse_args()


def stock(stock_prices):
    buy_day = 0
    max_profit = 0
    max_buy = 0
    max_sell = 0
    for buy_day in range(len(stock_prices) - 2):
        # maybe do a max(here)
        for sell_day in range(buy_day + 2, len(stock_prices)):
            profit = stock_prices[sell_day] - stock_prices[buy_day]
            if profit > max_profit:
                max_profit = profit
                max_buy = buy_day
                max_sell = sell_day
    print("max profit: %.2f from buy on day %d at %.2f sell on day %d at %.2f" %
          (max_profit, max_buy, stock_prices[max_buy], max_sell, stock_prices[max_sell]))


if __name__ == '__main__':
    args = parse_args()
    stock([float(price) for price in args.stock_prices])
