package model

import org.joda.time.DateTime

case class Ticker (tradePair: TradePair, time: DateTime, interval: Interval, open: Price, high: Price, low: Price, close: Price, volume: Volume)
