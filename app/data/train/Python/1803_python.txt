#!/usr/bin/env python
# -*- coding: utf-8 -*-
from time import gmtime, strftime
import ephem
import wx.calendar
# Test
# here = ephem.Observer()
# here.lat = '-17.576166667'
# here.lon = '-149.618575000'


class App(wx.App):
	def OnInit(self):
		self.frame = MyFrame("Lunacy", (50, 60), (640, 220))
		self.frame.Show()
		self.SetTopWindow(self.frame)
		return True


##########################################################################
## Class MyFrame
###########################################################################

class MyFrame(wx.Frame):
	def __init__(self, title, pos, size):
		wx.Frame.__init__(self, None, -1, title, pos, size)
		path = "/usr/share/pixmaps/pidgin/emotes/default/moon.png"
		icon = wx.Icon(path, wx.BITMAP_TYPE_PNG)
		self.SetIcon(icon)

		self.SetSizeHintsSz(wx.Size(640, 220), wx.DefaultSize)

		gSizer1 = wx.GridSizer(1, 2, 0, 0)

		fgSizer1 = wx.FlexGridSizer(1, 1, 0, 0)
		fgSizer1.SetFlexibleDirection(wx.BOTH)
		fgSizer1.SetNonFlexibleGrowMode(wx.FLEX_GROWMODE_SPECIFIED)

		cal = wx.calendar.CalendarCtrl(self, wx.ID_ANY, wx.DefaultDateTime, wx.DefaultPosition, wx.DefaultSize,
		                               wx.calendar.CAL_SHOW_HOLIDAYS |
		                               wx.calendar.CAL_SHOW_SURROUNDING_WEEKS |
		                               wx.calendar.CAL_SUNDAY_FIRST |
		                               wx.SUNKEN_BORDER, u"Date of Lunacy")
		self.cal = cal
		self.cal.SetFont(wx.Font(12, 74, 90, 90, False, "Sans"))
		self.cal.SetToolTipString(u"Date for Next Event")
		self.cal.SetHelpText(u"Renders Lunar/Solar events for the date.")

		self.Bind(wx.calendar.EVT_CALENDAR_SEL_CHANGED, self.OnDateSelect, id=cal.GetId())

		fgSizer1.Add(self.cal, 0, wx.ALIGN_CENTER_VERTICAL | wx.ALIGN_RIGHT | wx.ALL, 5)

		fgSizer1.AddSpacer(( 0, 5), 1, wx.EXPAND, 5)

		gSizer1.Add(fgSizer1, 1, 0, 0)

		fgSizer2 = wx.FlexGridSizer(8, 3, 3, 0)
		fgSizer2.SetFlexibleDirection(wx.HORIZONTAL)
		fgSizer2.SetNonFlexibleGrowMode(wx.FLEX_GROWMODE_SPECIFIED)
		fgSizer2.SetMinSize(wx.Size(-1, 220))

		self.staticText_Moonrise = wx.StaticText(self, wx.ID_ANY, u"Moonrise", wx.DefaultPosition, wx.DefaultSize, 0)
		self.staticText_Moonrise.Wrap(-1)
		self.staticText_Moonrise.SetFont(wx.Font(12, 74, 90, 90, False, "Sans"))

		fgSizer2.Add(self.staticText_Moonrise, 0, 0, 5)

		self.mrtime = wx.StaticText(self, wx.ID_ANY, u"next rise", wx.DefaultPosition, wx.DefaultSize, 0)
		self.mrtime.Wrap(-1)
		fgSizer2.Add(self.mrtime, 0, 0, 5)

		self.mraz = wx.StaticText(self, wx.ID_ANY, u"azimuth", wx.DefaultPosition, wx.DefaultSize, 0)
		self.mraz.Wrap(-1)
		fgSizer2.Add(self.mraz, 0, 0, 5)

		self.staticText_Moonset = wx.StaticText(self, wx.ID_ANY, u"Moonset", wx.DefaultPosition, wx.DefaultSize, 0)
		self.staticText_Moonset.Wrap(-1)
		self.staticText_Moonset.SetFont(wx.Font(12, 74, 90, 90, False, "Sans"))

		fgSizer2.Add(self.staticText_Moonset, 0, 0, 10)

		self.mstime = wx.StaticText(self, wx.ID_ANY, u"next set", wx.DefaultPosition, wx.DefaultSize, 0)
		self.mstime.Wrap(-1)
		fgSizer2.Add(self.mstime, 0, 0, 5)

		self.msaz = wx.StaticText(self, wx.ID_ANY, u"azimuth", wx.DefaultPosition, wx.DefaultSize, 0)
		self.msaz.Wrap(-1)
		fgSizer2.Add(self.msaz, 0, 0, 5)

		self.staticText_Phase = wx.StaticText(self, wx.ID_ANY, u"Phase", wx.DefaultPosition, wx.DefaultSize, 0)
		self.staticText_Phase.Wrap(-1)
		self.staticText_Phase.SetFont(wx.Font(12, 74, 90, 90, False, "Sans"))

		fgSizer2.Add(self.staticText_Phase, 0, 0, 10)

		self.moonphase = wx.StaticText(self, wx.ID_ANY, u"moonphase", wx.DefaultPosition, wx.DefaultSize, 0)
		self.moonphase.Wrap(-1)
		fgSizer2.Add(self.moonphase, 0, 0, 5)

		self.phasepercent = wx.StaticText(self, wx.ID_ANY, u"% illuminated", wx.DefaultPosition, wx.DefaultSize, 0)
		self.phasepercent.Wrap(-1)
		fgSizer2.Add(self.phasepercent, 0, 0, 5)

		self.staticText_NewMoon = wx.StaticText(self, wx.ID_ANY, u"New Moon   ", wx.DefaultPosition, wx.DefaultSize,
		                                        wx.ST_NO_AUTORESIZE)
		self.staticText_NewMoon.Wrap(-1)
		self.staticText_NewMoon.SetFont(wx.Font(12, 74, 90, 90, False, "Sans"))

		fgSizer2.Add(self.staticText_NewMoon, 0, 0, 10)

		self.newmoondate = wx.StaticText(self, wx.ID_ANY, u"next new moon", wx.DefaultPosition, wx.DefaultSize, 0)
		self.newmoondate.Wrap(-1)
		fgSizer2.Add(self.newmoondate, 0, 0, 10)

		self.newmoonhour = wx.StaticText(self, wx.ID_ANY, u"hour", wx.DefaultPosition, wx.DefaultSize, 0)
		self.newmoonhour.Wrap(-1)
		fgSizer2.Add(self.newmoonhour, 0, 0, 10)

		self.staticText_FullMoon = wx.StaticText(self, wx.ID_ANY, u"Full Moon", wx.DefaultPosition, wx.DefaultSize, 0)
		self.staticText_FullMoon.Wrap(-1)
		self.staticText_FullMoon.SetFont(wx.Font(12, 74, 90, 90, False, "Sans"))

		fgSizer2.Add(self.staticText_FullMoon, 0, 0, 10)

		self.fullmoondate = wx.StaticText(self, wx.ID_ANY, u"next full moon", wx.DefaultPosition, wx.DefaultSize, 0)
		self.fullmoondate.Wrap(-1)
		fgSizer2.Add(self.fullmoondate, 0, 0, 5)

		self.fullmoonhour = wx.StaticText(self, wx.ID_ANY, u"hour", wx.DefaultPosition, wx.DefaultSize, 0)
		self.fullmoonhour.Wrap(-1)
		fgSizer2.Add(self.fullmoonhour, 0, 0, 5)

		self.staticText_Sunrise = wx.StaticText(self, wx.ID_ANY, u"Sunrise", wx.DefaultPosition, wx.DefaultSize, 0)
		self.staticText_Sunrise.Wrap(-1)
		self.staticText_Sunrise.SetFont(wx.Font(12, 74, 90, 90, False, "Sans"))

		fgSizer2.Add(self.staticText_Sunrise, 0, 0, 10)

		self.srtime = wx.StaticText(self, wx.ID_ANY, u"next rise", wx.DefaultPosition, wx.DefaultSize, 0)
		self.srtime.Wrap(-1)
		fgSizer2.Add(self.srtime, 0, 0, 5)

		self.sraz = wx.StaticText(self, wx.ID_ANY, u"azimuth", wx.DefaultPosition, wx.DefaultSize, 0)
		self.sraz.Wrap(-1)
		fgSizer2.Add(self.sraz, 0, 0, 5)

		self.staticText_SolarNoon = wx.StaticText(self, wx.ID_ANY, u"High Noon", wx.DefaultPosition, wx.DefaultSize, 0)
		self.staticText_SolarNoon.Wrap(-1)
		self.staticText_SolarNoon.SetFont(wx.Font(12, 74, 90, 90, False, "Sans"))

		fgSizer2.Add(self.staticText_SolarNoon, 0, 0, 10)

		self.sntime = wx.StaticText(self, wx.ID_ANY, u"solar noon", wx.DefaultPosition, wx.DefaultSize, 0)
		self.sntime.Wrap(-1)
		fgSizer2.Add(self.sntime, 0, 0, 5)

		self.snaltitude = wx.StaticText(self, wx.ID_ANY, u"altitude", wx.DefaultPosition, wx.DefaultSize, 0)
		self.snaltitude.Wrap(-1)
		fgSizer2.Add(self.snaltitude, 0, 0, 5)

		self.staticText_Sunset = wx.StaticText(self, wx.ID_ANY, u"Sunset", wx.DefaultPosition, wx.DefaultSize, 0)
		self.staticText_Sunset.Wrap(-1)
		self.staticText_Sunset.SetFont(wx.Font(12, 74, 90, 90, False, "Sans"))

		fgSizer2.Add(self.staticText_Sunset, 0, 0, 10)

		self.sstime = wx.StaticText(self, wx.ID_ANY, u"next set", wx.DefaultPosition, wx.DefaultSize, 0)
		self.sstime.Wrap(-1)
		fgSizer2.Add(self.sstime, 0, 0, 5)

		self.ssaz = wx.StaticText(self, wx.ID_ANY, u"azimuth", wx.DefaultPosition, wx.DefaultSize, 0)
		self.ssaz.Wrap(-1)
		fgSizer2.Add(self.ssaz, 0, 0, 5)

		gSizer1.Add(fgSizer2, 1, wx.TOP, 5)

		self.SetSizer(gSizer1)
		self.Layout()

		self.Centre(wx.BOTH)

	def __del__(self):
		pass

	def OnDateSelect(self, evt):

		f = open(r'/etc/nx.lat')  # Lat/lon files for Navigatrix
		lat = f.readline(12)
		f.close()

		f = open(r'/etc/nx.lon')
		lon = f.readline(12)
		f.close()

		lat = float(lat)
		lon = float(lon)

		degrees = int(lat)
		mnn = (lat - degrees) * 60
		minutes = int(mnn)
		seconds = round(((mnn - minutes) * 60), 3)
		lat = str(degrees) + str(minutes) + str(seconds)

		degrees = int(lon)
		mnn = (lon - degrees) * 60
		minutes = int(mnn)
		seconds = round(((mnn - minutes) * 60), 3)
		lon = str(degrees) + str(minutes) + str(seconds)

		here = ephem.Observer()
		here.lat = lat
		here.lon = lon

		here.pressure = 0  # barometric pressure not factored
		here.horizon = '-0:34'  # fudge factor from the US Navel Observatory
		here.elevation = 2.0  # 2 Meters elevation
		here.temp = 25.0  # and a balmy 25 degrees

		cal = evt.GetEventObject()

		year = (str(self.cal.GetDate().GetYear()))
		month = (str(self.cal.GetDate().GetMonth() + 1))
		day = (str(self.cal.GetDate().GetDay()))
		hour = strftime("%H:%M:%S", gmtime())

		datefig = year + '/' + month + '/' + day + ' ' + hour

		here.date = datefig

		sun = ephem.Sun(here)
		moon = ephem.Moon(here)

		moon.compute(here)

		#
		# Moon Rise
		#
		#        mrtime = str(here.next_rising(moon))

		mrtime = here.next_rising(moon)
		lt = ephem.localtime(mrtime)
		mrtime = str(lt).split()
		mrtime = mrtime[1].split(".")
		self.mrtime.SetLabel(str(mrtime[0]))

		mraz = str(moon.az).partition(':')
		self.mraz.SetLabel(str(mraz[0]) + u'\u00B0 from North')
		#
		# Moonset        moon.compute(here)
		#
		#
		mstime = here.next_setting(moon)
		lt = ephem.localtime(mstime)
		mstime = str(lt).split()
		mstime = mstime[1].split(".")
		self.mstime.SetLabel(mstime[0])

		msaz = str(moon.az).partition(':')
		self.msaz.SetLabel(str(msaz[0]) + u'\u00B0 from North')

		#
		# Moon Phase
		# TODO Clearly these numbers are pulled out of a hat.
		# they are a very rough approximation of the phases and
		# do not account for waxing and waning
		phasepercent = int(moon.moon_phase * 100)
		self.phasepercent.SetLabel(str(phasepercent) + " %")

		if phasepercent <= 2.0:
			moonphase = "New Moon"
		if 2.1 < phasepercent <= 20.0:
			moonphase = "Crescent"
		if 20.1 < phasepercent <= 60.0:
			moonphase = "Quarter Moon"
		if 60.1 < phasepercent <= 95.0:
			moonphase = "Gibbous"
		if phasepercent > 95.1:
			moonphase = "Full Moon"
		self.moonphase.SetLabel(moonphase)
		#
		# New Moon Date
		#
		newmoondate = ephem.next_new_moon(datefig)
		lt = ephem.localtime(newmoondate)
		newmoondate = str(lt).split()
		newmoonhour = newmoondate[1].split(".")

		self.newmoondate.SetLabel(str(newmoondate[0]))
		self.newmoonhour.SetLabel(str(newmoonhour[0]))
		#
		# Full Moon Date
		#
		fullmoondate = ephem.next_full_moon(datefig)
		lt = ephem.localtime(fullmoondate)
		fullmoondate = str(lt).split()
		fullmoonhour = fullmoondate[1].split(".")

		self.fullmoondate.SetLabel(str(fullmoondate[0]))
		self.fullmoonhour.SetLabel(str(fullmoonhour[0]))
		#
		# Sun Rise
		#

		sun.compute(here)

		srtime = here.next_rising(sun)
		lt = ephem.localtime(srtime)
		srtime = str(lt).split()
		srtime = srtime[1].split(".")
		self.srtime.SetLabel(srtime[0])

		sraz = str(sun.az).partition(':')
		self.sraz.SetLabel(str(sraz[0]) + u'\u00B0 from North')
		#
		# High Noon
		#
		sntime = here.next_transit(sun)
		lt = ephem.localtime(sntime)
		sntime = str(lt).split()
		sntime = sntime[1].split(".")
		self.sntime.SetLabel(sntime[0])

		snaltitude = str(sun.alt).partition(':')
		self.snaltitude.SetLabel(str(snaltitude[0]) + u'\u00B0 above Horizon')
		#
		# Sun Set
		#
		sstime = here.next_setting(sun)
		lt = ephem.localtime(sstime)
		sstime = str(lt).split()
		sstime = sstime[1].split(".")
		self.sstime.SetLabel(sstime[0])

		ssaz = str(sun.az).partition(':')
		self.ssaz.SetLabel(str(ssaz[0]) + u'\u00B0 from North')


if __name__ == '__main__':
	app = App()
	app.MainLoop()

