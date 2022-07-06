# can_openner
Data Scraper for FIPS, ZIP, and Area Codes

## What does Can Openner do?
It *can open* data. (Yeah, bad pun.)  One of the crazy things I've run into when going to website like [UnitedStatesZipCodes.org](http://www.unitedstateszipcodes.org/) or [AllAreacodes.com](http://www.allareacodes.com/) is they give you their website data, but will charge $40 to get it.  Keep in mind this is free public data posted online.

Can Openner doesn't give you their data but a means to get their data.  Just as there is a package on for Linux for your to get the Microsoft fonts, which their isn't a package that contains the fonts but gets them for your, Can Openner works on the same legitimate rules.  You get the data using a script. Nobody gets in trouble for infringement.

## Why call it Can Openner?
The icon commonly associated with a database looks like a metal can.  If I ever get around to drawing some icons or art for this program, it would probably be either a can opener or a cat eating tuna out of a can.  (Note: If you have a kitty, don't use the can openers that cut the lid open from the top, use the ones that open the can from the side.  It's dangerous to feed kitty straight from the can! Put it in a bowl or a saucer.)

## Why FIPS instead of INCITS?
Despite [Federal Information Processing Standards](https://en.wikipedia.org/wiki/Federal_Information_Processing_Standards) (FIPS) being phased out in favor of [International Committee for Information Technology Standards](https://en.wikipedia.org/wiki/International_Committee_for_Information_Technology_Standards) (INCITS), INCITS have not been formally published for the public.  A bit of hackery needs to be done to get them online.  Who knows, maybe WikiLeaks has them.  Regardless, it is rediculous that INCITS is not as open as FIPS.

However, there are a lot of places where FIPS codes are still used such as [Specific Area](https://en.wikipedia.org/wiki/Specific_Area_Message_Encoding) such as NOAA Weather Radio, the US Census Bureau's American Fact Finder, and the US Bureau of Labor Statistics.

## Why are some data sets not being fetched?
Sites like Canada Post and the National Weather Service recently revamped their websites, but a lot of old links are broken.  I could try using the Wayback Machine to fetch archives but there is not guarantee that they have the data.  I was looking to including Canadian postal codes since a lot of the area code information included the area codes for Canada.

## What would you do with all this data?
I'd really like to use it for projects like Open Street Maps or whatever can create KML files that Google Earth uses.  These sets probably exist already but for the most part, I focus on not giving this information but HOW to get this information.

As they saying goes: "Give a man a fish, you feed him for a day. Teach a man how to fish, you feed him for a lifetime."
