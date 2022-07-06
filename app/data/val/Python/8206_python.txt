
#!/usr/bin/env python


def makeYqlQuery(req):

    result = req.get("result")

    parameters = result.get("parameters")

    city = parameters.get("geo-city")

    if city is None:

        return None

    return "select * from weather.forecast where woeid in (select woeid from geo.places(1) where text='" + city + "') and u='c'"
