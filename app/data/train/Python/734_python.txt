import sys
sys.path.append("helper")

import web
from helper import session


web.config.debug = False

urls = (
"/", "controller.start.index", 
"/1", "controller.start.one", 
"/2", "controller.start.two", 
)


app = web.application(urls, globals())
sessions = session.Sessions()


if __name__ == "__main__": 
	app.run()        


