import os
import sys

# Import the Flask Framework
from flask import Flask, request, render_template

isDev = os.environ["SERVER_SOFTWARE"].find('Development') == 0

app = Flask( __name__ )

# Note: We don't need to call run() since our application is embedded within
# the App Engine WSGI application server.
@app.route('/')
def home():
	return render_template( 'index.html', isDev=isDev )

@app.errorhandler(404)
def page_not_found(e):
    """Return a custom 404 error."""
    return 'Sorry, Nothing at this URL.', 404


@app.errorhandler(500)
def page_not_found(e):
    """Return a custom 500 error."""
    return 'Sorry, unexpected error: {}'.format(e), 500
