import express = require('express');
import path = require('path');

import BookRoutes = require('../routes/BookRoutes');
import FeaturedBooksRoutes = require('../routes/FeaturedBooksRoutes');

var app = express();

class Routes {
    get routes() {
        app.use("/", new BookRoutes().routes);
        app.use("/", new FeaturedBooksRoutes().routes);
        return app;
    }
}
export = Routes;