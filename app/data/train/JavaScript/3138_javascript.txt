import Express from 'express';
import compression from 'compression';
import mongoose from 'mongoose';
import bodyParser from 'body-parser';
import path from 'path';
import IntlWrapper from '../client/modules/Intl/IntlWrapper';

// Webpack Requirements
import webpack from 'webpack';
import config from '../webpack.config.dev';
import webpackDevMiddleware from 'webpack-dev-middleware';
import webpackHotMiddleware from 'webpack-hot-middleware';

// Initialize the Express App
const app = new Express();

// Run Webpack dev server in development mode
if (process.env.NODE_ENV === 'development') {
  const compiler = webpack(config);
  app.use(webpackDevMiddleware(compiler, { noInfo: true, publicPath: config.output.publicPath }));
  app.use(webpackHotMiddleware(compiler));
}

// React And Redux Setup
import { configureStore } from '../client/store';
import { Provider } from 'react-redux';
import React from 'react';
import { renderToString } from 'react-dom/server';
import { match, RouterContext } from 'react-router';
import Helmet from 'react-helmet';

// Import required modules
import routes from '../client/routes';
import { fetchComponentData } from './util/fetchData';
import posts from './routes/post.routes';
import medicalRights from './routes/medicalrights.routes';
import populateDB from './routes/populateDB.routes';
import dummyData from './dummyData';
import serverConfig from './config';

// Set native promises as mongoose promise
mongoose.Promise = global.Promise;

// MongoDB Connection
mongoose.connect(serverConfig.mongoURL, (error) => {
  if (error) {
    console.error('Please make sure Mongodb is installed and running!'); // eslint-disable-line no-console
    throw error;
  }

  // feed some dummy data in DB.
  //dummyData();
});

// Apply body Parser and server public assets and routes
app.use(compression());
app.use(bodyParser.json({ limit: '20mb' }));
app.use(bodyParser.urlencoded({ limit: '20mb', extended: false }));
app.use(Express.static(path.resolve(__dirname, '../dist')));
app.use('/api', posts);
app.use('/api', medicalRights);
app.use('/api', populateDB);

// Render Initial HTML
const renderFullPage = (html, initialState) => {
  const head = Helmet.rewind();

  // Import Manifests
  const assetsManifest = process.env.webpackAssets && JSON.parse(process.env.webpackAssets);
  const chunkManifest = process.env.webpackChunkAssets && JSON.parse(process.env.webpackChunkAssets);

  return `
    <!doctype html>
    <html>
      <head>
        ${head.base.toString()}
        ${head.title.toString()}
        ${head.meta.toString()}
        ${head.link.toString()}
        ${head.script.toString()}

        ${process.env.NODE_ENV === 'production' ? `<link rel='stylesheet' href='${assetsManifest['/app.css']}' />` : ''}
        <link href='https://fonts.googleapis.com/css?family=Lato:400,300,700' rel='stylesheet' type='text/css'/>
        <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/latest/css/bootstrap.min.css" type='text/css'>
        <link rel="shortcut icon" href="http://res.cloudinary.com/hashnode/image/upload/v1455629445/static_imgs/mern/mern-favicon-circle-fill.png" type="image/png" />
        <style>
  /* local page styles */
  html h1 {
    font-size: 26px;
    margin-left: 10px;
  }
  html h2 {
    font-size: 22px;
    margin-left: 10px;
  }
  html h3 {
    font-size: 14px;
    margin-left: 10px;
  }
  html h4 {
    font-size: 16px;
  }
  .progtrckr {
    text-align: center;
    padding-bottom: 16px;
    // border-bottom: solid 1px;
  }
  .progtrckr li {
    margin-bottom: 10px;
  }
  .val-err-tooltip {
    background-color: red;
    padding: 3px 5px 3px 10px;
    font-size: 14px;
    color: #fff;
  }
  .step {
    // background-color: #ccc;
    border:1px solid #e5e5e5;
    min-height: 437px;
    padding: 10px;
    max-width: 815px;
  }
  html .row, html .form-horizontal .form-group {
    margin: 0;
  }
  .footer-buttons {
    margin-top: 10px;
    margin-bottom: 50px;
  }
  html .step3 label, html .step4 label {
    font-size: 20px;
    text-align: left;
  }
  html .form-horizontal .control-label {
    text-align: left;
  }
  .review .txt {
    font-size: 20px;
    text-align: left;
    margin: 0;
    padding: 0;
  }
  html body .saving {
    background-color: #5cb85c;
    width: 90%;
    padding: 5px;
    font-size: 16px;
  }
  code {
    position: relative;
    left: 12px;
    line-height: 25px;
  }
  .eg-jump-lnk {
    margin-top: 50px;
    font-style: italic;
  }
  .lib-version {
    font-size: 12px;
    background-color: rgba(255, 255, 0, 0.38);
    position: absolute;
    right: 10px;
    top: 10px;
    padding: 5px;
  }
  html .content {
    margin-left: 10px;
  }
  span.red {
    color: #d9534f;
  }
  span.green {
    color: #3c763d;
  }
  span.bold {
    font-weight: bold;
  }
  html .hoc-alert {
    margin-top: 20px;
  }
  html .form-block-holder {
    margin-top: 20px !important;
  }
  ol.progtrckr {
  margin: 0;
  padding-bottom: 2.2rem;
  list-style-type: none;
}
ol.progtrckr li {
  display: inline-block;
  text-align: center;
  line-height: 4.5rem;
  padding: 0 0.7rem;
  cursor: pointer;
}
ol.progtrckr li span {
  padding: 0 1.5rem;
}
@media (max-width: 650px) {
  .progtrckr li span {
    display: none;
  }
}
.progtrckr em {
  display: none;
  font-weight: 700;
  padding-left: 1rem;
}
@media (max-width: 650px) {
  .progtrckr em {
    display: inline;
  }
  border-bottom: solid 1px;
}

@media (max-width: 650px) {
 .step {
    max-height=320px;
    min-height=437px;
    min-width=300px;
  }
}





}

ol.progtrckr li.progtrckr-todo {
  color: silver;
  border-bottom: 4px solid silver;
}
ol.progtrckr li.progtrckr-doing {
  color: black;
  border-bottom: 4px solid #33C3F0;
}
ol.progtrckr li.progtrckr-done {
  color: black;
  border-bottom: 4px solid #33C3F0;
}
ol.progtrckr li:after {
  content: "\\00a0\\00a0";
}
ol.progtrckr li:before {
  position: relative;
  bottom: -3.7rem;
  float: left;
  left: 50%;
}
ol.progtrckr li.progtrckr-todo:before {
  content: "\\039F";
  color: silver;
  background-color: white;
  width: 1.2em;
  line-height: 1.4em;
}
ol.progtrckr li.progtrckr-todo:hover:before {
  color: #0FA0CE;
}

ol.progtrckr li.progtrckr-doing:before {
  content: "\\2022";
  color: white;
  background-color: #33C3F0;
  width: 1.2em;
  line-height: 1.2em;
  border-radius: 1.2em;
}
ol.progtrckr li.progtrckr-doing:hover:before {
  color: #0FA0CE;
}

ol.progtrckr li.progtrckr-done:before {
  content: "\\2713";
  color: white;
  background-color: #33C3F0;
  width: 1.2em;
  line-height: 1.2em;
  border-radius: 1.2em;
}
ol.progtrckr li.progtrckr-done:hover:before {
  color: #0FA0CE;
}

  </style>
      </head>
      <body>
        <div id="root">${html}</div>
        <script>
          window.__INITIAL_STATE__ = ${JSON.stringify(initialState)};
          ${process.env.NODE_ENV === 'production' ?
          `//<![CDATA[
          window.webpackManifest = ${JSON.stringify(chunkManifest)};
          //]]>` : ''}
        </script>
        <script src='${process.env.NODE_ENV === 'production' ? assetsManifest['/vendor.js'] : '/vendor.js'}'></script>
        <script src='${process.env.NODE_ENV === 'production' ? assetsManifest['/app.js'] : '/app.js'}'></script>
      </body>
    </html>
  `;
};

const renderError = err => {
  const softTab = '&#32;&#32;&#32;&#32;';
  const errTrace = process.env.NODE_ENV !== 'production' ?
    `:<br><br><pre style="color:red">${softTab}${err.stack.replace(/\n/g, `<br>${softTab}`)}</pre>` : '';
  return renderFullPage(`Server Error${errTrace}`, {});
};

// Server Side Rendering based on routes matched by React-router.
app.use((req, res, next) => {
  match({ routes, location: req.url }, (err, redirectLocation, renderProps) => {
    if (err) {
      return res.status(500).end(renderError(err));
    }

    if (redirectLocation) {
      return res.redirect(302, redirectLocation.pathname + redirectLocation.search);
    }

    if (!renderProps) {
      return next();
    }

    const store = configureStore();

    return fetchComponentData(store, renderProps.components, renderProps.params)
      .then(() => {
        const initialView = renderToString(
          <Provider store={store}>
            <IntlWrapper>
              <RouterContext {...renderProps} />
            </IntlWrapper>
          </Provider>
        );
        const finalState = store.getState();

        res
          .set('Content-Type', 'text/html')
          .status(200)
          .end(renderFullPage(initialView, finalState));
      })
      .catch((error) => next(error));
  });
});

// start app
app.listen(serverConfig.port, (error) => {
  if (!error) {
    console.log(`MERN is running on port: ${serverConfig.port}! Build something amazing!`); // eslint-disable-line
  }
});

export default app;
