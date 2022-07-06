// bundles everything except TS files which will be built by rollup.

// webpack stuff
var webpack = require('webpack');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var ExtractTextPlugin = require('extract-text-webpack-plugin');
var failPlugin = require('webpack-fail-plugin');
var helpers = require('./helpers');

const basePlugins = [
  new webpack.DefinePlugin({
    'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
  }),
  new webpack.optimize.CommonsChunkPlugin('globals'),
  new HtmlWebpackPlugin({
    template: 'index.template.html',
    favicon: 'favicon.ico'
  }),
  new ExtractTextPlugin("styles.css"),
  failPlugin
];

const devPlugins = [
  new webpack.optimize.UglifyJsPlugin({
    compress: { warnings: false }
  })
];

const plugins = basePlugins
  .concat((process.env.NODE_ENV === 'development') ? devPlugins: []);

module.exports = {
  entry: {
    globals: [
      'core-js',
      'zone.js',
      'reflect-metadata'
    ]
  },

  output: {
    path: helpers.root(''),
    publicPath: '',
    filename: '[name].js',
    chunkFilename: '[id].chunk.js'
  },

  resolve: {
    extensions: [
      '.webpack.js', '.web.js', '.js', '.html'
    ]
  },

  module: {

    loaders:  [
      { test: /\.json$/, loader: 'json-loader' },
      { test: /\.html$/, loader: 'raw' },
      { test: /\.(ico)$/, loader: 'file' },
      { test: /\.(png|jpe?g|gif)$/, loader: 'file', query: {name: 'assets/[name].[hash].[ext]'} },
      { test: /\.css$/, exclude: helpers.root('src', 'app'), loader: ExtractTextPlugin.extract({
        fallbackLoader: "style-loader",
        loader: "css-loader"
      }) },
      { test: /\.css$/, include: helpers.root('src', 'app'), loader: 'raw' },
      { test: /\.woff(2)?(\?v=\d+\.\d+\.\d+)?$/, loader: 'url', query: {limit: '10000', mimetype: 'application/font-woff'} },
      { test: /\.ttf(\?v=\d+\.\d+\.\d+)?$/, loader: 'url', query: {limit: '10000', mimetype: 'application/octet-stream'} },
      { test: /\.eot(\?v=\d+\.\d+\.\d+)?$/, loader: 'file' },
      { test: /\.svg(\?v=\d+\.\d+\.\d+)?$/, loader: 'url', query: {limit: '10000', mimetype: 'image/svg+xml'} }
    ]
  },

  plugins: plugins
};


// gulp tasks
var gulp = require('gulp');
var watch = require('gulp-watch');
var batch = require('gulp-batch');

gulp.task('build', function () { 
  console.log('build Working!'); 
});

gulp.task('watch', function () {
    watch('**/*.ts', batch(function (events, done) {
        gulp.start('build', done);
    }));
});