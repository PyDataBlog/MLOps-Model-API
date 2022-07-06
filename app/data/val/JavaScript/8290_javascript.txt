module.exports = {
  entry: {
    main: ['babel-polyfill', './lib/index.js'],
    test: ['babel-polyfill', 'mocha!./test/index.js'],
  },
  output: {
    path: __dirname,
    filename: '[name].bundle.js',
  },
  module: {
    loaders: [
      {
        test: /\.jsx?$/,
        exclude: /(node_modules|bower_components)/,
        loader: 'babel',
        query: {
          presets: ['es2015', 'react'],
        },
      },
      { test: /\.css$/, loader: 'style!css' },
      { test: /\.scss$/, loader: 'style!css!sass' },
      { test: /\.svg/, loader: 'svg-url-loader'},
      { test: /\.png$/, loader: "url-loader", query: { mimetype: "image/png" },
      },
      { test: /\.(jpe?g|png|gif|svg)$/i, loaders: [
        'file?hash=sha512&digest=hex&name=[hash].[ext]',
        'image-webpack?bypassOnDebug&optimizationLevel=7&interlaced=false']
      }
    ],
  },
  resolve: {
    extensions: ['', '.js', '.jsx', '.json', '.scss', '.css'],
  },
};
