var path = require('path');

module.exports = {
  entry: {
      main: `./src/shared/main.js`,
      listwindow: `./src/shared/listwindow.js`
  },
  target: process.env.TARGET,
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {verbose: true, warn: true}
        }
      },
      {
        test: require.resolve("textarea-autosize"),
        use: {
          loader: 'imports-loader',
          options: {jQuery : 'jquery'}
        }
      }
    ]
  },
  resolve: {
    extensions: ['.js', '.elm']
  },
  externals: {
    pouchdb: 'require(\'pouchdb\')'
  },
  devtool: 'source-map',
  output: {
    path: path.join(__dirname, 'dist'),
    publicPath: '/dist/',
    filename: `[name]-${process.env.TARGET}.js`
  }
}
