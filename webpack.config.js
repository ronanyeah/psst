const webpack = require('webpack')
const { resolve } = require( 'path' )

const PROD = process.env.NODE_ENV === 'production'

module.exports = {
  entry: './src/index.js',
  output: {
    path: resolve('./public/'),
    filename: 'bundle.js'
  },
  devServer: {
    contentBase: './public'
  },
  module: {
    rules: [{
      test: /\.elm$/,
      exclude: [/elm-stuff/, /node_modules/],
      use: {
        loader: 'elm-webpack-loader',
        options: {
          cwd: __dirname,
          debug: !PROD,
          warn: !PROD
        }
      }
    }]
  },
  plugins:
    PROD
      ? [ new webpack.optimize.UglifyJsPlugin() ]
      : []
}
