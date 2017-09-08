const webpack = require('webpack')
const {resolve} = require('path')

const PROD = process.env.NODE_ENV === 'production'

if (!PROD) {
  require('dotenv').config()
}

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
  plugins: [
    new webpack.DefinePlugin({
      WS_API: JSON.stringify(process.env.WS_API || 'ws://localhost:3000')
    }),
    ...PROD
      ? [ new webpack.optimize.UglifyJsPlugin() ]
      : []
  ]
}
