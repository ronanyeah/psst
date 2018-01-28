const CopyWebpackPlugin = require("copy-webpack-plugin");
const webpack = require("webpack");
const { resolve } = require("path");

const { NODE_ENV, WS_URL, REST_URL, DEBUG } = process.env;

const publicFolder = resolve("./public");

module.exports = {
  entry: "./src/index.js",
  output: {
    path: publicFolder,
    filename: "bundle.js"
  },
  devServer: {
    contentBase: publicFolder
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        use: {
          loader: "babel-loader",
          options: {
            presets: ["@babel/preset-env"]
          }
        }
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          ...(NODE_ENV === "development" ? [{ loader: "elm-hot-loader" }] : []),
          {
            loader: "elm-webpack-loader",
            options: {
              cwd: __dirname,
              debug: DEBUG === "true",
              warn: NODE_ENV === "development"
            }
          }
        ]
      }
    ]
  },
  plugins: [
    new webpack.DefinePlugin({
      WS_URL: `"${WS_URL}"`,
      REST_URL: `"${REST_URL}"`
    }),
    new webpack.NamedModulesPlugin(),
    new webpack.NoEmitOnErrorsPlugin(),
    new CopyWebpackPlugin(["static"])
  ]
};
