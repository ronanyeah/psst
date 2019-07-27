const CopyWebpackPlugin = require("copy-webpack-plugin");
const webpack = require("webpack");
const { resolve } = require("path");

const { NODE_ENV, WS_URL, DEBUG, PORT } = process.env;

const publicFolder = resolve("./public");

const production = NODE_ENV === "production";

module.exports = {
  mode: production ? "production" : "development",
  entry: "./src/index.js",
  output: {
    path: publicFolder,
    filename: "bundle.js"
  },
  devServer: {
    contentBase: publicFolder,
    port: PORT
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          ...(production ? [] : [{ loader: "elm-hot-webpack-loader" }]),
          {
            loader: "elm-webpack-loader",
            options: {
              cwd: __dirname,
              debug: DEBUG === "true",
              optimize: production
            }
          }
        ]
      }
    ]
  },
  plugins: [
    new webpack.DefinePlugin({
      WS_URL: `"${WS_URL}"`
    }),
    new webpack.NamedModulesPlugin(),
    new webpack.NoEmitOnErrorsPlugin(),
    new CopyWebpackPlugin(["static"])
  ]
};
