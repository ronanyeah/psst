const CopyWebpackPlugin = require("copy-webpack-plugin");
const webpack = require("webpack");
const { resolve } = require("path");

const { NODE_ENV, WS_URL, REST_URL, DEBUG } = process.env;

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
          ...(production ? [] : [{ loader: "elm-hot-loader" }]),
          {
            loader: "elm-webpack-loader",
            options: {
              cwd: __dirname,
              debug: DEBUG === "true",
              warn: !production
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
