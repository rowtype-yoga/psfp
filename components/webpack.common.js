"use strict";

const path = require("path");
const MonacoWebpackPlugin = require("monaco-editor-webpack-plugin");

const isWebpackDevServer = process.argv.some(
  (a) => path.basename(a) === "webpack-dev-server"
);

const isWatch = process.argv.some((a) => a === "--watch");

module.exports = {
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "bundle.js",
  },
  plugins: [
    new MonacoWebpackPlugin({
      languages: ["json"],
    }),
  ],
  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: "purs-loader",
            options: {
              src: ["src/**/*.purs", "test/**/*.purs", "stories/**/*.purs"],
              spago: true,
              watch: isWebpackDevServer || isWatch,
              pscIde: true,
            },
          },
        ],
      },
      {
        test: /\.css$/i,
        use: ["style-loader", "css-loader"],
      },
      {
        test: /\.(png|jpg|gif)$/i,
        use: [
          {
            loader: "url-loader",
            options: {
              limit: 8192,
            },
          },
        ],
      },
      {
        test: /\.svg$/,
        loader: "react-svg-loader",
      },
      {
        test: /\.(woff|woff2|eot|ttf|otf)$/,
        use: ["file-loader"],
      },
    ],
  },

  resolve: {
    modules: ["node_modules"],
    extensions: [".purs", ".js"],
  },
};
