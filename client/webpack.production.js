"use strict";
const merge = require("webpack-merge");
const common = require("./webpack.common.js");
const path = require("path");

module.exports = merge(common, {
  entry: "./src/entrypoint.js",
  resolve: {
    alias: {
      // Only resolve react and dom once
      react: path.resolve("./node_modules/react"),
      "react-dom": path.resolve("./node_modules/react-dom")
    }
  },
  mode: "production"
});
