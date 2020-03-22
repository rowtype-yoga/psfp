"use strict";

var reactJss = require("react-jss");

exports.mkThemeProviderImpl = function() {
  return reactJss.ThemeProvider;
};
