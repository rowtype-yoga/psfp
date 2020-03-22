"use strict";

var reactJss = require("react-jss");

exports.makeStylesWithPropsImpl = styles => () => props => () =>
  reactJss.createUseStyles(styles)(props);
exports.useThemeImpl = reactJss.useTheme;
