"use strict";

var reactJss = require("react-jss");

exports.makeStylesWithPropsImpl = (styles) => () => {
  const useStyles = reactJss.createUseStyles(styles);
  // For injection order this needs to be run now!
  return (props) => () => useStyles(props);
};
exports.useThemeImpl = reactJss.useTheme;
