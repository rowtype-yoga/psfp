"use strict";

var ReactDOMServer = require("react-dom/server");

exports.isServerSide = typeof document === "undefined";

exports.renderToString = ReactDOMServer.renderToString;
