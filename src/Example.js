'use strict';

var ReactDOMServer = require('react-dom/server');

exports.isServerSide = typeof document === 'undefined';

console.log('image from Example.js', require('./logo.png'))

exports.renderToString = ReactDOMServer.renderToString;

