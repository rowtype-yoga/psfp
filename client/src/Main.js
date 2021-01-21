'use strict';

var ReactDOMServer = require('react-dom/server');
var ReactDOM = require('react-dom');

exports.isServerSide = typeof document === 'undefined';

exports.renderToString = ReactDOMServer.renderToString;

exports.createRoot = function(elem) {
  return function() {
    return ReactDOM.createRoot(elem);
  }
}

exports.renderRoot = function(component) {
  return function(root) {
    return function() {
      root.render(component)
    }
  }
}
