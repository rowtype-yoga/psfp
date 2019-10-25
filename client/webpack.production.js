'use strict';
const merge = require('webpack-merge');
const common = require('./webpack.common.js');
const path = require('path');

module.exports = merge(common, {
  entry: './src/entrypoint.js',
  mode: 'production'
});
