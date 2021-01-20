const path = require('path');
const webpack = require('webpack');

module.exports = {
  stories: ['../output/Stories.*/**/index.js'],
  addons: [],
  webpackFinal: async config => {
    //   config.plugins.push(new webpack.SourceMapDevToolPlugin({
    //     append: '\n//# sourceMappingURL=./[url]',
    //   }))

    // do mutation to the config
    return config;
  },
};
