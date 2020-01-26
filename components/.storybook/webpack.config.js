const custom = require("../webpack.dev.js");
const path = require("path");

module.exports = async ({ config, mode }) => {
  return {
    ...config,
    module:
    { ...config.module
    , rules: custom.module.rules
    }
  };
};
