const MonacoWebpackPlugin = require(`monaco-editor-webpack-plugin`);
exports.onCreateWebpackConfig = ({ actions }) => {
  actions.setWebpackConfig({
    plugins: [
      new MonacoWebpackPlugin()
    ]
  });
};